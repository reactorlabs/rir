#include "../pir/pir_impl.h"
#include "../transform/bb.h"
#include "../translations/rir_compiler.h"
#include "../util/cfg.h"
#include "../util/visitor.h"
#include "R/Funtab.h"
#include "R/Symbols.h"
#include "R/r.h"
#include "pass_definitions.h"

#include <unordered_set>

namespace {

using namespace rir::pir;

static LdConst* isConst(Value* instr) {
    instr = instr->followCastsAndForce();
    if (auto cst = LdConst::Cast(instr)) {
        return cst;
    }
    return nullptr;
}

#define FOLD_BINARY_NATIVE(Instruction, Operation)                             \
    do {                                                                       \
        if (auto instr = Instruction::Cast(i)) {                               \
            if (auto lhs = isConst(instr->arg<0>().val())) {                   \
                if (auto rhs = isConst(instr->arg<1>().val())) {               \
                    auto res = Rf_eval(Rf_lang3(Operation, lhs->c, rhs->c),    \
                                       R_BaseEnv);                             \
                    cmp.preserve(res);                                         \
                    auto resi = new LdConst(res);                              \
                    instr->replaceUsesWith(resi);                              \
                    bb->replace(ip, resi);                                     \
                }                                                              \
            }                                                                  \
        }                                                                      \
    } while (false)

#define FOLD_UNARY(Instruction, Operation)                                     \
    do {                                                                       \
        if (auto instr = Instruction::Cast(i)) {                               \
            if (auto arg = isConst(instr->arg<0>().val())) {                   \
                Operation(arg->c);                                             \
            }                                                                  \
        }                                                                      \
    } while (false)

#define FOLD_BINARY(Instruction, Operation)                                    \
    do {                                                                       \
        if (auto instr = Instruction::Cast(i)) {                               \
            if (auto lhs = isConst(instr->arg<0>().val())) {                   \
                if (auto rhs = isConst(instr->arg<1>().val())) {               \
                    Operation(lhs->c, rhs->c);                                 \
                }                                                              \
            }                                                                  \
        }                                                                      \
    } while (false)

static bool convertsToLogicalWithoutWarning(SEXP arg) {
    switch (TYPEOF(arg)) {
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case CHARSXP:
        return !isObject(arg);
    default:
        return false;
    }
};

} // namespace

namespace rir {
namespace pir {

void Constantfold::apply(RirCompiler& cmp, ClosureVersion* function,
                         LogStream&) const {
    std::unordered_map<BB*, bool> branchRemoval;
    DominanceGraph dom(function);

    Visitor::run(function->entry, [&](BB* bb) {
        if (bb->isEmpty())
          return;
        auto ip = bb->begin();
        while (ip != bb->end()) {
            auto i = *ip;
            auto next = ip + 1;

            // Constantfolding of some common operations
            FOLD_BINARY_NATIVE(Add, symbol::Add);
            FOLD_BINARY_NATIVE(Sub, symbol::Sub);
            FOLD_BINARY_NATIVE(Mul, symbol::Mul);
            FOLD_BINARY_NATIVE(Div, symbol::Div);
            FOLD_BINARY_NATIVE(IDiv, symbol::Idiv);
            FOLD_BINARY_NATIVE(Lt, symbol::Lt);
            FOLD_BINARY_NATIVE(Gt, symbol::Gt);
            FOLD_BINARY_NATIVE(Lte, symbol::Le);
            FOLD_BINARY_NATIVE(Gte, symbol::Ge);
            FOLD_BINARY_NATIVE(Eq, symbol::Eq);
            FOLD_BINARY_NATIVE(Neq, symbol::Ne);
            FOLD_BINARY_NATIVE(Pow, symbol::Pow);

            FOLD_UNARY(AsLogical, [&](SEXP arg) {
                if (convertsToLogicalWithoutWarning(arg)) {
                    auto res = Rf_asLogical(arg);
                    auto c = new LdConst(Rf_ScalarLogical(res));
                    i->replaceUsesWith(c);
                    bb->replace(ip, c);
                }
            });

            FOLD_UNARY(AsTest, [&](SEXP arg) {
                if (Rf_length(arg) == 1 &&
                    convertsToLogicalWithoutWarning(arg)) {
                    auto res = Rf_asLogical(arg);
                    if (res != NA_LOGICAL) {
                        i->replaceUsesWith(res ? (Value*)True::instance()
                                               : (Value*)False::instance());
                        next = bb->remove(ip);
                    }
                }
            });

            FOLD_BINARY(Identical, [&](SEXP a, SEXP b) {
                i->replaceUsesWith(a == b ? (Value*)True::instance()
                                          : (Value*)False::instance());
                next = bb->remove(ip);
            });

            if (auto isObj = IsObject::Cast(i)) {
                if (!isObj->arg<0>().val()->type.maybeObj()) {
                    i->replaceUsesWith(False::instance());
                    next = bb->remove(ip);
                }
            }

            if (auto assume = Assume::Cast(i)) {
                if (assume->arg<0>().val() == True::instance() &&
                    assume->assumeTrue)
                    next = bb->remove(ip);
                else if (assume->arg<0>().val() == False::instance() &&
                         !assume->assumeTrue)
                    next = bb->remove(ip);
            }

            if (auto callb = CallBuiltin::Cast(i)) {
                static int nargs = findBuiltin("nargs");
                assert(function->assumptions().includes(
                    Assumption::NotTooManyArguments));
                // PIR functions are always compiled for a particular number of
                // arguments
                if (callb->builtinId == nargs) {
                    auto nargsC = new LdConst(
                        ScalarInteger(function->nargs() -
                                      function->assumptions().numMissing()));
                    callb->replaceUsesAndSwapWith(nargsC, ip);
                }
            }

            if (auto missing = Missing::Cast(i)) {
                if (auto e = MkEnv::Cast(missing->env())) {
                    e->eachLocalVar([&](SEXP name, Value* v) {
                        if (name == missing->varName &&
                            v == MissingArg::instance()) {
                            missing->replaceUsesAndSwapWith(
                                new LdConst(R_TrueValue), ip);
                        }
                    });
                }
            }

            ip = next;
        }

        if (!bb->isEmpty())
            if (auto branch = Branch::Cast(bb->last())) {
                auto condition = branch->arg<0>().val();
                if (condition == True::instance()) {
                    branchRemoval.emplace(bb, true);
                } else if (condition == False::instance()) {
                    branchRemoval.emplace(bb, false);
                }
            }
    });

    std::unordered_set<BB*> toDelete;
    // Find all dead basic blocks
    for (auto e : branchRemoval) {
        auto bb = e.first;
        auto deadBranch = e.second ? bb->falseBranch() : bb->trueBranch();
        deadBranch->collectDominated(toDelete, dom);
        toDelete.insert(deadBranch);
    }

    BBTransform::removeBBs(function, toDelete);

    for (auto e : branchRemoval) {
        auto branch = e.first;
        auto condition = e.second;
        branch->remove(branch->end() - 1);
        if (condition) {
            branch->next1 = nullptr;
        } else {
            branch->next0 = branch->next1;
            branch->next1 = nullptr;
        }
    }
}
} // namespace pir
} // namespace rir
