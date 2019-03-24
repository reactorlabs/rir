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
                    auto res = Rf_eval(                                        \
                        Rf_lang3(Operation, lhs->c(), rhs->c()), R_BaseEnv);   \
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
                Operation(arg->c());                                           \
            }                                                                  \
        }                                                                      \
    } while (false)

#define FOLD_BINARY(Instruction, Operation)                                    \
    do {                                                                       \
        if (auto instr = Instruction::Cast(i)) {                               \
            if (auto lhs = isConst(instr->arg<0>().val())) {                   \
                if (auto rhs = isConst(instr->arg<1>().val())) {               \
                    Operation(lhs->c(), rhs->c());                             \
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

            FOLD_UNARY(AsInt, [&](SEXP arg) {
                if (IS_SIMPLE_SCALAR(arg, INTSXP)) {
                    i->replaceUsesWith(i->arg(0).val());
                    next = bb->remove(ip);
                }
            });

            FOLD_BINARY(Identical, [&](SEXP a, SEXP b) {
                i->replaceUsesWith(a == b ? (Value*)True::instance()
                                          : (Value*)False::instance());
                next = bb->remove(ip);
            });

            if (auto isTest = IsObject::Cast(i)) {
                if (!isTest->arg<0>().val()->type.maybeObj()) {
                    i->replaceUsesWith(False::instance());
                    next = bb->remove(ip);
                }
            }

            if (auto isTest = IsEnvStub::Cast(i)) {
                if (auto environment = MkEnv::Cast(isTest->env())) {
                    static std::unordered_set<Tag> tags{Tag::Force};
                    if (environment->usesDoNotInclude(bb, tags)) {
                        i->replaceUsesWith(True::instance());
                        next = bb->remove(ip);
                    }
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
                // PIR functions are always compiled for a particular number
                // of arguments
                if (callb->builtinId == nargs) {
                    auto nargsC = new LdConst(
                        ScalarInteger(function->nargs() -
                                      function->assumptions().numMissing()));
                    callb->replaceUsesAndSwapWith(nargsC, ip);
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

    // If we deleted a branch, then it can happen that some phi inputs are
    // now dead. We need to take care of them and remove them. The canonical
    // case is the following:
    //
    //    BB0:
    //      a <- 1
    //      branch TRUE -> BB1 | BB2
    //    BB1:
    //      b <- 2
    //      goto BB3
    //    BB2:
    //      goto BB3
    //    BB3:
    //      phi(BB0:a, BB1:b)
    //
    // In this case removing the branch BB2, also kills the input `BB0:a`.
    //
    // TODO: currently the algorithm is very slow. For each phi that comes
    // after a removed branch, we check if (in the modified CFG) we have two
    // inputs, both dominating the phi. If so we will remove the one that
    // comes earlier (ie. the one that dominates the other).
    if (!branchRemoval.empty()) {
        DominanceGraph dom(function);
        CFG cfg(function);
        Visitor::run(function->entry, [&](BB* bb) {
            bool afterARemovedBranch = false;
            for (auto& d : branchRemoval)
                if (!afterARemovedBranch && cfg.isPredecessor(d.first, bb))
                    afterARemovedBranch = true;

            if (afterARemovedBranch) {
                for (auto it = bb->begin(); it != bb->end(); ++it) {
                    auto i = *it;
                    if (auto phi = Phi::Cast(i)) {
                        std::unordered_set<BB*> deadInput;
                        phi->eachArg([&](BB* a, Value* va) {
                            if (toDelete.find(a) != toDelete.end()) {
                                deadInput.insert(a);
                            } else if (dom.dominates(a, phi->bb())) {
                                phi->eachArg([&](BB* b, Value* vb) {
                                    if (va != vb &&
                                        dom.dominates(b, phi->bb()) &&
                                        dom.dominates(a, b))
                                        deadInput.insert(a);
                                });
                            }
                        });
                        phi->removeInputs(deadInput);
                    }
                }
            }
        });
    }

    for (auto bb : toDelete)
        delete bb;
}
} // namespace pir
} // namespace rir
