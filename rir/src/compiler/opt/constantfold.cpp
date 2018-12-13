#include "../pir/pir_impl.h"
#include "../transform/bb.h"
#include "../translations/rir_compiler.h"
#include "../util/cfg.h"
#include "../util/visitor.h"
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

#define FOLD2_SIMPLE(Instruction, Operation)                                   \
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

#define FOLD1(Instruction, Operation)                                          \
    do {                                                                       \
        if (auto instr = Instruction::Cast(i)) {                               \
            if (auto arg = isConst(instr->arg<0>().val())) {                   \
                Operation(arg->c);                                             \
            }                                                                  \
        }                                                                      \
    } while (false)

#define FOLD2(Instruction, Operation)                                          \
    do {                                                                       \
        if (auto instr = Instruction::Cast(i)) {                               \
            if (auto lhs = isConst(instr->arg<0>().val())) {                   \
                if (auto rhs = isConst(instr->arg<1>().val())) {               \
                    Operation(lhs->c, rhs->c);                                 \
                }                                                              \
            }                                                                  \
        }                                                                      \
    } while (false)

} // namespace

namespace rir {
namespace pir {

void Constantfold::apply(RirCompiler& cmp, Closure* function,
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
            FOLD2_SIMPLE(Add, symbol::Add);
            FOLD2_SIMPLE(Sub, symbol::Sub);
            FOLD2_SIMPLE(Mul, symbol::Mul);
            FOLD2_SIMPLE(Div, symbol::Div);
            FOLD2_SIMPLE(IDiv, symbol::Idiv);
            FOLD2_SIMPLE(Lt, symbol::Lt);
            FOLD2_SIMPLE(Gt, symbol::Gt);
            FOLD2_SIMPLE(Lte, symbol::Le);
            FOLD2_SIMPLE(Gte, symbol::Ge);
            FOLD2_SIMPLE(Eq, symbol::Eq);
            FOLD2_SIMPLE(Neq, symbol::Ne);
            FOLD2_SIMPLE(Pow, symbol::Pow);

            auto convertsToLglWithoutWarning = [](SEXP arg) {
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

            FOLD1(AsLogical, [&](SEXP arg) {
                if (convertsToLglWithoutWarning(arg)) {
                    auto res = Rf_asLogical(arg);
                    auto c = new LdConst(Rf_ScalarLogical(res));
                    i->replaceUsesWith(c);
                    bb->replace(ip, c);
                }
            });

            FOLD1(AsTest, [&](SEXP arg) {
                if (Rf_length(arg) == 1 && convertsToLglWithoutWarning(arg)) {
                    auto res = Rf_asLogical(arg);
                    if (res != NA_LOGICAL) {
                        i->replaceUsesWith(res ? (Value*)True::instance()
                                               : (Value*)False::instance());
                        next = bb->remove(ip);
                    }
                }
            });

            FOLD2(Identical, [&](SEXP a, SEXP b) {
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

            ip = next;
        }

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
