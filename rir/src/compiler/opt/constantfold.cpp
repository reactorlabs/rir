#include "../pir/pir_impl.h"
#include "../transform/bb.h"
#include "../transform/replace.h"
#include "../translations/rir_compiler.h"
#include "../util/cfg.h"
#include "../util/visitor.h"
#include "R/Symbols.h"
#include "pass_definitions.h"

#include <unordered_set>

namespace {

using namespace rir::pir;

static LdConst* isConst(Value* instr) {
    if (auto shared = SetShared::Cast(instr)) {
        instr = shared->arg<0>().val();
    }
    if (auto cst = LdConst::Cast(instr)) {
        return cst;
    }
    return nullptr;
}

#define FOLD(Instruction, Operation)                                           \
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

#define FOLD2(__i__, Instruction, Operation)                                   \
    do {                                                                       \
        if (auto instr = Instruction::Cast(__i__)) {                           \
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
        for (auto ip = bb->begin(); ip != bb->end(); ++ip) {
            auto i = *ip;

            // Constantfolding of some common operations
            FOLD(Add, symbol::Add);
            FOLD(Sub, symbol::Sub);
            FOLD(Mul, symbol::Mul);
            FOLD(Div, symbol::Div);
            FOLD(IDiv, symbol::Idiv);
            FOLD(Lt, symbol::Lt);
            FOLD(Gt, symbol::Gt);
            FOLD(Lte, symbol::Le);
            FOLD(Gte, symbol::Ge);
            FOLD(Eq, symbol::Eq);
            FOLD(Neq, symbol::Ne);
            FOLD(Pow, symbol::Pow);
        }

        if (auto branch = Branch::Cast(bb->last())) {
            if (auto tst = AsTest::Cast(branch->arg<0>().val())) {
                // Try to detect constant branch conditions and mark such
                // branches for removal
                auto cnst = isConst(tst->arg<0>().val());
                if (!cnst)
                    if (auto lgl = AsLogical::Cast(tst->arg<0>().val()))
                        cnst = isConst(lgl->arg<0>().val());
                if (cnst) {
                    SEXP c = cnst->c;
                    // Non length 1 condition throws warning
                    if (Rf_length(c) == 1) {
                        auto cond = Rf_asLogical(c);
                        // NA throws an error
                        if (cond != NA_LOGICAL) {
                            branchRemoval.emplace(bb, cond);
                        }
                    }
                }

                // If the `Identical` instruction can be resolved statically,
                // use it for branch removal as well.
                FOLD2(tst->arg<0>().val(), Identical, [&](SEXP a, SEXP b) {
                    branchRemoval.emplace(bb, a == b);
                });
            }
        }
    });

    std::unordered_set<BB*> toDelete;
    // Find all dead basic blocks
    for (auto e : branchRemoval) {
        auto bb = e.first;
        if (e.second)
            toDelete.insert(bb->next1);
        else
            toDelete.insert(bb->next0);
    }

    BBTransform::removeBBsWithChildren(dom, function, toDelete);

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
