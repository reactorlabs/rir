#include "../pir/pir_impl.h"
#include "../util/phi_placement.h"
#include "../util/visitor.h"
#include "R/BuiltinIds.h"
#include "R/Funtab.h"
#include "R/Symbols.h"
#include "R/r.h"
#include "compiler/analysis/cfg.h"
#include "compiler/compiler.h"
#include "interpreter/interp.h"
#include "pass_definitions.h"
#include "runtime/DispatchTable.h"

#include <cmath>
#include <iterator>
#include <list>
#include <memory>
#include <unordered_set>

namespace rir {
namespace pir {

static SEXP isConst(Value* instr, Preserve& p) {
    instr = instr->followCastsAndForce();

    if (auto cst = Const::Cast(instr))
        return cst->c();

    if (instr->asRValue() && instr != MissingArg::instance())
        return instr->asRValue();

    return nullptr;
}

SEXP qt(SEXP c, Preserve& p) {
    if (TYPEOF(c) == SYMSXP)
        return p.operator()(Rf_lang2(symbol::quote, c));
    return c;
}

#define FOLD_BINARY_NATIVE(Instruction, Operation)                             \
    do {                                                                       \
        if (auto instr = Instruction::Cast(i)) {                               \
            if (auto lhs = isConst(instr->arg<0>().val(), p)) {                \
                if (auto rhs = isConst(instr->arg<1>().val(), p)) {            \
                    auto res = Rf_eval(                                        \
                        p(Rf_lang3(Operation, qt(lhs, p), qt(rhs, p))),        \
                        R_BaseEnv);                                            \
                    iterAnyChange = true;                                      \
                    instr->replaceUsesWith(cmp.module->c(res));                \
                    next = bb->remove(ip);                                     \
                }                                                              \
            }                                                                  \
        }                                                                      \
    } while (false)

#define FOLD_UNARY(Instruction, Operation)                                     \
    do {                                                                       \
        if (auto instr = Instruction::Cast(i)) {                               \
            if (auto arg = isConst(instr->arg<0>().val(), p))                  \
                Operation(arg);                                                \
        }                                                                      \
    } while (false)

#define FOLD_BINARY(Instruction, Operation)                                    \
    do {                                                                       \
        if (auto instr = Instruction::Cast(i)) {                               \
            if (auto lhs = isConst(instr->arg<0>().val(), p)) {                \
                if (auto rhs = isConst(instr->arg<1>().val(), p)) {            \
                    Operation(lhs, rhs);                                       \
                }                                                              \
            }                                                                  \
        }                                                                      \
    } while (false)

#define FOLD_BINARY_EITHER(Instruction, Operation)                             \
    do {                                                                       \
        if (auto instr = Instruction::Cast(i)) {                               \
            if (auto lhs = isConst(instr->arg<0>().val(), p)) {                \
                if (Operation(lhs, instr->arg<1>().val()))                     \
                    break;                                                     \
            }                                                                  \
            if (auto rhs = isConst(instr->arg<1>().val(), p)) {                \
                Operation(rhs, instr->arg<0>().val());                         \
            }                                                                  \
        }                                                                      \
    } while (false)

static bool convertsToRealWithoutWarning(SEXP arg) {
    if (Rf_isVectorAtomic(arg) && XLENGTH(arg) == 1) {
        switch (TYPEOF(arg)) {
        case LGLSXP:
        case INTSXP:
        case REALSXP:
            return true;
        case CPLXSXP:
            return COMPLEX(arg)[0].i == 0;
        default:
            return false;
        }
    } else {
        return false;
    }
}

static bool convertsToLogicalWithoutWarning(SEXP arg) {
    switch (TYPEOF(arg)) {
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case CHARSXP:
        return !Rf_isObject(arg);
    default:
        return false;
    }
}

static bool isStaticallyTrue(Value* i) {
    if (i == True::instance())
        return true;
    if (auto ld = Const::Cast(i)) {
        if (convertsToLogicalWithoutWarning(ld->c())) {
            auto a = ld->c();
            if (Rf_length(a) == 1 && Rf_asLogical(a) == TRUE)
                return true;
        }
    }
    return false;
}

static bool isStaticallyFalse(Value* i) {
    if (i == False::instance())
        return true;
    if (auto ld = Const::Cast(i)) {
        if (convertsToLogicalWithoutWarning(ld->c())) {
            auto a = ld->c();
            if (Rf_length(a) == 1 && Rf_asLogical(a) == FALSE)
                return true;
        }
    }
    return false;
}

static bool isStaticallyNA(Value* i) {
    if (i == NaLogical::instance())
        return true;
    if (auto ld = Const::Cast(i)) {
        if (convertsToLogicalWithoutWarning(ld->c())) {
            auto a = ld->c();
            if (Rf_length(a) == 1 && Rf_asLogical(a) == NA_LOGICAL)
                return true;
        }
    }
    return false;
}

bool Constantfold::apply(Compiler& cmp, ClosureVersion* cls, Code* code,
                         AbstractLog& log, size_t iteration) const {
    EarlyConstantfold cf;
    bool anyChange = cf.apply(cmp, cls, code, log, iteration);

    Preserve p;
    std::unordered_map<BB*, bool> branchRemoval;

    {
        DominanceGraph dom(code);
        std::unique_ptr<DominanceFrontier> dfront;
        // Branch Elimination
        //
        // Given branch `a` and `b`, where both have the same
        // condition and `a` dominates `b`, we can replace the condition of
        // `b` by
        //
        //     c' = Phi([TRUE, a->trueBranch()], [FALSE, a->falseBranch()])
        //     b  = Branch(c')
        //
        // and even constantfold the `b` branch if either
        // `a->bb()->trueBranch()` or `a->bb()->falseBranch()` do not reach
        std::unordered_map<Instruction*, std::vector<Branch*>> condition;

        DominatorTreeVisitor<>(dom).run(code->entry, [&](BB* bb) {
            if (bb->isEmpty())
                return;
            auto branch = Branch::Cast(bb->last());
            if (!branch)
                return;
            if (auto i = Instruction::Cast(branch->arg(0).val())) {

                auto ii = branch->arg(0).val();
                if (ii != True::instance() && ii != False::instance()) {
                    condition[i].push_back(branch);
                }
            }
        });

        for (auto& c : condition) {
            std::unordered_set<Branch*> removed;
            auto& uses = c.second;
            if (uses.size() > 1) {
                for (auto a = uses.begin(); (a + 1) != uses.end(); a++) {
                    if (removed.count(*a))
                        continue;

                    auto phisPlaced = false;
                    std::unique_ptr<PhiPlacement> pl;
                    std::unordered_map<BB*, Phi*> newPhisByBB;
                    newPhisByBB.clear();
                    for (auto b = a + 1; b != uses.end(); b++) {
                        if (removed.count(*b))
                            continue;

                        auto bb1 = (*a)->bb();
                        auto bb2 = (*b)->bb();
                        if (dom.dominates(bb1, bb2)) {
                            if (dom.dominates(bb1->trueBranch(), bb2)) {
                                anyChange = true;
                                (*b)->arg(0).val() = True::instance();
                            } else if (dom.dominates(bb1->falseBranch(), bb2)) {
                                anyChange = true;
                                (*b)->arg(0).val() = False::instance();
                            } else {
                                if (!phisPlaced) {
                                    // create and place phi
                                    std::unordered_map<BB*, Value*> inputs;
                                    inputs[bb1->trueBranch()] =
                                        True::instance();
                                    inputs[bb1->falseBranch()] =
                                        False::instance();
                                    if (!dfront)
                                        dfront =
                                            std::make_unique<DominanceFrontier>(
                                                code, dom);
                                    assert(!pl);
                                    pl = std::make_unique<PhiPlacement>(
                                        code, inputs, dom, *dfront);
                                    assert(pl);

                                    assert(pl->placement.size() > 0);
                                    anyChange = true;

                                    for (auto& placement : pl->placement) {
                                        auto targetForPhi = placement.first;
                                        newPhisByBB[targetForPhi] = new Phi;
                                    }

                                    for (auto& placement : pl->placement) {
                                        auto targetForPhi = placement.first;
                                        auto phi = newPhisByBB[targetForPhi];

                                        for (auto& input : placement.second) {

                                            if (input.aValue)
                                                phi->addInput(input.inputBlock,
                                                              input.aValue);
                                            else {

                                                phi->addInput(
                                                    input.inputBlock,
                                                    newPhisByBB.at(
                                                        input.otherPhi));
                                            }
                                        }
                                        phi->type = PirType::test();
                                        targetForPhi->insert(
                                            targetForPhi->begin(), phi);
                                    }
                                    phisPlaced = true;
                                }

                                if (phisPlaced) {
                                    assert(pl->dominatingPhi.count(bb2) > 0);
                                    auto phi = newPhisByBB.at(
                                        pl->dominatingPhi.at(bb2));
                                    (*b)->arg(0).val() = phi;
                                }
                            }
                        }
                        removed.insert(*b);
                    }
                }
            }
        }
    }

    DominanceGraph::BBSet newUnreachable;
    for (auto i = 0; i < 2; ++i) {
        bool iterAnyChange = false;
        Visitor::run(code->entry, [&](BB* bb) {
            if (bb->isEmpty())
                return;
            auto ip = bb->begin();
            while (ip != bb->end()) {
                auto i = *ip;
                auto next = ip + 1;

                auto killUnreachable = [&]() {
                    if (ip == bb->end() || Unreachable::Cast(bb->last()))
                        return;
                    ip = bb->insert(ip + 1, new Unreachable()) + 1;
                    while (ip != bb->end())
                        ip = bb->remove(ip);
                    newUnreachable.insert(bb);
                    next = bb->end();
                };

                auto foldLglCmp = [&](SEXP carg, Value* varg, bool isEq) {
                    if (!isConst(varg, p) && // was already folded
                        IS_SIMPLE_SCALAR(carg, LGLSXP) &&
                        varg->type.isA(PirType::simpleScalarLogical())) {
                        int larg = *LOGICAL(carg);
                        if (larg == (int)isEq) {
                            iterAnyChange = true;
                            i->replaceUsesWith(varg);
                            next = bb->remove(ip);
                        } else if (larg == (int)!isEq) {
                            auto res = new Not(varg, i->env(), i->srcIdx);
                            // Guarenteed, and required by replaceUsesWith
                            res->type = PirType::simpleScalarLogical();
                            iterAnyChange = true;
                            i->replaceUsesWith(res);
                            bb->replace(ip, res);
                        } else if (larg == NA_LOGICAL) {
                            // Even NA == NA (and NA != NA) yield NA
                            iterAnyChange = true;
                            i->replaceUsesWith(NaLogical::instance());
                            next = bb->remove(ip);
                        } else {
                            assert(false);
                        }
                        return true;
                    } else {
                        return false;
                    }
                };

                if (ChkMissing::Cast(i)) {
                    if (i->arg(0).val() == MissingArg::instance())
                        killUnreachable();
                }
                if (CheckTrueFalse::Cast(i)) {
                    auto a = i->arg(0).val();
                    if (isStaticallyNA(a)) {
                        killUnreachable();
                    } else if (isStaticallyTrue(a) || isStaticallyFalse(a)) {
                        auto replace = isStaticallyTrue(a)
                                           ? (Value*)True::instance()
                                           : (Value*)False::instance();
                        i->replaceUsesWith(replace);
                        next = bb->remove(ip);
                    } else if (isConst(a, p) &&
                               convertsToLogicalWithoutWarning(isConst(a, p))) {
                        auto replace = Rf_asLogical(isConst(a, p)) == TRUE
                                           ? (Value*)True::instance()
                                           : (Value*)False::instance();
                        i->replaceUsesWith(replace);
                        next = bb->remove(ip);
                    }
                }
                if (LAnd::Cast(i)) {
                    auto a = i->arg(0).val();
                    auto b = i->arg(1).val();
                    Value* n = nullptr;
                    if (a == b) {
                        iterAnyChange = true;
                        i->replaceUsesWith(i->arg(0).val());
                        next = bb->remove(ip);
                    } else if (isStaticallyTrue(a)) {
                        n = new AsLogical(b, i->srcIdx);
                    } else if (isStaticallyFalse(a)) {
                        n = False::instance();
                    } else if (isStaticallyTrue(b)) {
                        n = new AsLogical(a, i->srcIdx);
                    } else if (isStaticallyFalse(b)) {
                        ip = bb->insert(ip, new AsLogical(a, i->srcIdx));
                        ip++;
                        next = ip + 1;
                        n = False::instance();
                    }
                    if (n) {
                        iterAnyChange = true;
                        i->replaceUsesWith(n);
                        if (auto j = Instruction::Cast(n))
                            bb->replace(ip, j);
                        else
                            next = bb->remove(ip);
                    }
                }
                if (LOr::Cast(i)) {
                    auto a = i->arg(0).val();
                    auto b = i->arg(1).val();
                    Value* n = nullptr;
                    if (a == b) {
                        iterAnyChange = true;
                        i->replaceUsesWith(i->arg(0).val());
                        next = bb->remove(ip);
                    } else if (isStaticallyTrue(a)) {
                        n = True::instance();
                    } else if (isStaticallyFalse(a)) {
                        n = new AsLogical(b, i->srcIdx);
                    } else if (isStaticallyTrue(b)) {
                        ip = bb->insert(ip, new AsLogical(a, i->srcIdx));
                        ip++;
                        next = ip + 1;
                        n = True::instance();
                    } else if (isStaticallyFalse(b)) {
                        n = new AsLogical(a, i->srcIdx);
                    }
                    if (n) {
                        iterAnyChange = true;
                        i->replaceUsesWith(n);
                        if (auto j = Instruction::Cast(n))
                            bb->replace(ip, j);
                        else
                            next = bb->remove(ip);
                    }
                }

                // Constantfolding of some common operations
                FOLD_BINARY_NATIVE(Add, symbol::Add);
                FOLD_BINARY_NATIVE(Sub, symbol::Sub);
                FOLD_BINARY_NATIVE(Mul, symbol::Mul);
                FOLD_BINARY_NATIVE(Div, symbol::Div);
                FOLD_BINARY_NATIVE(Pow, symbol::Pow);
                FOLD_BINARY_NATIVE(IDiv, symbol::Idiv);
                FOLD_BINARY_NATIVE(Mod, symbol::Mod);
                FOLD_BINARY_NATIVE(Lt, symbol::Lt);
                FOLD_BINARY_NATIVE(Gt, symbol::Gt);
                FOLD_BINARY_NATIVE(Lte, symbol::Le);
                FOLD_BINARY_NATIVE(Gte, symbol::Ge);
                FOLD_BINARY_NATIVE(Eq, symbol::Eq);
                FOLD_BINARY_NATIVE(Neq, symbol::Ne);

                FOLD_BINARY_EITHER(Eq, [&](SEXP carg, Value* varg) {
                    return foldLglCmp(carg, varg, true);
                });
                FOLD_BINARY_EITHER(Neq, [&](SEXP carg, Value* varg) {
                    return foldLglCmp(carg, varg, false);
                });

                FOLD_UNARY(Minus, [&](SEXP arg) {
                    auto t = TYPEOF(arg);
                    if (!Rf_isObject(arg) &&
                        (t == INTSXP || t == LGLSXP || t == REALSXP)) {
                        auto res =
                            Rf_eval(Rf_lang2(symbol::Sub, arg), R_BaseEnv);
                        i->replaceUsesWith(cmp.module->c(res));
                        next = bb->remove(ip);
                        iterAnyChange = true;
                    }
                });
                FOLD_UNARY(AsLogical, [&](SEXP arg) {
                    if (convertsToLogicalWithoutWarning(arg)) {
                        auto res = Rf_asLogical(arg);
                        i->replaceUsesWith(
                            cmp.module->c(Rf_ScalarLogical(res)));
                        next = bb->remove(ip);
                        iterAnyChange = true;
                    }
                });

                if (Identical::Cast(i)) {
                    // Those are targeting the checks for default argument
                    // evaluation after inlining
                    if (i->arg(1).val() == MissingArg::instance()) {
                        if (i->arg(0).val() == MissingArg::instance()) {
                            iterAnyChange = true;
                            i->replaceUsesWith(True::instance());
                            next = bb->remove(ip);
                        } else if (!i->arg(0).val()->type.maybeMissing()) {
                            iterAnyChange = true;
                            i->replaceUsesWith(False::instance());
                            next = bb->remove(ip);
                        }
                    } else if (i->arg(0).val() == i->arg(1).val()) {
                        iterAnyChange = true;
                        i->replaceUsesWith(True::instance());
                        next = bb->remove(ip);
                    } else if (isConst(i->arg(0).val(), p) &&
                               isConst(i->arg(0).val(), p) == R_TrueValue &&
                               i->arg(1).val()->type.isA(PirType::test())) {
                        iterAnyChange = true;
                        i->replaceUsesWith(i->arg(1).val());
                        next = bb->remove(ip);
                    } else if (isConst(i->arg(1).val(), p) &&
                               isConst(i->arg(1).val(), p) == R_TrueValue &&
                               i->arg(0).val()->type.isA(PirType::test())) {
                        iterAnyChange = true;
                        i->replaceUsesWith(i->arg(0).val());
                        next = bb->remove(ip);
                    } else if (isConst(i->arg(0).val(), p) &&
                               isConst(i->arg(0).val(), p) == R_FalseValue &&
                               i->arg(1).val()->type.isA(PirType::test())) {
                        iterAnyChange = true;
                        auto neg =
                            new Not(i->arg(1).val(), Env::elided(), i->srcIdx);
                        neg->type = PirType::test();
                        i->replaceUsesAndSwapWith(neg, ip);
                    } else if (isConst(i->arg(1).val(), p) &&
                               isConst(i->arg(1).val(), p) == R_FalseValue &&
                               i->arg(0).val()->type.isA(PirType::test())) {
                        iterAnyChange = true;
                        auto neg =
                            new Not(i->arg(0).val(), Env::elided(), i->srcIdx);
                        neg->type = PirType::test();
                        i->replaceUsesAndSwapWith(neg, ip);
                    } else {
                        FOLD_BINARY(Identical, [&](SEXP a, SEXP b) {
                            iterAnyChange = true;
                            i->replaceUsesWith(a == b
                                                   ? (Value*)True::instance()
                                                   : (Value*)False::instance());
                            next = bb->remove(ip);
                        });
                    }
                }
                if (auto isTest = IsType::Cast(i)) {
                    auto arg = isTest->arg<0>().val();
                    if (arg->type.isA(isTest->typeTest)) {
                        iterAnyChange = true;
                        i->replaceUsesWith(True::instance());
                        next = bb->remove(ip);
                    } else if (!arg->type.maybe(isTest->typeTest)) {
                        iterAnyChange = true;
                        i->replaceUsesWith(False::instance());
                        next = bb->remove(ip);
                    }
                }
                if (auto isTest = Is::Cast(i)) {
                    auto arg = isTest->arg<0>().val();
                    if (arg->type.isA(isTest->lowerBound())) {
                        iterAnyChange = true;
                        auto n = True::instance();
                        i->replaceUsesWith(n);
                        next = bb->remove(ip);
                    } else if (!arg->type.maybe(isTest->upperBound())) {
                        iterAnyChange = true;
                        auto n = False::instance();
                        i->replaceUsesWith(n);
                        next = bb->remove(ip);
                    }
                }
                if (auto assume = Assume::Cast(i)) {
                    bool isdead = false;
                    if (assume->arg<0>().val() == True::instance()) {
                        if (assume->assumeTrue) {
                            iterAnyChange = true;
                            next = bb->remove(ip);
                        } else {
                            isdead = true;
                        }
                    } else if (assume->arg<0>().val() == False::instance()) {
                        if (!assume->assumeTrue) {
                            iterAnyChange = true;
                            next = bb->remove(ip);
                        } else {
                            isdead = true;
                        }
                    } else if (auto n = Not::Cast(assume->condition())) {
                        if (n->arg<0>().val()->type.isA(PirType::test())) {
                            assume->arg<0>().val() = n->arg<0>().val();
                            assume->assumeTrue = !assume->assumeTrue;
                            iterAnyChange = true;
                        }
                    }
                    if (isdead) {
                        killUnreachable();
                    }
                }
                if (auto cl = Colon::Cast(i)) {
                    if (auto a = isConst(cl->arg(0).val(), p)) {
                        if (TYPEOF(a) == REALSXP && Rf_length(a) == 1 &&
                            REAL(a)[0] == (double)(int)REAL(a)[0]) {
                            iterAnyChange = true;
                            cl->arg(0).val() = cmp.module->c((int)REAL(a)[0]);
                        }
                    }
                    if (auto a = isConst(cl->arg(1).val(), p)) {
                        if (TYPEOF(a) == REALSXP && Rf_length(a) == 1 &&
                            REAL(a)[0] == (double)(int)REAL(a)[0]) {
                            iterAnyChange = true;
                            cl->arg(1).val() = cmp.module->c((int)REAL(a)[0]);
                        }
                    }
                    next = ip + 1;
                }

                if (Length::Cast(i)) {
                    auto t = i->arg(0).val()->type;
                    if (t.isA(PirType::anySimpleScalar())) {
                        iterAnyChange = true;
                        i->replaceUsesWith(cmp.module->c(1));
                        next = bb->remove(ip);
                    }
                }

                if (CallSafeBuiltin::Cast(i) || CallBuiltin::Cast(i)) {
                    int builtinId = CallBuiltin::Cast(i)
                                        ? CallBuiltin::Cast(i)->builtinId
                                        : CallSafeBuiltin::Cast(i)->builtinId;
                    size_t nargs = CallInstruction::CastCall(i)->nCallArgs();
                    assert(cls->context().includes(
                        Assumption::NotTooManyArguments));
                    // PIR functions are always compiled for a particular number
                    // of arguments
                    auto noExplMissing = cls->context().includes(
                        Assumption::NoExplicitlyMissingArgs);
                    if (builtinId == blt("nargs") && noExplMissing) {
                        // nargs inside inlinee refers to nargs passed to
                        // inlinee, which is something we cannot recover.
                        bool notInlined =
                            Visitor::check(code->entry, [](Instruction* i) {
                                return !PushContext::Cast(i);
                            });
                        if (notInlined && !cls->owner()->formals().hasDots()) {
                            iterAnyChange = true;
                            i->replaceUsesWith(
                                cmp.module->c((int)cls->effectiveNArgs()));
                            next = bb->remove(ip);
                        }
                    } else if (builtinId == blt("invisible") && nargs == 1) {
                        i->replaceUsesWith(
                            CallInstruction::CastCall(i)->callArg(0).val());
                        bb->replace(ip, new Invisible());
                    } else if (builtinId == blt("invisible") && nargs == 0) {
                        i->replaceUsesWith(Nil::instance());
                        bb->replace(ip, new Invisible());
                    } else if (builtinId == blt("length") && nargs == 1) {
                        auto t = i->arg(0).val()->type;
                        if (t.isA(PirType::anySimpleScalar())) {
                            iterAnyChange = true;
                            i->replaceUsesWith(cmp.module->c(1));
                            next = bb->remove(ip);
                        }
                    } else if (builtinId == blt("list")) {
                        bool allConst = true;
                        i->eachArg([&](Value* v) {
                            if (!Const::Cast(v))
                                allConst = false;
                        });
                        if (allConst) {
                            auto list = Rf_allocVector(VECSXP, i->nargs());
                            auto pos = 0;
                            i->eachArg([&](Value* v) {
                                VECTOR_ELT(list, pos++) = Const::Cast(v)->c();
                            });
                            iterAnyChange = true;
                            i->replaceUsesWith(cmp.module->c(list));
                            next = bb->remove(ip);
                        }
                    } else if (builtinId == blt("as.logical") && nargs == 1 &&
                               !i->arg(0).val()->type.maybeObj() &&
                               i->arg(0).val()->type.isScalar()) {
                        i->replaceUsesAndSwapWith(
                            new AsLogical(i->arg(0).val(), i->srcIdx), ip);
                        iterAnyChange = true;
                    } else if (builtinId == blt("as.character") && nargs == 1) {
                        auto t = i->arg(0).val()->type;
                        if (t.isA(PirType(RType::str)
                                      .notPromiseWrapped()
                                      .notObject())) {
                            iterAnyChange = true;
                            i->replaceUsesWith(i->arg(0).val());
                            next = bb->remove(ip);
                        } else if (auto con = isConst(i->arg(0).val(), p)) {
                            auto t = TYPEOF(con);
                            if (t == REALSXP || t == INTSXP || t == LGLSXP) {
                                auto res =
                                    p(Rf_eval(p(Rf_lang2(symbol::ascharacter,
                                                         qt(con, p))),
                                              R_BaseEnv));
                                iterAnyChange = true;
                                i->replaceUsesWith(cmp.module->c(res));
                                next = bb->remove(ip);
                            }
                        }
                    } else if (builtinId == blt("as.integer") && nargs == 1) {
                        auto t = i->arg(0).val()->type;
                        if (t.isA(PirType(RType::integer)
                                      .notPromiseWrapped()
                                      .notObject())) {
                            iterAnyChange = true;
                            i->replaceUsesWith(i->arg(0).val());
                            next = bb->remove(ip);
                        } else if (auto con = isConst(i->arg(0).val(), p)) {
                            if (IS_SIMPLE_SCALAR(con, REALSXP)) {
                                if (REAL(con)[0] == REAL(con)[0]) {
                                    iterAnyChange = true;
                                    i->replaceUsesWith(
                                        cmp.module->c((int)REAL(con)[0]));
                                    next = bb->remove(ip);
                                } else {
                                    iterAnyChange = true;
                                    i->replaceUsesWith(
                                        cmp.module->c(NA_INTEGER));
                                    next = bb->remove(ip);
                                }
                            } else if (IS_SIMPLE_SCALAR(con, INTSXP)) {
                                iterAnyChange = true;
                                i->replaceUsesWith(cmp.module->c(con));
                                next = bb->remove(ip);
                            }
                        }
                    } else if (builtinId == blt("is.vector") && nargs == 2) {
                        // We could do better here but for now we just look
                        // at the default mode "any"
                        // Also, the names attribute is allowed but we
                        // approximate by checking that there are no attributes
                        auto argType = i->arg(0).val()->type;
                        auto modeType = i->arg(1).val()->type;
                        auto mode = Const::Cast(i->arg(1).val());
                        if (!argType.maybeHasAttrs() &&
                            argType.isA(PirType::vecs()) &&
                            modeType.isA(PirType::simpleScalarString()) &&
                            mode &&
                            staticStringEqual(CHAR(STRING_ELT(mode->c(), 0)),
                                              "any")) {
                            iterAnyChange = true;
                            i->replaceUsesWith(True::instance());
                            next = bb->remove(ip);
                        }
                    } else if (builtinId == blt("is.function") && nargs == 1) {
                        auto t = i->arg(0).val()->type;
                        if (t.isA(PirType::function())) {
                            iterAnyChange = true;
                            i->replaceUsesWith(True::instance());
                            next = bb->remove(ip);
                        } else if (!t.maybe(PirType::function())) {
                            iterAnyChange = true;
                            i->replaceUsesWith(False::instance());
                            next = bb->remove(ip);
                        }
                    } else if (builtinId == blt("is.character") && nargs == 1) {
                        auto t = i->arg(0).val()->type;
                        if (t.isA(RType::str)) {
                            iterAnyChange = true;
                            i->replaceUsesWith(True::instance());
                            next = bb->remove(ip);
                        } else if (!t.maybe(RType::str)) {
                            iterAnyChange = true;
                            i->replaceUsesWith(False::instance());
                            next = bb->remove(ip);
                        }
                    } else if (builtinId == blt("is.logical") && nargs == 1) {
                        auto t = i->arg(0).val()->type;
                        if (t.isA(RType::logical)) {
                            iterAnyChange = true;
                            i->replaceUsesWith(True::instance());
                            next = bb->remove(ip);
                        } else if (!t.maybe(RType::logical)) {
                            iterAnyChange = true;
                            i->replaceUsesWith(False::instance());
                            next = bb->remove(ip);
                        }
                    } else if (builtinId == blt("is.double") && nargs == 1) {
                        auto t = i->arg(0).val()->type;
                        if (t.isA(RType::real)) {
                            iterAnyChange = true;
                            i->replaceUsesWith(True::instance());
                            next = bb->remove(ip);
                        } else if (!t.maybe(RType::real)) {
                            iterAnyChange = true;
                            i->replaceUsesWith(False::instance());
                            next = bb->remove(ip);
                        }
                    } else if (builtinId == blt("is.integer") && nargs == 1) {
                        auto t = i->arg(0).val()->type;
                        if (t.isA(PirType(RType::integer))) {
                            iterAnyChange = true;
                            i->replaceUsesWith(True::instance());
                            next = bb->remove(ip);
                        } else if (!t.maybe(RType::integer)) {
                            iterAnyChange = true;
                            i->replaceUsesWith(False::instance());
                            next = bb->remove(ip);
                        }
                    } else if (builtinId == blt("is.complex") && nargs == 1) {
                        auto t = i->arg(0).val()->type;
                        if (t.isA(RType::cplx)) {
                            iterAnyChange = true;
                            i->replaceUsesWith(True::instance());
                            next = bb->remove(ip);
                        } else if (!t.maybe(RType::cplx)) {
                            iterAnyChange = true;
                            i->replaceUsesWith(False::instance());
                            next = bb->remove(ip);
                        }
                    } else if (builtinId == blt("is.character") && nargs == 1) {
                        auto t = i->arg(0).val()->type;
                        if (t.isA(RType::str)) {
                            iterAnyChange = true;
                            i->replaceUsesWith(True::instance());
                            next = bb->remove(ip);
                        } else if (!t.maybe(RType::str)) {
                            iterAnyChange = true;
                            i->replaceUsesWith(False::instance());
                            next = bb->remove(ip);
                        }
                    } else if (builtinId == blt("is.atomic") && nargs == 1) {
                        auto t = i->arg(0).val()->type;
                        static auto atomicType =
                            PirType() | RType::nil | RType::chr |
                            RType::logical | RType::integer | RType::real |
                            RType::cplx | RType::str | RType::raw;
                        if (t.isA(atomicType)) {
                            iterAnyChange = true;
                            i->replaceUsesWith(True::instance());
                            next = bb->remove(ip);
                        } else if (!t.maybe(atomicType)) {
                            iterAnyChange = true;
                            i->replaceUsesWith(False::instance());
                            next = bb->remove(ip);
                        }
                    } else if (builtinId == blt("anyNA") && nargs == 1) {
                        auto t = i->arg(0).val()->type;
                        if (!t.maybeNAOrNaN()) {
                            iterAnyChange = true;
                            i->replaceUsesWith(False::instance());
                            next = bb->remove(ip);
                        }
                    } else if (builtinId == blt("is.object") && nargs == 1) {
                        auto t = i->arg(0).val()->type;
                        if (!t.maybeObj()) {
                            iterAnyChange = true;
                            i->replaceUsesWith(False::instance());
                            next = bb->remove(ip);
                        }
                    } else if ((builtinId == blt("is.na") ||
                                builtinId == blt("is.nan")) &&
                               nargs == 1) {
                        auto t = i->arg(0).val()->type;
                        static PirType typeThatDoesntError =
                            (PirType::num() | RType::chr | RType::str |
                             RType::vec)
                                .orAttribsOrObj();
                        if (typeThatDoesntError.isA(t) && !t.maybeNAOrNaN()) {
                            iterAnyChange = true;
                            i->replaceUsesWith(False::instance());
                            next = bb->remove(ip);
                        }
                    } else if (builtinId == blt("bodyCode") && nargs == 1) {
                        auto in = i->arg(0).val()->followCastsAndForce();
                        if (auto mk = MkCls::Cast(in)) {
                            iterAnyChange = true;
                            i->replaceUsesWith(
                                cmp.module->c(mk->originalBody->container()));
                            next = bb->remove(ip);
                        } else if (auto mk = MkArg::Cast(in)) {
                            // This can happen after inlining, since we use
                            // "bodyCode" in guards without forcing the promise.
                            if (auto cast = CastType::Cast(i->arg(0).val())) {
                                if (cast != in &&
                                    cast->kind == CastType::Downcast &&
                                    cast->type.isA(PirType::function())) {
                                    if (auto cast2 = CastType::Cast(
                                            cast->arg(0).val())) {
                                        if (cast2 != in &&
                                            cast2->kind == CastType::Upcast) {
                                            iterAnyChange = true;
                                            cast->replaceUsesAndSwapWith(
                                                new Force(
                                                    cast2, mk->env(),
                                                    Tombstone::framestate()),
                                                cast->bb()->atPosition(cast));
                                        }
                                    }
                                }
                            }
                        }
                    } else if (builtinId == blt("environment") && nargs == 1) {
                        auto in = i->arg(0).val()->followCastsAndForce();
                        if (auto mk = MkCls::Cast(in)) {
                            iterAnyChange = true;
                            i->replaceUsesWith(mk->lexicalEnv());
                        }
                    }
                }
                if (auto not_ = Not::Cast(i)) {
                    Value* arg = not_->arg<0>().val();
                    if (auto carg = isConst(arg, p)) {
                        if (IS_SIMPLE_SCALAR(carg, LGLSXP) ||
                            IS_SIMPLE_SCALAR(carg, INTSXP) ||
                            IS_SIMPLE_SCALAR(carg, REALSXP)) {
                            assert(NA_LOGICAL == NA_INTEGER);
                            int larg = -1;
                            switch (TYPEOF(carg)) {
                            case REALSXP:
                                larg = *REAL(carg) == NA_REAL
                                           ? NA_LOGICAL
                                           : (int)*REAL(carg);
                                break;
                            case INTSXP:
                                larg = *INTEGER(carg);
                                break;
                            case LGLSXP:
                                larg = *LOGICAL(carg);
                                break;
                            default:
                                assert(false);
                            }
                            int negArg;
                            if (larg == 0)
                                negArg = 1;
                            else if (larg == NA_LOGICAL)
                                negArg = NA_LOGICAL;
                            else
                                negArg = 0;
                            iterAnyChange = true;
                            i->replaceUsesWith(
                                cmp.module->c(Rf_ScalarLogical(negArg)));
                            next = bb->remove(ip);
                        }
                    } else if (auto not2 = Not::Cast(arg)) {
                        // Double negation
                        // not2 might still be used, if not then it will be
                        // removed in a later pass
                        iterAnyChange = true;
                        Value* rep = not2->arg<0>().val();
                        i->replaceUsesWith(rep);
                        next = bb->remove(ip);
                    }
                }
                if (auto colonInputEffects = ColonInputEffects::Cast(i)) {
                    bool done = false;

                    FOLD_BINARY(ColonInputEffects, [&](SEXP lhs, SEXP rhs) {
                        if (isColonFastcase(lhs, rhs)) {
                            i->replaceUsesWith(True::instance());
                            if (convertsToRealWithoutWarning(lhs) &&
                                convertsToRealWithoutWarning(rhs)) {
                                if (XLENGTH(lhs) == 1 && XLENGTH(rhs) == 1) {
                                    iterAnyChange = true;
                                    done = true;
                                    next = bb->remove(ip);
                                }
                            }
                        }
                    });

                    if (!done) {
                        auto lhs = colonInputEffects->arg<0>().val();
                        auto rhs = colonInputEffects->arg<1>().val();
                        auto notFactor = !lhs->type.maybeHasAttrs() ||
                                         !rhs->type.maybeHasAttrs();
                        // TODO this condition needs to go! see isColonFastcase
                        // in interp.cpp
                        auto cannotCauseCoercionProblems =
                            !lhs->type.maybe(PirType::num()) ||
                            !rhs->type.maybe(PirType::num());
                        if (notFactor && cannotCauseCoercionProblems) {
                            colonInputEffects->replaceUsesWith(
                                True::instance());
                            iterAnyChange = true;
                        }
                    }
                }

                if (auto castLhs = ColonCastLhs::Cast(i)) {
                    bool done = false;
                    FOLD_UNARY(ColonCastLhs, [&](SEXP lhs) {
                        if (convertsToRealWithoutWarning(lhs)) {
                            double lhsNum = Rf_asReal(lhs);
                            if (!std::isnan(lhsNum)) {
                                SEXP newLhs = colonCastLhs(lhs);
                                done = true;
                                iterAnyChange = true;
                                i->replaceUsesWith(cmp.module->c(newLhs));
                                next = bb->remove(ip);
                            }
                        }
                    });

                    if (!done && castLhs->arg(0).val()->type.isA(
                                     PirType(RType::integer).notNAOrNaN())) {
                        iterAnyChange = true;
                        i->replaceUsesWith(castLhs->arg(0).val());
                        next = bb->remove(ip);
                    }
                }

                if (auto castRhs = ColonCastRhs::Cast(i)) {
                    bool done = false;
                    FOLD_BINARY(ColonCastRhs, [&](SEXP newLhs, SEXP rhs) {
                        if (convertsToRealWithoutWarning(rhs) &&
                            convertsToRealWithoutWarning(newLhs)) {
                            double rhsNum = Rf_asReal(rhs);
                            if (!std::isnan(rhsNum)) {
                                done = true;
                                iterAnyChange = true;
                                SEXP newRhs = colonCastRhs(newLhs, rhs);
                                i->replaceUsesWith(cmp.module->c(newRhs));
                                next = bb->remove(ip);
                            }
                        }
                    });

                    if (!done && i->effects.includes(Effect::Error) &&
                        castRhs->arg(0).val()->type.isA(
                            PirType(RType::integer).notNAOrNaN()) &&
                        castRhs->arg(1).val()->type.isA(
                            PirType(RType::integer).notNAOrNaN())) {
                        iterAnyChange = true;
                        i->effects.reset(Effect::Error);
                    }
                }
                ip = next;
            }
            if (!bb->isEmpty())
                if (auto branch = Branch::Cast(bb->last())) {
                    auto condition = branch->arg<0>().val();
                    if (condition == True::instance()) {
                        iterAnyChange = true;
                        branchRemoval.emplace(bb, true);
                    } else if (condition == False::instance()) {
                        iterAnyChange = true;
                        branchRemoval.emplace(bb, false);
                    }
                }
        });
        if (!iterAnyChange)
            break;
        anyChange = true;
    }

    DominanceGraph::BBSet maybeDead;
    // Find all dead basic blocks
    for (const auto& e : branchRemoval) {
        const auto& bb = e.first;
        const auto& condition = e.second;
        for (auto i : *bb->getBranch(!condition))
            if (auto phi = Phi::Cast(i))
                phi->removeInputs({bb});
        bb->remove(bb->end() - 1);
        maybeDead.insert(bb->getBranch(!condition));
        bb->convertBranchToJmp(condition);
    }

    for (auto bb : newUnreachable) {
        auto succ = bb->successors();
        for (auto n : succ)
            for (auto i : *n)
                if (auto phi = Phi::Cast(i))
                    phi->removeInputs({bb});
        maybeDead.insert(succ.begin(), succ.end());
        bb->deleteSuccessors();
    }

    DominanceGraph::BBSet reachable;
    // Mark all still reachable BBs, the rest will be surely dead
    Visitor::run(code->entry, [&](BB* bb) { reachable.insert(bb); });

    DominanceGraph::BBSet dead;
    for (auto bb : maybeDead) {
        if (!reachable.count(bb)) {
            Visitor::run(bb, [&](BB* bb) {
                if (!reachable.count(bb))
                    dead.insert(bb);
            });
        }
    }

    // Needs to happen in two steps in case dead bb point to dead bb
    for (const auto& bb : dead) {
        for (auto n : bb->successors())
            if (reachable.count(n))
                for (auto i : *n)
                    if (auto phi = Phi::Cast(i))
                        phi->removeInputs({bb});
        bb->deleteSuccessors();
    }
    for (auto bb : dead) {
        assert(!reachable.count(bb));
        anyChange = true;
        delete bb;
    }

    return anyChange;
}

} // namespace pir
} // namespace rir
