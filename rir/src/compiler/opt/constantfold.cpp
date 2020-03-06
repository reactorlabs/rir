#include "../pir/pir_impl.h"
#include "../transform/bb.h"
#include "../translations/rir_compiler.h"
#include "../util/cfg.h"
#include "../util/visitor.h"
#include "R/BuiltinIds.h"
#include "R/Funtab.h"
#include "R/Symbols.h"
#include "R/r.h"
#include "interpreter/interp.h"
#include "pass_definitions.h"
#include "runtime/DispatchTable.h"

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

#define FOLD_BINARY_EITHER(Instruction, Operation)                             \
    do {                                                                       \
        if (auto instr = Instruction::Cast(i)) {                               \
            if (auto lhs = isConst(instr->arg<0>().val())) {                   \
                if (Operation(lhs->c(), instr->arg<1>().val()))                \
                    break;                                                     \
            }                                                                  \
            if (auto rhs = isConst(instr->arg<1>().val())) {                   \
                Operation(rhs->c(), instr->arg<0>().val());                    \
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
};

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

    {
        // Branch Elimination
        //
        // Given branch `a` and `b`, where both have the same
        // condition and `a` dominates `b`, we can replace the condition of `b`
        // by
        //
        //     c' = Phi([TRUE, a->trueBranch()], [FALSE, a->falseBranch()])
        //     b  = Branch(c')
        //
        // and even constantfold the `b` branch if either
        // `a->bb()->trueBranch()` or `a->bb()->falseBranch()` do not reach `b`.
        std::unordered_map<Instruction*, std::vector<Branch*>> condition;
        Visitor::run(function->entry, [&](BB* bb) {
            if (bb->isEmpty())
                return;
            auto branch = Branch::Cast(bb->last());
            if (!branch)
                return;
            if (auto i = Instruction::Cast(branch->arg(0).val()))
                condition[i].push_back(branch);
        });
        std::unordered_set<Branch*> removed;
        for (auto& c : condition) {
            auto uses = c.second;
            if (uses.size() > 1) {
                for (auto a = uses.begin(); (a + 1) != uses.end(); a++) {
                    if (removed.count(*a))
                        continue;
                    for (auto b = a + 1; b != uses.end(); b++) {
                        if (removed.count(*b))
                            continue;
                        auto bb1 = (*a)->bb();
                        auto bb2 = (*b)->bb();
                        if (dom.dominates(bb1, bb2)) {
                            if (dom.dominates(bb1->trueBranch(), bb2)) {
                                (*b)->arg(0).val() = True::instance();
                            } else if (dom.dominates(bb1->falseBranch(), bb2)) {
                                (*b)->arg(0).val() = False::instance();
                            } else {

                                bool success = true;

                                BB* next = dom.immediateDominator(bb2);
                                BB* target = bb2;

                                while (next != bb1) {
                                    if (dom.dominates(next, bb2))
                                        target = next;

                                    next = dom.immediateDominator(next);
                                }

                                auto p = new Phi;
                                for (auto pred : target->predecessors()) {
                                    if (dom.dominates(bb1->trueBranch(),
                                                      pred)) {
                                        p->addInput(pred, True::instance());
                                    } else if (dom.dominates(bb1->falseBranch(),
                                                             pred)) {
                                        p->addInput(pred, False::instance());
                                    } else {
                                        success = false;
                                    }
                                }

                                if (!success) {
                                    delete p;
                                    continue;
                                }
                                p->type = NativeType::test;
                                target->insert(target->begin(), p);
                                (*b)->arg(0).val() = p;
                            }
                            removed.insert(*b);
                        }
                    }
                }
            }
        }
    }

    Visitor::run(function->entry, [&](BB* bb) {
        if (bb->isEmpty())
            return;
        auto ip = bb->begin();
        while (ip != bb->end()) {
            auto i = *ip;
            auto next = ip + 1;

            auto foldLglCmp = [&](SEXP carg, Value* varg, bool isEq) {
                if (!isConst(varg) && // If this is true, was already folded
                    IS_SIMPLE_SCALAR(carg, LGLSXP) &&
                    varg->type.isA(PirType::simpleScalarLogical())) {
                    int larg = *LOGICAL(carg);
                    if (larg == (int)isEq) {
                        i->replaceUsesWith(varg);
                        next = bb->remove(ip);
                    } else if (larg == (int)!isEq) {
                        auto res = new Not(varg, i->env(), i->srcIdx);
                        // Guarenteed, and required by replaceUsesWith
                        res->type = PirType::simpleScalarLogical();
                        i->replaceUsesWith(res);
                        bb->replace(ip, res);
                    } else if (larg == NA_LOGICAL) {
                        // Even NA == NA (and NA != NA) yield NA
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

            if (LOr::Cast(i) || LAnd::Cast(i)) {
                if (i->arg(0).val() == i->arg(1).val()) {
                    i->replaceUsesWith(i->arg(0).val());
                    next = bb->remove(ip);
                }
            }

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
            FOLD_BINARY_NATIVE(Pow, symbol::Pow);
            FOLD_BINARY_NATIVE(Eq, symbol::Eq);
            FOLD_BINARY_NATIVE(Neq, symbol::Ne);

            FOLD_BINARY_EITHER(Eq, [&](SEXP carg, Value* varg) {
                return foldLglCmp(carg, varg, true);
            });

            FOLD_BINARY_EITHER(Neq, [&](SEXP carg, Value* varg) {
                return foldLglCmp(carg, varg, false);
            });

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

            if (Identical::Cast(i)) {
                // Those are targeting the checks for default argument
                // evaluation after inlining
                if (i->arg(1).val() == MissingArg::instance()) {
                    if (i->arg(0).val() == MissingArg::instance()) {
                        i->replaceUsesWith(True::instance());
                        next = bb->remove(ip);
                    } else if (!i->arg(0).val()->type.maybeMissing()) {
                        i->replaceUsesWith(False::instance());
                        next = bb->remove(ip);
                    }
                } else {
                    FOLD_BINARY(Identical, [&](SEXP a, SEXP b) {
                        i->replaceUsesWith(a == b ? (Value*)True::instance()
                                                  : (Value*)False::instance());
                        next = bb->remove(ip);
                    });
                }
            }

            if (auto isTest = IsType::Cast(i)) {
                auto arg = isTest->arg<0>().val();
                if (arg->type.isA(isTest->typeTest)) {
                    i->replaceUsesWith(True::instance());
                    next = bb->remove(ip);
                } else if (!arg->type.maybe(isTest->typeTest)) {
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

            if (auto cl = Colon::Cast(i)) {
                if (auto a = isConst(cl->arg(0).val())) {
                    if (TYPEOF(a->c()) == REALSXP && Rf_length(a->c()) == 1 &&
                        REAL(a->c())[0] == (double)(int)REAL(a->c())[0]) {
                        ip = bb->insert(ip, new LdConst((int)REAL(a->c())[0]));
                        cl->arg(0).val() = *ip;
                        ip++;
                    }
                }
                if (auto a = isConst(cl->arg(1).val())) {
                    if (TYPEOF(a->c()) == REALSXP && Rf_length(a->c()) == 1 &&
                        REAL(a->c())[0] == (double)(int)REAL(a->c())[0]) {
                        ip = bb->insert(ip, new LdConst((int)REAL(a->c())[0]));
                        cl->arg(1).val() = *ip;
                        ip++;
                    }
                }
                next = ip + 1;
            }

            if (CallSafeBuiltin::Cast(i) || CallBuiltin::Cast(i)) {
                int builtinId = CallBuiltin::Cast(i)
                                    ? CallBuiltin::Cast(i)->builtinId
                                    : CallSafeBuiltin::Cast(i)->builtinId;
                size_t nargs = CallInstruction::CastCall(i)->nCallArgs();
                assert(function->assumptions().includes(
                    Assumption::NotTooManyArguments));
                // PIR functions are always compiled for a particular number
                // of arguments
                auto noExplMissing = function->assumptions().includes(
                    Assumption::NoExplicitlyMissingArgs);
                if (builtinId == blt("nargs") && noExplMissing) {
                    // nargs inside inlinee refers to nargs passed to inlinee,
                    // which is something we cannot recover.
                    bool notInlined =
                        Visitor::check(function->entry, [](Instruction* i) {
                            return !PushContext::Cast(i);
                        });
                    if (notInlined) {
                        auto nargsC = new LdConst(ScalarInteger(
                            function->nargs() -
                            function->assumptions().numMissing()));
                        i->replaceUsesAndSwapWith(nargsC, ip);
                    }
                } else if (builtinId == blt("length") && nargs == 1) {
                    auto t = i->arg(0).val()->type;
                    if (t.isA(PirType::simpleScalar())) {
                        i->replaceUsesAndSwapWith(new LdConst(1), ip);
                    }
                } else if (builtinId == blt("as.character") && nargs == 1) {
                    auto t = i->arg(0).val()->type;
                    if (t.isA(PirType(RType::str)
                                  .notPromiseWrapped()
                                  .notObject())) {
                        i->replaceUsesWith(i->arg(0).val());
                        next = bb->remove(ip);
                    } else if (auto con = isConst(i->arg(0).val())) {
                        auto t = TYPEOF(con->c());
                        if (t == REALSXP || t == INTSXP || t == LGLSXP) {
                            auto res = Rf_eval(
                                Rf_lang2(Rf_install("as.character"), con->c()),
                                R_BaseEnv);
                            i->replaceUsesAndSwapWith(new LdConst(res), ip);
                        }
                    }
                } else if (builtinId == blt("as.integer") && nargs == 1) {
                    auto t = i->arg(0).val()->type;
                    if (t.isA(PirType(RType::integer)
                                  .notPromiseWrapped()
                                  .notObject())) {
                        i->replaceUsesWith(i->arg(0).val());
                        next = bb->remove(ip);
                    } else if (auto con = isConst(i->arg(0).val())) {
                        if (IS_SIMPLE_SCALAR(con->c(), REALSXP)) {
                            if (REAL(con->c())[0] == REAL(con->c())[0]) {
                                i->replaceUsesAndSwapWith(
                                    new LdConst((int)REAL(con->c())[0]), ip);
                            } else {
                                i->replaceUsesAndSwapWith(
                                    new LdConst(NA_INTEGER), ip);
                            }
                        } else if (IS_SIMPLE_SCALAR(con->c(), INTSXP)) {
                            i->replaceUsesAndSwapWith(new LdConst(con->c()),
                                                      ip);
                        }
                    }
                } else if (builtinId == blt("is.vector") && nargs == 2) {
                    // We could do better here but for now we just look
                    // at the default mode "any"
                    // Also, the names attribute is allowed but we approximate
                    // by checking that there are no attributes
                    auto argType = i->arg(0).val()->type;
                    auto modeType = i->arg(1).val()->type;
                    auto mode = LdConst::Cast(i->arg(1).val());
                    if (!argType.maybeHasAttrs() &&
                        argType.isA(PirType::vecs()) &&
                        modeType.isA(PirType::simpleScalarString()) && mode &&
                        staticStringEqual(CHAR(STRING_ELT(mode->c(), 0)),
                                          "any")) {
                        i->replaceUsesAndSwapWith(new LdConst(R_TrueValue), ip);
                    }
                } else if (builtinId == blt("is.function") && nargs == 1) {
                    auto t = i->arg(0).val()->type;
                    if (t.isA(RType::closure))
                        i->replaceUsesAndSwapWith(new LdConst(R_TrueValue), ip);
                    else if (!t.maybe(RType::closure))
                        i->replaceUsesAndSwapWith(new LdConst(R_FalseValue),
                                                  ip);
                } else if (builtinId == blt("is.complex") && nargs == 1) {
                    auto t = i->arg(0).val()->type;
                    if (t.isA(RType::cplx))
                        i->replaceUsesAndSwapWith(new LdConst(R_TrueValue), ip);
                    else if (!t.maybe(RType::cplx))
                        i->replaceUsesAndSwapWith(new LdConst(R_FalseValue),
                                                  ip);
                } else if (builtinId == blt("is.character") && nargs == 1) {
                    auto t = i->arg(0).val()->type;
                    if (t.isA(RType::str)) {
                        i->replaceUsesAndSwapWith(new LdConst(R_TrueValue), ip);
                    } else if (!t.maybe(RType::str)) {
                        i->replaceUsesAndSwapWith(new LdConst(R_FalseValue),
                                                  ip);
                    }
                } else if (builtinId == blt("is.atomic") && nargs == 1) {
                    auto t = i->arg(0).val()->type;
                    static auto atomicType =
                        PirType() | RType::nil | RType::chr | RType::logical |
                        RType::integer | RType::real | RType::cplx |
                        RType::str | RType::raw;
                    if (t.isA(atomicType)) {
                        i->replaceUsesAndSwapWith(new LdConst(R_TrueValue), ip);
                    } else if (!t.maybe(atomicType)) {
                        i->replaceUsesAndSwapWith(new LdConst(R_FalseValue),
                                                  ip);
                    }
                } else if (builtinId == blt("is.object") && nargs == 1) {
                    auto t = i->arg(0).val()->type;
                    if (!t.maybeObj()) {
                        i->replaceUsesAndSwapWith(new LdConst(R_FalseValue),
                                                  ip);
                    }
                } else if (builtinId == blt("is.na") && nargs == 1) {
                    auto t = i->arg(0).val()->type;
                    static PirType typeThatDoesntError =
                        (PirType::num() | RType::chr | RType::str | RType::vec)
                            .orAttribs();
                    if (typeThatDoesntError.isA(t) && !t.maybeNan()) {
                        i->replaceUsesAndSwapWith(new LdConst(R_FalseValue),
                                                  ip);
                    }
                } else if (builtinId == blt("bodyCode") && nargs == 1) {
                    auto in = i->arg(0).val()->followCastsAndForce();
                    if (auto mk = MkFunCls::Cast(in)) {
                        i->replaceUsesAndSwapWith(
                            new LdConst(mk->originalBody->container()), ip);
                    } else if (auto mk = MkCls::Cast(in)) {
                        i->replaceUsesWith(mk->code());
                    } else if (auto mk = MkArg::Cast(in)) {
                        // This can happen after inlining, since we use
                        // "bodyCode" in guards without forcing the promise.
                        if (auto cast = CastType::Cast(i->arg(0).val())) {
                            if (cast != in &&
                                cast->kind == CastType::Downcast &&
                                cast->type.isA(RType::closure)) {
                                if (auto cast2 =
                                        CastType::Cast(cast->arg(0).val())) {
                                    if (cast2 != in &&
                                        cast2->kind == CastType::Upcast) {
                                        cast->replaceUsesAndSwapWith(
                                            new Force(cast2, mk->env()),
                                            cast->bb()->atPosition(cast));
                                    }
                                }
                            }
                        }
                    }
                } else if (builtinId == blt("environment") && nargs == 1) {
                    auto in = i->arg(0).val()->followCastsAndForce();
                    if (auto mk = MkFunCls::Cast(in)) {
                        i->replaceUsesWith(mk->lexicalEnv());
                    } else if (auto mk = MkCls::Cast(in)) {
                        i->replaceUsesWith(mk->lexicalEnv());
                    }
                }
            }

            if (auto not_ = Not::Cast(i)) {
                Value* arg = not_->arg<0>().val();
                if (auto varg = isConst(arg)) {
                    SEXP carg = varg->c();
                    if (IS_SIMPLE_SCALAR(carg, LGLSXP) ||
                        IS_SIMPLE_SCALAR(carg, INTSXP) ||
                        IS_SIMPLE_SCALAR(carg, REALSXP)) {
                        assert(NA_LOGICAL == NA_INTEGER);
                        int larg = -1;
                        switch (TYPEOF(carg)) {
                        case REALSXP:
                            larg = *REAL(carg) == NA_REAL ? NA_LOGICAL
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
                        auto rep = new LdConst(Rf_ScalarLogical(negArg));
                        i->replaceUsesWith(rep);
                        bb->replace(ip, rep);
                    }
                } else if (auto not2 = Not::Cast(arg)) {
                    // Double negation
                    // not2 might still be used, if not then it will be removed
                    // in a later pass
                    Value* rep = not2->arg<0>().val();
                    i->replaceUsesWith(rep);
                    next = bb->remove(ip);
                }
            }

            if (auto colonInputEffects = ColonInputEffects::Cast(i)) {
                auto lhs = colonInputEffects->arg<0>().val();
                auto rhs = colonInputEffects->arg<1>().val();
                if ((!lhs->type.maybeHasAttrs() ||
                     !rhs->type.maybeHasAttrs()) &&
                    !((lhs->type.maybe(RType::integer) ||
                       lhs->type.maybe(RType::logical)) &&
                      rhs->type.maybe(RType::integer))) {
                    // We still need to keep the colonInputEffects because it
                    // could raise warnings / errors
                    colonInputEffects->replaceUsesWith(True::instance());
                }
            }

            FOLD_UNARY(ColonCastLhs, [&](SEXP lhs) {
                if (convertsToRealWithoutWarning(lhs)) {
                    SEXP newLhs = colonCastLhs(lhs);
                    LdConst* newLhsInstr = new LdConst(newLhs);
                    i->replaceUsesAndSwapWith(newLhsInstr, ip);
                }
            });

            FOLD_BINARY(ColonCastRhs, [&](SEXP newLhs, SEXP rhs) {
                if (convertsToRealWithoutWarning(rhs) &&
                    convertsToRealWithoutWarning(newLhs)) {
                    SEXP newRhs = colonCastRhs(newLhs, rhs);
                    LdConst* newRhsInstr = new LdConst(newRhs);
                    i->replaceUsesAndSwapWith(newRhsInstr, ip);
                }
            });

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

    // Find all dead basic blocks
    DominanceGraph::BBSet dead;
    for (const auto& e : branchRemoval) {
        const auto& branch = e.first;
        const auto& condition = e.second;
        dead.insert(condition ? branch->falseBranch() : branch->trueBranch());
    }
    auto toDelete = DominanceGraph::dominatedSet(function, dead);

    Visitor::run(function->entry, [&](Instruction* i) {
        if (auto phi = Phi::Cast(i))
            phi->removeInputs(toDelete);
    });

    for (const auto& e : branchRemoval) {
        const auto& branch = e.first;
        const auto& condition = e.second;
        for (auto i : *branch->getBranch(!condition))
            if (auto phi = Phi::Cast(i))
                phi->removeInputs({branch});
        branch->remove(branch->end() - 1);
        branch->convertBranchToJmp(condition);
    }

    // Needs to happen in two steps in case dead bb point to dead bb
    for (const auto& bb : toDelete)
        bb->deleteSuccessors();
    for (const auto& bb : toDelete)
        delete bb;
}
} // namespace pir
} // namespace rir
