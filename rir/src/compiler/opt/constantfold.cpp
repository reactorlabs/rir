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
                    anyChange = true;                                          \
                    instr->replaceUsesWith(resi);                              \
                    bb->replace(ip, resi);                                     \
                }                                                              \
            }                                                                  \
        }                                                                      \
    } while (false)
#define FOLD_UNARY(Instruction, Operation)                                     \
    do {                                                                       \
        if (auto instr = Instruction::Cast(i)) {                               \
            if (auto arg = isConst(instr->arg<0>().val()))                     \
                Operation(arg->c());                                           \
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

bool Constantfold::apply(Compiler& cmp, ClosureVersion* cls, Code* code,
                         LogStream&) const {
    bool anyChange = false;

    std::unordered_map<BB*, bool> branchRemoval;

    DominanceGraph dom(code);
    DominanceFrontier dfront(code, dom);
    {
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
        std::unordered_set<Branch*> removed;

        for (auto& c : condition) {

            removed.clear();
            auto& uses = c.second;
            if (uses.size() > 1) {

                for (auto a = uses.begin(); (a + 1) != uses.end(); a++) {

                    if (removed.count(*a))
                        continue;

                    PhiPlacement* pl = nullptr;
                    auto phisPlaced = false;
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
                                    pl = new PhiPlacement(code, inputs, dom,
                                                          dfront);

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
                                        phi->type = NativeType::test;
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

                    if (pl != nullptr) {
                        delete pl;
                        pl = nullptr;
                    }
                }
            }
        }
    }

    Visitor::run(code->entry, [&](BB* bb) {
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
                        anyChange = true;
                        i->replaceUsesWith(varg);
                        next = bb->remove(ip);
                    } else if (larg == (int)!isEq) {
                        auto res = new Not(varg, i->env(), i->srcIdx);
                        // Guarenteed, and required by replaceUsesWith
                        res->type = PirType::simpleScalarLogical();
                        anyChange = true;
                        i->replaceUsesWith(res);
                        bb->replace(ip, res);
                    } else if (larg == NA_LOGICAL) {
                        // Even NA == NA (and NA != NA) yield NA
                        anyChange = true;
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
                    anyChange = true;
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
                    anyChange = true;
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
                        anyChange = true;
                    }
                }
            });
            if (Identical::Cast(i)) {
                // Those are targeting the checks for default argument
                // evaluation after inlining
                if (i->arg(1).val() == MissingArg::instance()) {
                    if (i->arg(0).val() == MissingArg::instance()) {
                        anyChange = true;
                        i->replaceUsesWith(True::instance());
                        next = bb->remove(ip);
                    } else if (!i->arg(0).val()->type.maybeMissing()) {
                        anyChange = true;
                        i->replaceUsesWith(False::instance());
                        next = bb->remove(ip);
                    }
                } else {
                    FOLD_BINARY(Identical, [&](SEXP a, SEXP b) {
                        anyChange = true;
                        i->replaceUsesWith(a == b ? (Value*)True::instance()
                                                  : (Value*)False::instance());
                        next = bb->remove(ip);
                    });
                }
            }
            if (auto isTest = IsType::Cast(i)) {
                auto arg = isTest->arg<0>().val();
                if (arg->type.isA(isTest->typeTest)) {
                    anyChange = true;
                    i->replaceUsesWith(True::instance());
                    next = bb->remove(ip);
                } else if (!arg->type.maybe(isTest->typeTest)) {
                    anyChange = true;
                    i->replaceUsesWith(False::instance());
                    next = bb->remove(ip);
                }
            }
            if (auto assume = Assume::Cast(i)) {
                if (assume->arg<0>().val() == True::instance() &&
                    assume->assumeTrue) {
                    anyChange = true;
                    next = bb->remove(ip);
                } else if (assume->arg<0>().val() == False::instance() &&
                           !assume->assumeTrue) {
                    anyChange = true;
                    next = bb->remove(ip);
                }
            }
            if (auto cl = Colon::Cast(i)) {
                if (auto a = isConst(cl->arg(0).val())) {
                    if (TYPEOF(a->c()) == REALSXP && Rf_length(a->c()) == 1 &&
                        REAL(a->c())[0] == (double)(int)REAL(a->c())[0]) {
                        anyChange = true;
                        ip = bb->insert(ip, new LdConst((int)REAL(a->c())[0]));
                        cl->arg(0).val() = *ip;
                        ip++;
                    }
                }
                if (auto a = isConst(cl->arg(1).val())) {
                    if (TYPEOF(a->c()) == REALSXP && Rf_length(a->c()) == 1 &&
                        REAL(a->c())[0] == (double)(int)REAL(a->c())[0]) {
                        anyChange = true;
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
                assert(
                    cls->context().includes(Assumption::NotTooManyArguments));
                // PIR functions are always compiled for a particular number
                // of arguments
                auto noExplMissing = cls->context().includes(
                    Assumption::NoExplicitlyMissingArgs);
                if (builtinId == blt("nargs") && noExplMissing) {
                    // nargs inside inlinee refers to nargs passed to inlinee,
                    // which is something we cannot recover.
                    bool notInlined =
                        Visitor::check(code->entry, [](Instruction* i) {
                            return !PushContext::Cast(i);
                        });
                    if (notInlined) {
                        auto nargsC =
                            new LdConst(ScalarInteger(cls->effectiveNArgs()));
                        anyChange = true;
                        i->replaceUsesAndSwapWith(nargsC, ip);
                    }
                } else if (builtinId == blt("length") && nargs == 1) {
                    auto t = i->arg(0).val()->type;
                    if (t.isA(PirType::simpleScalar())) {
                        anyChange = true;
                        i->replaceUsesAndSwapWith(new LdConst(1), ip);
                    }
                } else if (builtinId == blt("as.character") && nargs == 1) {
                    auto t = i->arg(0).val()->type;
                    if (t.isA(PirType(RType::str)
                                  .notPromiseWrapped()
                                  .notObject())) {
                        anyChange = true;
                        i->replaceUsesWith(i->arg(0).val());
                        next = bb->remove(ip);
                    } else if (auto con = isConst(i->arg(0).val())) {
                        auto t = TYPEOF(con->c());
                        if (t == REALSXP || t == INTSXP || t == LGLSXP) {
                            auto res = Rf_eval(
                                Rf_lang2(Rf_install("as.character"), con->c()),
                                R_BaseEnv);
                            anyChange = true;
                            i->replaceUsesAndSwapWith(new LdConst(res), ip);
                        }
                    }
                } else if (builtinId == blt("as.integer") && nargs == 1) {
                    auto t = i->arg(0).val()->type;
                    if (t.isA(PirType(RType::integer)
                                  .notPromiseWrapped()
                                  .notObject())) {
                        anyChange = true;
                        i->replaceUsesWith(i->arg(0).val());
                        next = bb->remove(ip);
                    } else if (auto con = isConst(i->arg(0).val())) {
                        if (IS_SIMPLE_SCALAR(con->c(), REALSXP)) {
                            if (REAL(con->c())[0] == REAL(con->c())[0]) {
                                anyChange = true;
                                i->replaceUsesAndSwapWith(
                                    new LdConst((int)REAL(con->c())[0]), ip);
                            } else {
                                anyChange = true;
                                i->replaceUsesAndSwapWith(
                                    new LdConst(NA_INTEGER), ip);
                            }
                        } else if (IS_SIMPLE_SCALAR(con->c(), INTSXP)) {
                            anyChange = true;
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
                        anyChange = true;
                        i->replaceUsesAndSwapWith(new LdConst(R_TrueValue), ip);
                    }
                } else if (builtinId == blt("is.function") && nargs == 1) {
                    auto t = i->arg(0).val()->type;
                    if (t.isA(RType::closure)) {
                        anyChange = true;
                        i->replaceUsesAndSwapWith(new LdConst(R_TrueValue), ip);
                    } else if (!t.maybe(RType::closure)) {
                        anyChange = true;
                        i->replaceUsesAndSwapWith(new LdConst(R_FalseValue),
                                                  ip);
                    }
                } else if (builtinId == blt("is.character") && nargs == 1) {
                    auto t = i->arg(0).val()->type;
                    if (t.isA(RType::str)) {
                        anyChange = true;
                        i->replaceUsesAndSwapWith(new LdConst(R_TrueValue), ip);
                    } else if (!t.maybe(RType::str)) {
                        anyChange = true;
                        i->replaceUsesAndSwapWith(new LdConst(R_FalseValue),
                                                  ip);
                    }
                } else if (builtinId == blt("is.logical") && nargs == 1) {
                    auto t = i->arg(0).val()->type;
                    if (t.isA(RType::logical)) {
                        anyChange = true;
                        i->replaceUsesAndSwapWith(new LdConst(R_TrueValue), ip);
                    } else if (!t.maybe(RType::logical)) {
                        anyChange = true;
                        i->replaceUsesAndSwapWith(new LdConst(R_FalseValue),
                                                  ip);
                    }
                } else if (builtinId == blt("is.double") && nargs == 1) {
                    auto t = i->arg(0).val()->type;
                    if (t.isA(RType::real)) {
                        anyChange = true;
                        i->replaceUsesAndSwapWith(new LdConst(R_TrueValue), ip);
                    } else if (!t.maybe(RType::real)) {
                        anyChange = true;
                        i->replaceUsesAndSwapWith(new LdConst(R_FalseValue),
                                                  ip);
                    }
                } else if (builtinId == blt("is.integer") && nargs == 1) {
                    auto t = i->arg(0).val()->type;
                    if (t.isA(PirType(RType::integer).noAttribs())) {
                        anyChange = true;
                        i->replaceUsesAndSwapWith(new LdConst(R_TrueValue), ip);
                    } else if (!t.maybe(RType::integer)) {
                        anyChange = true;
                        i->replaceUsesAndSwapWith(new LdConst(R_FalseValue),
                                                  ip);
                    }
                } else if (builtinId == blt("is.complex") && nargs == 1) {
                    auto t = i->arg(0).val()->type;
                    if (t.isA(RType::cplx)) {
                        anyChange = true;
                        i->replaceUsesAndSwapWith(new LdConst(R_TrueValue), ip);
                    } else if (!t.maybe(RType::cplx)) {
                        anyChange = true;
                        i->replaceUsesAndSwapWith(new LdConst(R_FalseValue),
                                                  ip);
                    }
                } else if (builtinId == blt("is.character") && nargs == 1) {
                    auto t = i->arg(0).val()->type;
                    if (t.isA(RType::str)) {
                        anyChange = true;
                        i->replaceUsesAndSwapWith(new LdConst(R_TrueValue), ip);
                    } else if (!t.maybe(RType::str)) {
                        anyChange = true;
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
                        anyChange = true;
                        i->replaceUsesAndSwapWith(new LdConst(R_TrueValue), ip);
                    } else if (!t.maybe(atomicType)) {
                        anyChange = true;
                        i->replaceUsesAndSwapWith(new LdConst(R_FalseValue),
                                                  ip);
                    }
                } else if (builtinId == blt("anyNA") && nargs == 1) {
                    auto t = i->arg(0).val()->type;
                    if (!t.maybeNAOrNaN()) {
                        anyChange = true;
                        i->replaceUsesAndSwapWith(new LdConst(R_FalseValue),
                                                  ip);
                    }
                } else if (builtinId == blt("is.object") && nargs == 1) {
                    auto t = i->arg(0).val()->type;
                    if (!t.maybeObj()) {
                        anyChange = true;
                        i->replaceUsesAndSwapWith(new LdConst(R_FalseValue),
                                                  ip);
                    }
                } else if ((builtinId == blt("is.na") ||
                            builtinId == blt("is.nan")) &&
                           nargs == 1) {
                    auto t = i->arg(0).val()->type;
                    static PirType typeThatDoesntError =
                        (PirType::num() | RType::chr | RType::str | RType::vec)
                            .orAttribs();
                    if (typeThatDoesntError.isA(t) && !t.maybeNAOrNaN()) {
                        anyChange = true;
                        i->replaceUsesAndSwapWith(new LdConst(R_FalseValue),
                                                  ip);
                    }
                } else if (builtinId == blt("bodyCode") && nargs == 1) {
                    auto in = i->arg(0).val()->followCastsAndForce();
                    if (auto mk = MkFunCls::Cast(in)) {
                        anyChange = true;
                        i->replaceUsesAndSwapWith(
                            new LdConst(mk->originalBody->container()), ip);
                    } else if (auto mk = MkCls::Cast(in)) {
                        anyChange = true;
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
                                        anyChange = true;
                                        cast->replaceUsesAndSwapWith(
                                            new Force(cast2, mk->env(),
                                                      Tombstone::framestate()),
                                            cast->bb()->atPosition(cast));
                                    }
                                }
                            }
                        }
                    }
                } else if (builtinId == blt("environment") && nargs == 1) {
                    auto in = i->arg(0).val()->followCastsAndForce();
                    if (auto mk = MkFunCls::Cast(in)) {
                        anyChange = true;
                        i->replaceUsesWith(mk->lexicalEnv());
                    } else if (auto mk = MkCls::Cast(in)) {
                        anyChange = true;
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
                        anyChange = true;
                        auto rep = new LdConst(Rf_ScalarLogical(negArg));
                        i->replaceUsesWith(rep);
                        bb->replace(ip, rep);
                    }
                } else if (auto not2 = Not::Cast(arg)) {
                    // Double negation
                    // not2 might still be used, if not then it will be removed
                    // in a later pass
                    anyChange = true;
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
                                anyChange = true;
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
                    // TODO this condition needs to go! see isColonFastcase in
                    // interp.cpp
                    auto cannotCauseCoercionProblems =
                        !lhs->type.maybe(PirType::num()) ||
                        !rhs->type.maybe(PirType::num());
                    if (notFactor && cannotCauseCoercionProblems) {
                        colonInputEffects->replaceUsesWith(True::instance());
                        anyChange = true;
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
                            LdConst* newLhsInstr = new LdConst(newLhs);
                            done = true;
                            anyChange = true;
                            i->replaceUsesAndSwapWith(newLhsInstr, ip);
                        }
                    }
                });

                if (!done && castLhs->arg(0).val()->type.isA(
                                 PirType(RType::integer).notNAOrNaN())) {
                    anyChange = true;
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
                            anyChange = true;
                            SEXP newRhs = colonCastRhs(newLhs, rhs);
                            LdConst* newRhsInstr = new LdConst(newRhs);
                            i->replaceUsesAndSwapWith(newRhsInstr, ip);
                        }
                    }
                });

                if (!done &&
                    castRhs->arg(0).val()->type.isA(
                        PirType(RType::integer).notNAOrNaN()) &&
                    castRhs->arg(1).val()->type.isA(
                        PirType(RType::integer).notNAOrNaN())) {
                    anyChange = true;
                    i->replaceUsesWith(castRhs->arg(0).val());
                    next = bb->remove(ip);
                }
            }
            ip = next;
        }
        if (!bb->isEmpty())
            if (auto branch = Branch::Cast(bb->last())) {
                auto condition = branch->arg<0>().val();
                if (condition == True::instance()) {
                    anyChange = true;
                    branchRemoval.emplace(bb, true);
                } else if (condition == False::instance()) {
                    anyChange = true;
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
    auto toDelete = DominanceGraph::dominatedSet(code, dead);
    Visitor::run(code->entry, [&](Instruction* i) {
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

    return anyChange;
}
} // namespace pir
} // namespace rir
