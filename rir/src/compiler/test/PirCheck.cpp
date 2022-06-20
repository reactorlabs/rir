#include "PirCheck.h"
#include "../analysis/query.h"
#include "../analysis/verifier.h"
#include "../pir/pir_impl.h"
#include "../util/visitor.h"
#include "api.h"
#include "bc/Compiler.h"
#include "compiler/compiler.h"
#include "compiler/parameter.h"
#include <string>
#include <vector>

namespace rir {

using namespace pir;

static ClosureVersion* recompilePir(SEXP f, Module* m) {
    if (TYPEOF(f) != CLOSXP) {
        Rf_warning("pir check failed: not a closure");
        return nullptr;
    }
    if (!isValidClosureSEXP(f)) {
        Rf_warning("pir check failed: not a RIR closure");
        return nullptr;
    }
    if (R_ENABLE_JIT == 0) {
        Rf_warning("R JIT disabled, this will prevent some optimizations");
    }
    assert(DispatchTable::check(BODY(f)));
    auto table = DispatchTable::unpack(BODY(f));
    auto assumptions = table->best()->context() | pir::Compiler::minimalContext;

    Log logger(DebugOptions::DefaultDebugOptions);
    logger.title("Pir Check");
    pir::Compiler cmp(m, logger);
    ClosureVersion* res = nullptr;
    cmp.compileClosure(
        f, "pirCheck", assumptions, true, [&](ClosureVersion* r) { res = r; },
        []() { Rf_warning("pir check failed: couldn't compile"); }, {});

    if (!res)
        return nullptr;
    cmp.optimizeModule();
    return res;
}

static bool testIsPirCompilable(ClosureVersion* f) {
    // Always true if we get here
    return true;
}

static bool testNoLoad(ClosureVersion* f) {
    return Visitor::check(f->entry, [&](Instruction* i) {
        return !LdVar::Cast(i) && !LdFun::Cast(i);
    });
}

static bool testNoStore(ClosureVersion* f) {
    return Visitor::check(f->entry, [&](Instruction* i) {
        return !StVar::Cast(i) && !StVarSuper::Cast(i);
    });
}

static bool testNoStSuper(ClosureVersion* f) {
    return Visitor::check(f->entry,
                          [&](Instruction* i) { return !StVarSuper::Cast(i); });
}

static bool testNoEnvForAdd(ClosureVersion* f) {
    return Visitor::check(f->entry, [&](BB* bb) -> bool {
        for (auto& i : *bb) {
            if (auto a = Add::Cast(i)) {
                if (a->env() != Env::elided()) {
                    return false;
                }
            }
        }
        return true;
    });
}

static bool testNoEnvSpec(ClosureVersion* f) { return Query::noEnvSpec(f); }

static bool testNoEnv(ClosureVersion* f) { return Query::noEnv(f); }

static bool testNoPromise(ClosureVersion* f) {
    return VisitorNoDeoptBranch::check(
        f->entry, [&](Instruction* i) { return !MkArg::Cast(i); });
}

static bool testNoExternalCalls(ClosureVersion* f) {
    return Visitor::check(f->entry, [&](Instruction* i) {
        return !CallInstruction::CastCall(i) || CallSafeBuiltin::Cast(i);
    });
}

static bool testUnboxedExtract(ClosureVersion* f) {
    return Visitor::check(f->entry, [&](Instruction* i) {
        switch (i->tag) {
        case Tag::Extract1_1D:
        case Tag::Extract1_2D:
        case Tag::Extract1_3D:
        case Tag::Extract2_1D:
        case Tag::Extract2_2D:
            return i->type.unboxable();
        default: {}
        }
        return true;
    });
}

static bool testReturns42L(ClosureVersion* f) {
    if (!Query::noEnvSpec(f))
        return false;
    auto r = Query::returned(f);
    if (r.size() != 1)
        return false;
    auto ld = Const::Cast((*r.begin()));
    if (ld == nullptr || TYPEOF(ld->c()) != INTSXP || *INTEGER(ld->c()) != 42)
        return false;
    return true;
}

static bool testNoColon(ClosureVersion* f) {
    return Visitor::check(f->entry,
                          [&](Instruction* i) { return !Colon::Cast(i); });
}

static bool testNoEq(ClosureVersion* f) {
    return Visitor::check(f->entry,
                          [&](Instruction* i) { return !Eq::Cast(i); });
}

static bool testOneEq(ClosureVersion* f) {
    int numEqs = 0;
    Visitor::run(f->entry, [&](Instruction* i) {
        if (Eq::Cast(i))
            numEqs++;
    });
    return numEqs == 1;
}

static bool testOneLdVar(ClosureVersion* f) {
    int numLdVar = 0;
    Visitor::run(f->entry, [&](Instruction* i) {
        if (LdVar::Cast(i))
            numLdVar++;
    });
    return numLdVar == 1;
}

static bool testNoLdFun(ClosureVersion* f) {
    int numLdFun = 0;
    Visitor::run(f->entry, [&](Instruction* i) {
        if (LdFun::Cast(i))
            numLdFun++;
    });
    return numLdFun == 0;
}

static bool testOneLdFun(ClosureVersion* f) {
    int numLdFun = 0;
    Visitor::run(f->entry, [&](Instruction* i) {
        if (LdFun::Cast(i))
            numLdFun++;
    });
    return numLdFun == 1;
}

static bool testOneNot(ClosureVersion* f) {
    int numNots = 0;
    Visitor::run(f->entry, [&](Instruction* i) {
        if (Not::Cast(i))
            numNots++;
    });
    return numNots == 1;
}

static bool testOneAdd(ClosureVersion* f) {
    int numAdds = 0;
    Visitor::run(f->entry, [&](Instruction* i) {
        if (Add::Cast(i))
            numAdds++;
    });
    return numAdds == 1;
}

static bool testTwoAdd(ClosureVersion* f) {
    int numAdds = 0;
    Visitor::run(f->entry, [&](Instruction* i) {
        if (Add::Cast(i))
            numAdds++;
    });
    return numAdds == 2;
}

static bool testLdVarVectorInFirstBB(ClosureVersion* f) {
    for (auto instruction : *f->entry) {
        if (auto ldvar = LdVar::Cast(instruction)) {
            return ldvar->varName == Rf_install("vector");
        }
    }
    return false;
}

static bool testLazyCallArgs(ClosureVersion* f) {
    bool success = true;
    for (auto instruction : *f->entry) {
        if (auto call = CallInstruction::CastCall(instruction)) {
            call->eachCallArg([&](Value* arg) {
                if (auto mk = MkArg::Cast(arg)) {
                    if (mk->isEager())
                        success = false;
                } else {
                    success = false;
                }
            });
        }
    }
    return success;
}

static bool testEagerCallArgs(ClosureVersion* f) {
    bool success = true;
    for (auto instruction : *f->entry) {
        if (auto call = CallInstruction::CastCall(instruction)) {
            call->eachCallArg([&](Value* arg) {
                if (auto mk = MkArg::Cast(arg))
                    if (!mk->isEager())
                        success = false;
            });
        }
    }
    return success;
}

static bool testAnAddIsNotNAOrNaN(ClosureVersion* f) {
    bool success = false;
    Visitor::run(f->entry, [&](Instruction* i) {
        if (Add::Cast(i) && !i->type.maybeNAOrNaN())
            success = true;
    });
    return success;
}

PirCheck::Type PirCheck::parseType(const char* str) {
#define V(Check)                                                               \
    if (strcmp(str, #Check) == 0)                                              \
        return PirCheck::Type::Check;                                          \
    else
    LIST_OF_PIR_CHECKS(V)
#undef V
    return PirCheck::Type::Invalid;
}

bool PirCheck::run(SEXP f) {
    Module m;
    ClosureVersion* pir = recompilePir(f, &m);
    bool success = pir;
    if (success) {
        for (PirCheck::Type type : types) {
            switch (type) {
#define V(Check)                                                               \
    case PirCheck::Type::Check:                                                \
        if (!test##Check(pir))                                                 \
            success = false;                                                   \
        break;
                LIST_OF_PIR_CHECKS(V)
#undef V
        default:
            assert(false);
        }
    }
    }
    if (!success)
        m.print(std::cout, false);
    return success;
}

} // namespace rir
