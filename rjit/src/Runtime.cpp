#include "Runtime.h"
#include "ICCompiler.h"
#include "StackMap.h"
#include "RIntlns.h"
#include "Compiler.h"
#include "api.h"
#include "ir/Builder.h"
#include "Instrumentation.h"

using namespace rjit;

extern "C" void patchIC(void* ic, uint64_t stackmapId, void* caller) {
    uint8_t* patchAddr = (uint8_t*)StackMap::getPatchpoint(stackmapId);

    // reg 0 = rax
    int reg = 0;
    uint8_t prefix = reg > 7 ? 0x49 : 0x48;
    uint8_t movinst = 0xb8 + (reg % 8);

    static_assert(patchpointSize == 12, "require 10 bytes to patch call");

    // mov to rax
    *patchAddr++ = prefix;
    *patchAddr++ = movinst;

    // absolute addr
    *(void**)(patchAddr) = ic;
    patchAddr += sizeof(void*);

    // call rax
    *patchAddr++ = 0xff;
    *patchAddr++ = 0xd0;
}

extern "C" void* compileIC(uint64_t numargs, SEXP call, SEXP fun, SEXP rho,
                           uint64_t stackmapId) {
    SEXP body = CDR(fun);
    SEXP formals = CAR(fun);

    std::string name = "rfunction";
    if (TYPEOF(CAR(call)) == SYMSXP) {
        name = CHAR(PRINTNAME(CAR(call)));
    }

    // We use the RJIT_COMPILE env to request rjit to compile functions before
    // calling them
    // The existing R_ENABLE_JIT is reused within our version of gnur to be able
    // to use rjit as a replacement for the bytecode compiler. This only works
    // if rjit is loaded as a real module in gnur. In accordance we use it here
    // as well. Values 1,2 are existing gnur compiler options, level 3 only
    // recompiles already compiled bytecode expressions (used for testing
    // purposes), level 4 compiles ast expressions, level 5 compiles ast &
    // bytecode expressions.
    bool compile =
        (RJIT_COMPILE > 0 &&
         (TYPEOF(body) == LANGSXP || TYPEOF(body) == BCODESXP)) ||
        (TYPEOF(body) == LANGSXP && R_ENABLE_JIT > 3) ||
        (TYPEOF(body) == BCODESXP && (R_ENABLE_JIT == 3 || R_ENABLE_JIT > 4));

    if (compile) {
        Compiler c("module");
        SEXP result = c.compile(name, body, formals);
        c.finalize();
        if (RJIT_DEBUG)
            std::cout << "Compiled " << name << " @ " << (void*)result << "\n";
        SETCDR(fun, result);
    } else {
        if (RJIT_DEBUG)
            std::cout << "Calling " << name << " @ " << (void*)fun << "\n";
    }

    if (TYPEOF(fun) == SPECIALSXP) {
        return ICCompiler::getSpecialIC(numargs);
    }

    ir::Builder b("ic");

    name.append("IC");
    ICCompiler compiler(numargs, b, name);
    void* res = compiler.compile(call, fun, rho);

    return res;
}

REXPORT SEXP jitPrintTypefeedback(SEXP f);
extern "C" void* recompileFunction(SEXP closure,
                                   SEXP (*caller)(SEXP, SEXP, SEXP),
                                   SEXP consts, SEXP rho) {
    assert(closure && TYPEOF(closure) == CLOSXP);

    if (RJIT_DEBUG) {
        std::cout << "Recompiling closure " << (void*)closure
                  << "  Typefeedback gathered :\n";
        jitPrintTypefeedback(closure);
    }

    SEXP body = BODY(closure);

    SEXP result;
    {
        Compiler c("optimized module");
        result = c.compile("rOptFunction", body, FORMALS(closure));
        c.finalize();
    }

    SEXP(*newCaller)(SEXP, SEXP, SEXP) = (SEXP(*)(SEXP, SEXP, SEXP))CAR(result);
    SEXP newConsts = CDR(result);
    SETCDR(closure, result);

    return newCaller(newConsts, rho, closure);
}
