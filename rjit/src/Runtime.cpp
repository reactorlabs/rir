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

    bool compile = R_ENABLE_JIT && TYPEOF(body) != NATIVESXP;

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

REXPORT SEXP rjit_PrintTypefeedback(SEXP f);
extern "C" void* recompileFunction(SEXP closure,
                                   SEXP (*caller)(SEXP, SEXP, SEXP),
                                   SEXP consts, SEXP rho) {
    assert(closure && TYPEOF(closure) == CLOSXP);

    if (RJIT_DEBUG) {
        std::cout << "Recompiling closure " << (void*)closure
                  << "  Typefeedback gathered :\n";
        rjit_PrintTypefeedback(closure);
    }

    SEXP body = BODY(closure);

    SEXP result;
    {
        Compiler c("optimized module");
        result =
            c.compileFunction("rOptFunction", body, FORMALS(closure), true);
        c.finalize();
    }

    SEXP(*newCaller)(SEXP, SEXP, SEXP) = (SEXP(*)(SEXP, SEXP, SEXP))CAR(result);
    SEXP newConsts = CDR(result);
    SETCDR(closure, result);

    return newCaller(newConsts, rho, closure);
}
