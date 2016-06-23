/** Enables the use of R internals for us so that we can manipulate R structures
 * in low level.
 */


#include "Compiler.h"

#include "api.h"

#include "RIntlns.h"

#include "ir/Ir.h"
#include "ir/Builder.h"
#include "ir/primitive_calls.h"
#include "TypeInfo.h"
#include "Flags.h"

#include "rir/Compiler.h"
#include "rir/OldInterpreter.h"
#include "rir/RBytecode.h"

#include "StackScan.h"

#include <llvm/IR/Module.h>


extern "C" {
    void gc_callback(void (*)(SEXP));
}


using namespace rjit;

REXPORT SEXP jitrbc(SEXP exp) {
    rir::Compiler c(exp);
    rir::Code* f = c.finalize();
    rir::RBytecode x = rir::RBytecode::serialize(f, rir::Code::CC::envLazy);
    rir::Code* ff = x.deserialize();
    return x;
}

REXPORT SEXP jitf(SEXP exp) {
    rir::Compiler c(exp);
    // rir::Function * f = c.finalize();
    return nullptr;
}

REXPORT SEXP jitRir(SEXP exp) {
    rir::Compiler c(exp);
    rir::Code* f = c.finalize();

    SEXP env = Rf_NewEnvironment(R_NilValue, R_NilValue, R_GlobalEnv);
    
    rir::Interpreter i(f);
    return i.run(env);
}

/** Compiles given ast and returns the NATIVESXP for it.
 */
REXPORT SEXP jitAst(SEXP ast, SEXP formals, SEXP rho) {
    Compiler c("module");
    SEXP result = c.compile("rfunction", ast, formals);
    c.finalize();
    return result;
}

REXPORT SEXP jitPrintTypefeedback(SEXP f) {
    if (TYPEOF(f) == CLOSXP)
        f = BODY(f);
    if (TYPEOF(f) != NATIVESXP) {
        warning("No nativesxp passed");
        return R_NilValue;
    }

    SEXP consts = CDR(f);
    SEXP typefeedback = VECTOR_ELT(consts, 1);
    SEXP typefeedbackName = VECTOR_ELT(consts, 2);
    assert(XLENGTH(typefeedback) == XLENGTH(typefeedbackName));

    SEXP invocationCount = VECTOR_ELT(consts, 3);
    std::cout << "Invocation count: " << INTEGER(invocationCount)[0] << "\n";

    for (int i = 0; i < XLENGTH(typefeedback); ++i) {
        TypeInfo info(INTEGER(typefeedback)[i]);
        SEXP sym = VECTOR_ELT(typefeedbackName, i);
        std::cout << CHAR(PRINTNAME(sym)) << ": " << info << "\n";
    }

    return R_NilValue;
}

REXPORT SEXP jitSwapForNative(SEXP original, SEXP native) {
    SETCAR(original, native);
    SETCDR(original, native);
    SET_TAG(original, native);
    return original;
}

/** More complex compilation method that compiles multiple functions into a
  specified module name.

  The module name is expected to be a STRSXP and the functions is expected to be
  a pairlist. If pairlist has tags associated with the elements, they will be
  used as function names.
 */
REXPORT SEXP jitFunctions(SEXP moduleName, SEXP functions) {
    char const* mName = CHAR(STRING_ELT(moduleName, 0));
    Compiler c(mName);
    while (functions != R_NilValue) {
        SEXP f = CAR(functions);
        // get the function ast
        SEXP body = BODY(f);
        SEXP formals = FORMALS(f);
        SEXP name = TAG(functions);
        char const* fName =
            (name == R_NilValue) ? "unnamed function" : CHAR(PRINTNAME(name));
        if (TYPEOF(body) == NATIVESXP)
            warning("Ignoring %s because it is already compiled", fName);
        else
            SET_BODY(f, c.compileFunction(fName, body, formals));
        // move to next function
        functions = CDR(functions);
    }
    c.finalize();
    return moduleName;
}

/** Returns the constant pool associated with the given NATIVESXP.
 */
REXPORT SEXP jitConstants(SEXP expression) {
    assert(TYPEOF(expression) == NATIVESXP and
           "JIT constants can only be extracted from a NATIVESXP argument");
    return CDR(expression);
}

/** Displays the LLVM IR for given NATIVESXP.
 */
REXPORT SEXP jitLLVM(SEXP expression) {
    assert(TYPEOF(expression) == NATIVESXP and
           "LLVM code can only be extracted from a NATIVESXP argument");
    llvm::Function* f = reinterpret_cast<llvm::Function*>(TAG(expression));
    f->dump();
    return R_NilValue;
}

REXPORT SEXP printWithoutSP(SEXP expr, SEXP formals) {
    Compiler c("module");
    SEXP result = c.compile("rfunction", expr, formals);
    llvm::Function* rfunction = reinterpret_cast<llvm::Function*>(TAG(result));
    rfunction->dump();
    return result;
}

// Should rjit code recompile uncompiled functions before calling them
int RJIT_COMPILE = getenv("RJIT_COMPILE") ? atoi(getenv("RJIT_COMPILE")) : 0;
// The status of R_ENABLE_JIT variable used by gnur
int R_ENABLE_JIT = getenv("R_ENABLE_JIT") ? atoi(getenv("R_ENABLE_JIT")) : 0;

int RJIT_DEBUG = getenv("RJIT_DEBUG") ? atoi(getenv("RJIT_DEBUG")) : 0;

REXPORT SEXP jitDisable(SEXP expression) {
    RJIT_COMPILE = false;
    return R_NilValue;
}

REXPORT SEXP jitEnable(SEXP expression) {
    RJIT_COMPILE = true;
    return R_NilValue;
}

REXPORT SEXP setFlag(SEXP name, SEXP value) {
    if (TYPEOF(value) != LGLSXP || XLENGTH(value) < 1) {
        std::cout << "value not a bool\n";
        return R_NilValue;
    }
    if (TYPEOF(name) != STRSXP || XLENGTH(name) < 1) {
        std::cout << "flag not a string\n";
        return R_NilValue;
    }
    SEXP c = STRING_ELT(name, 0);
    if (TYPEOF(c) != CHARSXP)
        return R_NilValue;
    const char* flag = CHAR(c);
    bool val = LOGICAL(value)[0];
    if (strcmp("recordTypes", flag) == 0) {
        rjit::Flag::singleton().recordTypes = val;
        return R_NilValue;
    }
    if (strcmp("recompileHot", flag) == 0) {
        rjit::Flag::singleton().recompileHot = val;
        return R_NilValue;
    }
    if (strcmp("staticNamedMatch", flag) == 0) {
        rjit::Flag::singleton().staticNamedArgMatch = val;
        return R_NilValue;
    }
    if (strcmp("unsafeOpt", flag) == 0) {
        rjit::Flag::singleton().unsafeOpt = val;
        return R_NilValue;
    }
    if (strcmp("unsafeNA", flag) == 0) {
        rjit::Flag::singleton().unsafeNA = val;
        return R_NilValue;
    }
    if (strcmp("printIR", flag) == 0) {
        rjit::Flag::singleton().printIR = val;
        return R_NilValue;
    }
    if (strcmp("useTypefeedback", flag) == 0) {
        rjit::Flag::singleton().useTypefeedback = val;
        return R_NilValue;
    }
    if (strcmp("printOptIR", flag) == 0) {
        rjit::Flag::singleton().printOptIR = val;
        return R_NilValue;
    }
    std::cout << "Unknown flag : " << flag << "\n";
    std::cout << " Valid flags are: recordTypes, recompileHot, "
              << "staticNamedMatch, unsafeNA, printIR, printOptIR\n";
    return R_NilValue;
}

namespace {


void rjit_gcCallback(void (*forward_node)(SEXP)) {
    StackScan::stackScanner(forward_node);
    Compiler::gcCallback(forward_node);
    // set the gc call back for the constant pool and the ast pool
    gc_callback(forward_node);
    // Precious::gcCallback(forward_node);
    rir::Interpreter::gcCallback(forward_node);
    gc_callback(forward_node);
}

int rjitStartup() {
    using namespace llvm;
    // initialize LLVM backend
    LLVMInitializeNativeTarget();
    LLVMInitializeNativeAsmPrinter();
    LLVMInitializeNativeAsmParser();
    linkStatepointExampleGC();

    registerGcCallback(&rjit_gcCallback);

    return 1;
}
}

int rjit_startup = rjitStartup();
