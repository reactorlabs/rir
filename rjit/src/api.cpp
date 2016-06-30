/** Enables the use of R internals for us so that we can manipulate R structures
 * in low level.
 */

#include "Compiler.h"

#include <cassert>

#include "api.h"

#include "ir/Ir.h"
#include "ir/Builder.h"
#include "ir/primitive_calls.h"
#include "TypeInfo.h"
#include "Flags.h"

#include "rir/Compiler.h"

#include "StackScan.h"

#include "rir/interp_context.h"
#include "rir/interp.h"
#include "rir/BC.h"

// r print statement
#include "R_ext/Print.h"

#include "rir/FunctionHandle.h"

using namespace rjit;

typedef bool (*callback_isValidFunction)(SEXP);
typedef SEXP (*callback_rirEval_f)(SEXP, SEXP);

extern "C" void initializeCallbacks(callback_isValidFunction,
                                    callback_isValidFunction,
                                    callback_rirEval_f);

// =======================================================================
// == RIR API
//


/** Compiles the given ast.
 */
REXPORT SEXP rir_compileAst(SEXP ast, SEXP env) {
    SEXP code = rir::Compiler::compile(ast);
    return code;
}

REXPORT SEXP rir_compileClosure(SEXP f) {
    assert(TYPEOF(f) == CLOSXP and "Can only do closures");
    SEXP body = BODY(f);

    if (TYPEOF(body) == BCODESXP) {
        // body = VECTOR_ELT(CDR(body), 0);
        // warning("Skipping jit of Bytecode");
        return f;
    }

    assert(TYPEOF(body) != INTSXP and TYPEOF(body) != BCODESXP and
           "Can only do asts");
    SEXP result = allocSExp(CLOSXP);
    PROTECT(result);
    SET_FORMALS(result, FORMALS(f));
    SET_CLOENV(result, CLOENV(f));
    SET_BODY(result, rir::Compiler::compile(body));
    UNPROTECT(1);
    return result;
}

extern "C" void resetCompileExpressionOverride();
extern "C" void resetCmpFunOverride();
extern "C" void setCompileExpressionOverride(int, SEXP (*fun)(SEXP, SEXP));
extern "C" void setCmpFunOverride(int, SEXP (*fun)(SEXP));

REXPORT SEXP rir_jitDisable(SEXP expression) {
    resetCompileExpressionOverride();
    resetCmpFunOverride();
    return R_NilValue;
}

REXPORT SEXP rir_jitEnable(SEXP expression) {
    setCompileExpressionOverride(INTSXP, &rir_compileAst);
    setCmpFunOverride(INTSXP, &rir_compileClosure);
    return R_NilValue;
}


REXPORT SEXP rir_compileClosureInPlace(SEXP f) {
    assert(TYPEOF(f) == CLOSXP and "Can only do closures");
    SEXP body = BODY(f);
    assert(TYPEOF(body) != INTSXP and TYPEOF(body) != BCODESXP and
           "Can only do asts");
    SEXP code = rir::Compiler::compile(body);
    SET_BODY(f, code);
    return f;
}

REXPORT SEXP rir_exec(SEXP bytecode, SEXP env) {
    assert(isValidFunction(bytecode));
    ::Function* f = reinterpret_cast<::Function*>(INTEGER(bytecode));
    return rirEval_c(functionCode(f), globalContext(), env, 0);
}

/** Helper function that prints the code object.
 */
extern "C" void printCode(::Code* c) {
    Rprintf("Code object (offset %x (hex))\n", c->header);
    Rprintf("  Magic:     %x (hex)\n", c->magic);
    Rprintf("  Source:    %u (index to src pool)\n", c->src);
    Rprintf("  Stack (o): %u\n", c->stackLength);
    Rprintf("  Stack (i): %u\n", c->iStackLength);
    Rprintf("  Num insns: %u\n", c->srcLength);
    Rprintf("  Code size: %u [b]\n", c->codeSize);
    if (c->magic != CODE_MAGIC)
        Rf_error("Wrong magic number -- corrupted IR bytecode");

    rir::CodeHandle(c).print();
}

extern "C" void printFunction(::Function* f) {
    Rprintf("Function object:\n");
    Rprintf("  Magic:           %x (hex)\n", f->magic);
    Rprintf("  Size:            %u\n", f->size);
    Rprintf("  Origin:          %s\n", f->origin ? "optimized" : "unoptimized");
    Rprintf("  Code objects:    %u\n", f->codeLength);
    Rprintf("  Fun code offset: %x (hex)\n", f->foffset);

    if (f->magic != FUNCTION_MAGIC)
        Rf_error("Wrong magic number -- not rir bytecode");

    // print respective code objects
    for (::Code *c = ::begin(f), *e = ::end(f); c != e; c = ::next(c))
        printCode(c);
}

/** Prints the information in given Function SEXP
 */
REXPORT SEXP rir_print(SEXP store) {
    if (TYPEOF(store) != INTSXP)
        Rf_error("Invalid type (expected INTSXP), got %u", TYPEOF(store));

    assert((unsigned)Rf_length(store) > sizeof(::Function) and
           "Corrupted int vector send");

    rir::FunctionHandle fun(store);
    Rprintf("Container length %u.\n", Rf_length(store));

    Function* f = fun.function;
    printFunction(f);
    return R_NilValue;
}


// =======================================================================
// == RJIT API
//

/** Displays the LLVM IR for given NATIVESXP.
 */
REXPORT SEXP rjit_print(SEXP expression) {
    assert(TYPEOF(expression) == NATIVESXP and
           "LLVM code can only be extracted from a NATIVESXP argument");
    llvm::Function* f = reinterpret_cast<llvm::Function*>(TAG(expression));
    f->dump();
    return R_NilValue;
}

/** Compiles given ast and returns the NATIVESXP for it.
 */
REXPORT SEXP rjit_jit(SEXP ast, SEXP formals, SEXP rho) {
    Compiler c("module");
    SEXP result = c.compile("rfunction", ast, formals);
    c.finalize();
    return result;
}

REXPORT SEXP rjit_SwapForNative(SEXP original, SEXP native) {
    SETCAR(original, native);
    SETCDR(original, native);
    SET_TAG(original, native);
    return original;
}

/** Returns the constant pool associated with the given NATIVESXP.
 */
REXPORT SEXP rjit_GetConstants(SEXP expression) {
    assert(TYPEOF(expression) == NATIVESXP and
           "JIT constants can only be extracted from a NATIVESXP argument");
    return CDR(expression);
}

// The status of R_ENABLE_JIT variable used by gnur
int R_ENABLE_JIT = getenv("R_ENABLE_JIT") ? atoi(getenv("R_ENABLE_JIT")) : 0;
int RJIT_DEBUG = getenv("RJIT_DEBUG") ? atoi(getenv("RJIT_DEBUG")) : 0;

REXPORT SEXP rjit_compileAst(SEXP ast, SEXP env) {
    return rjit_jit(ast, nullptr, env);
}

REXPORT SEXP rjit_compileClosure(SEXP f) {
    assert(TYPEOF(f) == CLOSXP and "Can only do closures");
    SEXP result = allocSExp(CLOSXP);
    PROTECT(result);
    SET_FORMALS(result, FORMALS(f));
    SET_CLOENV(result, CLOENV(f));
    SET_BODY(result, rjit_jit(BODY(f), FORMALS(f), CLOENV(f)));
    UNPROTECT(1);
    return result;
}


REXPORT SEXP rjit_jitDisable(SEXP expression) {
    resetCompileExpressionOverride();
    resetCmpFunOverride();
    return R_NilValue;
}

REXPORT SEXP rjit_jitEnable(SEXP expression) {
    setCompileExpressionOverride(NATIVESXP, &rjit_compileAst);
    setCmpFunOverride(NATIVESXP, &rjit_compileClosure);
    return R_NilValue;
}

REXPORT SEXP rjit_setFlag(SEXP name, SEXP value) {
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

REXPORT SEXP rjit_PrintTypefeedback(SEXP f) {
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


// =======================================================================
// == Callbacks
//

void rjit_globalGcCallback(void (*forward_node)(SEXP)) {
    StackScan::stackScanner(forward_node);
    Compiler::gcCallback(forward_node);
    Precious::gcCallback(forward_node);
    rir_interp_gc_callback(forward_node);
}

/** Initializes the rir contexts, registers the gc and so on...
 */
bool startup() {
    // initialize LLVM backend
    LLVMInitializeNativeTarget();
    LLVMInitializeNativeAsmPrinter();
    LLVMInitializeNativeAsmParser();
    
    // TODO give a compiler proper
    interp_initialize(rir_compileAst);

    registerGcCallback(&rjit_globalGcCallback);

    initializeCallbacks(isValidFunction, isValidPromise, rirEval_f);

    return true;
}

bool startup_ok = startup();
