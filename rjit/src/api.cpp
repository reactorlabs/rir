/** Enables the use of R internals for us so that we can manipulate R structures
 * in low level.
 */


//#include "Compiler.h"

#include <cassert>

#include "api.h"

//#include "RIntlns.h"

//#include "ir/Ir.h"
//#include "ir/Builder.h"
//#include "ir/primitive_calls.h"
//#include "TypeInfo.h"
//#include "Flags.h"

#include "rir/Compiler.h"
//#include "rir/OldInterpreter.h"
//#include "rir/RBytecode.h"

//#include "StackScan.h"

//#include <llvm/IR/Module.h>

#include "rir/interp_context.h"
#include "rir/interp.h"
#include "rir/BC_inc.h"

// r print statement
#include "R_ext/Print.h"

using namespace rjit;
using namespace rir;

namespace {

/** Initializes the rir contexts, registers the gc and so on...
 */
bool startup() {
    // initialize the interpreter
    // TODO give a compiler proper
    interp_initialize(nullptr);
    // register gc callback
    registerGcCallback(&gc_callback);

    return true;
}

} // anonymous namespace

bool startup_ok = startup();

/** Compiles given closure.

 */
REXPORT SEXP rir_compileClosure(SEXP closure) {
    assert(startup_ok and "Not initialized");

    return R_NilValue;
}

/** Compiles the given ast.
 */
REXPORT SEXP rir_compile(SEXP ast) {
    assert(startup_ok and "Not initialized");

    return R_NilValue;
}


/** Helper function that prints the code object.
 */
void print(::Code * c) {
    Rprintf("Code object (offset %x (hex))\n", c->header);
    Rprintf("  Magic:     %x (hex)\n", c->magic);
    Rprintf("  Source:    %u (index to src pool)\n", c->src);
    Rprintf("  Stack (o): %u\n", c->stackLength);
    Rprintf("  Stack (i): %u\n", c->iStackLength);
    Rprintf("  Num insns: %u\n", c->srcLength);
    Rprintf("  Code size: %u [b]\n", c->codeSize);
    if (c->magic != CODE_MAGIC)
        Rf_error("Wrong magic number -- corrupted IR bytecode");

    // now prettyprint the code
    BC_t * bc = reinterpret_cast<BC_t *>(code(c));
    unsigned * sources = src(c);
    for (unsigned ic = 0, pc = 0, e = c->srcLength; ic != e; ++ic) {
        if (sources[ic] != 0)
            // TODO print also the reference
            Rprintf("        # src %u\n", sources[ic]);
        Rprintf("%x(8)%s", pc, BC::name(bc[pc]));
        BC_t opcode = bc[pc++];
        switch (opcode) {
        case BC_t::br_:
        case BC_t::brtrue_:
        case BC_t::brfalse_: {
            // for branches, get the address and display the actual pc it will jump to
            int targetPc = * reinterpret_cast<int*>(bc + pc);
            pc += sizeof(int) / sizeof (OpcodeT);
            targetPc = pc + targetPc;
            Rprintf(" %x (hex)", targetPc);
            break;
        }
        case BC_t::call_: {
            unsigned * argIdxs = reinterpret_cast<unsigned*>(bc + pc);
            pc += sizeof(unsigned) * 2 / sizeof(OpcodeT);
            SEXP args = cp_pool_at(globalContext(), argIdxs[0]);
            SEXP names = cp_pool_at(globalContext(), argIdxs[1]);
            Rprintf(" [");
            for (size_t i = 0, e = Rf_length(args); i != e; ++i)
                Rprintf(" %x", INTEGER(args)[i]);
            Rprintf("]");
            if (names) {
                Rprintf(" [");
                for (size_t i = 0, e = Rf_length(names); i != e; ++i) {
                    SEXP n = VECTOR_ELT(names, i);
                    if (n == R_NilValue)
                        Rprintf(" -");
                    else
                        Rprintf(" %s", CHAR(PRINTNAME(n)));
                }
                Rprintf("]");
            }
        }
        default:
            // default case only prints the unsigned immediate arguments
            for (size_t i = 0, e = BC::immCount(opcode); i != e; ++i) {
                Rprintf(" %u",  * reinterpret_cast<unsigned*>(bc + pc));
                pc += sizeof(unsigned) / sizeof (OpcodeT);
            }
        }
        Rprintf("\n");
    }



}


/** Prints the information in given Function SEXP
 */
REXPORT SEXP rir_print(SEXP function) {
    if (TYPEOF(function) != INTSXP)
        Rf_error("Invalid type (expected INTSXP), got %u", TYPEOF(function));
    assert((unsigned)Rf_length(function) > sizeof(::Function) and "Corrupted int vector send");
    ::Function * f = reinterpret_cast<Function *>(INTEGER(function));
    Rprintf("Function object (int vector size: %u)\n", Rf_length(function));
    Rprintf("  Magic:        %x (hex)\n", f->magic);
    Rprintf("  Size:         %u\n", f->size);
    Rprintf("  Origin:       %s\n", f->origin ? "optimized" : "unoptimized");
    Rprintf("  Code objects: %u\n", f->codeLength);
    if (f->magic != FUNCTION_MAGIC)
        Rf_error("Wrong magic number -- not rir bytecode");
    // print respective code objects
    for (::Code * c = begin(f), * e = end(f); c != e; c = next(c))
        print(c);
    return R_NilValue;
}




REXPORT SEXP jitrbc(SEXP exp) {
/*    rir::Compiler c(exp);
    rir::Code* f = c.finalize();
    rir::RBytecode x = rir::RBytecode::serialize(f, rir::Code::CC::envLazy);
    rir::Code* ff = x.deserialize();
    return x; */
    return R_NilValue;
}

REXPORT SEXP jitf(SEXP exp) {
//    rir::Compiler c(exp);
    // rir::Function * f = c.finalize();
    return R_NilValue;
}

REXPORT SEXP jitRir(SEXP exp) {
/*    rir::Compiler c(exp);
    rir::Code* f = c.finalize();

    SEXP env = Rf_NewEnvironment(R_NilValue, R_NilValue, R_GlobalEnv);

    rir::Interpreter i(f);
    return i.run(env); */
    return R_NilValue;
}

/** Compiles given ast and returns the NATIVESXP for it.
 */
REXPORT SEXP jitAst(SEXP ast, SEXP formals, SEXP rho) {
/*    Compiler c("module");
    SEXP result = c.compile("rfunction", ast, formals);
    c.finalize();
    return result; */
    return R_NilValue;

}

REXPORT SEXP jitPrintTypefeedback(SEXP f) {
/*    if (TYPEOF(f) == CLOSXP)
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
    } */

    return R_NilValue;
}

REXPORT SEXP jitSwapForNative(SEXP original, SEXP native) {
/*    SETCAR(original, native);
    SETCDR(original, native);
    SET_TAG(original, native);
    return original; */
    return R_NilValue;

}

/** More complex compilation method that compiles multiple functions into a
  specified module name.

  The module name is expected to be a STRSXP and the functions is expected to be
  a pairlist. If pairlist has tags associated with the elements, they will be
  used as function names.
 */
REXPORT SEXP jitFunctions(SEXP moduleName, SEXP functions) {
/*    char const* mName = CHAR(STRING_ELT(moduleName, 0));
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
    return moduleName; */
    return R_NilValue;

}

/** Returns the constant pool associated with the given NATIVESXP.
 */
REXPORT SEXP jitConstants(SEXP expression) {
/*    assert(TYPEOF(expression) == NATIVESXP and
           "JIT constants can only be extracted from a NATIVESXP argument");
    return CDR(expression); */
    return R_NilValue;

}

/** Displays the LLVM IR for given NATIVESXP.
 */
REXPORT SEXP jitLLVM(SEXP expression) {
/*    assert(TYPEOF(expression) == NATIVESXP and
           "LLVM code can only be extracted from a NATIVESXP argument");
    llvm::Function* f = reinterpret_cast<llvm::Function*>(TAG(expression));
    f->dump(); */
    return R_NilValue;
}

REXPORT SEXP printWithoutSP(SEXP expr, SEXP formals) {
/*    Compiler c("module");
    SEXP result = c.compile("rfunction", expr, formals);
    llvm::Function* rfunction = reinterpret_cast<llvm::Function*>(TAG(result));
    rfunction->dump();
    return result; */
    return R_NilValue;

}

// Should rjit code recompile uncompiled functions before calling them
int RJIT_COMPILE = getenv("RJIT_COMPILE") ? atoi(getenv("RJIT_COMPILE")) : 0;
// The status of R_ENABLE_JIT variable used by gnur
int R_ENABLE_JIT = getenv("R_ENABLE_JIT") ? atoi(getenv("R_ENABLE_JIT")) : 0;

int RJIT_DEBUG = getenv("RJIT_DEBUG") ? atoi(getenv("RJIT_DEBUG")) : 0;

REXPORT SEXP jitDisable(SEXP expression) {
//    RJIT_COMPILE = false;
    return R_NilValue;
}

REXPORT SEXP jitEnable(SEXP expression) {
/*    RJIT_COMPILE = true; */
    return R_NilValue;
}

REXPORT SEXP setFlag(SEXP name, SEXP value) {
/*    if (TYPEOF(value) != LGLSXP || XLENGTH(value) < 1) {
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
              */
    return R_NilValue;
}

namespace {

void rjit_gcCallback(void (*forward_node)(SEXP)) {
/*    StackScan::stackScanner(forward_node);
    Compiler::gcCallback(forward_node);
    // set the gc call back for the constant pool and the ast pool
    gc_callback(forward_node);
    // Precious::gcCallback(forward_node);
    rir::Interpreter::gcCallback(forward_node);
    gc_callback(forward_node); */
}

}



