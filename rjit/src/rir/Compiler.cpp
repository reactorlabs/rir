#include "Compiler.h"

#include "Function.h"
#include "BC.h"
#include "CodeStream.h"

#include "../Sexp.h"
#include "../RIntlns.h"
#include "../RList.h"

#include "Pool.h"
#include "Interpreter.h"

namespace rjit {
namespace rir {

namespace {

fun_idx_t compileExpression(Function& f, SEXP exp);
void compileExpression(Function& f, CodeStream& cs, SEXP exp);

// function application
void compileCall(Function& f, CodeStream& cs, SEXP ast, SEXP fun, SEXP args) {
    // application has the form:
    // LHS ( ARGS )

    // LHS can either be an identifier or an expression
    Match(fun) {
        Case(SYMSXP) { cs << BC::getfun(fun); }
        Else(compileExpression(f, cs, fun));
    }

    // Process arguments:
    // Arguments can be optionally named
    size_t numArgs = 0;
    RVector names;
    bool hasNames = false;

    for (auto arg = RList(args).begin(); arg != RList::end(); ++arg) {
        numArgs++;

        // (1) Arguments are wrapped as Promises:
        //     create a new Code object for the promise
        size_t prom = compileExpression(f, *arg);

        // (2) remember if the argument had a name associated
        if (arg.hasTag()) {
            hasNames = true;
            names.append(arg.tag());
        } else {
            names.append(R_NilValue);
        }

        // (3) insert promise creation instruction into current CodeStream
        cs << BC::mkprom(prom);
    }
    assert(numArgs < MAX_NUM_ARGS);

    if (hasNames) {
        cs << BC::call_name(names);
    } else {
        cs << BC::call(numArgs);
    }
}

// Lookup
void compileGetvar(CodeStream& cs, SEXP name) { cs << BC::getvar(name); }

// Constant
void compileConst(CodeStream& cs, SEXP constant) { cs << BC::push(constant); }

void compileExpression(Function& f, CodeStream& cs, SEXP exp) {
    // Dispatch on the current type of AST node
    Match(exp) {
        // Function application
        Case(LANGSXP, fun, args) { compileCall(f, cs, exp, fun, args); }
        // Variable lookup
        Case(SYMSXP) { compileGetvar(cs, exp); }
        // Constant
        Else(compileConst(cs, exp));
    }
}

fun_idx_t compileExpression(Function& f, SEXP exp) {
    CodeStream cs(f);
    compileExpression(f, cs, exp);
    cs << BC::ret();
    return cs.finalize();
}
}

Function* Compiler::finalize() {
    Protect p;
    Function* f = new Function;

    compileExpression(*f, exp);

    return f;
}
}
}
