#include "Compiler.h"

#include "BC.h"
#include "CodeStream.h"

#include "../RIntlns.h"
#include "../RList.h"
#include "../Sexp.h"
#include "../Symbols.h"

#include "OldInterpreter.h"
#include "Optimizer.h"
#include "Pool.h"

namespace rjit {
namespace rir {

namespace {

fun_idx_t compilePromise(Code* f, SEXP exp);
void compileExpression(Code* f, CodeStream& cs, SEXP exp);

// function application
void compileCall(Code* parent, CodeStream& cs, SEXP ast, SEXP fun, SEXP args) {
    // application has the form:
    // LHS ( ARGS )

    // LHS can either be an identifier or an expression
    Match(fun) {
        Case(SYMSXP) { cs << BC::getfun(fun); }
        Else({
            compileExpression(parent, cs, fun);
            cs << BC::check_function();
        });
    }

    // Process arguments:
    // Arguments can be optionally named
    std::vector<fun_idx_t> callArgs;
    std::vector<SEXP> names;

    for (auto arg = RList(args).begin(); arg != RList::end(); ++arg) {
        // (1) Arguments are wrapped as Promises:
        //     create a new Code object for the promise
        size_t prom = compilePromise(parent, *arg);
        callArgs.push_back(prom);

        // (2) remember if the argument had a name associated
        names.push_back(arg.hasTag() ? arg.tag() : R_NilValue);
    }
    assert(callArgs.size() < MAX_NUM_ARGS);

    cs << BC::call(callArgs, names);

    cs.addAst(ast);
}

// Lookup
void compileGetvar(CodeStream& cs, SEXP name) { cs << BC::getvar(name); }

// Constant
void compileConst(CodeStream& cs, SEXP constant) {
    SET_NAMED(constant, 2);
    cs << BC::push(constant);
}

void compileExpression(Code* parent, CodeStream& cs, SEXP exp) {
    // Dispatch on the current type of AST node
    Match(exp) {
        // Function application
        Case(LANGSXP, fun, args) { compileCall(parent, cs, exp, fun, args); }
        // Variable lookup
        Case(SYMSXP) { compileGetvar(cs, exp); }
        // Constant
        Else(compileConst(cs, exp));
    }
}

void compileFormals(CodeStream& cs, SEXP formals) {
    size_t narg = 0;
    for (auto arg = RList(formals).begin(); arg != RList::end(); ++arg) {
        // TODO support default args
        assert(*arg == R_MissingArg);

        SEXP name = arg.tag();
        assert(name && name != R_NilValue);

        // TODO
        assert(name != symbol::Ellipsis);

        narg++;
    }
}

fun_idx_t compilePromise(Code* parent, SEXP exp) {
    CodeStream cs(parent, exp);
    compileExpression(cs.getCurrentCode(), cs, exp);
    cs << BC::ret();
    return cs.finalize();
}
}

SEXP Compiler::finalize() {
    CodeStream cs(exp);
    if (formals)
        compileFormals(cs, formals);
    compileExpression(cs.current, cs, exp);
    cs << BC::ret();
    Code* code = cs.toCode();
    // TODO reenable optimizations !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    //Optimizer::optimize(code);
    // call the c function here that linearise the code produced from the 
    // codestream in order to be used for the interpreter
    SEXP result = code->linearize(globalContext());
    return result;
}
}
}
