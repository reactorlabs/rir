#include "Compiler.h"

#include "BC.h"
#include "CodeStream.h"

#include "R/RIntlns.h"
#include "R/RList.h"
#include "R/Sexp.h"
#include "R/Symbols.h"

#include "Optimizer.h"
#include "utils/Pool.h"

#include "CodeVerifier.h"

namespace rir {

namespace {

fun_idx_t compilePromise(FunctionHandle& f, SEXP exp);
void compileExpr(FunctionHandle& f, CodeStream& cs, SEXP exp);

// function application
void compileCall(FunctionHandle& parent, CodeStream& cs, SEXP ast, SEXP fun,
                 SEXP args) {
    // application has the form:
    // LHS ( ARGS )

    // LHS can either be an identifier or an expression
    Match(fun) {
        Case(SYMSXP) { cs << BC::ldfun(fun); }
        Else({
            compileExpr(parent, cs, fun);
            cs << BC::isfun();
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
        if (*arg == R_DotsSymbol){
            names.push_back(R_DotsSymbol);
        } else {
            names.push_back(arg.hasTag() ? arg.tag() : R_NilValue);
        }
    }
    assert(callArgs.size() < MAX_NUM_ARGS);

    cs << BC::call(callArgs, names);

    cs.addAst(ast);
}

// Lookup
void compileGetvar(CodeStream& cs, SEXP name) {
    if (DDVAL(name)) {
        cs << BC::ldddvar(name);
    } else if (name == R_MissingArg) {
        cs << BC::push(R_MissingArg);
    } else {
        cs << BC::ldvar(name);
    }
}

// Constant
void compileConst(CodeStream& cs, SEXP constant) {
    SET_NAMED(constant, 2);
    cs << BC::push(constant);
}

void compileExpr(FunctionHandle& function, CodeStream& cs, SEXP exp) {
    // Dispatch on the current type of AST node
    Match(exp) {
        // Function application
        Case(LANGSXP, fun, args) { compileCall(function, cs, exp, fun, args); }
        // Variable lookup
        Case(SYMSXP) { compileGetvar(cs, exp); }
        // Constant
        Else(compileConst(cs, exp));
    }
}

std::vector<fun_idx_t> compileFormals(FunctionHandle& fun, SEXP formals) {
    std::vector<fun_idx_t> res;

    for (auto arg = RList(formals).begin(); arg != RList::end(); ++arg) {
        if (*arg != R_MissingArg)
            res.push_back(compilePromise(fun, *arg));
        else
            res.push_back(MISSING_ARG_IDX);
    }

    return res;
}

fun_idx_t compilePromise(FunctionHandle& function, SEXP exp) {
    CodeStream cs(function, exp);
    compileExpr(function, cs, exp);
    cs << BC::ret();
    return cs.finalize();
}
}

Compiler::CompilerRes Compiler::finalize() {
    // Rprintf("****************************************************\n");
    // Rprintf("Compiling function\n");
    FunctionHandle function = FunctionHandle::create();

    auto formProm = compileFormals(function, formals);

    CodeStream cs(function, exp);
    compileExpr(function, cs, exp);
    cs << BC::ret();
    cs.finalize();

    Protect p;
    SEXP formout = R_NilValue;
    SEXP f = formout;
    SEXP formin = formals;
    for (auto prom : formProm) {
        SEXP arg = (prom == MISSING_ARG_IDX) ? 
            R_MissingArg : (SEXP)function.codeAtOffset(prom);
        SEXP next = CONS_NR(arg, R_NilValue);
        SET_TAG(next, TAG(formin));
        formin = CDR(formin);
        if (formout == R_NilValue) {
            formout = f = next;
            p(formout);
        } else {
            SETCDR(f, next);
            f = next;
        }
    }

    CodeVerifier::vefifyFunctionLayout(function.store, globalContext());

    FunctionHandle opt = Optimizer::optimize(function);
    CodeVerifier::vefifyFunctionLayout(opt.store, globalContext());

    return {opt.store, formout};
}
}
