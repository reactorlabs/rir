#include "Compiler.h"

#include "BC.h"
#include "CodeStream.h"

#include "R/r.h"
#include "R/RList.h"
#include "R/Sexp.h"
#include "R/Symbols.h"

#include "Optimizer.h"
#include "utils/Pool.h"

#include "CodeVerifier.h"

namespace rir {

namespace {

class Context {
    enum class ExpT {
        // Normal expression
        expression,
        // inside a complex assignment
        complexAssignment,
    };

    bool noOpt_ = false;
    ExpT expT_;
    FunctionHandle& fun_;

    Context(ExpT expT, FunctionHandle& fun) : expT_(expT), fun_(fun) {}

  public:
    Context(FunctionHandle& fun) : expT_(ExpT::expression), fun_(fun) {}

    FunctionHandle& fun() { return fun_; }

    Context asComplexAssignment() {
        Context c(ExpT::complexAssignment, fun());
        return c;
    }

    Context asNoopt() {
        Context c(expT_, fun());
        c.noOpt_ = true;
        return c;
    }

    bool isNoopt() { return noOpt_; }
    bool isComplexAssignment() { return expT_ == ExpT::complexAssignment; }
};

fun_idx_t compilePromise(Context ctx, SEXP exp);
void compileExpr(Context ctx, CodeStream& cs, SEXP exp);
void compileCall(Context ctx, CodeStream& cs, SEXP ast, SEXP fun, SEXP args);

void compileDispatch(Context ctx, CodeStream& cs, SEXP selector, SEXP ast,
                     SEXP fun, SEXP args) {
    // Process arguments:
    // Arguments can be optionally named
    std::vector<fun_idx_t> callArgs;
    std::vector<SEXP> names;

    // This is the sane as in doCall
    for (auto arg = RList(args).begin(); arg != RList::end(); ++arg) {
        if (*arg == R_DotsSymbol) {
            callArgs.push_back(DOTS_ARG_IDX);
            names.push_back(R_NilValue);
            continue;
        }
        if (*arg == R_MissingArg) {
            callArgs.push_back(MISSING_ARG_IDX);
            names.push_back(R_NilValue);
            continue;
        }

        // (1) Arguments are wrapped as Promises:
        //     create a new Code object for the promise
        size_t prom = compilePromise(ctx, *arg);
        callArgs.push_back(prom);

        // (2) remember if the argument had a name associated
        names.push_back(arg.tag());
    }
    assert(callArgs.size() < MAX_NUM_ARGS);

    cs << BC::dispatch(selector, callArgs, names);

    cs.addAst(ast);
}

// Inline some specials
// TODO: once we have sufficiently powerful analysis this should (maybe?) go
//       away and move to an optimization phase.
bool compileSpecialCall(Context ctx, CodeStream& cs, SEXP ast, SEXP fun,
                        SEXP args_) {
    RList args(args_);

    // TODO: this is not ready for primetime...
    if (false && fun == symbol::Assign) {
        if (args.length() != 2)
            return false;

        auto lhs = args[0];
        Match(lhs) {
            Case(SYMSXP) {
                cs << BC::isspecial(fun);
                compileExpr(ctx, cs, args[1]);
                cs << BC::push(lhs) << BC::stvar();
            }
            Else({
                // lhs of assignment is not a symbol
                //   -> this is a complex assignment, we have to remember
                //      since this changes semantics of lhs expression
                compileExpr(ctx.asComplexAssignment(), cs, args_);
            })
        }
        return true;
    }

    if (fun == symbol::Internal) {
        // TODO: Needs more thought
        return false;
    }

    if (fun == symbol::DoubleBracket && !ctx.isComplexAssignment()) {
        if (args.length() == 2) {
            auto lhs = args[0];
            auto idx = args[1];

            Label objBranch = cs.mkLabel();
            Label nextBranch = cs.mkLabel();

            cs << BC::isspecial(fun);
            compileExpr(ctx, cs, args[0]);
            cs << BC::brobj(objBranch);

            compileExpr(ctx, cs, args[1]);
            cs << BC::extract1();
            cs.addAst(ast);
            cs << BC::br(nextBranch);

            cs << objBranch;
            compileDispatch(ctx, cs, symbol::DoubleBracket, ast, fun, args_);

            cs << nextBranch;
            return true;
        }
    }

    return false;
}

// function application
void compileCall(Context ctx, CodeStream& cs, SEXP ast, SEXP fun, SEXP args) {
    // application has the form:
    // LHS ( ARGS )

    // LHS can either be an identifier or an expression
    Match(fun) {
        Case(SYMSXP) {
            if (!ctx.isNoopt() && compileSpecialCall(ctx, cs, ast, fun, args))
                return;

            cs << BC::ldfun(fun);
        }
        Else({
            compileExpr(ctx, cs, fun);
            cs << BC::isfun();
        });
    }

    // Process arguments:
    // Arguments can be optionally named
    std::vector<fun_idx_t> callArgs;
    std::vector<SEXP> names;

    for (auto arg = RList(args).begin(); arg != RList::end(); ++arg) {
        if (*arg == R_DotsSymbol) {
            callArgs.push_back(DOTS_ARG_IDX);
            names.push_back(R_NilValue);
            continue;
        }
        if (*arg == R_MissingArg) {
            callArgs.push_back(MISSING_ARG_IDX);
            names.push_back(R_NilValue);
            continue;
        }

        // (1) Arguments are wrapped as Promises:
        //     create a new Code object for the promise
        size_t prom = compilePromise(ctx, *arg);
        callArgs.push_back(prom);

        // (2) remember if the argument had a name associated
        names.push_back(arg.tag());
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

void compileExpr(Context ctx, CodeStream& cs, SEXP exp) {
    // Dispatch on the current type of AST node
    Match(exp) {
        // Function application
        Case(LANGSXP, fun, args) { compileCall(ctx, cs, exp, fun, args); }
        // Variable lookup
        Case(SYMSXP) { compileGetvar(cs, exp); }
        Case(PROMSXP, value, expr) {
            // TODO: honestly I do not know what should be the semantics of
            //       this shit.... For now force it here and see what
            //       breaks...
            //       * One of the callers that does this is eg. print.c:1013
            //       * Another (a bit more sane) producer of this kind of ast
            //         is eval.c::applydefine (see rhsprom). At least there
            //         the prom is already evaluated and only used to attach
            //         the expression to the already evaled value
            SEXP val = forcePromise(exp);
            Protect p(val);
            compileConst(cs, val);
            cs.addAst(expr);
        }
        Case(BCODESXP) {
            assert(false);
        }
        // TODO : some code (eg. serialize.c:2154) puts closures into asts...
        //        not sure how we want to handle it...
        // Case(CLOSXP) {
        //     assert(false);
        // }

        // Constant
        Else(compileConst(cs, exp));
    }
}

std::vector<fun_idx_t> compileFormals(Context ctx, SEXP formals) {
    std::vector<fun_idx_t> res;

    for (auto arg = RList(formals).begin(); arg != RList::end(); ++arg) {
        if (*arg == R_MissingArg)
            res.push_back(MISSING_ARG_IDX);
        else
            res.push_back(compilePromise(ctx, *arg));
    }

    return res;
}

fun_idx_t compilePromise(Context ctx, SEXP exp) {
    CodeStream cs(ctx.fun(), exp);
    compileExpr(ctx, cs, exp);
    cs << BC::ret();
    return cs.finalize();
}
}

Compiler::CompilerRes Compiler::finalize() {
    // Rprintf("****************************************************\n");
    // Rprintf("Compiling function\n");
    FunctionHandle function = FunctionHandle::create();
    Context ctx(function);

    auto formProm = compileFormals(ctx, formals);

    CodeStream cs(function, exp);
    compileExpr(ctx, cs, exp);
    cs << BC::ret();
    cs.finalize();

    FunctionHandle opt = Optimizer::optimize(function);
    CodeVerifier::vefifyFunctionLayout(opt.store, globalContext());
    // opt.print();

    // Protect p;
    // SEXP formout = R_NilValue;
    // SEXP f = formout;
    // SEXP formin = formals;
    // for (auto prom : formProm) {
    //     SEXP arg = (prom == MISSING_ARG_IDX) ? 
    //         R_MissingArg : (SEXP)opt.codeAtOffset(prom);
    //     SEXP next = CONS_NR(arg, R_NilValue);
    //     SET_TAG(next, TAG(formin));
    //     formin = CDR(formin);
    //     if (formout == R_NilValue) {
    //         formout = f = next;
    //         p(formout);
    //     } else {
    //         SETCDR(f, next);
    //         f = next;
    //     }
    // }

    // TODO compiling the formals is broken, since the optimizer drops the
    // formals code from the function object since they are not referenced!
    // 
    return {opt.store, formals /* formout */ };
}
}
