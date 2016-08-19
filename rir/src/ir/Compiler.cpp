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
  public:
    FunctionHandle& fun;
    Preserve& preserve;

    Context(FunctionHandle& fun, Preserve& preserve)
        : fun(fun), preserve(preserve) {}
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

    if (fun == symbol::Assign) {
        assert(args.length() == 2);
        cs << BC::isspecial(fun);

        auto lhs = args[0];
        auto rhs = args[1];

        Match(lhs) {
            Case(SYMSXP) {
                compileExpr(ctx, cs, rhs);
                cs << BC::push(lhs) << BC::stvar() << BC::invisible();
                return true;
            }
            Else(break)
        }

        compileExpr(ctx, cs, rhs);
        cs << BC::dup();

        // Find all parts of the lhs
        SEXP target = nullptr;
        SEXP l = lhs;
        std::vector<SEXP> lhsParts;
        while (!target) {
            Match(l) {
                Case(LANGSXP, fun, args) {
                    assert(TYPEOF(fun) == SYMSXP);
                    lhsParts.push_back(l);
                    l = CAR(args);
                }
                Case(SYMSXP) {
                    lhsParts.push_back(l);
                    target = l;
                }
                Case(STRSXP) {
                    assert(Rf_length(l) == 1);
                    target = Rf_install(CHAR(STRING_ELT(l, 0)));
                    lhsParts.push_back(target);
                }
                Else(assert(false);)
            }
        }

        // Evaluate the getter list and push it to the stack in reverse order
        for (unsigned i = lhsParts.size() - 1; i > 0; --i) {
            auto g = lhsParts[i];

            Match(g) {
                Case(SYMSXP) { cs << BC::ldvar(g); }
                Case(LANGSXP) {
                    SEXP fun = CAR(g);
                    RList args(CDR(g));
                    std::vector<SEXP> names;

                    auto arg = args.begin();
                    // Skip first arg (is already on the stack)
                    ++arg;
                    names.push_back(R_NilValue);

                    // Load function and push it before the first arg
                    cs << BC::ldfun(fun) << BC::swap();

                    for (; arg != RList::end(); ++arg) {
                        if (*arg == R_DotsSymbol || *arg == R_MissingArg) {
                            names.push_back(R_NilValue);
                            cs << BC::push(*arg);
                            continue;
                        }

                        names.push_back(arg.tag());
                        if (TYPEOF(*arg) == LANGSXP || TYPEOF(*arg) == SYMSXP) {
                            auto p = compilePromise(ctx, *arg);
                            cs << BC::promise(p);
                        } else {
                            compileExpr(ctx, cs, *arg);
                        }
                    }

                    cs << BC::call_stack(names.size(), names);
                    // TODO: replace target with *tmp*
                    cs.addAst(g);
                }
                Else(assert(false);)
            }
            if (i > 1)
                cs << BC::dup();

            // The setter internals are allowed to modify the lhs, thus
            // we need to make sure its not shared!
            cs << BC::uniq();

            if (i > 1)
                cs << BC::swap();
        }

        // Get down the initial rhs value
        cs << BC::pick(lhsParts.size() - 1);

        // Run the setters
        for (auto g = lhsParts.begin(); (g + 1) != lhsParts.end(); ++g) {
            SEXP fun = CAR(*g);
            RList args(CDR(*g));
            std::string name(CHAR(PRINTNAME(fun)));
            name.append("<-");
            SEXP setterName = Rf_install(name.c_str());

            std::vector<SEXP> names;

            auto arg = RList(args).begin();

            unsigned nargs = 0;
            // Skip first arg (is already on the stack)
            ++arg;
            names.push_back(R_NilValue);

            // Load function and push it before the first arg and the value
            // from the last setter.
            cs << BC::ldfun(setterName) << BC::put(2);

            for (; arg != RList::end(); ++arg) {
                nargs++;
                if (*arg == R_DotsSymbol || *arg == R_MissingArg) {
                    names.push_back(R_NilValue);
                    cs << BC::push(*arg);
                    continue;
                }

                names.push_back(arg.tag());
                if (TYPEOF(*arg) == LANGSXP || TYPEOF(*arg) == SYMSXP) {
                    auto p = compilePromise(ctx, *arg);
                    cs << BC::promise(p);
                } else {
                    compileExpr(ctx, cs, *arg);
                }
            }

            names.push_back(symbol::value);
            // the rhs (aka "value") needs to come last, if we pushed some args
            // we need to swap the order
            if (nargs > 0)
                cs << BC::pick(nargs);

            cs << BC::call_stack(names.size(), names);

            SEXP rewrite = Rf_shallow_duplicate(*g);
            ctx.preserve(rewrite);
            SETCAR(rewrite, setterName);

            SEXP lastArg = rewrite;
            while (CDR(lastArg) != R_NilValue)
                lastArg = CDR(lastArg);
            SEXP value = CONS_NR(symbol::templateValue, R_NilValue);
            SET_TAG(value, symbol::value);
            SETCDR(lastArg, value);
            cs.addAst(rewrite);

            cs << BC::uniq();
        }

        cs << BC::push(target)
           << BC::stvar()
           << BC::pop()
           << BC::invisible();

        return true;
    }

    if (fun == symbol::Internal) {
        // TODO: Needs more thought
        return false;
    }

    if (fun == symbol::isnull && args.length() == 1) {
        cs << BC::isspecial(fun);
        compileExpr(ctx, cs, args[0]);
        cs << BC::is(NILSXP);
        return true;
    }

    if (fun == symbol::islist && args.length() == 1) {
        cs << BC::isspecial(fun);
        compileExpr(ctx, cs, args[0]);
        cs << BC::is(VECSXP);
        return true;
    }

    if (fun == symbol::ispairlist && args.length() == 1) {
        cs << BC::isspecial(fun);
        compileExpr(ctx, cs, args[0]);
        cs << BC::is(LISTSXP);
        return true;
    }

    if (fun == symbol::DoubleBracket) {
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
            if (compileSpecialCall(ctx, cs, ast, fun, args))
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
    CodeStream cs(ctx.fun, exp);
    compileExpr(ctx, cs, exp);
    cs << BC::ret();
    return cs.finalize();
}
}

Compiler::CompilerRes Compiler::finalize() {
    // Rprintf("****************************************************\n");
    // Rprintf("Compiling function\n");
    FunctionHandle function = FunctionHandle::create();
    Context ctx(function, preserve);

    auto formProm = compileFormals(ctx, formals);

    CodeStream cs(function, exp);
    compileExpr(ctx, cs, exp);
    cs << BC::ret();
    cs.finalize();

    FunctionHandle opt = Optimizer::optimize(function);
    // opt.print();
    CodeVerifier::vefifyFunctionLayout(opt.store, globalContext());

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
