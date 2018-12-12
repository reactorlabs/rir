#include "Compiler.h"

#include "BC.h"
#include "CodeStream.h"

#include "R/r.h"
#include "R/RList.h"
#include "R/Sexp.h"
#include "R/Symbols.h"
#include "R/Funtab.h"

#include "utils/Pool.h"

#include "CodeVerifier.h"

#include <stack>

namespace rir {

namespace {

class Context {
  public:
    class LoopContext {
      public:
        BC::Label next_;
        BC::Label break_;
        bool context_needed_ = false;
        LoopContext(BC::Label next_, BC::Label break_)
            : next_(next_), break_(break_) {}
    };

    class CodeContext {
      public:
        CodeStream cs;
        std::stack<LoopContext> loops;
        SEXP env;
        CodeContext* parent;
        CodeContext(SEXP ast, SEXP env, FunctionWriter& fun, CodeContext* p)
            : cs(fun, ast), env(env), parent(p) {}
        virtual ~CodeContext() {}
        bool inLoop() {
            return !loops.empty() ||
                    (parent && parent->inLoop());
        }
        BC::Label loopNext() {
            assert(!loops.empty());
            return loops.top().next_;
        }
        BC::Label loopBreak() {
            assert(!loops.empty());
            return loops.top().break_;
        }
        void setContextNeeded() {
            if (loops.empty() && parent)
                parent->setContextNeeded();
            else
                loops.top().context_needed_ = true;
        }
        virtual bool loopIsLocal() {
            if (loops.empty()) return false;
            return true;
        }
    };

    class PromiseContext : public CodeContext {
      public:
        PromiseContext(SEXP ast, FunctionWriter& fun, CodeContext* p)
            : CodeContext(ast, nullptr, fun, p) {}
        bool loopIsLocal() override {
            if (loops.empty()) {
                parent->setContextNeeded();
                return false;
            }
            return true;
        }
    };

    std::stack<CodeContext*> code;

    CodeStream& cs() { return code.top()->cs; }
    SEXP env() { return code.top()->env; }

    FunctionWriter& fun;
    Preserve& preserve;

    Context(FunctionWriter& fun, Preserve& preserve)
        : fun(fun), preserve(preserve) {}

    ~Context() { assert(code.empty()); }

    bool inLoop() const { return code.top()->inLoop(); }

    LoopContext& loop() { return code.top()->loops.top(); }

    bool loopNeedsContext() {
        assert(inLoop());
        return code.top()->loops.top().context_needed_;
    }

    bool loopIsLocal() {
        return code.top()->loopIsLocal();
    }

    BC::Label loopNext() { return code.top()->loopNext(); }

    BC::Label loopBreak() { return code.top()->loopBreak(); }

    void pushLoop(BC::Label next_, BC::Label break_) {
        code.top()->loops.emplace(next_, break_);
    }

    void popLoop() { code.top()->loops.pop(); }

    void push(SEXP ast, SEXP env) {
        code.push(new CodeContext(ast, env, fun,
                                  code.empty() ? nullptr : code.top()));
    }

    void pushPromiseContext(SEXP ast) { code.push(new PromiseContext(ast, fun, code.empty() ? nullptr : code.top())); }

    Code* pop() {
        auto res = cs().finalize(0);
        delete code.top();
        code.pop();
        return res;
    }
};

Code* compilePromise(Context& ctx, SEXP exp);
void compileExpr(Context& ctx, SEXP exp);
void compileCall(Context& ctx, SEXP ast, SEXP fun, SEXP args);

// Inline some specials
// TODO: once we have sufficiently powerful analysis this should (maybe?) go
//       away and move to an optimization phase.
bool compileSpecialCall(Context& ctx, SEXP ast, SEXP fun, SEXP args_) {
    RList args(args_);
    CodeStream& cs = ctx.cs();

    if (fun == symbol::Function && args.length() == 3) {
        auto fun = Compiler::compileFunction(args[1], args[0]);
        assert(TYPEOF(fun) == EXTERNALSXP);
        cs << BC::push(args[0]) << BC::push(fun) << BC::push(args[2])
           << BC::close();
        return true;
    }

    // if (fun == symbol::seq && args.length() >= 2 && args.length() <= 3) {
    //     static SEXP seqFun = nullptr;
    //     if (!seqFun)
    //         seqFun = findFun(fun, R_GlobalEnv);

    //     cs << BC::guardName(fun, seqFun);

    //     for (auto a = args.begin(); a != args.end(); ++a)
    //         if (a.hasTag())
    //             return false;

    //     LabelT objBranch = cs.mkLabel();
    //     LabelT nextBranch = cs.mkLabel();

    //     compileExpr(ctx, args[0]);

    //     cs << BC::brobj(objBranch);

    //     compileExpr(ctx, args[1]);
    //     if (args.length() == 3) {
    //         compileExpr(ctx, args[2]);
    //     } else {
    //         cs << BC::push((int)1);
    //     }
    //     cs << BC::seq();
    //     cs.addSrc(ast);
    //     cs << BC::br(nextBranch);

    //     cs << objBranch;
    //     compileDispatch(ctx, fun, ast, args_);

    //     cs << nextBranch;
    //     return true;
    // }

    if (args.length() == 2 &&
        (fun == symbol::Add || fun == symbol::Sub ||
         fun == symbol::Mul || fun == symbol::Div || fun == symbol::Idiv ||
         fun == symbol::Mod || fun == symbol::Pow ||
         fun == symbol::Lt || fun == symbol::Gt ||
         fun == symbol::Le || fun == symbol::Ge ||
         fun == symbol::Eq || fun == symbol::Ne ||
         fun == symbol::Colon)) {
        cs << BC::guardNamePrimitive(fun);

        compileExpr(ctx, args[0]);
        compileExpr(ctx, args[1]);

        cs << BC::recordBinop();
        if (fun == symbol::Add)
            cs << BC::add();
        else if (fun == symbol::Sub)
            cs << BC::sub();
        else if (fun == symbol::Lt)
            cs << BC::lt();
        else if (fun == symbol::Gt)
            cs << BC::gt();
        else if (fun == symbol::Le)
            cs << BC::le();
        else if (fun == symbol::Ge)
            cs << BC::ge();
        else if (fun == symbol::Eq)
            cs << BC::eq();
        else if (fun == symbol::Ne)
            cs << BC::ne();
        else if (fun == symbol::Mul)
            cs << BC::mul();
        else if (fun == symbol::Div)
            cs << BC::div();
        else if (fun == symbol::Mod)
            cs << BC::mod();
        else if (fun == symbol::Idiv)
            cs << BC::idiv();
        else if (fun == symbol::Pow)
            cs << BC::pow();
        else if (fun == symbol::Colon)
            cs << BC::colon();
        cs.addSrc(ast);

        return true;
    }

    if (args.length() == 1 &&
        (fun == symbol::Add || fun == symbol::Sub ||
         fun == symbol::Not)) {
        cs << BC::guardNamePrimitive(fun);

        compileExpr(ctx, args[0]);

        if (fun == symbol::Add)
            cs << BC::uplus();
        else if (fun == symbol::Sub)
            cs << BC::uminus();
        else if (fun == symbol::Not)
            cs << BC::Not();
        cs.addSrc(ast);

        return true;
    }

    if (fun == symbol::And && args.length() == 2) {
        cs << BC::guardNamePrimitive(fun);

        BC::Label nextBranch = cs.mkLabel();

        compileExpr(ctx, args[0]);

        cs << BC::asLogical();
        cs.addSrc(args[0]);
        cs << BC::dup()
           << BC::brfalse(nextBranch);

        compileExpr(ctx, args[1]);

        cs << BC::asLogical();
        cs.addSrc(args[1]);
        cs << BC::lglAnd();

        cs << nextBranch;

        return true;
    }

    if (fun == symbol::Or && args.length() == 2) {
        cs << BC::guardNamePrimitive(fun);

        BC::Label nextBranch = cs.mkLabel();

        compileExpr(ctx, args[0]);

        cs << BC::asLogical();
        cs.addSrc(args[0]);
        cs << BC::dup()
           << BC::brtrue(nextBranch);

        compileExpr(ctx, args[1]);

        cs << BC::asLogical();
        cs.addSrc(args[1]);
        cs << BC::lglOr();

        cs << nextBranch;

        return true;
    }

    if (fun == symbol::quote && args.length() == 1) {
        cs << BC::guardNamePrimitive(fun) << BC::push(args[0]);
        return true;
    }

    if (fun == symbol::Assign || fun == symbol::Assign2 || fun == symbol::SuperAssign) {
        assert(args.length() == 2);

        bool superAssign = fun == symbol::SuperAssign;

        auto lhs = args[0];
        auto rhs = args[1];

        // 1) Verify lhs is valid
        SEXP l = lhs;
        while (l) {
            Match(l) {
                Case(LANGSXP, fun, args) {
                    if (TYPEOF(fun) == SYMSXP) {
                        l = CAR(args);
                    } else {
                        // Cant rewrite this statically...
                        return false;
                    }
                }
                Case(SYMSXP) { l = nullptr; }
                Case(STRSXP) { l = nullptr; }
                Else({
                    // Probably broken assignment
                    return false;
                })
            }
        }

        // 2) Specialcalse normal assignment (ie. "i <- expr")
        Match(lhs) {
            Case(SYMSXP) {
                cs << BC::guardNamePrimitive(fun);
                compileExpr(ctx, rhs);
                cs << BC::setShared() << BC::dup()
                   << (superAssign ? BC::stvarSuper(lhs) : BC::stvar(lhs))
                   << BC::invisible();
                return true;
            }
            Else(break)
        }

        // Find all parts of the lhs
        SEXP target = nullptr;
        l = lhs;
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
                Else({
                    errorcall(ast,
                              "invalid (do_set) left-hand side to assignment");
                })
            }
        }

        // TODO: There is some issue with supper assing and probably [[, needs
        // to investigate more...
        if (superAssign)
            return false;

        // 3) Special case [ and [[
        if (lhsParts.size() == 2) {
            RList g(lhsParts[0]);
            if (g.length() == 3) {
                SEXP fun2 = *g.begin();
                auto idx = g.begin() + 2;
                if (*idx != R_DotsSymbol && *idx != R_MissingArg &&
                    !idx.hasTag()) {
                    if (fun2 == symbol::DoubleBracket ||
                        fun2 == symbol::Bracket) {

                        cs << BC::guardNamePrimitive(fun);

                        // First rhs (assign is right-associative)
                        compileExpr(ctx, rhs);
                        // Keep a copy of rhs since its the result of this
                        // expression
                        cs << BC::dup() << BC::ensureNamed();

                        // Now load index and target
                        cs << BC::ldvar(target);
                        compileExpr(ctx, *idx);

                        // do the thing
                        if (fun2 == symbol::DoubleBracket)
                            cs << BC::subassign2();
                        else
                            cs << BC::subassign1();
                        cs.addSrc(ast);

                        // store the result as "target"
                        cs << BC::stvar(target);

                        cs << BC::invisible();
                        return true;
                    }
                }
            }
        }

        return false;
    }

    if (fun == symbol::Block) {
        cs << BC::guardNamePrimitive(fun);

        if (args.length() == 0) {
            cs << BC::push(R_NilValue);
            return true;
        }

        for (auto e = args.begin(); e != args.end(); ++e) {
            compileExpr(ctx, *e);
            if (e + 1 != args.end())
                cs << BC::pop();
        }

        return true;
    }

    if (fun == symbol::If) {
        if (args.length() < 2 || args.length() > 3)
            return false;

        cs << BC::guardNamePrimitive(fun);
        BC::Label trueBranch = cs.mkLabel();
        BC::Label nextBranch = cs.mkLabel();

        compileExpr(ctx, args[0]);
        cs << BC::asbool() << BC::brtrue(trueBranch);

        if (args.length() < 3) {
            cs << BC::push(R_NilValue)
               << BC::invisible();
        } else {
            compileExpr(ctx, args[2]);
        }
        cs << BC::br(nextBranch);

        cs << trueBranch;
        compileExpr(ctx, args[1]);

        cs << nextBranch;
        return true;
    }

    if (fun == symbol::Parenthesis) {
        if (args.length() != 1 || args[0] == R_DotsSymbol)
            return false;

        cs << BC::guardNamePrimitive(fun);
        compileExpr(ctx, args[0]);
        cs << BC::visible();
        return true;
    }

    if (fun == symbol::Return && args.length() < 2) {
        cs << BC::guardNamePrimitive(fun);

        if (args.length() == 0)
            cs << BC::push(R_NilValue);
        else
            compileExpr(ctx, args[0]);

        cs << BC::return_();
        return true;
    }

    if (fun == symbol::isnull && args.length() == 1) {
        cs << BC::guardNamePrimitive(fun);
        compileExpr(ctx, args[0]);
        cs << BC::is(NILSXP);
        return true;
    }

    if (fun == symbol::islist && args.length() == 1) {
        cs << BC::guardNamePrimitive(fun);
        compileExpr(ctx, args[0]);
        cs << BC::is(VECSXP);
        return true;
    }

    if (fun == symbol::ispairlist && args.length() == 1) {
        cs << BC::guardNamePrimitive(fun);
        compileExpr(ctx, args[0]);
        cs << BC::is(LISTSXP);
        return true;
    }

    if (fun == symbol::DoubleBracket || fun == symbol::Bracket) {
        if (args.length() == 2 || args.length() == 3) {
            auto lhs = *args.begin();
            auto idx = args.begin() + 1;

            if (*idx == R_DotsSymbol || idx.hasTag() ||
                *(idx + 1) == R_DotsSymbol || (idx + 1).hasTag())
                return false;

            cs << BC::guardNamePrimitive(fun);
            compileExpr(ctx, lhs);

            compileExpr(ctx, *idx);
            if (args.length() == 2) {
                if (fun == symbol::DoubleBracket)
                    cs << BC::extract2_1();
                else
                    cs << BC::extract1_1();
            } else {
                compileExpr(ctx, *(idx + 1));
                if (fun == symbol::DoubleBracket)
                    cs << BC::extract2_2();
                else
                    cs << BC::extract1_2();
            }
            cs.addSrc(ast);
            return true;
        }
    }

    if (fun == symbol::Missing && args.length() == 1 &&
        TYPEOF(args[0]) == SYMSXP && !DDVAL(args[0])) {
        cs << BC::guardNamePrimitive(fun) << BC::missing(args[0]);
        return true;
    }

    if (fun == symbol::While) {
        assert(args.length() == 2);

        auto cond = args[0];
        auto body = args[1];

        cs << BC::guardNamePrimitive(fun);

        BC::Label loopBranch = cs.mkLabel();
        BC::Label nextBranch = cs.mkLabel();

        ctx.pushLoop(loopBranch, nextBranch);

        unsigned beginLoopPos = cs.currentPos();

        cs << BC::beginloop(nextBranch)
           << loopBranch;

        compileExpr(ctx, cond);
        cs << BC::asbool()
           << BC::brfalse(nextBranch);

        compileExpr(ctx, body);
        cs << BC::pop()
           << BC::br(loopBranch)
           << nextBranch;

        if (ctx.loopNeedsContext()) {
            cs << BC::endloop();
        } else {
            cs.remove(beginLoopPos);
        }

        cs << BC::push(R_NilValue)
           << BC::invisible();

        ctx.popLoop();

        return true;
    }

    if (fun == symbol::Repeat) {
        assert(args.length() == 1);

        auto body = args[0];

        cs << BC::guardNamePrimitive(fun);

        BC::Label loopBranch = cs.mkLabel();
        BC::Label nextBranch = cs.mkLabel();

        ctx.pushLoop(loopBranch, nextBranch);

        unsigned beginLoopPos = cs.currentPos();

        cs << BC::beginloop(nextBranch)
           << loopBranch;

        compileExpr(ctx, body);
        cs << BC::pop()
           << BC::br(loopBranch)
           << nextBranch;

        if (ctx.loopNeedsContext()) {
            cs << BC::endloop();
        } else {
            cs.remove(beginLoopPos);
        }

        cs << BC::push(R_NilValue)
           << BC::invisible();

        ctx.popLoop();

        return true;
    }

    if (fun == symbol::For) {
        // TODO: if the seq is not a vector, we need to throw an error!
        assert(args.length() == 3);

        auto sym = args[0];
        auto seq = args[1];
        auto body = args[2];

        assert(TYPEOF(sym) == SYMSXP);

        cs << BC::guardNamePrimitive(fun);

        BC::Label loopBranch = cs.mkLabel();
        BC::Label breakBranch = cs.mkLabel();
        BC::Label endForBranch = cs.mkLabel();

        ctx.pushLoop(loopBranch, breakBranch);

        compileExpr(ctx, seq);
        cs << BC::setShared() << BC::forSeqSize() << BC::push((int)0);

        auto beginLoopPos = cs.currentPos();
        cs << BC::beginloop(breakBranch)
           << loopBranch;

        cs << BC::inc() << BC::ensureNamed() << BC::dup2() << BC::lt();
        // We know this is an int and won't do dispatch.
        // TODO: add a integer version of lt_
        cs.addSrc(R_NilValue);

        cs << BC::brtrue(endForBranch) << BC::pull(2) << BC::pull(1)
           << BC::extract2_1();
        // We know this is a loop sequence and won't do dispatch.
        // TODO: add a non-object version of extract2_1
        cs.addSrc(R_NilValue);

        // Set the loop variable
        cs << BC::stvar(sym);

        compileExpr(ctx, body);
        cs << BC::pop()
           << BC::br(loopBranch);

        cs << endForBranch;

        cs << breakBranch;

        if (ctx.loopNeedsContext()) {
            cs << BC::endloop();
        } else {
            cs.remove(beginLoopPos);
        }

        cs << BC::pop() << BC::pop() << BC::pop() << BC::push(R_NilValue)
           << BC::invisible();

        ctx.popLoop();

        return true;
    }

    if (fun == symbol::Next) {
        assert(args.length() == 0);

        if (!ctx.inLoop()) {
            // notify wrong next
            return false;
        }

        if (ctx.loopIsLocal()) {
            cs << BC::guardNamePrimitive(fun)
               << BC::br(ctx.loopNext())
               << BC::push(R_NilValue);
            return true;
        }
    }

    if (fun == symbol::Break) {
        assert(args.length() == 0);

        if (!ctx.inLoop()) {
            // notify wrong break
            return false;
        }

        if (ctx.loopIsLocal()) {
            cs << BC::guardNamePrimitive(fun)
               << BC::br(ctx.loopBreak())
               << BC::push(R_NilValue);
            return true;
        }
    }

    if (fun == symbol::Internal) {
        SEXP inAst = args[0];
        SEXP args_ = CDR(inAst);
        RList args(args_);
        SEXP fun = CAR(inAst);

        if (TYPEOF(fun) == SYMSXP) {
            for (auto a = args.begin(); a != args.end(); ++a)
                if (a.hasTag() || *a == R_DotsSymbol || *a == R_MissingArg)
                    return false;

            SEXP internal = fun->u.symsxp.internal;
            int i = ((sexprec_rjit*)internal)->u.i;

            // If the .Internal call goes to a builtin, then we call eagerly
            if (R_FunTab[i].eval % 10 == 1) {
                cs << BC::guardNamePrimitive(symbol::Internal);
                for (auto a : args)
                    compileExpr(ctx, a);
                cs << BC::staticCall(args.length(), inAst, internal);

                return true;
            }
        }
    }

    // The code bellow hardwires any call to a function that also exists as a
    // builtin in the global namespace. That is probably not the best idea and
    // much broader than the unsound optimizations of the gnu R BC interpreter.
    // Let's just disable that for now.
    //
    // SEXP builtin = fun->u.symsxp.value;
    // if (TYPEOF(builtin) == BUILTINSXP) {
    //     for (auto a = args.begin(); a != args.end(); ++a)
    //         if (a.hasTag() || *a == R_DotsSymbol || *a == R_MissingArg)
    //             return false;

    //     // Those are somehow overloaded in std libs
    //     if (fun == symbol::standardGeneric)
    //         return false;

    //     cs << BC::guardNamePrimitive(fun);

    //     for (auto a : args)
    //         compileExpr(ctx, a);
    //     cs << BC::staticCall(args.length(), ast, builtin);

    //     return true;
    // }

    if (fun == symbol::debugBreak) {
        // Push R_NilValue because this is a "function call" and needs to return
        // something.
        cs << BC::push(R_NilValue) << BC::int3();
        cs.addSrc(ast);
        return true;
    }

    return false;
}

// function application
void compileCall(Context& ctx, SEXP ast, SEXP fun, SEXP args) {
    CodeStream& cs = ctx.cs();

    // application has the form:
    // LHS ( ARGS )

    // LHS can either be an identifier or an expression
    Match(fun) {
        Case(SYMSXP) {
            if (compileSpecialCall(ctx, ast, fun, args))
                return;

            cs << BC::ldfun(fun);
        }
        Else({
            compileExpr(ctx, fun);
            cs << BC::isfun();
        });
    }

    // Process arguments:
    // Arguments can be optionally named
    std::vector<BC::FunIdx> callArgs;
    std::vector<SEXP> names;

    bool hasNames = false;
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
        Code* prom = compilePromise(ctx, *arg);
        auto idx = cs.addPromise(prom);
        callArgs.push_back(idx);

        // (2) remember if the argument had a name associated
        names.push_back(arg.tag());
        if (arg.tag() != R_NilValue)
            hasNames = true;
    }
    assert(callArgs.size() < BC::MAX_NUM_ARGS);

    cs << BC::recordCall();
    if (hasNames) {
        cs << BC::callImplicit(callArgs, names, ast);
    } else {
        cs << BC::callImplicit(callArgs, ast);
    }
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

void compileExpr(Context& ctx, SEXP exp) {
    // Dispatch on the current type of AST node
    Match(exp) {
        // Function application
        Case(LANGSXP, fun, args) { compileCall(ctx, exp, fun, args); }
        // Variable lookup
        Case(SYMSXP) { compileGetvar(ctx.cs(), exp); }
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
            compileConst(ctx.cs(), val);
            ctx.cs().addSrc(expr);
        }
        Case(BCODESXP) {
            assert(false);
        }
        Case(EXTERNALSXP) {
            assert(false);
        }
        // TODO : some code (eg. serialize.c:2154) puts closures into asts...
        //        not sure how we want to handle it...
        // Case(CLOSXP) {
        //     assert(false);
        // }

        // Constant
        Else(compileConst(ctx.cs(), exp));
    }
}

Code* compilePromise(Context& ctx, SEXP exp) {
    ctx.pushPromiseContext(exp);
    compileExpr(ctx, exp);
    ctx.cs() << BC::ret();
    return ctx.pop();
}

}  // anonymous namespace

SEXP Compiler::finalize() {
    FunctionWriter function;
    Context ctx(function, preserve);

    FunctionSignature signature(FunctionSignature::Environment::CallerProvided,
                                FunctionSignature::OptimizationLevel::Baseline);

    // Compile formals (if any) and create signature
    for (auto arg = RList(formals).begin(); arg != RList::end(); ++arg) {
        if (*arg == R_MissingArg) {
            function.addArgWithoutDefault();
        } else {
            auto compiled = compilePromise(ctx, *arg);
            function.addDefaultArg(compiled);
        }
        signature.pushDefaultArgument();
    }

    ctx.push(exp, closureEnv);
    compileExpr(ctx, exp);
    ctx.cs() << BC::ret();
    auto body = ctx.pop();
    function.finalize(body, signature);

#ifdef ENABLE_SLOWASSERT
    CodeVerifier::verifyFunctionLayout(function.function()->container(),
                                       globalContext());
#endif

    return function.function()->container();
}

}  // namespace rir
