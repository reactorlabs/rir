#include "bc/Compiler.h"
#include "R/Funtab.h"
#include "R/RList.h"
#include "R/Symbols.h"
#include "R/r.h"
#include "bc/BC.h"
#include "bc/CodeStream.h"
#include "bc/CodeVerifier.h"
#include "interpreter/cache.h"
#include "interpreter/interp.h"
#include "interpreter/interp_incl.h"
#include "interpreter/safe_force.h"
#include "simple_instruction_list.h"
#include "utils/Pool.h"

#include <stack>

namespace rir {

namespace {

static bool isConstant(SEXP exp) {
    // Dispatch on the current type of AST node
    switch (TYPEOF(exp)) {
    case LANGSXP:
    case SYMSXP:
    case PROMSXP:
        return false;
    default:
        return true;
    }
}

static bool containsLoop(SEXP exp) {
    if (TYPEOF(exp) != LANGSXP)
        return false;

    auto fun = CAR(exp);
    auto args_ = CDR(exp);

    if (TYPEOF(fun) != SYMSXP) {
        return false;
    } else if (fun == symbol::Repeat) {
        return true;
    } else if (fun == symbol::While) {
        return true;
    } else if (fun == symbol::For) {
        return true;
    }

    RList args(args_);
    bool res = false;
    for (RListIter e = args.begin(); e != args.end(); ++e) {
        res = res || containsLoop(*e);
    }
    return res;
}

class CompilerContext {
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
        typedef size_t CacheSlotNumber;
        static constexpr CacheSlotNumber BindingCacheDisabled = (size_t)-1;

        CodeStream cs;
        std::stack<LoopContext> loops;
        CodeContext* parent;
        std::unordered_map<SEXP, CacheSlotNumber> loadsSlotInCache;

        CodeContext(SEXP ast, FunctionWriter& fun, CodeContext* p)
            : cs(fun, ast), parent(p) {}
        virtual ~CodeContext() {}
        bool inLoop() { return !loops.empty() || (parent && parent->inLoop()); }
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
        size_t isCached(SEXP name) {
            assert(loadsSlotInCache.size() <= MAX_CACHE_SIZE);
            auto f = loadsSlotInCache.find(name);
            return f != loadsSlotInCache.end() &&
                   f->second != BindingCacheDisabled;
        }
        size_t nCached = 0;
        size_t cacheSlotFor(SEXP name) {
            auto f = loadsSlotInCache.find(name);
            if (f != loadsSlotInCache.end())
                return f->second;
            if (nCached >= MAX_CACHE_SIZE)
                return BindingCacheDisabled;
            return loadsSlotInCache.emplace(name, nCached++).first->second;
        }
        virtual bool loopIsLocal() { return !loops.empty(); }
        virtual bool isPromiseContext() { return false; }
    };

    class PromiseContext : public CodeContext {

      public:
        PromiseContext(SEXP ast, FunctionWriter& fun, CodeContext* p)
            : CodeContext(ast, fun, p) {}
        bool loopIsLocal() override {
            if (loops.empty()) {
                parent->setContextNeeded();
                return false;
            }
            return true;
        }

        bool isPromiseContext() override { return true; }
    };

    std::stack<CodeContext*> code;

    CodeStream& cs() { return code.top()->cs; }

    FunctionWriter& fun;
    Preserve& preserve;

    CompilerContext(FunctionWriter& fun, Preserve& preserve)
        : fun(fun), preserve(preserve) {}

    ~CompilerContext() { assert(code.empty()); }

    bool inLoop() const { return code.top()->inLoop(); }

    LoopContext& loop() { return code.top()->loops.top(); }

    bool loopNeedsContext() {
        assert(inLoop());
        return code.top()->loops.top().context_needed_;
    }

    bool loopIsLocal() { return code.top()->loopIsLocal(); }

    BC::Label loopNext() { return code.top()->loopNext(); }

    BC::Label loopBreak() { return code.top()->loopBreak(); }

    void pushLoop(BC::Label next_, BC::Label break_) {
        code.top()->loops.emplace(next_, break_);
    }

    void pushFakeLoop() {
        code.top()->loops.emplace(-1, -1);
        code.top()->setContextNeeded();
    }

    void popLoop() { code.top()->loops.pop(); }

    void push(SEXP ast, SEXP env) {
        code.push(
            new CodeContext(ast, fun, code.empty() ? nullptr : code.top()));
    }

    bool isInPromise() { return pushedPromiseContexts > 0; }

    void pushPromiseContext(SEXP ast) {
        pushedPromiseContexts++;

        code.push(
            new PromiseContext(ast, fun, code.empty() ? nullptr : code.top()));
    }

    Code* pop() {
        Code* res = cs().finalize(0, code.top()->loadsSlotInCache.size());
        if (code.top()->isPromiseContext())
            pushedPromiseContexts--;
        delete code.top();
        code.pop();
        return res;
    }

    void emitError(const char* msg, SEXP ast) {
        cs() << BC::push(R_TrueValue) << BC::push(Rf_mkString(msg))
             << BC::callBuiltin(2, ast, getBuiltinFun("stop")) << BC::return_();
    }
    void emitWarning(const char* msg, SEXP ast) {
        cs() << BC::push(R_TrueValue) << BC::push(R_FalseValue)
             << BC::push(R_FalseValue) << BC::push(Rf_mkString(msg))
             << BC::callBuiltin(4, ast, getBuiltinFun("warning")) << BC::pop();
    }

  private:
    unsigned int pushedPromiseContexts = 0;
};

struct LoadArgsResult {
    bool hasNames = false;
    bool hasDots = false;
    std::vector<SEXP> names;
    Context assumptions;
    int numArgs = 0;
};

Code* compilePromise(CompilerContext& ctx, SEXP exp);
Code* compilePromiseNoRir(CompilerContext& ctx, SEXP exp);
// If we are in a void context, then compile expression will not leave a value
// on the stack. For example in `{a; b}` the expression `a` is in a void
// context, but `b` is not. In `while(...) {...}` all loop body expressions are
// in a void context, since the loop as an expression is always nil.
void compileExpr(CompilerContext& ctx, SEXP exp, bool voidContext = false);
void compileCall(CompilerContext& ctx, SEXP ast, SEXP fun, SEXP args,
                 bool voidContext);

// EAGER_PROMISE_FROM_TOS is for the special case when the expression has
// already been evaluated: wrap the value at TOS into a promise. This is used in
// particular for the complex assignment: the expression
//    f(x) <- z
// returns z, z must be evaluated first, and z must be passed as en eager
// promise to `f<-` as its last argument.
enum class ArgType {
    PROMISE,
    EAGER_PROMISE,
    RAW_VALUE,
    EAGER_PROMISE_FROM_TOS
};

static void compileLoadOneArg(CompilerContext& ctx, SEXP arg, ArgType arg_type,
                              LoadArgsResult& res);

static void compileLoadArgs(CompilerContext& ctx, SEXP ast, SEXP fun, SEXP args,
                            LoadArgsResult& info, bool voidContext,
                            int skipArgs = 0, int eager = 0);

void compileWhile(CompilerContext& ctx, std::function<void()> compileCond,
                  std::function<void()> compileBody, bool peelLoop = false) {
    CodeStream& cs = ctx.cs();

    BC::Label nextBranch = cs.mkLabel();
    BC::Label breakBranch = cs.mkLabel();
    ctx.pushLoop(nextBranch, breakBranch);

    unsigned beginLoopPos = cs.currentPos();
    cs << BC::beginloop(breakBranch);

    // loop peel is a copy of the condition and body, with no backwards jumps
    if (Compiler::loopPeelingEnabled && peelLoop) {
        compileCond();
        cs << BC::recordTest() << BC::brfalse(breakBranch);
        compileBody();
    }

    cs << nextBranch;
    compileCond();
    cs << BC::brfalse(breakBranch);

    compileBody();
    cs << BC::br(nextBranch) << breakBranch;

    if (ctx.loopNeedsContext()) {
        cs << BC::endloop();
    } else {
        cs.remove(beginLoopPos);
    }

    ctx.popLoop();
}

void emitGuardForNamePrimitive(CodeStream& cs, SEXP fun) {
    if (!Compiler::unsoundOpts) {
        cs << BC::guardNamePrimitive(fun);
    }
}

/**
 * Try to convert this loop into a C-style for loop. If it fails or must compile
 * a regular loop, it will use the given function.
 */
bool compileSimpleFor(CompilerContext& ctx, SEXP fullAst, SEXP sym, SEXP seq,
                      SEXP body, bool voidContext) {
    if (TYPEOF(seq) != LANGSXP)
        return false;

    auto fun = CAR(seq);
    auto argsSexp = CDR(seq);

    RList args(argsSexp);
    if (fun != symbol::Colon || args.length() != 2) {
        return false;
    }

    // for(i in m:n) {
    //   ...
    // }
    // =>
    // m' <- m
    // n' <- n
    // if (!colonInputEffects(m, n)) {
    //    <regular for>
    // } else {
    //   m' <- colonCastLhs(m')
    //   n' <- colonCastRhs(m', n')
    //   step <- if (m' <= n') 1L else -1L
    //   i' <- m'
    //   while (i' != n') {
    //     i <- i'
    //     i' <- i' + step
    //     ...
    //   }
    // }

    SEXP start = args[0];
    SEXP end = args[1];
    CodeStream& cs = ctx.cs();

    BC::Label skipRegularForBranch = cs.mkLabel();
    BC::Label stepElseBranch = cs.mkLabel();
    BC::Label stepEndBranch = cs.mkLabel();
    BC::Label endBranch = cs.mkLabel();

    // m' <- m
    compileExpr(ctx, start);
    cs << BC::force();
    // n' <- n
    compileExpr(ctx, end);
    cs << BC::force();

    // if (!colonInputEffects(m, n)) {
    cs << BC::colonInputEffects();
    cs.addSrc(seq);
    bool staticFastcase = TYPEOF(start) != LANGSXP && TYPEOF(start) != SYMSXP &&
                          TYPEOF(end) != LANGSXP && TYPEOF(end) != SYMSXP &&
                          isColonFastcase(start, end);
    if (staticFastcase) {
        // We statically know that colonInputEffects is true, so we can
        // just pop the result and don't need to compile the slowcase
        // branch
        cs << BC::pop();
    } else {
        cs << BC::recordTest() << BC::brtrue(skipRegularForBranch);
        //   <regular for>
        // Note that we call the builtin `for` and pass the body as a
        // promise to lower the bytecode size

        // 1) Finish creating the seq, and add its SEXP as a promise
        // (it's eager but it needs to be a promise to be an arg)
        cs << BC::colon();
        cs.addSrc(seq);
        Code* seqProm = compilePromise(ctx, seq);
        size_t seqPromIdx = cs.addPromise(seqProm);

        // 2) Create a promise with the body
        ctx.pushFakeLoop();
        Code* bodyProm = compilePromise(ctx, body);
        ctx.popLoop();
        size_t bodyPromIdx = cs.addPromise(bodyProm);

        // 3) Add the function, arguments, and call
        Context assumptions;
        assumptions.setEager(0);
        assumptions.add(Assumption::CorrectOrderOfArguments);
        assumptions.add(Assumption::NotTooManyArguments);

        cs << BC::ldfun(symbol::For) << BC::swap()
           << BC::mkEagerPromise(seqPromIdx) << BC::mkPromise(bodyPromIdx)
           << BC::call(2, fullAst, assumptions);
        if (voidContext)
            cs << BC::pop();
        else if (Compiler::profile)
            cs << BC::recordType();

        cs << BC::br(endBranch);
        cs << skipRegularForBranch;
    }
    // } else {

    // m' <- colonCastLhs(m')
    cs << BC::swap() << BC::colonCastLhs() << BC::recordType()
       << BC::ensureNamed() << BC::swap();

    // n' <- colonCastRhs(m', n')
    cs << BC::colonCastRhs() << BC::ensureNamed() << BC::recordType();

    // step <- if (m' <= n') 1L else -1L
    cs << BC::dup2() << BC::le();
    cs.addSrc(R_NilValue);
    cs << BC::recordTest() << BC::brfalse(stepElseBranch) << BC::push(1)
       << BC::br(stepEndBranch) << stepElseBranch << BC::push(-1)
       << stepEndBranch;

    // i' <- m' (we just reuse m', but we need to fix the stack as the
    //           following bytecode expects: lhs :: rhs :: step :: ...)
    cs << BC::swap() << BC::pick(2);

    // while
    compileWhile(
        ctx,
        [&cs]() {
            // (i' != n')
            cs << BC::dup2() << BC::ne();
            cs.addSrc(R_NilValue);
        },
        [&ctx, &cs, &sym, &body]() {
            // {
            // i <- i'
            cs << BC::dup();
            if (ctx.code.top()->isCached(sym))
                cs << BC::stvarCached(sym, ctx.code.top()->cacheSlotFor(sym));
            else
                cs << BC::stvar(sym);
            // i' <- i' + step
            cs << BC::pull(2) << BC::ensureNamed() << BC::add();
            cs.addSrc(R_NilValue);
            // ...
            compileExpr(ctx, body, true);
            // }
        },
        !containsLoop(body));
    cs << BC::popn(3);
    if (!voidContext)
        cs << BC::push(R_NilValue) << BC::invisible();
    cs << endBranch;
    return true;
}

// A very conservative estimation if the ast could contain an assignment, or
// subset into sym
static bool maybeChanges(SEXP sym, SEXP ast) {
    if (TYPEOF(ast) != LANGSXP)
        return false;
    if (CADR(ast) == sym)
        return true;
    for (auto s : RList(CDR(ast))) {
        if (maybeChanges(sym, s))
            return true;
    }
    return false;
}

// Inline some specials
// TODO: once we have sufficiently powerful analysis this should (maybe?) go
//       away and move to an optimization phase.
bool compileSpecialCall(CompilerContext& ctx, SEXP ast, SEXP fun, SEXP args_,
                        bool voidContext) {
    // `true` if an argument isn't labeled, or `...`.
    auto isRegularArg = [](RListIter arg) {
        return *arg != R_DotsSymbol && !arg.hasTag();
    };

    RList args(args_);
    CodeStream& cs = ctx.cs();

    // TODO: this is not sound... There are other ways to call remove... What we
    // should do instead is trap do_remove in gnur and clear the cache!
    if (fun == symbol::remove || fun == symbol::rm) {
        CompilerContext::CodeContext::CacheSlotNumber min = MAX_CACHE_SIZE;
        CompilerContext::CodeContext::CacheSlotNumber max = 0;
        for (auto c : ctx.code.top()->loadsSlotInCache) {
            auto i = c.second;
            if (i == CompilerContext::CodeContext::BindingCacheDisabled)
                continue;
            if (i < min)
                min = i;
            if (i > max)
                max = i;
        }
        if (min < max)
            cs << BC::clearBindingCache(min, max - min);
        return false;
    }

    if (fun == symbol::Function && args.length() == 3) {
        if (!voidContext) {
            auto dt = Compiler::compileFunction(args[1], args[0]);
            Protect p(dt);
            // Mark this as an inner function to prevent the optimizer from
            // assuming a stable environment
            DispatchTable::check(dt)->baseline()->flags.set(
                Function::InnerFunction);
            assert(TYPEOF(dt) == EXTERNALSXP);
            cs << BC::push(args[0]) << BC::push(dt) << BC::push(args[2])
               << BC::close();
        }
        return true;
    }

    if (args.length() == 2 &&
        (fun == symbol::Add || fun == symbol::Sub || fun == symbol::Mul ||
         fun == symbol::Div || fun == symbol::Idiv || fun == symbol::Mod ||
         fun == symbol::Pow || fun == symbol::Eq || fun == symbol::Ne ||
         fun == symbol::Lt || fun == symbol::Le || fun == symbol::Gt ||
         fun == symbol::Ge || fun == symbol::Colon)) {
        emitGuardForNamePrimitive(cs, fun);

        compileExpr(ctx, args[0]);
        compileExpr(ctx, args[1]);

        if (fun == symbol::Add)
            cs << BC::add();
        else if (fun == symbol::Sub)
            cs << BC::sub();
        else if (fun == symbol::Mul)
            cs << BC::mul();
        else if (fun == symbol::Div)
            cs << BC::div();
        else if (fun == symbol::Idiv)
            cs << BC::idiv();
        else if (fun == symbol::Mod)
            cs << BC::mod();
        else if (fun == symbol::Pow)
            cs << BC::pow();
        else if (fun == symbol::Eq)
            cs << BC::eq();
        else if (fun == symbol::Ne)
            cs << BC::ne();
        else if (fun == symbol::Lt)
            cs << BC::lt();
        else if (fun == symbol::Le)
            cs << BC::le();
        else if (fun == symbol::Gt)
            cs << BC::gt();
        else if (fun == symbol::Ge)
            cs << BC::ge();
        else if (fun == symbol::Colon)
            cs << BC::colon();
        cs.addSrc(ast);

        if (voidContext)
            cs << BC::pop();
        else if (Compiler::profile)
            cs << BC::recordType();

        return true;
    }

    if (fun == symbol::And && args.length() == 2) {
        emitGuardForNamePrimitive(cs, fun);

        BC::Label nextBranch = cs.mkLabel();

        compileExpr(ctx, args[0]);

        cs << BC::aslogical();
        cs.addSrc(args[0]);
        cs << BC::dup() << BC::brfalse(nextBranch);

        compileExpr(ctx, args[1]);

        cs << BC::aslogical();
        cs.addSrc(args[1]);
        cs << BC::lglAnd();

        cs << nextBranch;

        if (voidContext)
            cs << BC::pop();
        return true;
    }

    if (fun == symbol::Or && args.length() == 2) {
        emitGuardForNamePrimitive(cs, fun);

        BC::Label nextBranch = cs.mkLabel();

        compileExpr(ctx, args[0]);

        cs << BC::aslogical();
        cs.addSrc(ast);
        cs << BC::dup() << BC::brtrue(nextBranch);

        compileExpr(ctx, args[1]);

        cs << BC::aslogical();
        cs.addSrc(ast);
        cs << BC::lglOr();

        cs << nextBranch;

        if (voidContext)
            cs << BC::pop();
        return true;
    }

    if (args.length() == 1 &&
        (fun == symbol::Add || fun == symbol::Sub || fun == symbol::Not)) {
        emitGuardForNamePrimitive(cs, fun);

        compileExpr(ctx, args[0]);

        if (fun == symbol::Add)
            cs << BC::uplus();
        else if (fun == symbol::Sub)
            cs << BC::uminus();
        else if (fun == symbol::Not)
            cs << BC::not_();
        cs.addSrc(ast);

        if (voidContext)
            cs << BC::pop();
        return true;
    }

    if (fun == symbol::quote && args.length() == 1) {
        emitGuardForNamePrimitive(cs, fun);
        if (!voidContext)
            cs << BC::push(args[0]);
        return true;
    }

    if (fun == symbol::Assign || fun == symbol::Assign2 ||
        fun == symbol::SuperAssign) {
        assert(args.length() == 2);

        bool superAssign = fun == symbol::SuperAssign;

        SEXP lhs = args[0];
        SEXP rhs = args[1];

        // 1) Verify lhs is valid
        SEXP l = lhs;
        while (l) {
            switch (TYPEOF(l)) {
            case LANGSXP: {
                auto fun = CAR(l);
                auto args = CDR(l);
                if (TYPEOF(fun) == SYMSXP) {
                    l = CAR(args);
                } else {
                    // Cant rewrite this statically...
                    return false;
                }
                break;
            }
            case SYMSXP: {
                l = nullptr;
                break;
            }
            case STRSXP: {
                l = nullptr;
                break;
            }
            default: {
                // Probably broken assignment
                return false;
            }
            }
        }

        if (!superAssign)
            MARK_ASSIGNMENT_CALL(ast);

        // 2) Specialcase normal assignment (ie. "i <- expr")
        if (TYPEOF(lhs) == SYMSXP) {
            emitGuardForNamePrimitive(cs, fun);
            compileExpr(ctx, rhs);
            if (!voidContext) {
                // No ensureNamed needed, stvar already ensures named
                cs << BC::dup() << BC::invisible();
            }
            if (superAssign) {
                cs << BC::stvarSuper(lhs);
            } else {
                if (ctx.code.top()->isCached(lhs))
                    cs << BC::stvarCached(lhs,
                                          ctx.code.top()->cacheSlotFor(lhs));
                else
                    cs << BC::stvar(lhs);
            }
            return true;
        }

        // Find all parts of the lhs
        SEXP target = nullptr;
        l = lhs;
        std::vector<SEXP> lhsParts;
        while (!target) {
            switch (TYPEOF(l)) {
            case LANGSXP: {
                auto fun = CAR(l);
                auto args = CDR(l);
                assert(TYPEOF(fun) == SYMSXP);
                lhsParts.push_back(l);
                l = CAR(args);
                break;
            }
            case SYMSXP: {
                target = l;
                lhsParts.push_back(target);
                break;
            }
            case STRSXP: {
                assert(Rf_length(l) == 1);
                target = Rf_install(CHAR(STRING_ELT(l, 0)));
                lhsParts.push_back(target);
                break;
            }
            default: {
                Rf_errorcall(ast,
                             "invalid (do_set) left-hand side to assignment");
                break;
            }
            }
        }

        // 3) Special case f(a) <- b

        // Only allow one level of nesting:
        //     f(x) <- 1         ok
        //     f(g(x)) <- 1      not supported
        // TODO: compile nested complex assignments
        if (lhsParts.size() != 2) {
            return false;
        }

        RList g(lhs);
        // If assignment is
        //       f(x, 2, 3) <- y
        // g = `f`, `x`, 2, 3

        // If we are here, it means that a complex assignment was requested
        // i.e. g != `x`
        assert(g.length() >= 2);

        SEXP fun2 = g[0]; // symbol `f`
        SEXP dest = g[1]; // symbol `x`

        // 3.a) Special case [ and [[
        if (fun2 == symbol::Bracket || fun2 == symbol::DoubleBracket) {
            int dims = g.length() - 2;
            if (dims < 1 || dims > 3) {
                return false;
            }

            SEXP fun2 = *g.begin();
            RListIter idx = g.begin() + 2;
            if (!isRegularArg(idx) || (dims > 1 && !isRegularArg(idx + 1)) ||
                (dims > 2 && !isRegularArg(idx + 2))) {
                return false;
            }
            if (dims == 3 && fun2 == symbol::DoubleBracket)
                return false;

            emitGuardForNamePrimitive(cs, fun);

            if (maybeChanges(target, rhs)) {
                if (ctx.code.top()->isCached(target))
                    cs << BC::ldvarForUpdateCached(
                        target, ctx.code.top()->cacheSlotFor(target));
                else
                    cs << BC::ldvarForUpdate(target);
                cs << BC::setShared() << BC::pop();
            }

            // First rhs (assign is right-associative)
            compileExpr(ctx, rhs);
            if (!voidContext) {
                // Keep a copy of rhs since it's the result of this expression
                cs << BC::dup();
                if (!isConstant(rhs))
                    cs << BC::setShared();
            }

            // Again, subassign bytecodes override objects with named count
            // of 1. If the target is from the outer scope that would be wrong.
            // For example
            //
            //     a <- 1
            //     f <- function()
            //         a[[1]] <- 2
            //
            // the f function should not override a.
            // The ldvarForUpdate BC increments the named count if the target is
            // not local to the current environment.

            if (superAssign) {
                cs << BC::ldvarSuper(target);
            } else {
                if (ctx.code.top()->isCached(target))
                    cs << BC::ldvarForUpdateCached(
                        target, ctx.code.top()->cacheSlotFor(target));
                else
                    cs << BC::ldvarForUpdate(target);
            }

            if (Compiler::profile)
                cs << BC::recordType();

            if (maybeChanges(target, *idx) ||
                (dims > 1 && maybeChanges(target, *(idx + 1))) ||
                (dims > 2 && maybeChanges(target, *(idx + 2))))
                cs << BC::setShared();

            // And index
            compileExpr(ctx, *idx);
            if (dims > 1)
                compileExpr(ctx, *(idx + 1));
            if (dims > 2)
                compileExpr(ctx, *(idx + 2));

            if (dims == 3) {
                assert(fun2 == symbol::Bracket);
                cs << BC::subassign1_3();
            } else if (dims == 2) {
                if (fun2 == symbol::DoubleBracket) {
                    cs << BC::subassign2_2();
                } else {
                    cs << BC::subassign1_2();
                }
            } else {
                if (fun2 == symbol::DoubleBracket) {
                    cs << BC::subassign2_1();
                } else {
                    cs << BC::subassign1_1();
                }
            }
            cs.addSrc(ast);

            // store the result as "target"
            if (superAssign) {
                cs << BC::stvarSuper(target);
            } else {
                if (ctx.code.top()->isCached(target))
                    cs << BC::stvarCached(target,
                                          ctx.code.top()->cacheSlotFor(target));
                else
                    cs << BC::stvar(target);
            }

            if (!voidContext)
                cs << BC::invisible();
        } else {
            /*
                3.b) Deal with all the other functions:
                    f(x,y) <- z
                i.e.
                    <-(f(x,y), value=z)
                will (almost) get rewritten into
                    <-( x, value=f<-(x,y,value=z) )

                This rewriting is theoretical. Indeed, there are some
                specificities to complex assignments:
                    - z is evaluated eagerly, followed by x
                    - the other arguments are passed as promises, as usual
                    - the complex assignment returns the value of z
            */

            std::string const fun2name = CHAR(PRINTNAME(fun2));

            // "slot<-" ignores value semantics and modifies shared objects
            // in-place, our implementation does not deal with this case.
            if (fun2name == "slot" || fun2name == "class") {
                return false;
            }

            // We need to get the SEXP for `f<-` from the SEXP for `f`
            std::string const fun2_replacement_name = fun2name + "<-";
            SEXP farrow_sym = Rf_install(fun2_replacement_name.c_str());

            /* Deal with special functions.
             The issue with special functions is that they do not use the
             arguments passed on the stack, but evaluate the arguments through
             the AST. For normal functions, in the assignment
                 f(x,a,b) <- z
             we emit the bytecode that will lead to the evaluation of z and x,
             and pass these values in evaluated promises on the object stack.

             If we use the same strategy for special function, the arguments
             will be evaluated a second time.

             There are only a couple special assignment functions
                - [[<-   (handled above)
                - [ <-   (handled above)
                - <-     (will not appear in a rewriting)
                - <<-    (will not appear in a rewriting)
                - @<-
                - $<-
             This leaves only two to deal with.

             The simple solution is to give up trying to compile the complex
             assignments for the two special assignment functions. In that case
             we lose the opportunity of compiling the RHS ; it will get
             interpreted by GNU R.

             It would still be interesting to compile the RHS and somehow pass
             the value to the special. The approach used in the GnuR BC compiler
             is to add the special instruction SETTER_CALL to deal with this
             situation at runtime: the AST of the RHS is replaced at runtime by
             an AST containing just the value obtained from the evaluation of
             the RHS. See
                https://github.com/reactorlabs/gnur/blob/R-3-6-2-branch-rir-patch/src/main/eval.c#L7128
            */

            bool const maybe_special = (fun2name == "$" || fun2name == "@");
            if (maybe_special) {
                return false;
            }

            // Get the LISTSXP of args for f
            SEXP f_args = CDR(CAR(args_));

            // Make the ast for the call : f<-, x, y, value=z
            // and protect it from GC
            SEXP farrow_ast;
            Protect farrow_ast_protect{farrow_ast =
                                           Rf_lcons(farrow_sym, R_NilValue)};
            // duplicate the args from the AST of the call to f into the AST for
            // the call to `f<-` (directly linked in the AST so that everything
            // is protected)
            SETCDR(farrow_ast, Rf_duplicate(f_args));

            SEXP last_farrow_cell = farrow_ast;
            while (CDR(last_farrow_cell) != R_NilValue) {
                last_farrow_cell = CDR(last_farrow_cell);
            }
            MARK_ASSIGNMENT_CALL(farrow_ast);

            // We need to append "value = z" to the list of args for f<-
            // Let's create the corresponding cell (directly linked in AST so
            // that it is protected)
            SETCDR(last_farrow_cell, Rf_lcons(rhs, R_NilValue));
            SEXP new_z_cell = CDR(last_farrow_cell);
            SET_TAG(new_z_cell, Rf_install("value"));

            // The RHS must be evaluated before the LHS
            // Additionnaly, the value of the RHS must be returned after the
            // assignment (in non-void contexts). It will be kept on the stack
            // before the call to `f<-`.
            // A copy will be wrapped in an evaluated promise and passed to f<-.
            compileExpr(ctx, rhs);

            // Prepare the call to f<-(x, y1, <...>, yn, z)
            cs << BC::ldfun(farrow_sym);

            if (Compiler::profile)
                cs << BC::recordCall();

            // prepare x, yk, z as promises
            LoadArgsResult load_arg_res;
            SEXP farrow_args = CDR(farrow_ast);

            // Load the value of x as a raw value
            // Passing x as a raw value instead of an evaluated promise is valid
            // in this case since R code is already discouraged from doing
            // non-standard evaluation on the destination of a complex
            // assignment. See "A Byte Code Compiler for R" p.76 for a
            // discussion on how one package used to do NSE on the destination
            // of complex assignments (by modifying `*tmp*` in the evaluation
            // environment) but was asked to abandon this practice.
            compileLoadOneArg(ctx, farrow_args, ArgType::RAW_VALUE,
                              load_arg_res);

            // load y1, <...>, yn

            for (SEXP cur_arg_cell = CDR(farrow_args);
                 cur_arg_cell != new_z_cell; cur_arg_cell = CDR(cur_arg_cell)) {
                compileLoadOneArg(ctx, cur_arg_cell, ArgType::PROMISE,
                                  load_arg_res);
            }

            // now, the value stack looks like this:

            // N+2      N+1       N   N-1                 0
            //  ??, z (raw),  `f<-`,    x,   y1,  <...>, yn

            // where N is the number of arguments _already_ passed to `f<-`
            // (load_arg_res.numArgs)
            if (voidContext) {
                // move the value of z to TOS
                cs << BC::pick(load_arg_res.numArgs + 1);
            } else {
                // keep a copy before `f<-` to return after the assignment
                cs << BC::pull(load_arg_res.numArgs + 1);
            }

            // after this instruction:

            // N+2     N+1     N   N-1           1        0
            //  ??,  `f<-`,    x,   y1,  <...>, yn, z (raw)

            // Wrap the value of z in an evaluated promise:
            compileLoadOneArg(ctx, new_z_cell, ArgType::EAGER_PROMISE_FROM_TOS,
                              load_arg_res);

            // call f<- with the arguments
            if (load_arg_res.hasDots) {
                cs << BC::callDots(load_arg_res.numArgs, load_arg_res.names,
                                   farrow_ast, load_arg_res.assumptions);
            } else {
                assert(load_arg_res.hasNames);
                cs << BC::call(load_arg_res.numArgs, load_arg_res.names,
                               farrow_ast, load_arg_res.assumptions);
            }

            // Bind the result to x
            if (superAssign) {
                cs << BC::stvarSuper(dest);
            } else {
                if (ctx.code.top()->isCached(dest))
                    cs << BC::stvarCached(dest,
                                          ctx.code.top()->cacheSlotFor(dest));
                else
                    cs << BC::stvar(dest);
            }

            if (!voidContext) {
                // The return value, RHS, is TOS
                cs << BC::invisible();
                if (Compiler::profile) {
                    cs << BC::recordType();
                }
            }

            return true;
        }

        return true;
    }

    if (fun == symbol::Block) {
        emitGuardForNamePrimitive(cs, fun);

        if (args.length() == 0) {
            if (!voidContext)
                cs << BC::push(R_NilValue);
            return true;
        }

        for (RListIter e = args.begin(); e != args.end(); ++e) {
            if (e + 1 != args.end()) {
                compileExpr(ctx, *e, true);
            } else {
                compileExpr(ctx, *e, voidContext);
            }
        }

        return true;
    }

    if (fun == symbol::If) {
        if (args.length() < 2 || args.length() > 3)
            return false;

        emitGuardForNamePrimitive(cs, fun);
        BC::Label trueBranch = cs.mkLabel();
        BC::Label nextBranch = cs.mkLabel();

        compileExpr(ctx, args[0]);
        cs << BC::asbool() << BC::brtrue(trueBranch);

        if (args.length() < 3) {
            if (!voidContext) {
                cs << BC::push(R_NilValue);
                cs << BC::invisible();
            }
        } else {
            compileExpr(ctx, args[2], voidContext);
        }
        cs << BC::br(nextBranch);

        cs << trueBranch;
        compileExpr(ctx, args[1], voidContext);

        cs << nextBranch;
        return true;
    }

    if (fun == symbol::Parenthesis) {
        if (args.length() != 1 || args[0] == R_DotsSymbol)
            return false;

        emitGuardForNamePrimitive(cs, fun);
        compileExpr(ctx, args[0]);
        if (!voidContext)
            cs << BC::visible();
        else
            cs << BC::pop();

        return true;
    }

    if (fun == symbol::Return && args.length() < 2) {
        emitGuardForNamePrimitive(cs, fun);

        if (args.length() == 0)
            cs << BC::push(R_NilValue);
        else
            compileExpr(ctx, args[0]);

        if (ctx.inLoop() || ctx.isInPromise())
            cs << BC::return_();
        else
            cs << BC::ret();
        return true;
    }

    if (fun == symbol::isnull && args.length() == 1) {
        emitGuardForNamePrimitive(cs, fun);
        compileExpr(ctx, args[0]);
        if (voidContext)
            cs << BC::pop();
        else
            cs << BC::is(BC::RirTypecheck::isNILSXP);
        return true;
    }

    if (fun == symbol::islist && args.length() == 1) {
        emitGuardForNamePrimitive(cs, fun);
        compileExpr(ctx, args[0]);
        if (voidContext)
            cs << BC::pop();
        else
            cs << BC::is(BC::RirTypecheck::isVECSXP);
        return true;
    }

    if (fun == symbol::ispairlist && args.length() == 1) {
        emitGuardForNamePrimitive(cs, fun);
        compileExpr(ctx, args[0]);
        if (voidContext)
            cs << BC::pop();
        else
            cs << BC::is(BC::RirTypecheck::isLISTSXP);
        return true;
    }

    if (fun == symbol::DoubleBracket || fun == symbol::Bracket) {
        int dims = args.length() - 1;
        if (dims < 1 || dims > 3) {
            return false;
        }

        SEXP lhs = *args.begin();
        RListIter idx = args.begin() + 1;

        if (!isRegularArg(idx) || (dims > 1 && !isRegularArg(idx + 1)) ||
            (dims > 2 && !isRegularArg(idx + 2)))
            return false;
        if (dims == 3 && fun == symbol::DoubleBracket)
            return false;

        emitGuardForNamePrimitive(cs, fun);
        compileExpr(ctx, lhs);

        BC::Label objBranch = cs.mkLabel();
        BC::Label nonObjBranch = cs.mkLabel();
        BC::Label contBranch = cs.mkLabel();

        cs << BC::dup() << BC::is(BC::RirTypecheck::isNonObject)
           << BC::recordTest() << BC::brfalse(objBranch)
           << BC::br(nonObjBranch);

        cs << objBranch;

        {
            LoadArgsResult dummy;
            compileLoadArgs(ctx, ast, fun, args_, dummy, voidContext, 1);
        }
        cs << BC::br(contBranch);

        cs << nonObjBranch;

        compileExpr(ctx, *idx);
        if (dims == 3) {
            compileExpr(ctx, *(idx + 1));
            compileExpr(ctx, *(idx + 2));
        } else if (dims == 2) {
            compileExpr(ctx, *(idx + 1));
        }
        cs << BC::br(contBranch);

        cs << contBranch;

        if (dims == 3) {
            assert(fun != symbol::DoubleBracket);
            cs << BC::extract1_3();
        } else if (dims == 2) {
            if (fun == symbol::DoubleBracket)
                cs << BC::extract2_2();
            else
                cs << BC::extract1_2();
        } else {
            if (fun == symbol::DoubleBracket)
                cs << BC::extract2_1();
            else
                cs << BC::extract1_1();
        }
        cs.addSrc(ast);
        if (!voidContext) {
            if (Compiler::profile)
                cs << BC::recordType();
            cs << BC::visible();
        } else {
            cs << BC::pop();
        }

        return true;
    }

    if (fun == symbol::Missing && args.length() == 1 &&
        TYPEOF(args[0]) == SYMSXP && !DDVAL(args[0])) {
        emitGuardForNamePrimitive(cs, fun);
        if (!voidContext) {
            cs << BC::missing(args[0]) << BC::visible();
        }
        return true;
    }

    if (fun == symbol::While) {
        assert(args.length() == 2);

        SEXP cond = args[0];
        SEXP body = args[1];

        emitGuardForNamePrimitive(cs, fun);

        compileWhile(
            ctx,
            [&ctx, &cs, &cond]() {
                compileExpr(ctx, cond);
                cs << BC::asbool();
            },
            [&ctx, &body]() { compileExpr(ctx, body, true); },
            !containsLoop(body));

        if (!voidContext)
            cs << BC::push(R_NilValue) << BC::invisible();

        return true;
    }

    if (fun == symbol::Repeat) {
        assert(args.length() == 1);

        SEXP body = args[0];

        emitGuardForNamePrimitive(cs, fun);

        BC::Label nextBranch = cs.mkLabel();
        BC::Label breakBranch = cs.mkLabel();
        ctx.pushLoop(nextBranch, breakBranch);

        unsigned beginLoopPos = cs.currentPos();
        cs << BC::beginloop(breakBranch);

        // loop peel is a copy of the body, with no backwards jumps
        if (Compiler::loopPeelingEnabled && !containsLoop(body)) {
            compileExpr(ctx, body, true);
        }

        cs << nextBranch;
        compileExpr(ctx, body, true);
        cs << BC::br(nextBranch) << breakBranch;

        if (ctx.loopNeedsContext()) {
            cs << BC::endloop();
        } else {
            cs.remove(beginLoopPos);
        }

        if (!voidContext)
            cs << BC::push(R_NilValue) << BC::invisible();

        ctx.popLoop();
        return true;
    }

    if (fun == symbol::For) {
        // TODO: if the seq is not a vector, we need to throw an error!
        assert(args.length() == 3);

        SEXP sym = args[0];
        SEXP seq = args[1];
        SEXP body = args[2];

        assert(TYPEOF(sym) == SYMSXP);

        emitGuardForNamePrimitive(cs, fun);

        if (compileSimpleFor(ctx, ast, sym, seq, body, voidContext)) {
            return true;
        }

        BC::Label nextBranch = cs.mkLabel();
        BC::Label breakBranch = cs.mkLabel();
        ctx.pushLoop(nextBranch, breakBranch);

        // Compile the seq expression (vector) and initialize the loop
        compileExpr(ctx, seq);
        cs << BC::forSeqSize() << BC::push((int)0);

        auto compileIndexOps = [&](bool record) {
            // Increment the index and compare to the seq upper bound
            cs << BC::inc() << BC::ensureNamed() << BC::dup2() << BC::lt();
            // We know this is an int and won't do dispatch.
            // TODO: add a integer version of lt_
            cs.addSrc(R_NilValue);

            if (record)
                cs << BC::recordTest();

            // If outside bound, branch, otherwise index into the vector
            cs << BC::brtrue(breakBranch) << BC::pull(2) << BC::pull(1)
               << BC::extract2_1();
            // We know this is a loop sequence and won't do dispatch.
            // TODO: add a non-object version of extract2_1
            cs.addSrc(R_NilValue);

            // Set the loop variable
            if (ctx.code.top()->isCached(sym))
                cs << BC::stvarCached(sym, ctx.code.top()->cacheSlotFor(sym));
            else
                cs << BC::stvar(sym);
        };

        unsigned int beginLoopPos = cs.currentPos();
        cs << BC::beginloop(breakBranch);

        // loop peel is a copy of the body (including indexing ops), with no
        // backwards jumps
        if (Compiler::loopPeelingEnabled && !containsLoop(body)) {
            compileIndexOps(true);
            compileExpr(ctx, body, true);
        }

        cs << nextBranch;
        compileIndexOps(false);

        // Compile the loop body
        compileExpr(ctx, body, true);
        cs << BC::br(nextBranch) << breakBranch;

        if (ctx.loopNeedsContext()) {
            cs << BC::endloop();
        } else {
            cs.remove(beginLoopPos);
        }

        cs << BC::popn(3);
        if (!voidContext) {
            cs << BC::push(R_NilValue) << BC::invisible();
        }

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
            emitGuardForNamePrimitive(cs, fun);
            cs << BC::br(ctx.loopNext()) << BC::push(R_NilValue);
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
            emitGuardForNamePrimitive(cs, fun);
            cs << BC::br(ctx.loopBreak()) << BC::push(R_NilValue);
            return true;
        }
    }

    if (fun == symbol::Switch) {
        /* # A high level overview
         * # Assume argLen > 1:
         *   compile arg[0]
         *   if (arg[0] is not length-1 vector) br vecECont
         *   error(...)
         *   # unreachable
         * vecECont:
         *   # this check is currently skipped
         *   if (not isFactor(arg[0])) br facWCont
         *   warning(...)
         * facWCont:
         *   if (isString(arg[0])) br str
         *   asInteger(arg[0])
         *   if (value == 1) br label[0]
         *   ...
         *   if (value == n) br label[n-1]
         *   br nil
         *
         * str:
         *   if (not is.na(value)) br strNACont
         *   value <- "NA"
         * strNACont:
         *   if (value == group[0]) br groupLabels[0]
         *   ...
         *   if (value == group[k]) br groupLabels[k]
         *   br default label[dftLabelIdx] # or nil if no default
         *
         * label[i]:
         *   # if arg[i+1] is missing, we must came from integer case
         *   # error(...)
         *   pop # pop evaluated 1st arg
         *   compile expression[i]
         *   br cont
         * ...
         *
         * nil:
         *   # stack is [1stArg]
         *   push R_NilValue
         *
         * cont:
         *   # stack is [retval]
         */
        int argLen = args.length();
        // argLen error/warning is statically determinable
        if (argLen == 0) {
            ctx.emitError("'EXPR' is missing", ast);
            return true;
        }
        auto expr = args.begin();
        if (expr.hasTag()) {
            auto supplied = CHAR(PRINTNAME(expr.tag()));
            auto ns = strlen(supplied);
            if (ns > strlen("EXPR") || strncmp(supplied, "EXPR", ns)) {
                ctx.emitError(std::string("supplied argument name '")
                                  .append(supplied)
                                  .append("' does not match 'EXPR'")
                                  .c_str(),
                              ast);
                return true;
            }
        }
        // when 1st arg is string, switch behaves like C/C++ switch/case.
        // Cases like `x=, y=, z=20` are grouped together.
        // groups[-1] is not used, for impl convenience
        std::vector<std::vector<SEXP>> groups = {{}};
        std::vector<BC::Label> groupLabels; // eval/return for each group
        std::vector<SEXP> expressions;      // return value ast for each group
        std::vector<BC::Label> labels;      // eval/return for each arg
        std::vector<bool> argMissing(argLen - 1, true);
        int dftLabelIdx = -1; // index into `labels` for default return arg
        bool dupDflt = false;
        BC::Label vecArityBr = cs.mkLabel();
        BC::Label vecErrorBr = cs.mkLabel();
        BC::Label vecEContBr = cs.mkLabel();
        BC::Label facWContBr = cs.mkLabel();
        BC::Label strBr = cs.mkLabel();
        BC::Label strNAContBr = cs.mkLabel();
        BC::Label nilBr = cs.mkLabel();
        BC::Label contBr = cs.mkLabel();

        // find default and group args
        int argIdx = 0;
        for (auto arg = args.begin() + 1; arg != args.end(); ++arg, ++argIdx) {
            auto label = cs.mkLabel();
            labels.push_back(label);
            if (!arg.hasTag()) {
                dupDflt |= (dftLabelIdx != -1);
                dftLabelIdx = argIdx;
            } else {
                groups.back().push_back(arg.tag());
            }
            if (*arg != R_MissingArg) { // tag must be present if value is not
                argMissing[argIdx] = false;
                expressions.push_back(*arg);
                groupLabels.push_back(label);
                groups.push_back({}); // start new group
            }
            if (*arg == R_DotsSymbol)
                return false;
        }

        /******************* INSTRUCTIONS START HERE *********************/
        compileExpr(ctx, args[0]);

        // !isVector(x)
        cs << BC::dup() << BC::is(BC::RirTypecheck::isVector)
           << BC::recordTest() << BC::brtrue(vecArityBr);
        cs << BC::br(vecErrorBr);

        // ... || LENGTH(x) != 1
        cs << vecArityBr << BC::dup() << BC::length_() << BC::push(1)
           << BC::eq();
        cs.addSrc(R_NilValue); // to make code verifier happy
        cs << BC::recordTest() << BC::brtrue(vecEContBr);

        cs << vecErrorBr;
        ctx.emitError("EXPR must be a length 1 vector", ast);

        // isFactor(x)
        cs << vecEContBr;

        cs << BC::dup() << BC::is(BC::RirTypecheck::isFactor)
           << BC::recordTest() << BC::brfalse(facWContBr);

        ctx.emitWarning("EXPR is a \"factor\", treated as integer.\n Consider "
                        "using 'switch(as.character( * ), ...)' instead.",
                        ast);

        cs << facWContBr;

        if (argLen == 1) { // inserted here to mimic behavior of builtin impl
            ctx.emitWarning("'switch' with no alternatives", ast);
            cs << BC::br(nilBr);
        }
        cs << BC::dup() << BC::is(BC::RirTypecheck::isSTRSXP)
           << BC::recordTest() << BC::brtrue(strBr);
        cs << BC::asSwitchIdx();

        // currently stack is [arg[0]] (converted to integer)
        for (size_t i = 0; i < labels.size(); ++i) {
            cs << BC::dup() << BC::push(Rf_ScalarInteger(i + 1)) << BC::eq();
            cs.addSrc(R_NilValue); // call argument for builtin
            cs << BC::asbool() << BC::recordTest() << BC::brtrue(labels[i]);
        }
        cs << BC::br(nilBr) << strBr;
        if (dupDflt) {
            ctx.emitError("duplicate 'switch' defaults", ast);
        } else {

            // If value is NA, set it to the string "NA". This solves two
            // problems: 1) In an R switch(), NA_character_ should match the
            // string "NA". 2) It ensures that BC:eq will always return a
            // boolean instead of NA. This allows us to use BC::eq and
            // BC::asbool to compare the cases.
            cs << BC::dup()
               << BC::callBuiltin(1, R_NilValue, getBuiltinFun("is.na"))
               << BC::asbool() << BC::recordTest() << BC::brfalse(strNAContBr)
               << BC::pop() << BC::push(Rf_mkString("NA")) << strNAContBr;

            for (size_t i = 0; i < expressions.size(); ++i) {
                for (auto& n : groups[i]) {
                    cs << BC::dup() << BC::push(n) << BC::eq();
                    cs.addSrc(R_NilValue); // call argument for builtin
                    cs << BC::asbool() << BC::recordTest()
                       << BC::brtrue(groupLabels[i]);
                }
            }

            auto fallbackLabel =
                (dftLabelIdx == -1) ? nilBr : labels[dftLabelIdx];
            cs << BC::br(fallbackLabel);
        }

        for (size_t i = 0, j = 0; i < labels.size(); ++i) {
            cs << labels[i];
            if (argMissing[i]) {
                ctx.emitError("empty alternative in numeric switch", ast);
                continue;
            } else {
                cs << BC::pop();
                compileExpr(ctx, expressions[j++]);
                cs << BC::br(contBr);
            }
        }

        cs << nilBr << BC::pop() << BC::push(R_NilValue);
        cs << contBr;
        if (voidContext)
            cs << BC::pop();
        return true;
    }

    if (fun == symbol::Internal) {
        SEXP inAst = args[0];
        SEXP args_ = CDR(inAst);
        RList args(args_);
        SEXP fun = CAR(inAst);

        if (TYPEOF(fun) == SYMSXP) {
            SEXP internal = fun->u.symsxp.internal;

            // Check if the .Internal call is malformed:
            //      .Internal(undefined_function())
            // This can occur in normal R code as some internal functions are
            // not defined on all platforms (see names.c). E.g.
            //      .Internal(tzone_name())
            // only works on win32.
            if (internal == R_NilValue) {
                return false;
            }

            int i = getBuiltinNr(internal);
            // If the .Internal call goes to a builtin, then we call eagerly
            if (R_FunTab[i].eval % 10 == 1) {
                emitGuardForNamePrimitive(cs, symbol::Internal);

                bool hasDots = false;
                for (RListIter arg = args.begin(); arg != RList::end(); ++arg)
                    if (*arg == R_DotsSymbol)
                        hasDots = true;

                if (hasDots)
                    cs << BC::push(internal);

                std::vector<SEXP> names;
                for (RListIter arg = args.begin(); arg != RList::end(); ++arg) {

                    if (*arg == R_DotsSymbol) {
                        // The name is ignored, eg. foo(x = ...) ~~~ foo(...),
                        // so we can use it to mark that we require dots
                        // expansion. The actual value pushed means that we at
                        // runtime should look up the ellipsis, as opposed to it
                        // being already on the stack (which is what pir does).
                        cs << BC::push(symbol::expandDotsTrigger);
                        names.push_back(symbol::expandDotsTrigger);
                        continue;
                    }

                    if (hasDots)
                        names.push_back(arg.tag());

                    if (*arg == R_MissingArg) {
                        cs << BC::push(R_MissingArg);
                        continue;
                    }

                    compileExpr(ctx, *arg);
                }

                if (hasDots) {
                    cs << BC::callDots(args.length(), names, inAst, Context());
                } else {
                    cs << BC::callBuiltin(args.length(), inAst, internal);
                }
                if (voidContext)
                    cs << BC::pop();

                return true;
            }

            // .Internal(lapply(X, FUN))
            if (fun == symbol::lapply && args.length() == 2) {

                BC::Label loopBranch = cs.mkLabel();
                BC::Label nextBranch = cs.mkLabel();

                compileExpr(ctx, args[0]); // [X]

                // get length and names of the vector X
                cs << BC::dup() << BC::names() << BC::swap()
                   << BC::length_() // [names(X), length(X)]
                   << BC::dup() << BC::push(Rf_mkString("list")) << BC::swap()
                   << BC::callBuiltin(
                          2, symbol::tmp,
                          getBuiltinFun("vector")) // [names(X), length(X), ans]
                   << BC::pick(2) << BC::setNames() << BC::swap()
                   << BC::push((int)0); // [ans, length(X), i]

                // loop invariant stack layout: [ans, length(X), i]

                // check end condition
                cs << loopBranch << BC::inc() << BC::dup2() << BC::lt();
                cs.addSrc(ast);

                SEXP isym = Rf_install("i");
                cs << BC::brtrue(nextBranch) << BC::dup() << BC::stvar(isym);

                // construct ast for FUN(X[[i]], ...)
                SEXP tmp = PROTECT(
                    Rf_lcons(symbol::DoubleBracket,
                             Rf_lcons(args[0], Rf_lcons(isym, R_NilValue))));
                SEXP call = Rf_lcons(
                    args[1], Rf_lcons(tmp, Rf_lcons(R_DotsSymbol, R_NilValue)));

                PROTECT(call);
                compileCall(ctx, call, CAR(call), CDR(call), false);
                UNPROTECT(2);

                // store result
                cs << BC::pull(1) << BC::pick(4)
                   << BC::swap() // [length(X), i, fun(X[[i]], ...), ans, i]
                   << BC::set_vec_elt();

                cs << BC::put(2) // [ans, length(X), i]
                   << BC::br(loopBranch);

                // put ans to the top and remove rest
                cs << nextBranch << BC::pop() << BC::pop() << BC::visible();

                if (voidContext)
                    cs << BC::pop();

                return true;
            }
        }
    }

#define V(NESTED, name, Name)                                                  \
    if (fun == symbol::name) {                                                 \
        cs << BC::push(R_NilValue) << BC::name();                              \
        cs.addSrc(ast);                                                        \
        return true;                                                           \
    }
    SIMPLE_INSTRUCTIONS(V, _)
#undef V

    return false;
}

static void compileLoadOneArg(CompilerContext& ctx, SEXP arg, ArgType arg_type,
                              LoadArgsResult& res) {
    // Prepare the argument arg for a function call.
    // The bytecode generated will return the result either as a promise, an
    // evaluated promise, or a raw value.

    CodeStream& cs = ctx.cs();
    int i = res.numArgs;
    res.numArgs += 1;

    if (CAR(arg) == R_DotsSymbol) {
        // The name is ignored, eg. foo(x = ...) ~~~ foo(...), so we can use it
        // to mark that we require dots expansion. The actual value pushed means
        // that we at runtime should look up the ellipsis, as opposed to it
        // being already on the stack (which is what pir does).
        cs << BC::push(symbol::expandDotsTrigger);
        res.names.push_back(symbol::expandDotsTrigger);
        res.hasDots = true;
        return;
    }

    // remember if the argument had a name associated (for missing too)
    res.names.push_back(TAG(arg));
    if (TAG(arg) != R_NilValue)
        res.hasNames = true;

    if (CAR(arg) == R_MissingArg) {
        cs << BC::push(R_MissingArg);
        return;
    }

    if (arg_type == ArgType::RAW_VALUE) {
        compileExpr(ctx, CAR(arg), false);
        return;
    }

    // Constant arguments do not need to be promise wrapped
    if (arg_type != ArgType::EAGER_PROMISE_FROM_TOS)
        switch (TYPEOF(CAR(arg))) {
        case LANGSXP:
        case SYMSXP:
            break;
        default:
            auto eager = CAR(arg);
            res.assumptions.setEager(i);
            if (!Rf_isObject(eager)) {
                res.assumptions.setNotObj(i);
                if (IS_SIMPLE_SCALAR(eager, REALSXP))
                    res.assumptions.setSimpleReal(i);
                if (IS_SIMPLE_SCALAR(eager, INTSXP))
                    res.assumptions.setSimpleInt(i);
            }
            cs << BC::push(eager);
            return;
        }

    Code* prom;
    if (arg_type == ArgType::EAGER_PROMISE) {
        // Compile the expression to evaluate it eagerly, and
        // wrap the return value in a promise without rir code
        compileExpr(ctx, CAR(arg), false);
        prom = compilePromiseNoRir(ctx, CAR(arg));
    } else if (arg_type == ArgType::EAGER_PROMISE_FROM_TOS) {
        // The value we want to wrap in the argument's promise is
        // already on TOS, no nead to compile the expression.
        // Wrap it in a promise without rir code.
        prom = compilePromiseNoRir(ctx, CAR(arg));
    } else { // ArgType::PROMISE
        // Compile the expression as a promise.
        prom = compilePromise(ctx, CAR(arg));
    }

    size_t idx = cs.addPromise(prom);

    if (arg_type == ArgType::EAGER_PROMISE ||
        arg_type == ArgType::EAGER_PROMISE_FROM_TOS) {
        res.assumptions.setEager(i);
        cs << BC::mkEagerPromise(idx);
    } else {
        cs << BC::mkPromise(idx);
    }
}

static void compileLoadArgs(CompilerContext& ctx, SEXP ast, SEXP fun, SEXP args,
                            LoadArgsResult& info, bool voidContext,
                            int skipArgs, int eager) {
    // Process arguments:
    // Arguments can be optionally named

    SEXP cur_cell = args;
    int i = 0;
    while (cur_cell != R_NilValue) {
        if (i >= skipArgs) {
            ArgType t = (i < eager) ? ArgType::RAW_VALUE : ArgType::PROMISE;
            compileLoadOneArg(ctx, cur_cell, t, info);
        }
        cur_cell = CDR(cur_cell);
        i++;
    }
}

// function application
void compileCall(CompilerContext& ctx, SEXP ast, SEXP fun, SEXP args,
                 bool voidContext) {

    CodeStream& cs = ctx.cs();

    // application has the form:
    // LHS ( ARGS )

    // LHS can either be an identifier or an expression
    bool speculateOnBuiltin = false;
    BC::Label eager = 0;
    BC::Label theEnd = 0;

    if (TYPEOF(fun) == SYMSXP) {
        if (compileSpecialCall(ctx, ast, fun, args, voidContext))
            return;

        if (!ctx.isInPromise()) {

            auto callHasDots = false;
            for (RListIter arg = RList(args).begin(); arg != RList::end();
                 ++arg) {

                if (*arg == R_DotsSymbol) {
                    callHasDots = true;
                    break;
                }
            }

            if (!callHasDots) {
                auto builtin = Rf_findVar(fun, R_BaseEnv);
                assert(builtin != R_NilValue);
                auto likelyBuiltin = TYPEOF(builtin) == BUILTINSXP;
                speculateOnBuiltin = likelyBuiltin;

                if (speculateOnBuiltin) {
                    eager = cs.mkLabel();
                    theEnd = cs.mkLabel();
                    cs << BC::push(builtin) << BC::dup()
                       << BC::ldvarNoForce(fun) << BC::identicalNoforce()
                       << BC::recordTest() << BC::brtrue(eager);

                    cs << BC::pop();
                }
            }
        }

        cs << BC::ldfun(fun);
    } else {
        compileExpr(ctx, fun);
        cs << BC::checkFunction();
    }

    if (Compiler::profile)
        cs << BC::recordCall();

    auto compileCall = [&](LoadArgsResult& info) {
        if (info.hasDots) {
            cs << BC::callDots(info.numArgs, info.names, ast, info.assumptions);
        } else if (info.hasNames) {
            cs << BC::call(info.numArgs, info.names, ast, info.assumptions);
        } else {
            info.assumptions.add(Assumption::CorrectOrderOfArguments);
            cs << BC::call(info.numArgs, ast, info.assumptions);
        }
    };

    LoadArgsResult info;
    if (fun == symbol::forceAndCall) {
        // forceAndCall is a special with signature `function(n, FUN, ...)`
        // The first two args are eager
        compileLoadOneArg(ctx, args, ArgType::RAW_VALUE, info);
        compileLoadOneArg(ctx, CDR(args), ArgType::RAW_VALUE, info);
        if (Compiler::profile)
            cs << BC::recordCall();
        // Load the rest of the args
        compileLoadArgs(ctx, ast, fun, args, info, voidContext, 2, 0);
    } else {
        compileLoadArgs(ctx, ast, fun, args, info, voidContext);
    }
    compileCall(info);

    if (speculateOnBuiltin) {
        cs << BC::br(theEnd) << eager;

        LoadArgsResult infoEager;
        compileLoadArgs(ctx, ast, fun, args, infoEager, voidContext, 0,
                        RList(args).length());

        compileCall(infoEager);

        cs << theEnd;
    }

    if (voidContext)
        cs << BC::pop();
    else if (Compiler::profile)
        cs << BC::recordType();
}

// Lookup
void compileGetvar(CompilerContext& ctx, SEXP name) {
    CodeStream& cs = ctx.cs();
    if (DDVAL(name)) {
        cs << BC::ldddvar(name);
    } else if (name == R_MissingArg) {
        cs << BC::push(R_MissingArg);
    } else {
        if (ctx.code.top()->isCached(name)) {
            auto const cache_slot = ctx.code.top()->cacheSlotFor(name);
            cs << BC::ldvarCached(name, cache_slot);
        } else {
            cs << BC::ldvar(name);
        }
        if (Compiler::profile)
            cs << BC::recordType();
    }
}

// Constant
void compileConst(CodeStream& cs, SEXP constant) {
    SET_NAMED(constant, 2);
    cs << BC::push(constant) << BC::visible();
}

void compileExpr(CompilerContext& ctx, SEXP exp, bool voidContext) {
    // Dispatch on the current type of AST node
    switch (TYPEOF(exp)) {
        // Function application
    case LANGSXP: {

        auto fun = CAR(exp);
        auto args = CDR(exp);
        compileCall(ctx, exp, fun, args, voidContext);
    } break;
        // Variable lookup
    case SYMSXP:
        compileGetvar(ctx, exp);
        if (voidContext)
            ctx.cs() << BC::pop();
        break;
    case PROMSXP: {
        auto expr = PREXPR(exp);
        // TODO: honestly I do not know what should be the semantics of
        //       this shit.... For now force it here and see what
        //       breaks...
        //       * One of the callers that does this is eg. print.c:1013
        //       * Another (a bit more sane) producer of this kind of ast
        //         is eval.c::applydefine (see rhsprom). At least there
        //         the prom is already evaluated and only used to attach
        //         the expression to the already evaled value
        if (!voidContext) {
            SEXP val = evaluatePromise(exp);
            Protect p(val);
            compileConst(ctx.cs(), val);
            ctx.cs().addSrc(expr);
        }
    } break;
    case BCODESXP:
    case EXTERNALSXP:
        assert(false);
        break;
        // TODO : some code (eg. serialize.c:2154) puts closures into asts...
        //        not sure how we want to handle it...
        // Case(CLOSXP) {
        //     assert(false);
        // }

    default:
        if (!voidContext)
            compileConst(ctx.cs(), exp);
        break;
    }
}

Code* compilePromise(CompilerContext& ctx, SEXP exp) {
    ctx.pushPromiseContext(exp);
    compileExpr(ctx, exp);
    ctx.cs() << BC::ret();
    return ctx.pop();
}

/* Create a promise code object without compiling the AST to RIR bytecode.
   This is useful for evaluated promises: the bytecode is never used since
   the value is already stored in the promise.
*/
Code* compilePromiseNoRir(CompilerContext& ctx, SEXP exp) {
    ctx.pushPromiseContext(exp);
    ctx.cs() << BC::push(R_NilValue) << BC::ret();
    return ctx.pop();
}

} // anonymous namespace

SEXP Compiler::finalize() {
    FunctionWriter function;
    CompilerContext ctx(function, preserve);

    FunctionSignature signature(FunctionSignature::Environment::CallerProvided,
                                FunctionSignature::OptimizationLevel::Baseline);

    // Compile formals (if any) and create signature
    for (RListIter arg = RList(formals).begin(); arg != RList::end(); ++arg) {
        if (*arg == R_MissingArg) {
            function.addArgWithoutDefault();
        } else {
            Code* compiled = compilePromise(ctx, *arg);
            function.addDefaultArg(compiled);
        }
        signature.pushFormal(*arg, arg.tag());
    }

    ctx.push(exp, closureEnv);

    // Prepopulate all binding cache numbers for all variables occuring in the
    // function.
    std::function<void(SEXP)> scanNames = [&](SEXP e) {
        if (TYPEOF(e) == LANGSXP)
            for (auto n : RList(CDR(e))) {
                if (CAR(e) == symbol::rm) {
                    ctx.code.top()->loadsSlotInCache[n] =
                        CompilerContext::CodeContext::BindingCacheDisabled;
                } else if (TYPEOF(n) == SYMSXP) {
                    ctx.code.top()->cacheSlotFor(n);
                } else {
                    scanNames(n);
                }
            }
    };
    scanNames(exp);

    compileExpr(ctx, exp);
    ctx.cs() << BC::ret();
    Code* body = ctx.pop();
    function.finalize(body, signature, Context());

#ifdef ENABLE_SLOWASSERT
    CodeVerifier::verifyFunctionLayout(function.function()->container());
#endif

    return function.function()->container();
}

bool Compiler::unsoundOpts =
    !(getenv("UNSOUND_OPTS") &&
      std::string(getenv("UNSOUND_OPTS")).compare("off") == 0);

bool Compiler::profile =
    !(getenv("RIR_PROFILING") &&
      std::string(getenv("RIR_PROFILING")).compare("off") == 0);

bool Compiler::loopPeelingEnabled = true;

} // namespace rir
