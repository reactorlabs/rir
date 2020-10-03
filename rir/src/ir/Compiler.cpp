#include "Compiler.h"

#include "BC.h"
#include "CodeStream.h"

#include "R/Funtab.h"
#include "R/RList.h"
#include "R/Sexp.h"
#include "R/Symbols.h"
#include "R/r.h"

#include "../interpreter/cache.h"
#include "../interpreter/interp.h"
#include "../interpreter/safe_force.h"
#include "interpreter/interp_incl.h"
#include "utils/Pool.h"

#include "CodeVerifier.h"

#include "simple_instruction_list.h"

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
    Match(exp) {
        Case(LANGSXP, fun, args_) {
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
        Else({
            return false;
        })
    }
    assert(false);
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
            return loadsSlotInCache.size() < MAX_CACHE_SIZE ||
                   loadsSlotInCache.count(name);
        }
        size_t cacheSlotFor(SEXP name) {
            return loadsSlotInCache.emplace(name, loadsSlotInCache.size())
                .first->second;
        }
        virtual bool loopIsLocal() { return !loops.empty(); }
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

    void popLoop() { code.top()->loops.pop(); }

    void push(SEXP ast, SEXP env) {
        code.push(
            new CodeContext(ast, fun, code.empty() ? nullptr : code.top()));
    }

    void pushPromiseContext(SEXP ast) {
        code.push(
            new PromiseContext(ast, fun, code.empty() ? nullptr : code.top()));
    }

    Code* pop() {
        Code* res = cs().finalize(0, code.top()->loadsSlotInCache.size());
        delete code.top();
        code.pop();
        return res;
    }
};

Code* compilePromise(CompilerContext& ctx, SEXP exp);
// If we are in a void context, then compile expression will not leave a value
// on the stack. For example in `{a; b}` the expression `a` is in a void
// context, but `b` is not. In `while(...) {...}` all loop body expressions are
// in a void context, since the loop as an expression is always nil.
void compileExpr(CompilerContext& ctx, SEXP exp, bool voidContext = false);
void compileCall(CompilerContext& ctx, SEXP ast, SEXP fun, SEXP args, bool voidContext);

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
    Match(seq) {
        Case(LANGSXP, fun, argsSexp) {
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
            bool staticFastcase =
                TYPEOF(start) != LANGSXP && TYPEOF(start) != SYMSXP &&
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
                Code* bodyProm = compilePromise(ctx, body);
                size_t bodyPromIdx = cs.addPromise(bodyProm);

                // 3) Add the function, arguments, and call
                Context assumptions = Context() | TypeAssumption::Arg0IsEager_ |
                                      Assumption::CorrectOrderOfArguments |
                                      Assumption::NotTooManyArguments;
                cs << BC::ldfun(symbol::For) << BC::swap()
                   << BC::mkEagerPromise(seqPromIdx)
                   << BC::mkPromise(bodyPromIdx)
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
                        cs << BC::stvarCached(
                            sym, ctx.code.top()->cacheSlotFor(sym));
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
            // clang-format off
        }
        Else({ 
            return false;
        })
    }
    // clang-format on
    assert(false);
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

    if (fun == symbol::Function && args.length() == 3) {
        if (!voidContext) {
            SEXP fun = Compiler::compileFunction(args[1], args[0]);
            // Mark this as an inner function to prevent the optimizer from
            // assuming a stable environment
            DispatchTable::check(fun)->baseline()->flags.set(
                Function::InnerFunction);
            assert(TYPEOF(fun) == EXTERNALSXP);
            cs << BC::push(args[0]) << BC::push(fun) << BC::push(args[2])
               << BC::close();
        }
        return true;
    }

    if (args.length() == 2 &&
        (fun == symbol::Add || fun == symbol::Sub ||
         fun == symbol::Mul || fun == symbol::Div || fun == symbol::Idiv ||
         fun == symbol::Mod || fun == symbol::Pow ||
         fun == symbol::Lt || fun == symbol::Gt ||
         fun == symbol::Le || fun == symbol::Ge ||
         fun == symbol::Eq || fun == symbol::Ne ||
         fun == symbol::Colon)) {
        emitGuardForNamePrimitive(cs, fun);

        compileExpr(ctx, args[0]);
        compileExpr(ctx, args[1]);

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

        if (voidContext)
            cs << BC::pop();
        else if (Compiler::profile)
            cs << BC::recordType();

        return true;
    }

    if (args.length() == 1 &&
        (fun == symbol::Add || fun == symbol::Sub ||
         fun == symbol::Not)) {
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

    if (fun == symbol::And && args.length() == 2) {
        emitGuardForNamePrimitive(cs, fun);

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

        if (voidContext)
            cs << BC::pop();
        return true;
    }

    if (fun == symbol::Or && args.length() == 2) {
        emitGuardForNamePrimitive(cs, fun);

        BC::Label nextBranch = cs.mkLabel();

        compileExpr(ctx, args[0]);

        cs << BC::asLogical();
        cs.addSrc(ast);
        cs << BC::dup()
           << BC::brtrue(nextBranch);

        compileExpr(ctx, args[1]);

        cs << BC::asLogical();
        cs.addSrc(ast);
        cs << BC::lglOr();

        cs << nextBranch;

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

    if (fun == symbol::Assign || fun == symbol::Assign2 || fun == symbol::SuperAssign) {
        assert(args.length() == 2);

        bool superAssign = fun == symbol::SuperAssign;

        SEXP lhs = args[0];
        SEXP rhs = args[1];

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
                        cs << BC::stvarCached(
                            lhs, ctx.code.top()->cacheSlotFor(lhs));
                    else
                        cs << BC::stvar(lhs);
                }
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
                    errorcall(ast, "invalid (do_set) left-hand side to assignment");
                })
            }
        }

        // 3) Special case [ and [[

        if (lhsParts.size() != 2) {
            return false;
        }

        RList g(lhsParts[0]);
        int dims = g.length() - 2;
        if (dims < 1 || dims > 3) {
            return false;
        }

        SEXP fun2 = *g.begin();
        RListIter idx = g.begin() + 2;
        if ((fun2 != symbol::Bracket && fun2 != symbol::DoubleBracket) ||
            !isRegularArg(idx) || (dims > 1 && !isRegularArg(idx + 1)) ||
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

        // Again, subassign bytecodes override objects with named count of 1. If
        // the target is from the outer scope that would be wrong. For example
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

        cs << BC::return_();
        return true;
    }

    if (fun == symbol::isnull && args.length() == 1) {
        emitGuardForNamePrimitive(cs, fun);
        compileExpr(ctx, args[0]);
        if (voidContext)
            cs << BC::pop();
        else
            cs << BC::is(NILSXP);
        return true;
    }

    if (fun == symbol::islist && args.length() == 1) {
        emitGuardForNamePrimitive(cs, fun);
        compileExpr(ctx, args[0]);
        if (voidContext)
            cs << BC::pop();
        else
            cs << BC::is(VECSXP);
        return true;
    }

    if (fun == symbol::ispairlist && args.length() == 1) {
        emitGuardForNamePrimitive(cs, fun);
        compileExpr(ctx, args[0]);
        if (voidContext)
            cs << BC::pop();
        else
            cs << BC::is(LISTSXP);
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

        compileExpr(ctx, *idx);
        if (dims == 3) {
            compileExpr(ctx, *(idx + 1));
            compileExpr(ctx, *(idx + 2));
            assert(fun != symbol::DoubleBracket);
            cs << BC::extract1_3();
        } else if (dims == 2) {
            compileExpr(ctx, *(idx + 1));
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

    if (fun == symbol::Missing && args.length() == 1 && TYPEOF(args[0]) == SYMSXP &&
        !DDVAL(args[0])) {
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

        compileWhile(ctx,
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
        if (!isConstant(seq))
            cs << BC::setShared();
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
            cs << BC::br(ctx.loopNext())
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
            emitGuardForNamePrimitive(cs, fun);
            cs << BC::br(ctx.loopBreak())
               << BC::push(R_NilValue);
            return true;
        }
    }

    if (fun == symbol::Switch) {
        /* # Assume argLen > 1:
         *   compile arg[0]
         *   if (arg[0] is not length-1 vector) br vecECont
         *   error(...)
         *   # unreachable
         * vecECont:
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
         *   if (value == group[0]) br labels[g0]
         *   ...
         *   if (value == group[k+1]) br labels[gk]
         *   br default label[?] # or nil if no default
         *
         * label[i]:
         *   # if arg[i+1] is missing, we must came from integer case
         *   # error(...)
         *   compile expression[i]
         *   br cont
         * ...
         *
         * nil:
         *   push R_NilValue
         *
         * cont:
         *   # (stack is [1stArg, retval])
         *   swap
         *   pop # 1stArg
         */
#define ERROR_CALL_CODEGEN(MSG, CONT) {                   \
    cs << BC::push(R_TrueValue) /* call site */           \
       << BC::push(Rf_mkString((MSG)))                    \
       << BC::callBuiltin(2, ast, getBuiltinFun("stop"))  \
       /* verifier doesn't know `stop` won't return */    \
       << BC::pop() << BC::br((CONT));                    \
}
#define WARNING_CALL_CODEGEN(MSG) {                          \
    cs << BC::push(R_TrueValue)  /* call site */             \
       << BC::push(R_FalseValue) /* immediate */             \
       << BC::push(R_FalseValue) /* noBreak   */             \
       << BC::push(Rf_mkString((MSG)))                       \
       << BC::callBuiltin(4, ast, getBuiltinFun("warning"))  \
       << BC::pop();                                         \
}
        int argLen = args.length();
        // argLen error/warning is statically determinable
        if (argLen == 0) {
            BC::Label exit = cs.mkLabel();
            ERROR_CALL_CODEGEN("'EXPR' is missing", exit);
            cs << exit << BC::push(R_NilValue);
            if (voidContext)
                cs << BC::pop();
            return true;
        }
        // when 1st arg is string, switch behaves like C/C++ switch/case.
        // Cases like `x=, y=, z=20` are grouped together.
        // groups[-1] is not used, for impl convenience
        std::vector<std::vector<SEXP>> groups = {{}};
        std::vector<BC::Label> groupLabels; // eval/return for each group
        std::vector<SEXP> expressions;      // return value ast for each group
        std::vector<BC::Label> labels;      // eval/return for each arg
        std::vector<bool> argMissing(argLen-1, true);
        int dftLabelIdx = -1; // index into `labels` for default return arg
        bool dupDflt = false;
        BC::Label vecArityBr = cs.mkLabel();
        BC::Label vecErrorBr = cs.mkLabel();
        BC::Label vecEContBr = cs.mkLabel();
        BC::Label facWContBr = cs.mkLabel();
        BC::Label strBr  = cs.mkLabel();
        BC::Label nilBr  = cs.mkLabel();
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
        }

        /******************* INSTRUCTIONS START HERE *********************/
        compileExpr(ctx, args[0]);
        // based on definition of `isVector` in `Rinlinedfuns.h`
        for (auto type : {LGLSXP, INTSXP, REALSXP, CPLXSXP, STRSXP,
                          RAWSXP, VECSXP, EXPRSXP}) {
          cs << BC::dup() << BC::is(type)
             << BC::recordTest() << BC::brtrue(vecArityBr);
        }
        cs << BC::br(vecErrorBr)
           << vecArityBr
           << BC::dup() << BC::xlength_() << BC::push(Rf_ScalarInteger(1))
           << BC::eq() << BC::brtrue(vecEContBr)
           << vecErrorBr;
        ERROR_CALL_CODEGEN("EXPR must be a length 1 vector", nilBr);
        cs << vecEContBr
           // isFactor is INTSXP + inherits("factor") from `Rinlinedfuns.h`
           << BC::dup() << BC::is(INTSXP) << BC::brfalse(facWContBr)
           << BC::dup() << BC::push(Rf_mkString("factor")) << BC::push(R_FalseValue)
           << BC::callBuiltin(3, ast, getBuiltinFun("inherits"))
           << BC::brfalse(facWContBr);
        WARNING_CALL_CODEGEN(
            "EXPR is a \"factor\", treated as integer.\n"
            " Consider using 'switch(as.character( * ), ...)' instead.")
        cs << facWContBr;
        if (argLen == 1) { // inserted here to mimic behavior of builtin impl
            WARNING_CALL_CODEGEN("'switch' with no alternatives");
            cs << BC::br(nilBr);
        }
        cs << BC::dup() << BC::is(STRSXP) << BC::brtrue(strBr);
        // TODO needs Rf_asInteger, builtin as.integer behaves differently
        // `raw(1)` errors on asInteger, but not on `as.integer`
        cs << BC::callBuiltin(1, ast, getBuiltinFun("as.integer"));
        // currently stack is [arg[0]] (converted to integer)
        for (size_t i = 0; i < labels.size(); ++i) {
            cs << BC::dup() << BC::push(Rf_ScalarInteger(i+1)) << BC::eq();
            cs.addSrc(R_NilValue); // call argument for builtin
            cs << BC::asbool() << BC::recordTest() << BC::brtrue(labels[i]);
        }
        cs << BC::br(nilBr) << strBr;
        if (dupDflt) {
            ERROR_CALL_CODEGEN("duplicate 'switch' defaults", nilBr);
        } else {
            for (size_t i = 0; i < expressions.size(); ++i) {
                for (auto& n : groups[i]) {
                    cs << BC::dup() << BC::push(n) << BC::eq();
                    cs.addSrc(R_NilValue); // call argument for builtin
                    cs << BC::asbool() << BC::recordTest() << BC::brtrue(groupLabels[i]);
                }
            }
            auto fallbackLabel = (dftLabelIdx == -1) ? nilBr : labels[dftLabelIdx];
            cs << BC::br(fallbackLabel);
        }

        for (size_t i=0, j=0; i < labels.size(); ++i) {
            cs << labels[i];
            if (argMissing[i]) {
                ERROR_CALL_CODEGEN("empty alternative in numeric switch", nilBr);
                cs << BC::br(nilBr);
                continue;
            } else {
                compileExpr(ctx, expressions[j++]);
                cs << BC::br(contBr);
            }
        }

        cs << nilBr << BC::push(R_NilValue);
        cs << contBr;
        cs << BC::swap() << BC::pop(); // pop off 1st arg, leave answer on top
        if (voidContext)
            cs << BC::pop();
        return true;
#undef ERROR_CALL_CODEGEN
#undef WARNING_CALL_CODEGEN
    }

    if (fun == symbol::Internal) {
        SEXP inAst = args[0];
        SEXP args_ = CDR(inAst);
        RList args(args_);
        SEXP fun = CAR(inAst);

        if (TYPEOF(fun) == SYMSXP) {
            SEXP internal = fun->u.symsxp.internal;
            int i = ((sexprec_rjit*)internal)->u.i;

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
                        cs << BC::push(R_DotsSymbol);
                        names.push_back(R_DotsSymbol);
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

            if (fun == symbol::lapply && args.length() == 2) {

                BC::Label loopBranch = cs.mkLabel();
                BC::Label nextBranch = cs.mkLabel();

                compileExpr(ctx, args[0]); // [X]

                // get length and names of the vector X
                cs << BC::dup()
                   << BC::names()
                   << BC::swap()
                   << BC::xlength_() // [names(X), length(X)]
                   << BC::dup()
                   << BC::push(Rf_mkString("list"))
                   << BC::swap()
                   << BC::callBuiltin(2, symbol::tmp, getBuiltinFun("vector")) // [names(X), length(X), ans]
                   << BC::pick(2)
                   << BC::setNames()
                   << BC::swap()
                   << BC::push((int)0); // [ans, length(X), i]

                // loop invariant stack layout: [ans, length(X), i]

                // check end condition
                cs << loopBranch
                   << BC::inc()
                   << BC::dup2()
                   << BC::lt();
                cs.addSrc(ast);

                SEXP isym = Rf_install("i");
                cs << BC::brtrue(nextBranch)
                   << BC::dup()
                   << BC::stvar(isym);

                // construct ast for FUN(X[[i]], ...)
                SEXP tmp = LCONS(symbol::DoubleBracket,
                                 LCONS(args[0], LCONS(isym, R_NilValue)));
                SEXP call =
                    LCONS(args[1], LCONS(tmp, LCONS(R_DotsSymbol, R_NilValue)));

                PROTECT(call);
                compileCall(ctx, call, CAR(call), CDR(call), false);
                UNPROTECT(1);

                // store result
                cs << BC::pull(1)
                   << BC::pick(4)
                   << BC::swap() // [length(X), i, fun(X[[i]], ...), ans, i]
                   << BC::subassign2_1();
                cs.addSrc(ast);

                cs << BC::put(2) // [ans, length(X), i]
                   << BC::br(loopBranch);

                // put ans to the top and remove rest
                cs << nextBranch
                   << BC::pop()
                   << BC::pop()
                   << BC::visible();

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

// function application
void compileCall(CompilerContext& ctx, SEXP ast, SEXP fun, SEXP args,
                 bool voidContext) {
    CodeStream& cs = ctx.cs();

    // application has the form:
    // LHS ( ARGS )

    // LHS can either be an identifier or an expression
    Match(fun) {
        Case(SYMSXP) {
            if (compileSpecialCall(ctx, ast, fun, args, voidContext))
                return;

            cs << BC::ldfun(fun);
        }
        Else({
            compileExpr(ctx, fun);
            cs << BC::checkClosure();
        });
    }

    if (Compiler::profile)
        cs << BC::recordCall();

    // Process arguments:
    // Arguments can be optionally named
    std::vector<SEXP> names;
    Context assumptions;

    bool hasNames = false;
    bool hasDots = false;
    int i = 0;
    for (RListIter arg = RList(args).begin(); arg != RList::end(); ++i, ++arg) {
        if (*arg == R_DotsSymbol) {
            cs << BC::push(R_DotsSymbol);
            names.push_back(R_DotsSymbol);
            hasDots = true;
            continue;
        }
        if (*arg == R_MissingArg) {
            cs << BC::push(R_MissingArg);
            names.push_back(R_NilValue);
            continue;
        }

        // (1) Arguments are wrapped as Promises:
        //     create a new Code object for the promise
        Code* prom = compilePromise(ctx, *arg);
        size_t idx = cs.addPromise(prom);

        // (2) remember if the argument had a name associated
        names.push_back(arg.tag());
        if (arg.tag() != R_NilValue)
            hasNames = true;

        // (3) "safe force" the argument to get static assumptions
        SEXP known = safeEval(*arg);
        // TODO: If we add more assumptions should probably abstract with
        // testArg in interp.cpp. For now they're both much different though
        if (known != R_UnboundValue) {
            assumptions.setEager(i);
            if (!isObject(known)) {
                assumptions.setNotObj(i);
                if (IS_SIMPLE_SCALAR(known, REALSXP))
                    assumptions.setSimpleReal(i);
                if (IS_SIMPLE_SCALAR(known, INTSXP))
                    assumptions.setSimpleInt(i);
            }
            cs << BC::push(known);
            cs << BC::mkEagerPromise(idx);
        } else {
            cs << BC::mkPromise(idx);
        }
    }

    if (hasDots) {
        cs << BC::callDots(i, names, ast, assumptions);
    } else if (hasNames) {
        cs << BC::call(i, names, ast, assumptions);
    } else {
        assumptions.add(Assumption::CorrectOrderOfArguments);
        cs << BC::call(i, ast, assumptions);
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
        if (ctx.code.top()->isCached(name))
            cs << BC::ldvarCached(name, ctx.code.top()->cacheSlotFor(name));
        else
            cs << BC::ldvar(name);
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
    Match(exp) {
        // Function application
        Case(LANGSXP, fun, args) {
            compileCall(ctx, exp, fun, args, voidContext);
        }
        // Variable lookup
        Case(SYMSXP) {
            compileGetvar(ctx, exp);
            if (voidContext)
                ctx.cs() << BC::pop();
        }
        Case(PROMSXP, value, expr) {
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
        }
        Case(BCODESXP) { assert(false); }
        Case(EXTERNALSXP) { assert(false); }
        // TODO : some code (eg. serialize.c:2154) puts closures into asts...
        //        not sure how we want to handle it...
        // Case(CLOSXP) {
        //     assert(false);
        // }

        // Constant
        Else(if (!voidContext) compileConst(ctx.cs(), exp));
    }
}

Code* compilePromise(CompilerContext& ctx, SEXP exp) {
    ctx.pushPromiseContext(exp);
    compileExpr(ctx, exp);
    ctx.cs() << BC::ret();
    return ctx.pop();
}

}  // anonymous namespace

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
    compileExpr(ctx, exp);
    ctx.cs() << BC::ret();
    Code* body = ctx.pop();
    function.finalize(body, signature, Context());

#ifdef ENABLE_SLOWASSERT
    CodeVerifier::verifyFunctionLayout(function.function()->container(),
                                       globalContext());
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
