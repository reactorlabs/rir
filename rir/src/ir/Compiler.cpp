#include "Compiler.h"

#include "BC.h"
#include "CodeStream.h"

#include "R/Funtab.h"
#include "R/RList.h"
#include "R/Sexp.h"
#include "R/Symbols.h"
#include "R/r.h"

#include "../interpreter/cache.h"
#include "../interpreter/safe_force.h"
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
        cs << BC::brfalse(breakBranch);
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

bool compileSimpleFor(CompilerContext& ctx, SEXP sym, SEXP seq, SEXP body,
                      bool voidContext) {
    Match(seq) {
        Case(LANGSXP, fun, argsSexp) {
            RList args(argsSexp);
            if (fun != symbol::Colon || args.length() != 2) {
                return false;
            }

            SEXP start = args[0];
            SEXP end = args[1];
            if (TYPEOF(start) != INTSXP && TYPEOF(start) != LGLSXP &&
                (TYPEOF(start) != REALSXP || XLENGTH(start) == 0 ||
                *REAL(start) != (int)*REAL(start))) {
                return false;
            }

            // for(i in m:n) {
            //   ...
            // }
            // =>
            // i' <- m
            // n' <- n
            // if (i' > n') {
            //   n' <- ceil(n') - 1
            //   diff' <- -1
            //   gt' <- TRUE
            // } else {
            //   n' <- floor(n')
            //   diff' <- 1
            //   gt' <- FALSE
            // }
            // while ((i' > n') == gt') {
            //   i <- i'
            //   i' <- i' + diff'
            //   ...
            // }

            CodeStream& cs = ctx.cs();
            BC::Label fwdBranch = cs.mkLabel();
            BC::Label startBranch = cs.mkLabel();

            // i' <- m
            compileExpr(ctx, start);
            cs << BC::floor() << BC::ensureNamed();
            // n' <- n
            compileExpr(ctx, end);
            cs << BC::ensureNamed();
            // if (i' > n')
            cs << BC::dup2() << BC::gt();
            cs.addSrc(R_NilValue);
            cs << BC::brfalse(fwdBranch);
            // {
            // n' <- ceil(n') - 1
            cs << BC::ceil() << BC::dec() << BC::ensureNamed() << BC::swap();
            // diff' <- -1
            cs << BC::push(-1);
            // gt' <- TRUE
            cs << BC::push(R_TrueValue);
            cs << BC::put(3) << BC::put(2) << BC::br(startBranch) << fwdBranch;
            // } else {
            // n' <- floor(n')
            cs << BC::floor() << BC::swap();
            // diff' <- 1
            cs << BC::push(1);
            // gt' <- FALSE
            cs << BC::push(R_FalseValue);
            cs << BC::put(3) << BC::put(2) << startBranch;
            // while
            compileWhile(ctx,
                         [&cs]() {
                             // ((i' > n') ...
                             cs << BC::dup2() << BC::lt();
                             cs.addSrc(R_NilValue);
                             // ... == gt')
                             cs << BC::pull(4) << BC::eq();
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
                             // i' <- i' + diff'
                             cs << BC::pull(2) << BC::add();
                             cs.addSrc(R_NilValue);
                             // ...
                             compileExpr(ctx, body, true);
                             // }
                         },
                         !containsLoop(body));
            // } else {
            cs << BC::popn(4);
            if (!voidContext)
                cs << BC::push(R_NilValue) << BC::invisible();

            return true;
        }
        Else({
            return false;
        })
    }
    assert(false);
}

// Inline some specials
// TODO: once we have sufficiently powerful analysis this should (maybe?) go
//       away and move to an optimization phase.
bool compileSpecialCall(CompilerContext& ctx, SEXP ast, SEXP fun, SEXP args_,
                        bool voidContext) {
    // `true` if an argument isn't labeled, or `...`.
    auto isRegularArg = [](RListIter& arg) {
        return *arg != R_DotsSymbol && !arg.hasTag();
    };

    RList args(args_);
    CodeStream& cs = ctx.cs();

    if (fun == symbol::Function && args.length() == 3) {
        if (!voidContext) {
            SEXP fun = Compiler::compileFunction(args[1], args[0]);
            // Mark this as an inner function to prevent the optimizer from
            // assuming a stable environment
            DispatchTable::check(fun)->baseline()->innerFunction = true;
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
        cs.addSrc(args[0]);
        cs << BC::dup()
           << BC::brtrue(nextBranch);

        compileExpr(ctx, args[1]);

        cs << BC::asLogical();
        cs.addSrc(args[1]);
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
        if (g.length() != 3 && g.length() != 4) {
            return false;
        }

        bool is2d = g.length() == 4;
        SEXP fun2 = *g.begin();
        RListIter idx = g.begin() + 2;
        RListIter idx2 = is2d ? (g.begin() + 3) : idx;
        if ((fun2 != symbol::Bracket && fun2 != symbol::DoubleBracket) ||
            !isRegularArg(idx) || (is2d && !isRegularArg(idx2))) {
            return false;
        }

        emitGuardForNamePrimitive(cs, fun);

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
                cs << BC::ldvar(target);
        }

        if (Compiler::profile)
            cs << BC::recordType();

        // And index
        compileExpr(ctx, *idx);
        if (is2d) {
            compileExpr(ctx, *idx2);
        }

        if (is2d) {
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
        if (args.length() != 2 && args.length() != 3) {
            return false;
        }

        bool is2d = args.length() == 3;
        SEXP lhs = *args.begin();
        RListIter idx = args.begin() + 1;
        RListIter idx2 = args.begin() + 2;

        if (!isRegularArg(idx) || (is2d && !isRegularArg(idx2)))
            return false;

        emitGuardForNamePrimitive(cs, fun);
        compileExpr(ctx, lhs);

        compileExpr(ctx, *idx);
        if (is2d) {
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

        if (compileSimpleFor(ctx, sym, seq, body, voidContext))
            return true;

        BC::Label nextBranch = cs.mkLabel();
        BC::Label breakBranch = cs.mkLabel();
        ctx.pushLoop(nextBranch, breakBranch);

        // Compile the seq expression (vector) and initialize the loop
        compileExpr(ctx, seq);
        if (!isConstant(seq))
            cs << BC::setShared();
        cs << BC::forSeqSize() << BC::push((int)0);

        auto compileIndexOps = [&]() {
            // Increment the index and compare to the seq upper bound
            cs << BC::inc() << BC::ensureNamed() << BC::dup2() << BC::lt();
            // We know this is an int and won't do dispatch.
            // TODO: add a integer version of lt_
            cs.addSrc(R_NilValue);

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
            compileIndexOps();
            compileExpr(ctx, body, true);
        }

        cs << nextBranch;
        compileIndexOps();

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

    if (fun == symbol::Internal) {
        SEXP inAst = args[0];
        SEXP args_ = CDR(inAst);
        RList args(args_);
        SEXP fun = CAR(inAst);

        if (TYPEOF(fun) == SYMSXP) {
            for (RListIter a = args.begin(); a != args.end(); ++a)
                if (a.hasTag() || *a == R_DotsSymbol || *a == R_MissingArg)
                    return false;

            SEXP internal = fun->u.symsxp.internal;
            int i = ((sexprec_rjit*)internal)->u.i;

            // If the .Internal call goes to a builtin, then we call eagerly
            if (R_FunTab[i].eval % 10 == 1) {
                emitGuardForNamePrimitive(cs, symbol::Internal);
                for (SEXP a : args)
                    compileExpr(ctx, a);
                cs << BC::callBuiltin(args.length(), inAst, internal);
                if (voidContext)
                    cs << BC::pop();

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
    //     for (RListIter a = args.begin(); a != args.end(); ++a)
    //         if (a.hasTag() || *a == R_DotsSymbol || *a == R_MissingArg)
    //             return false;

    //     // Those are somehow overloaded in std libs
    //     if (fun == symbol::standardGeneric)
    //         return false;

    //     cs << BC::guardNamePrimitive(fun);

    //     for (SEXP a : args)
    //         compileExpr(ctx, a);
    //     cs << BC::staticCall(args.length(), ast, builtin);

    //     return true;
    // }

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
            cs << BC::isfun();
        });
    }

    if (Compiler::profile)
        cs << BC::recordCall();

    // Process arguments:
    // Arguments can be optionally named
    std::vector<SEXP> names;
    Assumptions assumptions;

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
        SEXP known = safeEval(*arg, nullptr);
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
void compileGetvar(CompilerContext& ctx, SEXP name, bool needsVisible = true) {
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
    if (needsVisible)
        cs << BC::visible();
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
            compileGetvar(ctx, exp, !voidContext);
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
                SEXP val = forcePromise(exp);
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
        signature.pushDefaultArgument();
    }

    ctx.push(exp, closureEnv);
    compileExpr(ctx, exp);
    ctx.cs() << BC::ret();
    Code* body = ctx.pop();
    function.finalize(body, signature);

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
