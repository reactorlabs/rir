#include "Compiler.h"

#include "BC.h"
#include "CodeStream.h"

#include "R/r.h"
#include "R/RList.h"
#include "R/Sexp.h"
#include "R/Symbols.h"
#include "R/Funtab.h"

#include "analysis/dataflow.h"
#include "ir/Optimizer.h"
#include "utils/Pool.h"

#include "CodeVerifier.h"

#include <stack>

namespace rir {

namespace {

class Context {
  public:
    class LoopContext {
      public:
        LabelT next_;
        LabelT break_;
        bool context_needed_ = false;
        LoopContext(LabelT next_, LabelT break_)
            : next_(next_), break_(break_) {}
    };

    class CodeContext {
      public:
        CodeStream cs;
        std::stack<LoopContext> loops;
        CodeContext* parent;
        CodeContext(SEXP ast, FunctionHandle& fun, CodeContext* p) : cs(fun, ast), parent(p) {}
        virtual ~CodeContext() {}
        bool inLoop() {
            return !loops.empty() ||
                    (parent && parent->inLoop());
        }
        LabelT loopNext() {
            assert(!loops.empty());
            return loops.top().next_;
        }
        LabelT loopBreak() {
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
        PromiseContext(SEXP ast, FunctionHandle& fun, CodeContext* p) : CodeContext(ast, fun, p) {}
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

    FunctionHandle& fun;
    Preserve& preserve;

    Context(FunctionHandle& fun, Preserve& preserve, SEXP env)
        : fun(fun), preserve(preserve) {}

    ~Context() { assert(code.empty()); }

    bool inLoop() { return code.top()->inLoop(); }

    LoopContext& loop() { return code.top()->loops.top(); }

    bool loopNeedsContext() {
        assert(inLoop());
        return code.top()->loops.top().context_needed_;
    }

    bool loopIsLocal() {
        return code.top()->loopIsLocal();
    }

    LabelT loopNext() {
        return code.top()->loopNext();
    }

    LabelT loopBreak() {
        return code.top()->loopBreak();
    }

    void pushLoop(LabelT next_, LabelT break_) {
        code.top()->loops.emplace(next_, break_);
    }

    void popLoop() { code.top()->loops.pop(); }

    void push(SEXP ast) { code.push(new CodeContext(ast, fun, code.empty() ? nullptr : code.top())); }

    void pushPromiseContext(SEXP ast) { code.push(new PromiseContext(ast, fun, code.empty() ? nullptr : code.top())); }

    FunIdxT pop() {
        auto idx = cs().finalize();
        delete code.top();
        code.pop();
        return idx;
    }
};

FunIdxT compilePromise(Context& ctx, SEXP exp);
void compileExpr(Context& ctx, SEXP exp);
void compileCall(Context& ctx, SEXP ast, SEXP fun, SEXP args);

void compileDispatch(Context& ctx, SEXP selector, SEXP ast, SEXP fun,
                     SEXP args) {
    // Process arguments:
    // Arguments can be optionally named
    std::vector<FunIdxT> callArgs;
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

    ctx.cs().insertCall(Opcode::dispatch_, callArgs, names, ast, selector);
}

// Inline some specials
// TODO: once we have sufficiently powerful analysis this should (maybe?) go
//       away and move to an optimization phase.
bool compileSpecialCall(Context& ctx, SEXP ast, SEXP fun, SEXP args_) {
    RList args(args_);
    CodeStream& cs = ctx.cs();

    if (fun == symbol::Function && args.length() == 3) {
        auto cls = Compiler::compileClosure(args[1], args[0]);
        cs << BC::push(cls.formals)
           << BC::push(cls.bc)
           << BC::push(args[2])
           << BC::close();
        return true;
    }

    if (fun == symbol::seq && args.length() >= 2 && args.length() <= 3) {
        static SEXP seqFun = nullptr;
        if (!seqFun)
            seqFun = findFun(fun, R_GlobalEnv);

        cs << BC::guardName(fun, seqFun);

        for (auto a = args.begin(); a != args.end(); ++a)
            if (a.hasTag())
                return false;

        LabelT objBranch = cs.mkLabel();
        LabelT nextBranch = cs.mkLabel();

        compileExpr(ctx, args[0]);

        cs << BC::brobj(objBranch);

        compileExpr(ctx, args[1]);
        if (args.length() == 3) {
            compileExpr(ctx, args[2]);
        } else {
            cs << BC::push((int)1);
        }
        cs << BC::seq();
        cs.addSrc(ast);
        cs << BC::br(nextBranch);

        cs << objBranch;
        compileDispatch(ctx, fun, ast, fun, args_);

        cs << nextBranch;
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
        cs << BC::guardNamePrimitive(fun);

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

        LabelT nextBranch = cs.mkLabel();

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

        LabelT nextBranch = cs.mkLabel();

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
                cs << BC::dup()
                   << BC::setShared()
                   << (superAssign ? BC::stvar2(lhs) : BC::stvar(lhs))
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

                        LabelT objBranch = cs.mkLabel();
                        LabelT nextBranch = cs.mkLabel();

                        cs << BC::guardNamePrimitive(fun);

                        // First rhs (assign is right-associative)
                        compileExpr(ctx, rhs);
                        // Keep a copy of rhs since its the result of this
                        // expression
                        cs << BC::dup()
                           << BC::setShared();

                        // Now load target and index
                        cs << (superAssign ? BC::ldvar2(target) : BC::ldvar(target));
                        compileExpr(ctx, *idx);

                        // check for object case
                        cs << BC::swap();
                        cs << BC::brobj(objBranch);

                        // do the thing
                        if (fun2 == symbol::DoubleBracket)
                            cs << BC::subassign2(target);
                        else
                            cs << BC::subassign();

                        cs << (superAssign ? BC::stvar2(target) : BC::stvar(target));
                        cs << BC::br(nextBranch);

                        // In the case the target is an object:
                        cs << objBranch;

                        // We need a patched ast again :(
                        SEXP setter = fun2 == symbol::DoubleBracket
                                           ? symbol::AssignDoubleBracket
                                           : symbol::AssignBracket;
                        SEXP rewrite = Rf_shallow_duplicate(lhs);
                        ctx.preserve(rewrite);
                        SETCAR(rewrite, setter);

                        SEXP a = CDR(rewrite);
                        SETCAR(a, symbol::setterPlaceholder);
                        while (CDR(a) != R_NilValue)
                            a = CDR(a);
                        SEXP value =
                            CONS_NR(symbol::setterPlaceholder, R_NilValue);
                        SET_TAG(value, symbol::value);
                        SETCDR(a, value);

                        // Reorder stack into correct ordering
                        cs << BC::swap() << BC::pick(2);

                        // Do dispatch using args from the stack
                        cs.insertStackCall(
                            Opcode::dispatch_stack_, 3,
                            {R_NilValue, R_NilValue, symbol::value}, rewrite,
                            setter);

                        // store the result as "target"
                        cs << (superAssign ? BC::stvar2(target) : BC::stvar(target));

                        cs << nextBranch
                           << BC::invisible();
                        return true;
                    }
                }
            }

            //            if (getter == symbol::Bracket)
        }

        if (superAssign) {
            return false;
        }

        cs << BC::guardNamePrimitive(fun);

        compileExpr(ctx, rhs);
        cs << BC::dup()
           << BC::setShared();

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
                            compileExpr(ctx, *arg);
                        }
                    }

                    SEXP rewrite = Rf_shallow_duplicate(g);
                    ctx.preserve(rewrite);
                    SETCAR(CDR(rewrite), symbol::getterPlaceholder);
                    cs.insertStackCall(Opcode::call_stack_, names.size(), names,
                                       rewrite);
                }
                Else(assert(false);)
            }
            if (i > 1) {
                cs << BC::dup();
            }
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

            cs << BC::pick(1)
               << BC::makeUnique()
               << BC::put(1);

            // Load function and push it before the first arg and the value
            // from the last setter.
            cs << BC::ldfun(setterName)
               << BC::put(2);

            for (; arg != RList::end(); ++arg) {
                nargs++;
                names.push_back(arg.tag());
                if (TYPEOF(*arg) == LANGSXP || TYPEOF(*arg) == SYMSXP) {
                    auto p = compilePromise(ctx, *arg);
                    cs << BC::promise(p);
                } else {
                    compileExpr(ctx, *arg);
                }
            }

            names.push_back(symbol::value);
            // the rhs (aka "value") needs to come last, if we pushed some args
            // we need to swap the order
            if (nargs > 0)
                cs << BC::pick(nargs);

            SEXP rewrite = Rf_shallow_duplicate(*g);
            ctx.preserve(rewrite);
            SETCAR(rewrite, setterName);

            SEXP a = CDR(rewrite);
            SETCAR(a, symbol::setterPlaceholder);
            while (CDR(a) != R_NilValue)
                a = CDR(a);
            SEXP value = CONS_NR(symbol::setterPlaceholder, R_NilValue);
            SET_TAG(value, symbol::value);
            SETCDR(a, value);

            cs.insertStackCall(Opcode::call_stack_, names.size(), names,
                               rewrite);
        }

        cs << BC::stvar(target)
           << BC::invisible();

        return true;
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
        LabelT trueBranch = cs.mkLabel();
        LabelT nextBranch = cs.mkLabel();

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

            LabelT objBranch = cs.mkLabel();
            LabelT nextBranch = cs.mkLabel();

            cs << BC::guardNamePrimitive(fun);
            compileExpr(ctx, lhs);
            cs << BC::brobj(objBranch);

            compileExpr(ctx, *idx);
            if (args.length() == 2) {
                if (fun == symbol::DoubleBracket)
                    cs << BC::extract1();
                else
                    cs << BC::subset1();
            } else {
                compileExpr(ctx, *(idx + 1));
                if (fun == symbol::DoubleBracket)
                    cs << BC::extract2();
                else
                    cs << BC::subset2();
            }

            cs << BC::br(nextBranch);

            cs << objBranch;
            compileDispatch(ctx, fun, ast, fun, args_);

            cs << nextBranch;
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

        LabelT loopBranch = cs.mkLabel();
        LabelT nextBranch = cs.mkLabel();

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
            cs << BC::endcontext();
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

        LabelT loopBranch = cs.mkLabel();
        LabelT nextBranch = cs.mkLabel();

        ctx.pushLoop(loopBranch, nextBranch);

        unsigned beginLoopPos = cs.currentPos();

        cs << BC::beginloop(nextBranch)
           << loopBranch;

        compileExpr(ctx, body);
        cs << BC::pop()
           << BC::br(loopBranch)
           << nextBranch;

        if (ctx.loopNeedsContext()) {
            cs << BC::endcontext();
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

        LabelT loopBranch = cs.mkLabel();
        LabelT breakBranch = cs.mkLabel();
        LabelT endForBranch = cs.mkLabel();

        ctx.pushLoop(loopBranch, breakBranch);

        compileExpr(ctx, seq);
        cs << BC::setShared()
           << BC::push((int)0);

        std::vector<unsigned> pcs;

        pcs.push_back(cs.currentPos());
        cs << BC::beginloop(breakBranch)
           << loopBranch;

        // Move context out of the way
        pcs.push_back(cs.currentPos());
        cs << BC::put(2);

        cs << BC::inc()
           << BC::testBounds()
           << BC::brfalse(endForBranch)
           << BC::dup2()
           << BC::extract1();

        // Put context back
        pcs.push_back(cs.currentPos());
        cs << BC::pick(3);
        pcs.push_back(cs.currentPos());
        cs << BC::swap();

        // Set the loop variable
        cs << BC::stvar(sym);

        compileExpr(ctx, body);
        cs << BC::pop()
           << BC::br(loopBranch);

        cs << endForBranch;
        // Put context back
        pcs.push_back(cs.currentPos());
        cs << BC::pick(2);

        cs << breakBranch;

        if (ctx.loopNeedsContext()) {
            cs << BC::endcontext();
        } else {
            for (auto pc : pcs)
                cs.remove(pc);
        }

        cs << BC::pop()
           << BC::pop()
           << BC::push(R_NilValue)
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
                cs.insertStackCall(Opcode::static_call_stack_, args.length(),
                                   {}, inAst, internal);

                return true;
            }


            if (false && fun == symbol::lapply && args.length() == 2) {
                LabelT loopBranch = cs.mkLabel();
                LabelT nextBranch = cs.mkLabel();

                compileExpr(ctx, args[0]);

                // get length and names of the vector X
                cs << BC::dup()
                   << BC::dup()
                   << BC::names()
                   << BC::swap()
                   << BC::length()
                   << BC::dup()
                   << BC::makeUnique()
                   << BC::inc()
                   << BC::swap()
                   // allocate the ans vector with same size and names
                   << BC::alloc(VECSXP)
                   << BC::pick(2)
                   << BC::setNames()
                   << BC::swap()
                   // Put the counter in place
                   << BC::push((int)0);

                // lookup of the function to apply
                if (TYPEOF(args[1]) == SYMSXP) {
                    cs << BC::ldfun(args[1]);
                } else if (TYPEOF(args[1]) == STRSXP) {
                    assert(Rf_length(args[1]) == 1);
                    cs << BC::ldfun(Rf_install(CHAR(STRING_ELT(args[1], 0))));
                } else {
                    compileExpr(ctx, args[1]);
                    cs << BC::isfun();
                }
                cs << BC::put(4);

                // loop invariant stack layout: [fun, X, ans, len(vector)+1, i]

                // check end condition
                cs << loopBranch
                   << BC::inc()
                   << BC::dup2()
                   << BC::swap()
                   << BC::lt()
                   << BC::brfalse(nextBranch);

                // X[[i]]
                LabelT objBranch = cs.mkLabel();
                LabelT contBranch = cs.mkLabel();

                cs << BC::guardNamePrimitive(symbol::DoubleBracket);
                cs << BC::pull(4)
                   << BC::pull(4)
                   << BC::pull(2)
                   << BC::brobj(objBranch);

                cs << BC::extract1()
                   << BC::br(contBranch);

                static SEXP extractCall = nullptr;
                if (!extractCall)
                    extractCall = LCONS(symbol::DoubleBracket,
                                LCONS(symbol::getterPlaceholder, R_NilValue));

                cs << objBranch;
                cs.insertStackCall(Opcode::dispatch_stack_, 2, {}, extractCall,
                                   symbol::DoubleBracket);

                cs << contBranch;

                // f(X[[i]])
                SEXP rewritten = LCONS(args[1],
                        LCONS(symbol::getterPlaceholder, R_NilValue));

                cs.insertStackCall(Opcode::call_stack_, 1, {}, rewritten);

                // store result
                cs << BC::pull(1)
                   << BC::pick(4)
                   << BC::subassign2(R_NilValue)
                   << BC::put(2)
                   << BC::br(loopBranch);

                // Put ans to the top and remove rest
                cs << nextBranch
                   << BC::pop()
                   << BC::pop()
                   << BC::swap()
                   << BC::pop()
                   << BC::swap()
                   << BC::pop()
                   << BC::visible();

                return true;
            }
        }
    }


    SEXP builtin = fun->u.symsxp.value;
    if (TYPEOF(builtin) == BUILTINSXP) {
        for (auto a = args.begin(); a != args.end(); ++a)
            if (a.hasTag() || *a == R_DotsSymbol || *a == R_MissingArg)
                return false;

        // Those are somehow overloaded in std libs
        if (fun == symbol::standardGeneric)
            return false;

        cs << BC::guardNamePrimitive(fun);

        for (auto a : args)
            compileExpr(ctx, a);
        cs.insertStackCall(Opcode::static_call_stack_, args.length(), {}, ast,
                           builtin);

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
    std::vector<FunIdxT> callArgs;
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

    cs.insertCall(Opcode::call_, callArgs, names, ast);
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
        // TODO : some code (eg. serialize.c:2154) puts closures into asts...
        //        not sure how we want to handle it...
        // Case(CLOSXP) {
        //     assert(false);
        // }

        // Constant
        Else(compileConst(ctx.cs(), exp));
    }
}

std::vector<FunIdxT> compileFormals(Context& ctx, SEXP formals) {
    std::vector<FunIdxT> res;

    for (auto arg = RList(formals).begin(); arg != RList::end(); ++arg) {
        if (*arg == R_MissingArg)
            res.push_back(MISSING_ARG_IDX);
        else
            res.push_back(compilePromise(ctx, *arg));
    }

    return res;
}

FunIdxT compilePromise(Context& ctx, SEXP exp) {
    ctx.pushPromiseContext(exp);
    compileExpr(ctx, exp);
    ctx.cs() << BC::ret();
    return ctx.pop();
}

}  // anonymous namespace

Compiler::CompilerRes Compiler::finalize() {
    // Rprintf("****************************************************\n");
    // Rprintf("Compiling function\n");
    FunctionHandle function = FunctionHandle::create();
    Context ctx(function, preserve, env);

    auto formProm = compileFormals(ctx, formals);

    ctx.push(exp);

    compileExpr(ctx, exp);
    ctx.cs() << BC::ret();
    ctx.pop();

    CodeEditor code(function.entryPoint(), formals);
    Optimizer::optimize(code);

    for (size_t i = 0; i < code.numPromises(); ++i)
        if (code.promise(i))
            Optimizer::optimize(*code.promise(i));

    FunctionHandle opt = code.finalize();
#ifdef ENABLE_SLOWASSERT
    CodeVerifier::vefifyFunctionLayout(opt.store, globalContext());
#endif

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
