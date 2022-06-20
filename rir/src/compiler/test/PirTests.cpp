#include "PirTests.h"
#include "../analysis/query.h"
#include "../analysis/verifier.h"
#include "../pir/pir_impl.h"
#include "../util/visitor.h"
#include "R/Protect.h"
#include "R/RList.h"
#include "R_ext/Parse.h"
#include "api.h"
#include "bc/Compiler.h"
#include "compiler/analysis/cfg.h"
#include "compiler/compiler.h"
#include "compiler/parameter.h"
#include <string>
#include <vector>

namespace {
using namespace rir;

SEXP compileToRir(const std::string& context, const std::string& expr,
                  SEXP super = R_GlobalEnv) {
    Protect p;

    auto eval = [&p](const std::string& expr, SEXP env) {
        ParseStatus status;
        // Parse expression
        SEXP str = p(Rf_mkString(("{" + expr + "}").c_str()));
        SEXP e = p(R_ParseVector(str, -1, &status, R_NilValue));

        // Compile expression to rir
        SEXP rirexp =
            p(rir::Compiler::compileFunction(VECTOR_ELT(e, 0), R_NilValue));

        // Evaluate expression under the fresh environment `env`
        Rf_eval(rirexp, env);
    };

    if (context != "") {
        SEXP contextEnv = p(Rf_allocSExp(ENVSXP));
        ENCLOS(contextEnv) = super;
        eval(context, contextEnv);
        super = contextEnv;
    }

    SEXP env = p(Rf_allocSExp(ENVSXP));
    ENCLOS(env) = super;
    eval(expr, env);
    return env;
}
typedef std::unordered_map<std::string, pir::ClosureVersion*> ClosuresByName;

ClosuresByName compileRir2Pir(SEXP env, pir::Module* m) {
    pir::Log logger({pir::DebugOptions::DebugFlags() |
                         // pir::DebugFlag::PrintIntoStdout |
                         // pir::DebugFlag::PrintEarlyPir |
                         // pir::DebugFlag::PrintOptimizationPasses |
                         pir::DebugFlag::PrintFinalPir,
                     std::regex(".*"), std::regex(".*"),
                     pir::DebugStyle::Standard});
    pir::Compiler cmp(m, logger);

    // Compile every function in the environment
    ClosuresByName results;
    auto envlist = RList(FRAME(env));
    for (auto f = envlist.begin(); f != envlist.end(); ++f) {
        auto fun = *f;
        if (TYPEOF(fun) == CLOSXP) {
            assert(isValidClosureSEXP(fun));
            cmp.compileClosure(
                fun, "test_function", pir::Compiler::defaultContext, true,
                [&](pir::ClosureVersion* cls) {
                    results[CHAR(PRINTNAME(f.tag()))] = cls;
                },
                []() { assert(false); }, {});
        }
    }

    cmp.optimizeModule();
    cmp.optimizeModule();
    return results;
}

ClosuresByName compile(const std::string& context, const std::string& expr,
                       pir::Module* m, SEXP super = R_GlobalEnv) {
    SEXP env = compileToRir(context, expr, super);
    return compileRir2Pir(env, m);
}

using namespace rir::pir;
typedef std::function<bool(void)> TestClosureVersion;
typedef std::pair<std::string, TestClosureVersion> Test;

#define CHECK(___test___)                                                      \
    if (!(___test___)) {                                                       \
        m.print(std::cerr);                                                    \
        std::cerr << "'" << #___test___ << "' failed\n";                       \
        assert(false);                                                         \
        return false;                                                          \
    }

bool test42(const std::string& input) {
    pir::Module m;
    auto res = compile("", "theFun <- function() " + input, &m);
    auto f = res["theFun"];

    CHECK(Query::noEnv(f));

    auto r = Query::returned(f);
    CHECK(r.size() == 1);

    auto ld = Const::Cast((*r.begin()));
    CHECK(ld);
    CHECK(TYPEOF(ld->c()) == INTSXP);
    CHECK(INTEGER(ld->c())[0] == 42);
    return true;
}

bool hasLoadVar(const std::string& input) {
    pir::Module m;
    auto res = compile("", input, &m);
    auto f = res["theFun"];
    bool noLdVar = Visitor::check(
        f->entry, [&](Instruction* i) { return !LdVar::Cast(i); });
    return !noLdVar;
}

class NullBuffer : public std::ostream, std::streambuf {
  public:
    NullBuffer() : std::ostream(this) {}
    int overflow(int c) {
        return (c == std::ostream::traits_type::eof()) ? '\0' : c;
    }
};

bool verify(Module* m) {
    m->eachPirClosureVersion(
        [](ClosureVersion* v) { Verify::apply(v, "Error in Module::verify"); });
    // TODO: find fix for osx
    NullBuffer nb;
    m->print(nb);
    // m->print(std::cout);
    return true;
}

bool compileAndVerify(const std::string& context, const std::string& input) {
    pir::Module m;
    compile(context, input, &m);
    bool t = verify(&m);
    return t;
}

bool compileAndVerify(const std::string& input) {
    return compileAndVerify("", input);
}

extern "C" SEXP Rf_NewEnvironment(SEXP, SEXP, SEXP);
SEXP parseCompileToRir(std::string);

bool testCondition(const std::string& input,
                   std::function<void(pir::ClosureVersion*)> condition) {
    pir::Module m;
    compile("", input, &m);
    bool t = verify(&m);
    m.eachPirClosureVersion(condition);
    // m.print(std::cout);
    return t;
}

bool canRemoveEnvironment(const std::string& input) {
    auto t = true;
    auto condition = [&t](pir::ClosureVersion* f) { t = t && Query::noEnv(f); };
    return testCondition(input, condition) && t;
}

bool testDeadStore() {
    auto hasAssign = [](pir::ClosureVersion* f) {
        size_t count = 0;
        Visitor::run(f->entry, [&](Instruction* i) {
            if (StVar::Cast(i))
                count++;
        });
        return count;
    };
    {
        pir::Module m;
        auto res = compile("", "f <- function(x) {y <- 2}", &m);
        auto f = res["f"];
        CHECK(!hasAssign(f));
    }
    {
        // Scope analysis promotes "y" to ssa variable. Thus the stores are not
        // needed anymore.
        pir::Module m;
        auto res =
            compile("", "f <- function(x) {if (x) y <- 1 else y <- 2; y}", &m);
        auto f = res["f"];
        CHECK(!hasAssign(f));
    }
    {
        // Both updates to "y" happen before the first leak. therefore they are
        // both folded into mkenv
        pir::Module m;
        auto res = compile("", "f <- function(x) {y <- 1; y <- 2; leak()}", &m);
        auto f = res["f"];
        CHECK(hasAssign(f) == 0);
    }
    {
        // both updates to "y" happen between observations. Only when we
        // return, the env could be observed again. Thus the first store can
        // be removed
        pir::Module m;
        auto res = compile("", "f <- function(x) {leak(); y <- 1; y <- 2}", &m);
        auto f = res["f"];
        CHECK(hasAssign(f) == 0);
    }
    {
        // Both updates to "y" are observable. The first by foo, the second by
        // anything that happens after exit.
        pir::Module m;
        auto res = compile(
            "", "f <- function(x) {leak(); y <- 1; foo(); y <- 2; foo(); y}",
            &m);
        auto f = res["f"];
        CHECK(hasAssign(f) == 2);
    }
    return true;
}

bool testSuperAssign() {
    auto hasAssign = [](pir::ClosureVersion* f) {
        return !Visitor::check(f->entry, [](Instruction* i) {
            if (StVar::Cast(i))
                return false;
            return true;
        });
    };
    auto hasSuperAssign = [](pir::ClosureVersion* f) {
        return !Visitor::check(f->entry, [](Instruction* i) {
            if (StVarSuper::Cast(i))
                return false;
            return true;
        });
    };
    {
        pir::Module m;
        // This super assign can be fully resolved, and dead store eliminated
        auto res =
            compile("", "f <- function() {a <- 1; (function() a <<- 1)()}", &m);
        auto f = res["f"];
        CHECK(!hasSuperAssign(f));
        CHECK(!hasAssign(f));
    }
    {
        pir::Module m;
        // This super assign cannot be removed, since the super env is unknown
        auto res = compile("", "f <- function() {(function() a <<- 1)()}", &m);
        auto f = res["f"];
        CHECK(hasSuperAssign(f));
    }
    {
        pir::Module m;
        // This super assign can be removed, since the super env is not tainted
        auto res = compile(
            "", "f <- function() {a <- 1; (function() {a <<- 1; asdf()})()}",
            &m);
        auto f = res["f"];
        CHECK(!hasSuperAssign(f));
    }
    {
        pir::Module m;
        // This super assign cannot be removed, since the super env is tainted
        auto res = compile(
            "", "f <- function() {a <- 1; asdf(); (function() a <<- 1)()}", &m);
        auto f = res["f"];
        CHECK(hasSuperAssign(f));
    }

    return true;
}

// ----------------- PIR to RIR tests -----------------

SEXP parseCompileToRir(std::string input) {
    Protect p;
    ParseStatus status;

    SEXP str = p(Rf_mkString(input.c_str()));
    SEXP expr = p(R_ParseVector(str, -1, &status, R_NilValue));
    SEXP cls = p(Rf_eval(VECTOR_ELT(expr, 0), R_GlobalEnv));

    rir::Compiler::compileClosure(cls);

    return cls;
}

SEXP createRWrapperCall(std::string wrapper) {
    Protect p;
    ParseStatus status;

    SEXP str = p(Rf_mkString(wrapper.c_str()));
    SEXP expr = p(R_ParseVector(str, -1, &status, R_NilValue));
    SEXP call = p(VECTOR_ELT(expr, 0));

    return call;
}

extern "C" Rboolean R_compute_identical(SEXP, SEXP, int);
bool checkPir2Rir(SEXP expected, SEXP result) {
    return R_compute_identical(expected, result, 15) == TRUE;
}

bool testPir2Rir(const std::string& name, const std::string& fun,
                 const std::string& args, bool useSame = false,
                 bool verbose = false) {
    Protect p;

    std::string wrapper =
        "rir.compile( function() " + name + "(" + args + ") )()";

    if (verbose) {
        Rprintf("   > %s <- %s\n", name.c_str(), fun.c_str());
        Rprintf("   > %s\n\n", wrapper.c_str());
    }

    auto execEnv = p(Rf_NewEnvironment(R_NilValue, R_NilValue, R_GlobalEnv));
    auto rirFun = p(parseCompileToRir(fun));
    SET_CLOENV(rirFun, execEnv);
    Rf_defineVar(Rf_install(name.c_str()), rirFun, execEnv);

    // bind `bar <- function(a) a + 1` in the execEnv to be called by the tested
    // function
    auto rirFun2 = p(parseCompileToRir("function(a) a + 1"));
    SET_CLOENV(rirFun2, execEnv);
    Rf_defineVar(Rf_install("bar"), rirFun2, execEnv);

    auto rCall = createRWrapperCall(wrapper);

    auto orig = p(Rf_eval(rCall, execEnv));
    if (verbose) {
        Rprintf(" orig = %p\n", orig);
        Rf_PrintValue(orig);
    }

    if (!useSame) {
        // redo everything
        execEnv = p(Rf_NewEnvironment(R_NilValue, R_NilValue, R_GlobalEnv));
        rirFun = p(parseCompileToRir(fun));
        SET_CLOENV(rirFun, execEnv);
        Rf_defineVar(Rf_install(name.c_str()), rirFun, execEnv);
        rirFun2 = p(parseCompileToRir("function(a) a + 1"));
        SET_CLOENV(rirFun2, execEnv);
        Rf_defineVar(Rf_install("bar"), rirFun2, execEnv);
        rCall = createRWrapperCall(wrapper);
    }

    pirCompile(rirFun, {}, "from_testPir2Rir", rir::pir::DebugOptions());

    auto after = p(Rf_eval(rCall, execEnv));
    if (verbose) {
        Rprintf("after = %p\n", after);
        Rf_PrintValue(after);
    }

    return checkPir2Rir(orig, after);
}

class MockBB : public BB {
    class MockCode : public pir::Code {
      public:
        void printName(std::ostream& out) const override {
            out << "mockCode_" << this;
        }
        MockCode(BB* e, size_t s) : Code() {
            entry = e;
            nextBBId = s;
        }
        ~MockCode() {
            // ~Code wants to delete something
            entry = new MockBB;
        }
        rir::Code* rirSrc() const override final { return nullptr; }
    };

  public:
    static MockCode code;
    MockBB() : BB(&code, code.nextBBId++) {
        if (!code.entry)
            code.entry = this;
    }
    static void reset() {
        code.entry = nullptr;
        code.nextBBId = 0;
    }
};
MockBB::MockCode MockBB::code = MockCode(nullptr, 0);

bool testCfg() {
    {
        /*
         *    A
         *   / \
         *  B   C
         *  |   |
         *  |   D
         *   \ /
         *    F
         *
         *    E (dead BB, for testing purposes)
         */
        MockBB::reset();
        MockBB A, B, C, D, E, F;
        A.setBranch(&B, &C);
        C.setNext(&D);
        D.setNext(&F);
        B.setNext(&F);

        CFG cfg(&MockBB::code);

        assert(cfg.isPredecessor(&A, &B));
        assert(cfg.isPredecessor(&A, &C));
        assert(cfg.isPredecessor(&A, &D));
        assert(cfg.isPredecessor(&A, &F));
        assert(!cfg.isPredecessor(&B, &C));
        assert(!cfg.isPredecessor(&D, &C));

        DominanceGraph dom(&MockBB::code);

        assert(dom.dominates(&A, &A));
        assert(dom.dominates(&B, &B));
        assert(dom.dominates(&C, &C));
        assert(dom.dominates(&D, &D));
        assert(dom.dominates(&F, &F));

        assert(!dom.strictlyDominates(&A, &A));
        assert(!dom.strictlyDominates(&B, &B));
        assert(!dom.strictlyDominates(&C, &C));
        assert(!dom.strictlyDominates(&D, &D));
        assert(!dom.strictlyDominates(&F, &F));

        assert(dom.dominates(&A, &B));
        assert(dom.dominates(&A, &C));
        assert(dom.dominates(&A, &D));
        assert(dom.dominates(&A, &F));
        assert(dom.dominates(&C, &D));
        assert(!dom.dominates(&B, &F));
        assert(!dom.dominates(&C, &F));

        assert(dom.strictlyDominates(&A, &B));
        assert(dom.strictlyDominates(&A, &C));
        assert(dom.strictlyDominates(&A, &D));
        assert(dom.strictlyDominates(&A, &F));
        assert(dom.strictlyDominates(&C, &D));

        assert(dom.immediatelyDominates(&A, &B));
        assert(dom.immediatelyDominates(&A, &C));
        assert(!dom.immediatelyDominates(&A, &D));
        assert(dom.immediatelyDominates(&A, &F));
    }

    {
        /*
         *    A
         *   / \
         *  B   C <-> E
         *   \ /
         *    D
         */

        MockBB::reset();
        MockBB A, B, C, D, E;
        A.setBranch(&B, &C);
        B.setNext(&D);
        C.setBranch(&E, &D);
        E.setNext(&C);

        CFG cfg(&MockBB::code);
        assert(cfg.isPredecessor(&A, &E));
        assert(cfg.isPredecessor(&C, &E));
        assert(cfg.isPredecessor(&E, &C));
        assert(cfg.isPredecessor(&C, &D));

        DominanceGraph dom(&MockBB::code);
        assert(dom.dominates(&A, &A));
        assert(dom.dominates(&B, &B));
        assert(dom.dominates(&C, &C));
        assert(dom.dominates(&D, &D));
        assert(dom.dominates(&E, &E));

        assert(!dom.strictlyDominates(&A, &A));
        assert(!dom.strictlyDominates(&B, &B));
        assert(!dom.strictlyDominates(&C, &C));
        assert(!dom.strictlyDominates(&D, &D));
        assert(!dom.strictlyDominates(&E, &E));

        assert(dom.dominates(&A, &B));
        assert(dom.dominates(&A, &C));
        assert(dom.dominates(&A, &D));
        assert(dom.dominates(&A, &E));

        assert(dom.strictlyDominates(&A, &B));
        assert(dom.strictlyDominates(&A, &C));
        assert(dom.strictlyDominates(&A, &D));
        assert(dom.strictlyDominates(&A, &E));

        assert(dom.immediatelyDominates(&A, &B));
        assert(dom.immediatelyDominates(&A, &C));
        assert(dom.immediatelyDominates(&A, &D));
        assert(!dom.immediatelyDominates(&A, &E));
        assert(dom.immediatelyDominates(&C, &E));
        assert(!dom.dominates(&E, &C));
        assert(!dom.dominates(&E, &D));

        DominanceFrontier f(&MockBB::code, dom);
        assert(f.at(&A).empty());
        assert(f.at(&B) == DominanceFrontier::BBList({&D}));
        assert(f.at(&C) == DominanceFrontier::BBList({&C, &D}));
        assert(f.at(&E) == DominanceFrontier::BBList({&C}));
        assert(f.at(&D) == DominanceFrontier::BBList({}));
    }

    {
        /*
         * Dominance algorithm requires the start node to have no preds.
         *
         *      S
         *      |
         *  .-> A <--.
         *  |  / \   |
         *  | /   |  |
         *  B <-- C  |
         *   \       |
         *     D ----‘
         */

        MockBB::reset();
        MockBB S, A, B, C, D;
        S.setNext(&A);
        A.setBranch(&B, &C);
        B.setBranch(&A, &D);
        C.setNext(&B);
        D.setNext(&A);

        CFG cfg(&MockBB::code);
        assert(cfg.isPredecessor(&S, &A));
        assert(cfg.isPredecessor(&A, &B));
        assert(cfg.isPredecessor(&B, &A));
        assert(cfg.isPredecessor(&D, &C));
        assert(cfg.isPredecessor(&C, &A));

        DominanceGraph dom(&MockBB::code);
        assert(dom.dominates(&S, &S));
        assert(dom.dominates(&A, &A));
        assert(dom.dominates(&B, &B));
        assert(dom.dominates(&C, &C));
        assert(dom.dominates(&D, &D));

        assert(!dom.strictlyDominates(&S, &S));
        assert(!dom.strictlyDominates(&A, &A));
        assert(!dom.strictlyDominates(&B, &B));
        assert(!dom.strictlyDominates(&C, &C));
        assert(!dom.strictlyDominates(&D, &D));

        assert(dom.dominates(&S, &A));
        assert(dom.dominates(&A, &B));
        assert(dom.dominates(&A, &C));
        assert(dom.dominates(&A, &D));
        assert(dom.dominates(&B, &D));
        assert(!dom.dominates(&B, &A));
        assert(!dom.dominates(&C, &A));
        assert(!dom.dominates(&D, &A));
        assert(!dom.dominates(&C, &D));

        assert(dom.strictlyDominates(&S, &A));
        assert(dom.strictlyDominates(&A, &B));
        assert(dom.strictlyDominates(&A, &C));
        assert(dom.strictlyDominates(&A, &D));
        assert(dom.strictlyDominates(&B, &D));

        assert(dom.immediatelyDominates(&S, &A));
        assert(dom.immediatelyDominates(&A, &B));
        assert(dom.immediatelyDominates(&A, &C));
        assert(dom.immediatelyDominates(&B, &D));
        assert(!dom.immediatelyDominates(&A, &D));

        assert(dom.dominators(&B) == DominanceGraph::BBSet({&S, &A, &B}));

        DominanceFrontier f(&MockBB::code, dom);
        assert(f.at(&S).empty());
        assert(f.at(&A) == DominanceFrontier::BBList({&A}));
        assert(f.at(&B) == DominanceFrontier::BBList({&A}));
        assert(f.at(&C) == DominanceFrontier::BBList({&B}));
        assert(f.at(&D) == DominanceFrontier::BBList({&A}));
    }

    {
        /*
         * Example taken from:
         * Appel, A. and Palsberg, J. Modern Compiler Implementation in Java,
         * 2nd ed. Figure 19.8 (a), pg 411.
         *
         * Note that our algorithm traverses child nodes from right-to-left.
         * To match the example from the book, we construct the CFG so that,
         * e.g., A's first child is C and its second child is B. This ensures
         * we visit B before C.
         *
         *              A
         *            /   \
         *           /     \
         *     .--> B --+   \
         *    /     |   | +-> C --+
         *   /      D   | |   |   |
         *  |     /   \ /  \- E   |
         *  |    F     G       \ /
         *  |  /   \    \       H
         *  | /  .--+--- J      |
         *  | |/    |          /
         *  | I     K     +---+
         *  |  \   /     /
         *  +--- L      /
         *         \   /
         *          \ /
         *           M
         */

        MockBB::reset();
        MockBB A, B, C, D, E, F, G, H, I, J, K, L, M;
        A.setBranch(&C, &B);
        B.setBranch(&G, &D);
        C.setBranch(&H, &E);
        D.setBranch(&G, &F);
        E.setBranch(&H, &C);
        F.setBranch(&K, &I);
        G.setNext(&J);
        H.setNext(&M);
        I.setNext(&L);
        J.setNext(&I);
        K.setNext(&L);
        L.setBranch(&M, &B);

        CFG cfg(&MockBB::code);
        assert(cfg.isPredecessor(&A, &B));
        assert(cfg.isPredecessor(&A, &C));
        assert(cfg.isPredecessor(&B, &J));
        assert(cfg.isPredecessor(&B, &L));
        assert(cfg.isPredecessor(&B, &M));
        assert(cfg.isPredecessor(&C, &H));
        assert(cfg.isPredecessor(&C, &M));
        assert(cfg.isPredecessor(&D, &B));
        assert(cfg.isPredecessor(&E, &C));
        assert(cfg.isPredecessor(&F, &L));
        assert(cfg.isPredecessor(&F, &G));
        assert(cfg.isPredecessor(&G, &F));
        assert(cfg.isPredecessor(&H, &M));
        assert(cfg.isPredecessor(&I, &B));
        assert(cfg.isPredecessor(&I, &J));
        assert(cfg.isPredecessor(&J, &B));
        assert(cfg.isPredecessor(&J, &F));
        assert(cfg.isPredecessor(&J, &G));
        assert(cfg.isPredecessor(&K, &B));
        assert(cfg.isPredecessor(&K, &F));
        assert(cfg.isPredecessor(&K, &G));
        assert(cfg.isPredecessor(&L, &M));

        /*
         * Dominator tree
         *
         *           A
         *        /  |  \
         *       B   M   C
         *     // \\     | \
         *    / / \ \    E  H
         *   D  G I  L
         *   |  |
         *   F  J
         *   |
         *   K
         *
         */
        DominanceGraph dom(&MockBB::code);
        assert(dom.immediatelyDominates(&A, &B));
        assert(dom.immediatelyDominates(&A, &M));
        assert(dom.immediatelyDominates(&A, &C));
        assert(dom.immediatelyDominates(&C, &E));
        assert(dom.immediatelyDominates(&C, &H));
        assert(dom.immediatelyDominates(&B, &D));
        assert(dom.immediatelyDominates(&B, &G));
        assert(dom.immediatelyDominates(&B, &I));
        assert(dom.immediatelyDominates(&B, &L));
        assert(dom.immediatelyDominates(&D, &F));
        assert(dom.immediatelyDominates(&F, &K));
        assert(dom.immediatelyDominates(&G, &J));

        assert(dom.dominates(&A, &B));
        assert(dom.dominates(&A, &M));
        assert(dom.dominates(&A, &C));
        assert(dom.dominates(&C, &E));
        assert(dom.dominates(&C, &H));
        assert(dom.dominates(&B, &D));
        assert(dom.dominates(&B, &G));
        assert(dom.dominates(&B, &I));
        assert(dom.dominates(&B, &L));
        assert(dom.dominates(&D, &F));
        assert(dom.dominates(&F, &K));
        assert(dom.dominates(&G, &J));

        assert(dom.dominates(&A, &E));
        assert(dom.dominates(&A, &H));
        assert(dom.dominates(&A, &D));
        assert(dom.dominates(&A, &G));
        assert(dom.dominates(&A, &I));
        assert(dom.dominates(&A, &L));
        assert(dom.dominates(&A, &F));
        assert(dom.dominates(&A, &J));
        assert(dom.dominates(&A, &K));
        assert(dom.dominates(&B, &F));
        assert(dom.dominates(&B, &J));
        assert(dom.dominates(&B, &K));

        assert(!dom.dominates(&B, &A));
        assert(!dom.dominates(&B, &M));
        assert(!dom.dominates(&B, &C));
        assert(!dom.dominates(&B, &E));
        assert(!dom.dominates(&B, &H));
        assert(!dom.dominates(&M, &A));
        assert(!dom.dominates(&M, &B));
        assert(!dom.dominates(&M, &C));
        assert(!dom.dominates(&M, &D));
        assert(!dom.dominates(&M, &E));
        assert(!dom.dominates(&C, &M));
        assert(!dom.dominates(&C, &I));
        assert(!dom.dominates(&D, &C));
        assert(!dom.dominates(&D, &G));
    }

    {
        /*
         *    A
         *   / \
         *  B   C
         *   \ /
         *    D
         */
        MockBB::reset();
        MockBB A, B, C, D;
        A.setBranch(&B, &C);
        B.setNext(&D);
        C.setNext(&D);

        std::deque<BB*> expected = {&A, &B, &C, &D};
        BreadthFirstVisitor::run(&A, [&](BB* bb) {
            assert(expected.front() == bb);
            expected.pop_front();
        });
    }

    {
        /*
         *    A
         *   / \
         *  B   C
         *   \ /
         *    D
         */
        MockBB::reset();
        MockBB A, B, C, D;
        A.setBranch(&B, &C);
        B.setNext(&D);
        C.setNext(&D);

        DominanceGraph dom(&MockBB::code);
        std::deque<BB*> expected = {&A, &B, &C, &D};
        DominatorTreeVisitor<>(dom).run(&A, [&](BB* bb) {
            assert(expected.front() == bb);
            expected.pop_front();
        });
        expected = {&A, &B, &C, &D};
        DominatorTreeVisitor<VisitorHelpers::PointerMarker>(dom).run(
            &A, [&](BB* bb) {
                assert(expected.front() == bb);
                expected.pop_front();
            });
    }

    {
        /*
         *    A
         *   / \
         *  B   E
         *  |   |
         *  C   |
         *   \ /
         *    D
         */
        MockBB::reset();
        MockBB A, B, C, D, E;
        A.setBranch(&B, &E);
        B.setNext(&C);
        C.setNext(&D);
        E.setNext(&D);

        DominanceGraph dom(&MockBB::code);
        std::deque<BB*> expected = {&A, &B, &C, &E, &D};
        DominatorTreeVisitor<>(dom).run(&A, [&](BB* bb) {
            assert(expected.front() == bb);
            expected.pop_front();
        });
        expected = {&A, &B, &C, &E, &D};
        DominatorTreeVisitor<VisitorHelpers::PointerMarker>(dom).run(
            &A, [&](BB* bb) {
                assert(expected.front() == bb);
                expected.pop_front();
            });
    }

    {
        /*
         *    A
         *   / \
         *  B   D
         *  |   |
         *  C   E
         *   \ /
         *    F
         */
        MockBB::reset();
        MockBB A, B, C, D, E, F;
        A.setBranch(&B, &D);
        B.setNext(&C);
        C.setNext(&F);
        D.setNext(&E);
        E.setNext(&F);

        DominanceGraph dom(&MockBB::code);
        std::deque<BB*> expected = {&A, &B, &C, &D, &E, &F};
        DominatorTreeVisitor<>(dom).run(&A, [&](BB* bb) {
            assert(expected.front() == bb);
            expected.pop_front();
        });
        expected = {&A, &B, &C, &D, &E, &F};
        DominatorTreeVisitor<VisitorHelpers::PointerMarker>(dom).run(
            &A, [&](BB* bb) {
                assert(expected.front() == bb);
                expected.pop_front();
            });
    }

    return true;
}

bool testTypeRules() {
    PirType r = RType::vec;
    PirType r2 = RType::logical;
    assert(r.subsetType(PirType::any()).isA(RType::vec));
    assert(!r.subsetType(PirType::any()).maybeObj());
    assert(!r.orObject().subsetType(PirType::bottom()).isA(RType::vec));
    assert(!r.orObject()
                .subsetType(PirType::bottom())
                .isA(PirType::val().noAttribsOrObject()));
    assert(r.extractType(PirType::any()).isA(PirType::val()));
    assert(!r.extractType(PirType::bottom()).isScalar());
    assert(!r.extractType(PirType::bottom()).maybeMissing());
    assert(!r.extractType(PirType::bottom()).isA(RType::vec));
    assert(r.orObject().extractType(PirType::simpleScalarInt()).maybeMissing());
    assert(r2.subsetType(RType::real).isA(RType::logical));
    assert(!r2.subsetType(RType::integer).isScalar());
    assert(!r2.scalar().subsetType(RType::integer).isScalar());
    assert(!r2.subsetType(PirType(RType::integer).scalar()).isScalar());
    assert(r2.extractType(RType::integer).isScalar());
    assert(!r2.subsetType(PirType::any()).maybeObj());
    assert(!PirType::val().simpleScalar().maybeHasAttrs());
    assert(PirType::val().scalar().maybeHasAttrs());
    auto t = PirType(RType::logical).notObject().notMissing().scalar();
    assert(t.mergeWithConversion(t).isA(t));

    auto a = (PirType() | RType::real | RType::integer)
                 .notPromiseWrapped()
                 .notObject()
                 .noAttribsOrObject();
    auto b = (PirType() | RType::integer)
                 .notPromiseWrapped()
                 .notObject()
                 .noAttribsOrObject();
    assert(a.mergeWithConversion(b) == a);
    assert(b.mergeWithConversion(a) == a);
    t = PirType::bottom();
    t = t.mergeWithConversion(a);
    t = t.mergeWithConversion(b);
    assert(t == a);
    t = t & PirType::num().notMissing();
    assert(t == a);
    t = PirType::any() & t;
    assert(t == a);
    auto real = PirType(RType::real).orAttribsOrObj().notObject();
    auto realScalar = PirType::simpleScalarReal();
    assert(real.maybeHasAttrs());
    assert(!realScalar.maybeHasAttrs());
    assert(real.mergeWithConversion(realScalar).maybeHasAttrs());
    assert(!PirType::val().isA(PirType::function()));
    assert(!PirType::simpleScalarInt()
                .extractType(PirType(RType::real).noAttribsOrObject())
                .isVoid());
    return true;
}

static Test tests[] = {
    Test("test cfg", &testCfg),
    Test("test_42L", []() { return test42("42L"); }),
    Test("test_inline", []() { return test42("{f <- function() 42L; f()}"); }),
    Test("test_inline_two",
         []() {
             return test42("{f <- function(val) (function(x) x)(val); f(42L)}");
         }),
    Test("test_inline_arg",
         []() { return test42("{f <- function(x) x; f(42L)}"); }),
    Test("test_assign",
         []() {
             return test42("{y<-42L; t<-FALSE; if (t) x<-y else x<-y; x}");
         }),
    Test(
        "test_super_assign",
        []() { return test42("{x <- 0; f <- function() x <<- 42L; f(); x}"); }),
    Test("return_cls",
         []() { return compileAndVerify("f <- function() 42L"); }),
    Test("index", []() { return compileAndVerify("f <- function(x) x[[2]]"); }),
    Test("return_cls",
         []() { return compileAndVerify("f <- function() {function() 42L}"); }),
    Test("deopt_in_prom",
         []() {
             return compileAndVerify(
                 "fun <- function(a) {f <- function(x) x; f(a[[1]])}");
         }),
    Test("context_load",
         []() { return canRemoveEnvironment("f <- function() 123"); }),
    Test("super_assign", &testSuperAssign),
    Test("loop",
         []() {
             return compileAndVerify(
                 "f <- function(x) {while (x < 10) if (x) x <- x + 1}");
         }),
    Test("static_call",
         []() {
             return compileAndVerify("f <- function(x) x",
                                     "g <- function() f(1); g()");
         }),
    Test("merge_missing_bl",
         []() {
             return !hasLoadVar(
                 "theFun <- function() {a<-FALSE; if (a) {q <-1} else "
                 "{if (a) q <- 3 else q <- 2}; q}");
         }),
    Test("merge_missing",
         []() {
             return hasLoadVar(
                 "theFun <- function(a, b) {if (a) {q <-1} else {if "
                 "(b) 3 else q <- 2}; q}");
         }),
    Test("PIR to RIR: basic",
         []() { return testPir2Rir("foo", "function() 42L", ""); }),
    Test("PIR to RIR: simple argument",
         []() { return testPir2Rir("foo", "function(x) x", "16L"); }),
    /*
        fails because RIR to PIR doesn't support default args for now

        Test("PIR to RIR: default arg",
             []() { return testPir2Rir("foo", "function(x = 3) x", ""); }),
    */
    Test("PIR to RIR: local binding",
         []() {
             return testPir2Rir("foo",
                                "function(dummy, a) { x <- 3; x + a + x + a }",
                                "cat('WHOA\n'), 1");
         }),
    Test("PIR to RIR: if else",
         []() {
             return testPir2Rir("foo", "function(x) if (x) 1 else 2", "TRUE");
         }),
    Test("PIR to RIR: if",
         []() { return testPir2Rir("foo", "function(x) if (x) 1", "F"); }),
    Test("PIR to RIR: simple loop",
         []() {
             return testPir2Rir("foo", "function(x) while (TRUE) if (x) break",
                                "T");
         }),
    Test("PIR to RIR: loop - sum",
         []() {
             return testPir2Rir("foo",
                                "function(x) {\n"
                                "  sum <- 0\n"
                                "  while (x > 0) {\n"
                                "    sum <- sum + x\n"
                                "    x <- x - 1\n"
                                "  }\n"
                                "  sum\n"
                                "}",
                                "10");
         }),
    Test("PIR to RIR: loop with break and next",
         []() {
             return testPir2Rir("foo",
                                "f <- function(x, y) {\n"
                                "    s <- 0L\n"
                                "    repeat {\n"
                                "        if (x > y)\n"
                                "            break\n"
                                "        if (x %% 2L == 1L) {\n"
                                "            x <- x + 1L\n"
                                "        } else {\n"
                                "            x <- x + 1L\n"
                                "            y <- y - 1L\n"
                                "            next\n"
                                "        }\n"
                                "        s <- s + x\n"
                                "    }\n"
                                "    s\n"
                                "}",
                                "1L, 10L");
         }),
    Test("PIR to RIR: simple for loop",
         []() {
             return testPir2Rir(
                 "foo", "function(x) { s = 0; for (i in 1:x) s = s + i; s }",
                 "10L");
         }),
    Test("PIR to RIR: inlined call",
         []() {
             return testPir2Rir("foo",
                                "function(x) {"
                                "  bar <- function(a) a + 1;"
                                "  bar(x)"
                                "}",
                                "1");
         }),
    Test("PIR to RIR: call safe builtin",
         []() {
             return testPir2Rir("foo", "function(x) vector('integer', x)", "4");
         }),
    Test("PIR to RIR: call builtin",
         []() {
             return testPir2Rir("foo",
                                "function(x) c(1, 2, 3, x, x + 1, x + 2)", "4");
         }),
    Test("PIR to RIR: call .Internal",
         []() {
             return testPir2Rir("foo", "function() .Internal(formals(bar))",
                                "");
         }),
    Test("PIR to RIR: call",
         []() { return testPir2Rir("foo", "function(x) bar(x)", "2"); }),
    Test("PIR to RIR: call twice",
         []() {
             return testPir2Rir("foo", "function(x) { bar(x); bar(x + 1) }",
                                "2");
         }),
    Test("PIR to RIR: with env",
         []() {
             return testPir2Rir("foo",
                                "function(x) {\n"
                                "  sum <- 0\n"
                                "  while (x > 0) {\n"
                                "    sum <- sum + x\n"
                                "    x <- x - 1\n"
                                "    bar(sum)\n"
                                "  }\n"
                                "  sum\n"
                                "}",
                                "4");
         }),
    Test(
        "Elide ldfun through promise",
        []() { return test42("{f <- function() 42L; (function(x) x())(f)}"); }),
    Test("Constantfolding1", []() { return test42("{if (1<2) 42L}"); }),
    Test("Constantfolding2",
         []() {
             return test42("{a<- 41L; b<- 1L; f <- function(x,y) x+y; f(a,b)}");
         }),
    Test("Test dead store analysis", &testDeadStore),
    Test("Test type rules", &testTypeRules)};

} // namespace

namespace rir {

void PirTests::run() {
    size_t oldconfig = pir::Parameter::MAX_INPUT_SIZE;
    pir::Parameter::MAX_INPUT_SIZE = 3500;
    for (auto t : tests) {
        std::cout << "> " << t.first << "\n";
        if (!t.second()) {
            std::cout << "failed\n";
            exit(1);
        }
    }
    pir::Parameter::MAX_INPUT_SIZE = oldconfig;
}

} // namespace rir
