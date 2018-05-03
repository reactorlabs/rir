#include "pir_tests.h"
#include "R/Protect.h"
#include "R/RList.h"
#include "R_ext/Parse.h"
#include "analysis/query.h"
#include "analysis/verifier.h"
#include "ir/Compiler.h"
#include "pir/pir_impl.h"
#include "translations/rir_2_pir/rir_2_pir.h"
#include "util/visitor.h"

namespace {
using namespace rir;

std::unordered_map<std::string, pir::Closure*>
compile(const std::string& context, const std::string& expr, pir::Module* m,
        SEXP super = R_GlobalEnv) {
    Protect p;

    auto eval = [&p](const std::string& expr, SEXP env) {
        ParseStatus status;
        // Parse expression
        SEXP str = p(Rf_mkString(("{" + expr + "}").c_str()));
        SEXP e = p(R_ParseVector(str, -1, &status, R_NilValue));

        // Compile expression to rir
        SEXP rirexp =
            p(Compiler::compileFunction(VECTOR_ELT(e, 0), R_NilValue));

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

    pir::Rir2PirCompiler cmp(m);

    // Compile every function the expression created
    std::unordered_map<std::string, pir::Closure*> results;
    auto envlist = RList(FRAME(env));
    for (auto f = envlist.begin(); f != envlist.end(); ++f) {
        auto fun = *f;
        if (TYPEOF(fun) == CLOSXP) {
            assert(isValidClosureSEXP(fun));
            // isValidClosureSEXP(fun)->body()->print();
            results[CHAR(PRINTNAME(f.tag()))] = cmp.compileClosure(fun);
        }
    }

    // cmp.setVerbose(true);
    cmp.optimizeModule();
    return results;
}

using namespace rir::pir;
typedef std::function<bool(void)> TestClosure;
typedef std::pair<std::string, TestClosure> Test;

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

    auto ld = LdConst::Cast((*r.begin()));
    CHECK(ld);
    CHECK(TYPEOF(ld->c) == INTSXP);
    CHECK(INTEGER(ld->c)[0] == 42);
    return true;
};

bool hasLoadVar(const std::string& input) {
    pir::Module m;
    auto res = compile("", input, &m);
    auto f = res["theFun"];
    bool noLdVar = Visitor::check(
        f->entry, [&](Instruction* i) { return !LdVar::Cast(i); });
    return !noLdVar;
};

class NullBuffer : public std::ostream, std::streambuf {
  public:
    NullBuffer() : std::ostream(this) {}
    int overflow(int c) {
        return (c == std::ostream::traits_type::eof()) ? '\0' : c;
    }
};

bool verify(Module* m) {
    bool success = true;
    m->eachPirFunction([&success](pir::Module::VersionedClosure& f) {
        f.eachVersion([&success](pir::Closure* f) {
            if (!Verify::apply(f))
                success = false;
        });
    });
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

bool canRemoveEnvironment(const std::string& input) {
    pir::Module m;
    compile("", input, &m);
    bool t = verify(&m);
    m.eachPirFunction([&t](pir::Closure* f) { t = t && Query::noEnv(f); });
    return t;
}

bool testDelayEnv() {
    // TODO: counterexample: closure creates circular dependency, need more
    //       analysis!
    // auto m = compile("{f <- function()1; arg1[[2]]}");

    pir::Module m;
    auto res = compile("", "a <- function(b) {f <- b; b[[2]]}", &m);
    bool t = Visitor::check(res["a"]->entry, [&m](Instruction* i, BB* bb) {
        if (i->hasEnv())
            CHECK(Deopt::Cast(bb->last()));
        return true;
    });
    return t;
}

extern "C" SEXP Rf_NewEnvironment(SEXP, SEXP, SEXP);
bool testSuperAssign() {
    auto hasAssign = [](pir::Closure* f) {
        return !Visitor::check(f->entry, [](Instruction* i) {
            if (StVar::Cast(i))
                return false;
            return true;
        });
    };
    auto hasSuperAssign = [](pir::Closure* f) {
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
        // This super assign can be converted into a store to the global env
        auto res = compile("", "f <- function() {(function() a <<- 1)()}", &m);
        auto f = res["f"];
        CHECK(!hasSuperAssign(f));
        CHECK(hasAssign(f));
    }
    {
        pir::Module m;
        // This super assign cannot be removed, since the super env is not
        // unknown.
        auto res = compile(
            "", "f <- function() {f <- (function() {qq(); a <<- 1})()}", &m);
        auto f = res["f"];
        CHECK(hasSuperAssign(f));
    }

    return true;
}

static Test tests[] = {
    Test("test_42L", []() { return test42("42L"); }),
    Test("test_inline", []() { return test42("{f <- function() 42L; f()}"); }),
    Test("test_inline_two",
         []() {
             return test42(
                 "{f <- function(val, fun) fun(val); f(42L, function(x)x)}");
         }),
    Test("test_inline_arg",
         []() { return test42("{f <- function(x) x; f(42L)}"); }),
    Test("test_assign",
         []() { return test42("{y<-42L; if (arg1) x<-y else x<-y; x}"); }),
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
    Test("delay_env", &testDelayEnv),
    Test("context_load",
         []() { return canRemoveEnvironment("f <- function() a"); }),
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
             return !hasLoadVar("theFun <- function(a) {if (a) {q <-1} else "
                                "{if (a) q <- 3 else q <- 2}; q}");
         }),
    Test("merge_missing",
         []() {
             return hasLoadVar("theFun <- function(a) {if (a) {q <-1} else {if "
                               "(a) 3 else q <- 2}; q}");
         }),
};
}

namespace rir {

void PirTests::run() {
    for (auto t : tests) {
        std::cout << "> " << t.first << "\n";
        if (!t.second()) {
            std::cout << "failed\n";
            exit(1);
        }
    }
}
}
