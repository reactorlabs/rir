#include "pir_tests.h"
#include "R/Protect.h"
#include "R_ext/Parse.h"
#include "analysis/query.h"
#include "analysis/verifier.h"
#include "ir/Compiler.h"
#include "pir/pir_impl.h"
#include "pir_compiler.h"
#include "util/visitor.h"

namespace {
using namespace rir;

pir::Module* compile(const std::string& inp) {
    Protect p;
    ParseStatus status;
    SEXP arg = p(CONS(R_NilValue, R_NilValue));
    SET_TAG(arg, Rf_install("arg1"));
    SEXP str = p(Rf_mkString(inp.c_str()));
    SEXP bdy = p(R_ParseVector(str, -1, &status, R_NilValue));
    SEXP fun = p(Compiler::compileClosure(CDR(bdy), arg));
    PirCompiler cmp;
    return cmp.compileFunction(fun);
}

using namespace rir::pir;
typedef std::function<bool(void)> TestFunction;
typedef std::pair<std::string, TestFunction> Test;

#define CHECK(___test___)                                                      \
    if (!(___test___)) {                                                       \
        m->print(std::cerr);                                                   \
        std::cerr << "'" << #___test___ << "' failed\n";                       \
        assert(false);                                                         \
        return false;                                                          \
    }

bool test42(const std::string& input) {
    auto m = compile(input);
    auto f = m->functions.front();

    CHECK(Query::noEnv(f));

    auto r = Query::returned(f);
    CHECK(r.size() == 1);

    auto ld = LdConst::Cast((*r.begin()));
    CHECK(ld);
    CHECK(TYPEOF(ld->c) == INTSXP);
    CHECK(INTEGER(ld->c)[0] == 42);

    delete m;
    return true;
};

class NullBuffer : public std::ostream {
  public:
    int overflow(int c) { return c; }
};

bool verify(Module* m) {
    for (auto f : m->functions)
        if (!Verify::apply(f))
            return false;
    // TODO: find fix for osx
    // NullBuffer nb;
    for (auto f : m->functions)
        f->print(std::cout);

    return true;
}

bool compileAndVerify(const std::string& input) {
    auto m = compile(input);
    bool t = verify(m);
    delete m;
    return t;
}

bool testDelayEnv() {
    // TODO: counterexample: closure creates circular dependency, need more
    //       analysis!
    // auto m = compile("{f <- function()1; arg1[[2]]}");

    auto m = compile("{f <- arg1; arg1[[2]]}");
    bool t = Visitor::check(m->functions.front()->entry, [&](BB* bb) {
        for (auto i : *bb) {
            if (i->hasEnv()) {
                CHECK(Deopt::Cast(bb->last()));
            }
        }
        return true;
    });
    delete m;
    return t;
}

static Test tests[] = {
    Test("test_42L", []() { return test42("42L"); }),
    Test("test_inline", []() { return test42("{f <- function() 42L; f()}"); }),
    Test("test_inline_arg",
         []() { return test42("{f <- function(x) x; f(42L)}"); }),
    Test("test_assign",
         []() { return test42("{y<-42L; if (arg1) x<-y else x<-y; x}"); }),
    Test(
        "test_super_assign",
        []() { return test42("{x <- 0; f <- function() x <<- 42L; f(); x}"); }),
    Test("return_cls", []() { return compileAndVerify("function() 42L"); }),
    Test("index", []() { return compileAndVerify("arg1[[2]]"); }),
    Test("delay_env", &testDelayEnv),
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
