#include "pir_tests.h"
#include "R/Protect.h"
#include "R/RList.h"
#include "R_ext/Parse.h"
#include "analysis/query.h"
#include "analysis/verifier.h"
#include "api.h"
#include "ir/Compiler.h"
#include "pir/pir_impl.h"
#include "translations/pir_2_rir.h"
#include "translations/rir_2_pir/rir_2_pir.h"
#include "util/visitor.h"
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
    return env;
}
typedef std::unordered_map<std::string, pir::Closure*> closuresByName;

closuresByName compileRir2Pir(SEXP env, pir::Module* m) {
    pir::StreamLogger logger(pir::DebugOptions() |
                             pir::DebugFlag::PrintFinalPir);
    pir::Rir2PirCompiler cmp(m, logger);

    // Compile every function in the environment
    closuresByName results;
    auto envlist = RList(FRAME(env));
    for (auto f = envlist.begin(); f != envlist.end(); ++f) {
        auto fun = *f;
        if (TYPEOF(fun) == CLOSXP) {
            assert(isValidClosureSEXP(fun));
            cmp.compileClosure(fun, "test_function",
                               [&](pir::Closure* cls) {
                                   results[CHAR(PRINTNAME(f.tag()))] = cls;
                               },
                               []() { assert(false); });
        }
    }

    cmp.optimizeModule();
    return results;
}

closuresByName compile(const std::string& context, const std::string& expr,
                       pir::Module* m, SEXP super = R_GlobalEnv) {
    SEXP env = compileToRir(context, expr, super);
    return compileRir2Pir(env, m);
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

void insertTypeFeedbackForBinops(rir::Function* srcFunction,
                                 std::array<SEXP, 2> typeFeedback) {
    for (auto function : *srcFunction) {
        Opcode* end = function->endCode();
        Opcode* finger = function->code();
        while (finger != end) {
            Opcode* prev = finger;
            BC bc = BC::advance(&finger, function);
            if (bc.bc == Opcode::record_binop_) {
                prev++;
                TypeFeedback* feedback = (TypeFeedback*)prev;
                feedback[0].record(typeFeedback[0]);
                feedback[1].record(typeFeedback[1]);
            }
        }
    }
}

extern "C" SEXP Rf_NewEnvironment(SEXP, SEXP, SEXP);
SEXP parseCompileToRir(std::string);

bool canRemoveEnvironmentIfTypeFeedback(const std::string& input) {
    Protect p;
    std::array<SEXP, 2> types;

    // Set type feedback to primitive int vectors
    SEXP res = p(Rf_allocVector(INTSXP, 1));
    INTEGER(res)[0] = 1;
    types[0] = res;
    types[1] = res;

    // Parser and compile the input
    auto execEnv = p(Rf_NewEnvironment(R_NilValue, R_NilValue, R_GlobalEnv));
    auto rirFun = p(parseCompileToRir(input));
    SET_CLOENV(rirFun, execEnv);
    Rf_defineVar(Rf_install("removeEnvInBinopTest"), rirFun, execEnv);
    rir::Function* srcFunction = isValidClosureSEXP(rirFun);
    assert(srcFunction != nullptr);
    insertTypeFeedbackForBinops(srcFunction, types);

    // Test if there is an environment in the resulted function
    pir::Module m;
    compileRir2Pir(execEnv, &m);
    bool t = verify(&m);
    m.eachPirFunction(
        [&t](pir::Closure* f) { t = t && Query::envOnlyBeforeDeopt(f); });
    return t;
}

bool canRemoveEnvironment(const std::string& input) {
    pir::Module m;
    compile("", input, &m);
    bool t = verify(&m);
    m.eachPirFunction([&t](pir::Closure* f) { t = t && Query::noEnv(f); });
    return t;
}

bool canRemoveEnvironmentIfNonTypeFeedback(const std::string& input) {
    pir::Module m;
    compile("", input, &m);
    bool t = verify(&m);
    m.eachPirFunction([&t](pir::Closure* f) {
        t = t && (Query::noEnv(f) || Query::envOnlyBeforeDeopt(f));
    });
    return t;
}

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
        // This super assign cannot be removed, since the super env is unknown
        auto res = compile("", "f <- function() {(function() a <<- 1)()}", &m);
        auto f = res["f"];
        CHECK(hasSuperAssign(f));
    }
    {
        pir::Module m;
        // This super assign can be removed, since the super env is not tainted
        auto res = compile(
            "", "f <- function() {a <- 1; (function() {asdf(); a <<- 1})()}",
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

    Compiler::compileClosure(cls);

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

    pirCompile(rirFun, "from_testPir2Rir", rir::pir::DebugOptions());

    auto after = p(Rf_eval(rCall, execEnv));
    if (verbose) {
        Rprintf("after = %p\n", after);
        Rf_PrintValue(after);
    }

    return checkPir2Rir(orig, after);
}

static Test tests[] = {
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
    Test("binop_nonobjects",
         []() {
             return canRemoveEnvironmentIfTypeFeedback("f <- function() 1 + 2");
         }),
    Test("binop_nonobjects_nofeedback",
         []() {
             return !canRemoveEnvironmentIfNonTypeFeedback(
                 "f <- function() 1 + 2");
         }),
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
             return hasLoadVar("theFun <- function(a) {if (a) {q <-1} else {if "
                               "(a) 3 else q <- 2}; q}");
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
};
} // namespace

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
} // namespace rir
