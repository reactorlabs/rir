/** Enables the use of R internals for us so that we can manipulate R structures
 * in low level.
 */

#include "api.h"
#include "R/Serialize.h"
#include "Rinternals.h"
#include "bc/BC.h"
#include "bc/Compiler.h"
#include "compiler/backend.h"
#include "compiler/compiler.h"
#include "compiler/log/debug.h"
#include "compiler/parameter.h"
#include "compiler/pir/closure.h"
#include "compiler/pir/type.h"
#include "compiler/test/PirCheck.h"
#include "compiler/test/PirTests.h"
#include "interpreter/interp_incl.h"
#include "recording_hooks.h"
#include "runtime/DispatchTable.h"
#include "utils/measuring.h"

#include <cassert>
#include <cstdio>
#include <list>
#include <memory>
#include <string>

#include <fstream>
#include <iostream>
#include <numeric>

extern "C" SEXP R_GetVarLocValue(R_varloc_t);

using namespace rir;

extern "C" Rboolean R_Visible;

int R_ENABLE_JIT = getenv("R_ENABLE_JIT") ? atoi(getenv("R_ENABLE_JIT")) : 3;

static size_t oldMaxInput = 0;
static size_t oldInlinerMax = 0;
static bool oldPreserve = false;
static unsigned oldSerializeChaos = false;
static size_t oldDeoptChaos = false;

bool parseDebugStyle(const char* str, pir::DebugStyle& s) {
#define V(style)                                                               \
    if (strcmp(str, #style) == 0) {                                            \
        s = pir::DebugStyle::style;                                            \
        return true;                                                           \
    } else
    LIST_OF_DEBUG_STYLES(V)
#undef V
    {
        return false;
    }
}

REXPORT SEXP rirDisassemble(SEXP what, SEXP verbose) {
    if (!what || TYPEOF(what) != CLOSXP)
        Rf_error("Not a rir compiled code (Not CLOSXP)");
    DispatchTable* t = DispatchTable::check(BODY(what));

    if (!t)
        Rf_error("Not a rir compiled code (CLOSXP but not DispatchTable)");

    std::cout << "== closure " << what << " (env " << CLOENV(what) << ") ==\n";

    t->print(std::cout, Rf_asLogical(verbose));

    return R_NilValue;
}

bool finalizerSet = false;
SEXP DTsSymbol = Rf_install("__DTs");

void showPercent(double percent, std::ostream& ss) {
    ss << percent * 100 << "%";
}

struct Stat {
    std::string name;
    size_t value = 0;

    void operator++(int) { value++; }
    void operator+=(size_t add) { value += add; }
    void operator+=(const Stat& other) { value += other.value; }
};

std::ostream& operator<<(std::ostream& os, const Stat& st) {
    os << st.value;
    return os;
}

struct MetricPercent {
    Stat* numerator;
    Stat* denominator;
    std::string name = "";

    double value() const {
        if (denominator->value == 0) {
            assert(false && "cannot divide by 0");
            // return 0;
        }

        return static_cast<double>(numerator->value) / denominator->value;
    }

    MetricPercent& named(const std::string& name) {
        this->name = name;
        return *this;
    }
};

MetricPercent operator/(Stat& lhs, Stat& rhs) {
    return MetricPercent{.numerator = &lhs, .denominator = &rhs};
}

std::ostream& operator<<(std::ostream& os, const MetricPercent& metric) {
    if (metric.name != "") {
        os << metric.name << " ";
    }

    os << "(" << metric.numerator->name << " / " << metric.denominator->name
       << "): " << metric.numerator->value << " / " << metric.denominator->value
       << " (";

    if (metric.denominator->value) {
        showPercent(metric.value(), os);
    } else {
        os << "-";
    }

    os << ")\n";

    return os;
};

struct FunctionAggregate {
    std::string name;
    std::vector<double> values{};

    void add(double value) { values.push_back(value); }
    void add(const MetricPercent& metric) { values.push_back(metric.value()); }

    double average() const {
        if (values.empty()) {
            assert(false && "empty aggregate");
            return 0.0;
        }

        double sum = std::accumulate(values.begin(), values.end(), 0.0);
        return sum / values.size();
    }
};

std::ostream& operator<<(std::ostream& os, const FunctionAggregate& agg) {
    if (agg.values.empty()) {
        return os;
    }

    os << agg.name << " (on average of " << agg.values.size() << " function): ";

    showPercent(agg.average(), os);

    os << "\n";

    return os;
}

void myFinalizer(SEXP) {
    std::ofstream null_stream("/dev/null");
    std::ostream& defaultOutput = std::cerr;

    std::ostream& outputInFunction = defaultOutput;

    SEXP DTs = Rf_findVar(DTsSymbol, R_GlobalEnv);

    Stat totalSlots{"total"};
    Stat referencedSlots{"referenced"};
    Stat readSlots{"read"};
    Stat readNonEmptySlots{"read non-empty"};
    Stat usedSlots{"used"};

    Stat usedSlotsOfKindType{"used (kind type)"};
    Stat exactMatchUsedSlots{"exact match"};
    Stat widenedUsedSlots{"widened"};
    Stat narrowedSlots{"narrowed"};

    Stat compiledFunctions{"compiled functions"};
    Stat functionsUsingFeedback{"functions using feedback"};

    Stat emptySlots{"empty"};

    auto list = RList(DTs);
    unsigned int totalFunctions = list.length();
    unsigned int totalFunctionsWithSlots = 0;

    FunctionAggregate emptySlotsOverTotalSlots{"empty slots"};
    FunctionAggregate slotsReadOverReferencedPerFunction{
        "read / referenced slots"};
    FunctionAggregate slotsUsedOverReadNonEmptyPerFunction{
        "used / read non-empty slots"};
    FunctionAggregate slotsUsedOverNonEmptyPerFunction{
        "used / non-empty slots"};

    for (auto a = list.begin(); a != list.end(); ++a) {
        DispatchTable* dt = DispatchTable::unpack(*a);
        auto baseline = dt->baseline();
        auto feedback = baseline->typeFeedback();

        outputInFunction << "--------------------------------------- \n\n";
        outputInFunction << "DT " << dt << " - name: " << dt->closureName
                         << (baseline->involvedInCompilation ? " (compiled)"
                                                             : "")
                         << "\n";
        outputInFunction << "baseline function: " << dt->baseline() << "\n";

        Stat slotsInFunction = {"slots in function", feedback->slotsSize()};
        totalSlots += slotsInFunction;
        if (slotsInFunction.value != 0) {
            totalFunctionsWithSlots++;
        }

        readSlots += baseline->slotsRead.size();

        Stat readNonEmptySlotsInFunction{"read non-empty"};
        for (auto& slot : baseline->slotsRead) {
            switch (slot.kind) {
            case rir::FeedbackKind::Type:
                if (!feedback->types(slot.idx).isEmpty())
                    readNonEmptySlotsInFunction++;
                break;

            case rir::FeedbackKind::Call:
                if (!feedback->callees(slot.idx).isEmpty())
                    readNonEmptySlotsInFunction++;
                break;

            case rir::FeedbackKind::Test:
                if (!feedback->test(slot.idx).isEmpty())
                    readNonEmptySlotsInFunction++;
                break;

            default:
                assert(false);
            }
        }
        readNonEmptySlots += readNonEmptySlotsInFunction;

        usedSlots += baseline->slotsUsed.size();
        narrowedSlots += baseline->slotsNarrowedWithStaticType.size();
        exactMatchUsedSlots += baseline->slotsUsedExactMatch.size();
        widenedUsedSlots += baseline->slotsUsedWidened.size();

        for (auto& s : baseline->slotsUsed) {
            if (s.kind == rir::FeedbackKind::Type)
                usedSlotsOfKindType++;
        }

        Stat emptySlotsCountInFunction{"empty slots in function"};

        for (size_t i = 0; i < feedback->tests_size(); i++) {
            if (feedback->test(i).isEmpty())
                emptySlotsCountInFunction++;
        }

        for (size_t i = 0; i < feedback->callees_size(); i++) {
            if (feedback->callees(i).isEmpty())
                emptySlotsCountInFunction++;
        }

        for (size_t i = 0; i < feedback->types_size(); i++) {
            if (feedback->types(i).isEmpty())
                emptySlotsCountInFunction++;
        }
        emptySlots += emptySlotsCountInFunction;

        Stat nonEmptySlotsCountInFunction = {
            "non-empty slots",
            slotsInFunction.value - emptySlotsCountInFunction.value};

        if (slotsInFunction.value != 0) {
            auto p = (nonEmptySlotsCountInFunction / slotsInFunction)
                         .named("non-empty slots");

            outputInFunction << p;

            emptySlotsOverTotalSlots.add(p);
        }

        if (baseline->involvedInCompilation) {
            compiledFunctions++;
            if (baseline->slotsUsed.size()) {
                functionsUsingFeedback++;
            }

            referencedSlots += slotsInFunction;

            if (slotsInFunction.value != 0) {
                Stat slotsRead{"read", baseline->slotsRead.size()};
                auto p = (slotsRead / slotsInFunction).named("slots read");

                outputInFunction << p;
                slotsReadOverReferencedPerFunction.add(p);
            }

            // USED / NON-EMPTY
            Stat usedSlots = {"used", baseline->slotsUsed.size()};

            outputInFunction
                << (usedSlots / nonEmptySlotsCountInFunction).named("used 1");

            if (nonEmptySlotsCountInFunction.value != 0) {
                slotsUsedOverNonEmptyPerFunction.add(
                    usedSlots / nonEmptySlotsCountInFunction);
            }

            // USED / READ NON-EMPTY
            outputInFunction
                << (usedSlots / readNonEmptySlotsInFunction).named("used 2");

            if (readNonEmptySlotsInFunction.value != 0) {
                slotsUsedOverReadNonEmptyPerFunction.add(
                    usedSlots / readNonEmptySlotsInFunction);
            }

            Stat deoptedSlots{"deopted", baseline->slotsDeopted.size()};

            outputInFunction
                << (deoptedSlots / usedSlots).named("deopted slots");
        }

        outputInFunction << "\n";
    }

    Stat nonEmptySlots{"non-empty", totalSlots.value - emptySlots.value};

    constexpr bool printSummaryToFile = false;
    std::ofstream fileStream("summary.txt", std::ios::app);
    std::ostream& ss = printSummaryToFile ? fileStream : defaultOutput;

    ss << "\n\n********** SUMMARY *************\n\n";
    ss << "Total functions (RIR compiled): " << totalFunctions << "\n";

    ss << "Compiled functions (PIR compiled): " << compiledFunctions << "\n";

    ss << "Total slots: " << totalSlots << "\n";
    ss << "\n";

    ss << (emptySlots / totalSlots).named("empty slots (never filled)");
    ss << emptySlotsOverTotalSlots;
    ss << "non-empty slots (on average of "
       << emptySlotsOverTotalSlots.values.size() << " functions)";
    showPercent(1 - emptySlotsOverTotalSlots.average(), ss);
    ss << "\n";
    ss << "\n";

    ss << (referencedSlots / totalSlots)
              .named("referenced slots in compilation");
    ss << (readSlots / referencedSlots).named("slots read");
    ss << slotsReadOverReferencedPerFunction;
    ss << "\n";

    // used slots
    ss << "--- USED SLOTS ---\n";

    // ss << (usedSlots / totalSlots).named("slots used in speculation");
    ss << (usedSlots / referencedSlots)
              .named("referenced slots used in speculation");
    ss << (usedSlots / readSlots).named("read slots used in speculation");
    ss << "\n";

    // USED / READ non-empty
    ss << (usedSlots / readNonEmptySlots)
              .named("read non-empty slots used in speculation ");
    ss << slotsUsedOverReadNonEmptyPerFunction;
    ss << "\n";

    // USED /  non-empty
    ss << (usedSlots / nonEmptySlots)
              .named("non-empty slots used in speculation");
    ss << slotsUsedOverNonEmptyPerFunction;
    ss << "\n";

    // USED MATCH
    ss << "used (of kind type): " << usedSlotsOfKindType << "\n";

    ss << (narrowedSlots / usedSlotsOfKindType)
              .named("narrowed with static type");
    ss << (exactMatchUsedSlots / usedSlotsOfKindType).named("exact match");
    ss << (widenedUsedSlots / usedSlotsOfKindType).named("widened");
    ss << "\n";

    // benefit
    ss << (functionsUsingFeedback / compiledFunctions)
              .named("benefited from feedback");
    ss << "\n";

    ss.flush();

    auto csv_file = getenv("STATS_CSV");
    if (csv_file != nullptr) {
        std::ofstream ofs{csv_file, std::ios::out | std::ios::app};

        ofs.seekp(0, std::ios::end);
        if (ofs.tellp() == 0) {
            // clang-format off
            ofs << "name,total functions,compiled functions,benefited functions,"
                << "total slots,empty slots,referenced slots,read slots,read non-empty slots,used slots,"
                << "avg empty slots (per function),avg read slots (per function),"
                << "avg used slots (per function),avg used slots from read (per function),"
                << "used type slots,narrowed,exact match,widened\n";
            // clang-format on
        }

        const char* stats_name = getenv("STATS_NAME");
        if (stats_name == nullptr) {
            stats_name = "?";
        }

        auto out = [&ofs](auto x) { ofs << x << ","; };

        auto qout = [&ofs](auto x) {
            ofs << "\"" << x << "\""
                << ",";
        };

        qout(stats_name);

        out(totalFunctions);
        out(compiledFunctions);
        out(functionsUsingFeedback);

        out(totalSlots);
        out(emptySlots);
        out(referencedSlots);
        out(readSlots);
        out(readNonEmptySlots);
        out(usedSlots);

        out(emptySlotsOverTotalSlots.average());
        out(slotsReadOverReferencedPerFunction.average());

        out(slotsUsedOverNonEmptyPerFunction.average());
        out(slotsUsedOverReadNonEmptyPerFunction.average());

        out(usedSlotsOfKindType);
        out(narrowedSlots);
        out(exactMatchUsedSlots);
        ofs << widenedUsedSlots << "\n";
    }
}

std::string getClosureName(SEXP cls) {
    std::string name = "";

    // 1. Look trhu frames
    auto frame = RList(FRAME(CLOENV(cls)));

    for (auto e = frame.begin(); e != frame.end(); ++e) {
        if (*e == cls) {
            name = CHAR(PRINTNAME(e.tag()));
            if (!name.empty()) {
                return name;
            }
        }
    }

    // 2. Try to look thru symbols
    auto env = PROTECT(CLOENV(cls));
    auto symbols = PROTECT(R_lsInternal3(env, TRUE, FALSE));

    auto size = Rf_length(symbols);
    for (int i = 0; i < size; i++) {
        const char* symbol_char = CHAR(VECTOR_ELT(symbols, i));

        // TODO: check parity with R_findVarInFrame
        auto symbol = PROTECT(Rf_install(symbol_char));
        R_varloc_t loc = R_findVarLocInFrame(env, symbol);
        UNPROTECT(1);
        auto cellValue = R_GetVarLocValue(loc);

        if (TYPEOF(cellValue) == PROMSXP) {
            cellValue = PRVALUE(cellValue);
        }

        if (cellValue == cls) {
            name = symbol_char;
            break;
        }
    }

    UNPROTECT(2);
    return name;
}

REXPORT SEXP rirCompile(SEXP what, SEXP env) {
    if (TYPEOF(what) == CLOSXP) {
        SEXP body = BODY(what);
        if (TYPEOF(body) == EXTERNALSXP)
            return what;



        // register rir closures
        SEXP DTs = Rf_findVar(DTsSymbol, R_GlobalEnv);
        if (DTs == R_UnboundValue) {
            Rf_setVar(DTsSymbol, R_NilValue, R_GlobalEnv);
            Compiler::onNewDt = [&](SEXP sexpDT) {
                SEXP currentDTs = Rf_findVar(DTsSymbol, R_GlobalEnv);
                currentDTs = Rf_cons(sexpDT, currentDTs);
                Rf_setVar(DTsSymbol, currentDTs, R_GlobalEnv);
            };

            if (!finalizerSet) {
                // Call `loadNamespace("Base")`
                SEXP baseStr = PROTECT(Rf_mkString("base"));
                SEXP expr =
                    PROTECT(Rf_lang2(Rf_install("loadNamespace"), baseStr));
                SEXP namespaceRes = PROTECT(Rf_eval(expr, R_GlobalEnv));
                R_RegisterCFinalizerEx(namespaceRes, &myFinalizer, TRUE);
                UNPROTECT(3);

                finalizerSet = true;
            }
        }

        // Change the input closure inplace

        Compiler::compileClosure(what);
        auto outerDT = DispatchTable::unpack(BODY(what));
        auto name = getClosureName(what);
        if (name != "") {
            outerDT->closureName = getClosureName(what);
        }

        return what;
    } else {
        if (TYPEOF(what) == BCODESXP) {
            what = VECTOR_ELT(CDR(what), 0);
        }
        SEXP result = Compiler::compileExpression(what);
        return result;
    }
}

REXPORT SEXP rirMarkFunction(SEXP what, SEXP which, SEXP reopt_,
                             SEXP forceInline_, SEXP disableInline_,
                             SEXP disableSpecialization_,
                             SEXP disableArgumentTypeSpecialization_,
                             SEXP disableNumArgumentSpecialization_,
                             SEXP depromiseArgs_) {
    if (!isValidClosureSEXP(what))
        Rf_error("Not rir compiled code");
    if (TYPEOF(which) != INTSXP || LENGTH(which) != 1)
        Rf_error("index not an integer");
    auto i = INTEGER(which)[0];
    SEXP b = BODY(what);
    DispatchTable* dt = DispatchTable::unpack(b);
    if (i < 0 || (size_t)i > dt->size())
        Rf_error("version with this number does not exist");

    auto getBool = [](SEXP v) {
        if (TYPEOF(v) != LGLSXP) {
            Rf_warning("non-boolean flag");
            return NA_LOGICAL;
        }
        if (LENGTH(v) == 0)
            return NA_LOGICAL;
        return LOGICAL(v)[0];
    };

    auto reopt = getBool(reopt_);
    auto forceInline = getBool(forceInline_);
    auto disableInline = getBool(disableInline_);
    auto disableSpecialization = getBool(disableSpecialization_);
    auto disableNumArgumentSpecialization =
        getBool(disableNumArgumentSpecialization_);
    auto disableArgumentTypeSpecialization =
        getBool(disableArgumentTypeSpecialization_);
    auto depromiseArgs = getBool(depromiseArgs_);

    Function* fun = dt->get(i);
    if (reopt != NA_LOGICAL) {
        if (reopt) {
            fun->flags.set(Function::MarkOpt);
            fun->flags.reset(Function::NotOptimizable);
        } else {
            fun->flags.reset(Function::MarkOpt);
        }
    }
    if (forceInline != NA_LOGICAL) {
        if (forceInline)
            fun->flags.set(Function::ForceInline);
        else
            fun->flags.reset(Function::ForceInline);
    }
    if (disableInline != NA_LOGICAL) {
        if (disableInline)
            fun->flags.set(Function::DisableInline);
        else
            fun->flags.reset(Function::DisableInline);
    }
    if (disableSpecialization != NA_LOGICAL) {
        if (disableSpecialization)
            fun->flags.set(Function::DisableAllSpecialization);
        else
            fun->flags.reset(Function::DisableAllSpecialization);
    }
    if (disableArgumentTypeSpecialization != NA_LOGICAL) {
        if (disableArgumentTypeSpecialization)
            fun->flags.set(Function::DisableArgumentTypeSpecialization);
        else
            fun->flags.reset(Function::DisableArgumentTypeSpecialization);
    }
    if (disableNumArgumentSpecialization != NA_LOGICAL) {
        if (disableNumArgumentSpecialization)
            fun->flags.set(Function::DisableNumArgumentsSpezialization);
        else
            fun->flags.reset(Function::DisableNumArgumentsSpezialization);
    }

    bool DISABLE_ANNOTATIONS = getenv("PIR_DISABLE_ANNOTATIONS") ? true : false;
    if (!DISABLE_ANNOTATIONS) {
        if (depromiseArgs != NA_LOGICAL) {
            if (depromiseArgs)
                fun->flags.set(Function::DepromiseArgs);
            else
                fun->flags.reset(Function::DepromiseArgs);
        }
    }

    return R_NilValue;
}

REXPORT SEXP rirFunctionVersions(SEXP what) {
    if (!isValidClosureSEXP(what))
        Rf_error("Not rir compiled code");
    DispatchTable* dt = DispatchTable::unpack(BODY(what));
    auto res = Rf_allocVector(INTSXP, dt->size());
    for (size_t i = 0; i < dt->size(); ++i)
        INTEGER(res)[i] = i;
    return res;
}

REXPORT SEXP rirBody(SEXP cls) {
    if (!isValidClosureSEXP(cls))
        Rf_error("Not a valid rir compiled function");
    return DispatchTable::unpack(BODY(cls))->baseline()->container();
}

REXPORT SEXP pirDebugFlags(
#define V(n) SEXP n,
    LIST_OF_PIR_DEBUGGING_FLAGS(V)
#undef V
        SEXP IHaveTooManyCommasDummy) {
    pir::DebugOptions opts;

#define V(n)                                                                   \
    if (Rf_asLogical(n))                                                       \
        opts.flags.set(pir::DebugFlag::n);
    LIST_OF_PIR_DEBUGGING_FLAGS(V)
#undef V

    SEXP res = Rf_allocVector(INTSXP, 1);
    INTEGER(res)[0] = (int)opts.flags.to_i();
    return res;
}

static pir::DebugOptions::DebugFlags getInitialDebugFlags() {
    auto verb = getenv("PIR_DEBUG");
    if (!verb)
        return pir::DebugOptions::DebugFlags();
    std::istringstream in(verb);

    pir::DebugOptions::DebugFlags flags;
    while (!in.fail()) {
        std::string opt;
        std::getline(in, opt, ',');
        if (opt.empty())
            continue;

        bool success = false;

#define V(flag)                                                                \
    if (opt.compare(#flag) == 0) {                                             \
        success = true;                                                        \
        flags = flags | pir::DebugFlag::flag;                                  \
    }
        LIST_OF_PIR_DEBUGGING_FLAGS(V)
#undef V
        if (!success) {
            std::cerr << "Unknown PIR debug flag " << opt << "\n"
                      << "Valid flags are:\n";
#define V(flag) std::cerr << "- " << #flag << "\n";
            LIST_OF_PIR_DEBUGGING_FLAGS(V)
#undef V
            exit(1);
        }
    }
    return flags;
}

static std::regex getInitialDebugPassFilter() {
    auto filter = getenv("PIR_DEBUG_PASS_FILTER");
    if (filter)
        return std::regex(filter);
    return std::regex(".*");
}

static std::regex getInitialDebugFunctionFilter() {
    auto filter = getenv("PIR_DEBUG_FUNCTION_FILTER");
    if (filter)
        return std::regex(filter);
    return std::regex(".*");
}

static pir::DebugStyle getInitialDebugStyle() {
    auto styleStr = getenv("PIR_DEBUG_STYLE");
    if (!styleStr) {
        return pir::DebugStyle::Standard;
    }
    pir::DebugStyle style;
    if (!parseDebugStyle(styleStr, style)) {
        std::cerr << "Unknown PIR debug print style " << styleStr << "\n"
                  << "Valid styles are:\n";
#define V(style) std::cerr << "- " << #style << "\n";
        LIST_OF_DEBUG_STYLES(V)
#undef V
        exit(1);
    }
    return style;
}

pir::DebugOptions pir::DebugOptions::DefaultDebugOptions = {
    getInitialDebugFlags(), getInitialDebugPassFilter(),
    getInitialDebugFunctionFilter(), getInitialDebugStyle()};

REXPORT SEXP pirSetDebugFlags(SEXP debugFlags) {
    if (TYPEOF(debugFlags) != INTSXP || Rf_length(debugFlags) < 1)
        Rf_error(
            "pirSetDebugFlags expects an integer vector as second parameter");
    pir::DebugOptions::DefaultDebugOptions.flags =
        pir::DebugOptions::DebugFlags(INTEGER(debugFlags)[0]);
    return R_NilValue;
}

SEXP pirCompile(SEXP what, const Context& assumptions, const std::string& name,
                const pir::DebugOptions& debug) {
    if (!isValidClosureSEXP(what)) {
        Rf_error("not a compiled closure");
    }
    if (!DispatchTable::check(BODY(what))) {
        Rf_error("Cannot optimize compiled expression, only closure");
    }

    PROTECT(what);

    REC_HOOK(recording::recordCompile(what, name, assumptions));

    bool dryRun = debug.includes(pir::DebugFlag::DryRun);
    // compile to pir
    pir::Module* m = new pir::Module;
    pir::Log logger(debug);
    logger.title("Compiling " + name);
    pir::Compiler cmp(m, logger);
    auto compile = [&](pir::ClosureVersion* c) {
        logger.flushAll();
        cmp.optimizeModule();

        if (dryRun)
            return;

        rir::Function* done = nullptr;
        {
            // Single Backend instance, gets destroyed at the end of this block
            // to finalize the LLVM module so that we can eagerly compile the
            // body
            pir::Backend backend(m, logger, name);
            auto apply = [&](SEXP body, pir::ClosureVersion* c) {
                c->scanForSpeculation();
                auto fun = backend.getOrCompile(c);
                Protect p(fun->container());
                DispatchTable::unpack(body)->insert(fun);
                if (body == BODY(what))
                    done = fun;
            };
            m->eachPirClosureVersion([&](pir::ClosureVersion* c) {
                if (c->owner()->hasOriginClosure()) {
                    auto cls = c->owner()->rirClosure();
                    auto body = BODY(cls);
                    auto dt = DispatchTable::unpack(body);
                    if (dt->contains(c->context())) {
                        // Dispatch also to versions with pending compilation
                        // since we're not evaluating
                        auto other = dt->dispatch(c->context(), false);
                        assert(other != dt->baseline());
                        assert(other->context() == c->context());
                        if (other->body()->isCompiled())
                            return;
                    }
                    // Don't lower functions that have not been called often, as
                    // they have incomplete type-feedback.
                    if (dt->size() == 1 &&
                        dt->baseline()->invocationCount() < 2)
                        return;
                    apply(body, c);
                }
            });
            if (!done)
                apply(BODY(what), c);
        }
        // Eagerly compile the main function
        done->body()->nativeCode();
    };

    REC_HOOK(bool successfulComp = true);

    cmp.compileClosure(what, name, assumptions, true, compile,
                       [&]() {
                           REC_HOOK(successfulComp = false);
                           if (debug.includes(pir::DebugFlag::ShowWarnings))
                               std::cerr << "Compilation failed\n";
                       },
                       {});

    REC_HOOK(recording::recordCompileFinish(successfulComp, m));

    delete m;
    UNPROTECT(1);

    return what;
}

REXPORT SEXP rirInvocationCount(SEXP what) {
    if (!isValidClosureSEXP(what)) {
        Rf_error("not a compiled closure");
    }
    auto dt = DispatchTable::check(BODY(what));
    assert(dt);

    SEXP res = Rf_allocVector(INTSXP, dt->size());
    for (size_t i = 0; i < dt->size(); ++i)
        INTEGER(res)[i] = dt->get(i)->invocationCount();

    return res;
}

REXPORT SEXP pirCompileWrapper(SEXP what, SEXP name, SEXP debugFlags,
                               SEXP debugStyle) {
    if (debugFlags != R_NilValue &&
        (TYPEOF(debugFlags) != INTSXP || Rf_length(debugFlags) != 1))
        Rf_error(
            "pirCompileWrapper expects an integer scalar as second parameter");
    if (debugStyle != R_NilValue && TYPEOF(debugStyle) != SYMSXP)
        Rf_error("pirCompileWrapper expects a symbol as third parameter");
    std::string n;
    if (TYPEOF(name) == SYMSXP)
        n = CHAR(PRINTNAME(name));
    pir::DebugOptions opts = pir::DebugOptions::DefaultDebugOptions;

    if (debugFlags != R_NilValue) {
        opts.flags = pir::DebugOptions::DebugFlags(*INTEGER(debugFlags));
    }
    if (debugStyle != R_NilValue) {
        if (!parseDebugStyle(CHAR(PRINTNAME(debugStyle)), opts.style)) {
            Rf_error("pirCompileWrapper - given unknown debug style");
        }
    }
    return pirCompile(what, rir::pir::Compiler::defaultContext, n, opts);
}

REXPORT SEXP pirTests() {
    if (pir::Parameter::PIR_OPT_LEVEL < 2) {
        Rf_warning("pirCheck only runs with opt level 2");
        return R_FalseValue;
    }

    PirTests::run();
    return R_NilValue;
}

REXPORT SEXP pirCheckWarmupBegin(SEXP f, SEXP checksSxp, SEXP env) {
    if (oldMaxInput == 0) {
        oldMaxInput = pir::Parameter::MAX_INPUT_SIZE;
        oldInlinerMax = pir::Parameter::INLINER_MAX_SIZE;
        oldSerializeChaos = pir::Parameter::RIR_SERIALIZE_CHAOS;
        oldDeoptChaos = pir::Parameter::DEOPT_CHAOS;
    }
    pir::Parameter::MAX_INPUT_SIZE = 3500;
    pir::Parameter::INLINER_MAX_SIZE = 4000;
    pir::Parameter::RIR_SERIALIZE_CHAOS = 0;
    pir::Parameter::DEOPT_CHAOS = 0;
    return R_NilValue;
}
REXPORT SEXP pirCheckWarmupEnd(SEXP f, SEXP checksSxp, SEXP env) {
    pir::Parameter::MAX_INPUT_SIZE = oldMaxInput;
    pir::Parameter::INLINER_MAX_SIZE = oldInlinerMax;
    pir::Parameter::RIR_SERIALIZE_CHAOS = oldSerializeChaos;
    pir::Parameter::DEOPT_CHAOS = oldDeoptChaos;
    return R_NilValue;
}

REXPORT SEXP pirCheck(SEXP f, SEXP checksSxp, SEXP env) {
    if (TYPEOF(checksSxp) != LISTSXP)
        Rf_error("pirCheck: 2nd parameter must be a pairlist (of symbols)");
    std::list<PirCheck::Type> checkTypes;
    for (SEXP c = checksSxp; c != R_NilValue; c = CDR(c)) {
        SEXP checkSxp = CAR(c);
        if (TYPEOF(checkSxp) != SYMSXP)
            Rf_error("pirCheck: each item in 2nd parameter must be a symbol");
        PirCheck::Type type = PirCheck::parseType(CHAR(PRINTNAME(checkSxp)));
        if (type == PirCheck::Type::Invalid)
            Rf_error("pirCheck: invalid check type. List of check types:"
#define V(Check) "\n    " #Check
                     LIST_OF_PIR_CHECKS(V)
#undef V
            );
        checkTypes.push_back(type);
    }
    // Automatically compile rir for convenience (necessary to get PIR)
    if (!isValidClosureSEXP(f))
        rirCompile(f, env);
    PirCheck check(checkTypes);
    bool res = check.run(f);
    return res ? R_TrueValue : R_FalseValue;
}

SEXP rirOptDefaultOpts(SEXP closure, const Context& assumptions, SEXP name) {
    std::string n = "";
    if (TYPEOF(name) == SYMSXP)
        n = CHAR(PRINTNAME(name));
    // PIR can only optimize closures, not expressions
    if (isValidClosureSEXP(closure))
        return pirCompile(closure, assumptions, n,
                          pir::DebugOptions::DefaultDebugOptions);
    else
        return closure;
}

SEXP rirOptDefaultOptsDryrun(SEXP closure, const Context& assumptions,
                             SEXP name) {
    std::string n = "";
    if (TYPEOF(name) == SYMSXP)
        n = CHAR(PRINTNAME(name));
    // PIR can only optimize closures, not expressions
    if (isValidClosureSEXP(closure))
        return pirCompile(
            closure, assumptions, n,
            pir::DebugOptions::DefaultDebugOptions |
                pir::DebugOptions::DebugFlags(pir::DebugFlag::DryRun));
    else
        return closure;
}

REXPORT SEXP rirSerialize(SEXP data, SEXP fileSexp) {
    oldPreserve = pir::Parameter::RIR_PRESERVE;
    pir::Parameter::RIR_PRESERVE = true;
    if (TYPEOF(fileSexp) != STRSXP)
        Rf_error("must provide a string path");
    FILE* file = fopen(CHAR(Rf_asChar(fileSexp)), "w");
    if (!file)
        Rf_error("couldn't open file at path");
    R_SaveToFile(data, file, 0);
    fclose(file);
    R_Visible = (Rboolean) false;
    pir::Parameter::RIR_PRESERVE = oldPreserve;
    return R_NilValue;
}

REXPORT SEXP rirDeserialize(SEXP fileSexp) {
    oldPreserve = pir::Parameter::RIR_PRESERVE;
    pir::Parameter::RIR_PRESERVE = true;
    if (TYPEOF(fileSexp) != STRSXP)
        Rf_error("must provide a string path");
    FILE* file = fopen(CHAR(Rf_asChar(fileSexp)), "r");
    if (!file)
        Rf_error("couldn't open file at path");
    SEXP res = R_LoadFromFile(file, 0);
    fclose(file);
    pir::Parameter::RIR_PRESERVE = oldPreserve;
    return res;
}

REXPORT SEXP rirEnableLoopPeeling() {
    Compiler::loopPeelingEnabled = true;
    return R_NilValue;
}

REXPORT SEXP rirDisableLoopPeeling() {
    Compiler::loopPeelingEnabled = false;
    return R_NilValue;
}

REXPORT SEXP rirResetMeasuring(SEXP outputOld) {
    if (TYPEOF(outputOld) != LGLSXP) {
        Rf_warning("non-boolean flag");
        return R_NilValue;
    }
    if (LENGTH(outputOld) == 0) {
        return R_NilValue;
    }
    Measuring::reset(LOGICAL(outputOld)[0]);
    return R_NilValue;
}

REXPORT SEXP rirPrintBuiltinIds() {
    FUNTAB* finger = R_FunTab;
    int i = 0;
    std::cout << "#ifndef RIR_BUILTIN_IDS_H\n"
              << "#define RIR_BUILTIN_IDS_H\n"
              << "// This file is generated using rir.printBuiltinIds()\n"
              << "#include \"utils/String.h\"\n"
              << "#include <cassert>\n"
              << "namespace rir {\n"
              << "static inline void errorWrongBuiltin() { "
              << "assert(false && \"wrong builtin id\"); }\n"
              << "constexpr static inline int blt(const char* name) {\n";
    while (finger->name) {
        std::cout << "    ";
        if (finger != R_FunTab)
            std::cout << "else ";
        std::cout << "if (staticStringEqual(name, \"" << finger->name
                  << "\"))\n"
                  << "        return " << i << ";\n";
        i++;
        finger++;
    }
    std::cout << "    else\n        errorWrongBuiltin();\n";
    std::cout << "    return -1;\n}\n} // namespace rir\n#endif\n";
    return R_NilValue;
}

REXPORT SEXP rirSetUserContext(SEXP f, SEXP userContext) {

    if (TYPEOF(f) != CLOSXP)
        Rf_error("f not closure");

    if (TYPEOF(BODY(f)) != EXTERNALSXP) {
        rirCompile(f, CLOENV(f));
    }

    if (TYPEOF(userContext) != INTSXP || LENGTH(userContext) != 2)
        Rf_error("userDefinedContext should be an Integer Array of size 2");

    Context newContext;
    auto p = (int*)((void*)&newContext);
    *p = INTEGER(userContext)[0];
    p++;
    *p = INTEGER(userContext)[1];

    auto tbl = DispatchTable::unpack(BODY(f));
    auto newTbl = tbl->newWithUserContext(newContext);
    SET_BODY(f, newTbl->container());
    return R_NilValue;
}

REXPORT SEXP rirCreateSimpleIntContext() {
    Context newContext = Context();
    newContext.setSimpleInt(0);

    int* p = (int*)((void*)&newContext);
    int n1 = *p;
    p++;
    int n2 = *p;

    auto res = Rf_allocVector(INTSXP, 2);
    INTEGER(res)[0] = n1;
    INTEGER(res)[1] = n2;
    return res;
}

REXPORT SEXP playground(SEXP what) {
    auto symbol = getClosureName(what);
    std::cerr << symbol << "\n";
    return R_NilValue;
}

bool startup() {
    initializeRuntime();
    return true;
}

bool startup_ok = startup();
