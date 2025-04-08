#include "report.h"
#include "compiler/pir/closure_version.h"
#include "compiler/pir/instruction.h"
#include "runtime/DispatchTable.h"
#include "runtime/Function.h"
#include "utils/Terminal.h"

// ------------------------------------------------------------

namespace StreamColor {
struct {
} red;

struct {
} yellow;

struct {
} blue;

struct {
} magenta;

struct {
} clear;

struct {
} bold;

#define COLOR_OPERATOR(color)                                                  \
    std::ostream& operator<<(std::ostream& os, decltype(StreamColor::color)) { \
        auto forceColor = std::getenv("STATS_COLOR");                          \
        if ((forceColor != nullptr && std::string(forceColor) == "1") ||       \
            ConsoleColor::isTTY(os)) {                                         \
            ConsoleColor::color(os);                                           \
        }                                                                      \
        return os;                                                             \
    }

COLOR_OPERATOR(red)
COLOR_OPERATOR(yellow)
COLOR_OPERATOR(blue)
COLOR_OPERATOR(magenta)
COLOR_OPERATOR(clear)
COLOR_OPERATOR(bold)

#undef COLOR_OPERATOR

}; // namespace StreamColor

// ------------------------------------------------------------

namespace rir {
namespace report {

pir::PirType getSlotPirType(size_t i, Function* baseline) {
    auto observed = baseline->typeFeedback()->types(i);
    auto t = pir::PirType::bottom();
    if (observed.numTypes) {
        t.merge(observed);
    }
    return t;
}

pir::PirType getSlotPirType(const FeedbackOrigin& origin) {
    return getSlotPirType(origin.index().idx, origin.function());
}

// ------------------------------------------------------------

template <typename K, typename V>
std::unordered_set<K> keys(const std::unordered_map<K, V>& map) {
    std::unordered_set<K> res;

    for (const auto& i : map) {
        res.insert(i.first);
    }

    return res;
}

template <typename T>
std::unordered_set<T> intersect(const std::unordered_set<T>& lhs,
                                const std::unordered_set<T>& rhs) {
    std::unordered_set<T> res;

    for (const auto& i : lhs) {
        if (rhs.count(i)) {
            res.insert(i);
        }
    }

    return res;
}

// LHS \ RHS
template <typename T>
std::unordered_set<T> difference(const std::unordered_set<T>& lhs,
                                 const std::unordered_set<T>& rhs) {
    std::unordered_set<T> res;

    for (const auto& i : lhs) {
        if (!rhs.count(i)) {
            res.insert(i);
        }
    }

    return res;
}

// ------------------------------------------------------------

std::string streamToString(std::function<void(std::stringstream&)> f) {
    std::stringstream ss;
    f(ss);
    return ss.str();
};

std::string boolToString(bool b) { return b ? "yes" : "no"; }

void showPercent(double percent, std::ostream& ss) {
    ss << percent * 100 << "%";
}

// ------------------------------------------------------------

MetricPercent Stat::operator/(Stat& denom) {
    return MetricPercent{.numerator = this, .denominator = &denom};
}

double MetricPercent::value() const {
    if (denominator->value == 0) {
        assert(false && "cannot divide by 0");
    }

    return static_cast<double>(numerator->value) / denominator->value;
}

double FunctionAggregate::average() const {
    if (values.empty()) {
        assert(false && "empty aggregate");
        return 0.0;
    }

    double sum = std::accumulate(values.begin(), values.end(), 0.0);
    return sum / values.size();
}

// ------------------------------------------------------------

std::ostream& operator<<(std::ostream& os, const Stat& st) {
    if (st.name != "") {
        os << st.name << ": ";
    }

    os << StreamColor::bold << st.value << StreamColor::clear << "\n";

    return os;
}

std::ostream& operator<<(std::ostream& os, const MetricPercent& metric) {
    if (metric.name != "") {
        os << metric.name << " ";
    }

    os << "(" << metric.numerator->name << " / " << metric.denominator->name
       << "): ";

    os << StreamColor::bold << metric.numerator->value << " / "
       << metric.denominator->value;

    if (metric.denominator->value) {
        os << " (";
        showPercent(metric.value(), os);
        os << ")";
    }

    os << StreamColor::clear << "\n";

    return os;
};

std::ostream& operator<<(std::ostream& os, const FunctionAggregate& agg) {
    if (agg.values.empty()) {
        return os;
    }

    os << agg.name << " (on average out of " << agg.values.size()
       << " values): ";

    os << StreamColor::bold;
    showPercent(agg.average(), os);
    os << StreamColor::clear << "\n";

    return os;
}

// ------------------------------------------------------------

pir::PirType SlotUsed::expectedType() const {
    pir::PirType expected = *staticType & *feedbackType;

    // Reflecting what happens in TypeTest::Create
    if (staticType->maybeNAOrNaN() && !expected.maybeNAOrNaN() &&
        !expected.isSimpleScalar()) {
        expected = expected.orNAOrNaN();
    }

    return expected;
}

bool SlotUsed::widened() const {
    // reflects "NA checks are only possible on scalars" in type_test
    return (!feedbackType->maybeNAOrNaN() && expectedType().maybeNAOrNaN()) ||
           (*checkFor != expectedType());
}

bool SlotUsed::narrowedWithStaticType() const {
    return !(feedbackType->isA(*staticType));
}

std::ostream& operator<<(std::ostream& os, const SlotUsed& slotUsed) {
    using namespace StreamColor;

    os << bold << slotUsed.speculatedOn << clear << "\n";
    os << bold << slotUsed.assumeInstr << clear << "\n";

    if (slotUsed.exactMatch()) {
        os << "exact match\n";
    } else {
        os << "narrowed with static type: "
           << boolToString(slotUsed.narrowedWithStaticType()) << "\n";
        os << "widened: " << boolToString(slotUsed.widened()) << "\n";
    }

    // clang-format off
    os << bold << "checkFor: " << clear << *slotUsed.checkFor << ", "
       << bold << "static: "   << clear << *slotUsed.staticType << ", "
       << bold << "feedback: " << clear << *slotUsed.feedbackType << ", "
       << bold << "expected: " << clear << slotUsed.expectedType() << ", "
       << bold << "required: " << clear << *slotUsed.requiredType << "\n";
    // clang-format on
    return os;
}

// ------------------------------------------------------------

std::ostream& operator<<(std::ostream& os, const Aggregate& agg) {
    // clang-format off
    return os << agg.referenced << agg.read << agg.used
              << "\n"
              << agg.referencedNonEmpty << agg.readNonEmpty << agg.usedNonEmpty
              << "\n"
              << StreamColor::blue << "Unused\n" << StreamColor::clear
              << agg.optimizedAway << agg.dependent << agg.unusedOther
              << "\n"
              << agg.optimizedAwayNonEmpty << agg.dependentNonEmpty << agg.unusedOtherNonEmpty;
    // clang-format on
}

// ------------------------------------------------------------

Aggregate FeedbackStatsPerFunction::getAgg(const FunctionInfo& info) const {
    Aggregate agg;

    agg.referenced = info.allTypeSlots.size();
    agg.read = slotsRead.size();
    agg.used = slotsUsed.size();

    agg.referencedNonEmpty =
        intersect(info.nonEmptySlots, keys(info.allTypeSlots)).size();
    agg.readNonEmpty = intersect(info.nonEmptySlots, slotsRead).size();
    agg.usedNonEmpty = intersect(info.nonEmptySlots, keys(slotsUsed)).size();

    auto usedFeedbackTypes = getUsedFeedbackTypes();

    auto unused = difference(keys(info.allTypeSlots), keys(slotsUsed));
    for (auto slot : unused) {
        if (!slotPresent.count(slot)) {
            agg.optimizedAway++;
            if (info.nonEmptySlots.count(slot)) {
                agg.optimizedAwayNonEmpty++;
            }
        } else if (usedFeedbackTypes.count(info.allTypeSlots.at(slot))) {
            agg.dependent++;
            if (info.nonEmptySlots.count(slot)) {
                agg.dependentNonEmpty++;
            }
        } else {
            agg.unusedOther++;
            if (info.nonEmptySlots.count(slot)) {
                agg.unusedOtherNonEmpty++;
            }
        }
    }

    // Sanity check
    assert(agg.optimizedAway.value + agg.dependent.value +
               agg.unusedOther.value ==
           info.allTypeSlots.size() - agg.used.value);

    return agg;
}

std::unordered_set<pir::PirType>
FeedbackStatsPerFunction::getUsedFeedbackTypes() const {
    std::unordered_set<pir::PirType> usedFeedbackTypes;
    for (const auto& i : slotsUsed) {
        usedFeedbackTypes.insert(*i.second.feedbackType);
    }
    return usedFeedbackTypes;
}

// ------------------------------------------------------------

Universe ClosureVersionStats::universe() const { return keys(feedbackStats); }

Aggregate ClosureVersionStats::getAgg(
    std::unordered_map<Function*, FunctionInfo>& functionsInfo) {
    Aggregate agg;

    for (auto& i : feedbackStats) {
        agg += i.second.getAgg(functionsInfo[i.first]);
    }

    return agg;
}

FinalAggregate ClosureVersionStats::getFinalAgg(
    std::unordered_map<Function*, FunctionInfo>& functionsInfo) {
    auto agg = FinalAggregate::from(getAgg(functionsInfo));

    agg.universe = universe();

    agg.referencedNonEmptyRatio.add(agg.referencedNonEmpty / agg.referenced);
    agg.readRatio.add(agg.readNonEmpty / agg.referenced);
    agg.usedRatio.add(agg.usedNonEmpty / agg.referenced);

    agg.compiledClosureVersions++;
    if (agg.used.value != 0) {
        agg.benefitedClosureVersions++;
    }

    return agg;
}

// ------------------------------------------------------------

void computeFunctionsInfo(
    std::unordered_map<Function*, FunctionInfo>& functionsInfo,
    std::vector<DispatchTable*> DTs) {
    for (auto dt : DTs) {
        auto baseline = dt->baseline();
        auto& slotData = functionsInfo[baseline];

        // Types
        auto feedback = baseline->typeFeedback();
        for (size_t i = 0; i < feedback->types_size(); ++i) {
            auto idx = FeedbackIndex::type(i);

            slotData.allTypeSlots[idx] = getSlotPirType(i, baseline);
            if (feedback->types(i).isEmpty()) {
                slotData.emptySlots.insert(idx);
            } else {
                slotData.nonEmptySlots.insert(idx);
            }
        }

        // Deopts
        slotData.deoptsCount = baseline->allDeoptsCount;

        for (auto origin : baseline->slotsDeopted) {
            if (origin.function() == baseline) {
                slotData.slotsDeopted.insert(origin.index());
            } else {
                functionsInfo[origin.function()].inlinedSlotsDeopted.insert(
                    origin.index());
            }
        }
    }
}

std::vector<CompilationSession> COMPILATION_SESSIONS;

CompilationSession&
CompilationSession::getNew(Function* compiledFunction,
                           const Context& compiledContext,
                           const std::vector<DispatchTable*>& DTs) {
    COMPILATION_SESSIONS.emplace_back(compiledFunction, compiledContext);
    auto& session = COMPILATION_SESSIONS.back();
    computeFunctionsInfo(session.functionsInfo, DTs);
    return session;
}

// ------------------------------------------------------------

void CompilationSession::addClosureVersion(pir::ClosureVersion* closureVersion,
                                           Function* compiledFunction) {
    auto baseline = compiledFunction->dispatchTable()->baseline();
    const auto& context = compiledFunction->context();

    closuresVersionStats.emplace_back(baseline, context,
                                      closureVersion->feedbackStatsByFunction);
}

Universe CompilationSession::universe() const {
    Universe u;
    for (const auto& i : closuresVersionStats) {
        auto u2 = i.universe();
        u.insert(u2.begin(), u2.end());
    }

    return u;
}

FinalAggregate CompilationSession::getFinalAgg() {
    FinalAggregate res;

    for (auto i : COMPILATION_SESSIONS) {
        for (auto j : i.closuresVersionStats) {
            res += j.getFinalAgg(i.functionsInfo);
        }
    }

    return res;
}

// ------------------------------------------------------------

const std::string& closureName(Function* fun) {
    return fun->dispatchTable()->closureName;
}

template <typename T>
std::vector<std::pair<FeedbackIndex, T>>
sortByFeedbackIndex(const std::unordered_map<FeedbackIndex, T>& map) {
    std::vector<std::pair<FeedbackIndex, T>> vec{map.begin(), map.end()};

    std::sort(vec.begin(), vec.end(),
              [](const std::pair<FeedbackIndex, T>& lhs,
                 const std::pair<FeedbackIndex, T>& rhs) {
                  return lhs.first.idx < rhs.first.idx;
              });

    return vec;
}

void report(std::ostream& os, bool breakdownInfo,
            const std::vector<DispatchTable*>& DTs) {
    auto printSlotBreakdown =
        [&](const FeedbackIndex& index, const pir::PirType& feedbackType,
            const FeedbackStatsPerFunction& stats, FunctionInfo& info) {
            bool used = stats.slotsUsed.count(index);

            os << StreamColor::red << index << StreamColor::clear;

            if (used) {
                os << " [used] ";
            } else {
                os << " [unused] ";
            }

            os << feedbackType << "\n";

            if (used) {
                os << stats.slotsUsed.at(index);
            } else {
                bool isDependency =
                    stats.getUsedFeedbackTypes().count(feedbackType);
                if (!stats.slotPresent.count(index)) {
                    os << "optimized away";
                    if (isDependency) {
                        os << " (dependency)";
                    }
                    os << "\n";
                } else {
                    auto present = stats.slotPresent.at(index);
                    os << StreamColor::bold << present.presentInstr
                       << StreamColor::clear << "\n";

                    if (isDependency) {
                        os << "dependent slot\n";
                    } else {
                        os << "other unused reason\n";
                    }
                }
            }
        };

    auto printFunctionInfo = [&](DispatchTable* dt,
                                 const FeedbackStatsPerFunction& stats,
                                 FunctionInfo& info, bool isInlinee = false) {
        // Header
        os << "----------------------\n";
        os << StreamColor::yellow;
        if (isInlinee) {
            os << "Inlinee: ";
        }
        os << dt->closureName << " [" << dt << "]\n" << StreamColor::clear;

        // Compilation slots info
        os << stats.getAgg(info) << "\n";

        if (breakdownInfo) {
            auto allSlots = sortByFeedbackIndex(info.allTypeSlots);
            for (auto& i : allSlots) {
                auto& index = i.first;
                auto& feedbackType = i.second;
                printSlotBreakdown(index, feedbackType, stats, info);
                os << "\n";
            }
        }
    };

    for (auto& session : COMPILATION_SESSIONS) {
        os << StreamColor::magenta << "*********************** Compilation: "
           << closureName(session.function) << " (" << session.context
           << ") ***********************\n"
           << StreamColor::clear;

        for (auto& cvstat : session.closuresVersionStats) {
            auto mainFun = cvstat.function;

            // Banner
            os << StreamColor::blue
               << "======================= ClosureVersion: "
               << closureName(mainFun) << " (" << cvstat.context
               << ") =======================\n"
               << StreamColor::clear;

            // clang-format off
            os << cvstat.getAgg(session.functionsInfo)
               << "\n";
            // clang-format on

            // Main
            auto& mainInfo = session.functionsInfo[mainFun];
            auto& mainFeedbackStats = cvstat.feedbackStats[mainFun];
            printFunctionInfo(mainFun->dispatchTable(), mainFeedbackStats,
                              mainInfo);

            // Rest
            for (auto& i : cvstat.feedbackStats) {
                auto fun = i.first;
                auto& info = session.functionsInfo[fun];
                auto& feedbackStats = i.second;

                if (fun == mainFun) {
                    continue;
                }

                printFunctionInfo(fun->dispatchTable(), feedbackStats, info,
                                  true);
            }
        }
    }

    os << StreamColor::magenta
       << "----------------------- Summary ------------------------\n"
       << StreamColor::clear;

    auto agg = CompilationSession::getFinalAgg();

    auto finalHeader = [&](const auto& name) {
        os << StreamColor::blue << name << "\n" << StreamColor::clear;
    };

    // clang-format off
    os  << Stat{"total functions (RIR compiled)", DTs.size()}
        << Stat{"compiled functions (PIR compiled)", agg.universe.size()}
        << agg.compiledClosureVersions
        << agg.benefitedClosureVersions
        << "\n";

    finalHeader("Slots");
    os  << agg.referenced
        << agg.read
        << agg.used
        << "\n";

    finalHeader("Non-empty slots");
    os  << agg.referencedNonEmpty
        << agg.readNonEmpty
        << agg.usedNonEmpty
        << "\n";

    os  << agg.referencedNonEmpty / agg.referenced
        << agg.readNonEmpty / agg.referenced
        << agg.usedNonEmpty / agg.referenced
        << "\n";

    finalHeader("Averaged per closure version");
    os  << agg.referencedNonEmptyRatio
        << agg.readRatio
        << agg.usedRatio
        << "\n";

    finalHeader("Unused slots");
    os  << agg.optimizedAway
        << agg.dependent
        << agg.unusedOther
        << "\n";

    os  << agg.optimizedAway / agg.referenced
        << agg.dependent / agg.referenced
        << agg.unusedOther / agg.referenced
        << "\n";

    finalHeader("Non-empty unused slots");
    os  << agg.optimizedAwayNonEmpty
        << agg.dependentNonEmpty
        << agg.unusedOtherNonEmpty
        << "\n";

    os  << agg.optimizedAwayNonEmpty / agg.referencedNonEmpty
        << agg.dependentNonEmpty / agg.referencedNonEmpty
        << agg.unusedOtherNonEmpty / agg.referencedNonEmpty;
    // clang-format on
}

void reportCsv(std::ostream& os, const std::string& program_name) {
    // Print the header if the file is empty
    os.seekp(0, std::ios::end);
    if (os.tellp() == 0) {
        // clang-format off
        os  << "name"
            << ",referenced,read,used"
            << ",referenced non-empty,read non-empty,used non-empty"
            << ",referenced non-empty / referenced,read non-empty / referenced,used non-empty / referenced"
            << ",optimized away,dependent"
            << ",optimized away non-empty,dependent non-empty"
            << "\n";
        // clang-format on
    }

    auto out = [&os](const auto& x, bool last = false) {
        os << x << (last ? "\n" : ",");
    };

    auto qout = [&os](const auto& x, bool last = false) {
        os << "\"" << x << "\"" << (last ? "\n" : ",");
    };

    auto agg = CompilationSession::getFinalAgg();

    qout(program_name);

    out(agg.referenced.value);
    out(agg.read.value);
    out(agg.used.value);

    out(agg.referencedNonEmpty.value);
    out(agg.readNonEmpty.value);
    out(agg.usedNonEmpty.value);

    out(agg.referencedNonEmptyRatio.average());
    out(agg.readRatio.average());
    out(agg.usedRatio.average());

    out(agg.optimizedAway.value);
    out(agg.dependent.value);

    out(agg.optimizedAwayNonEmpty.value);
    out(agg.dependentNonEmpty.value, true);
}

void reportIndividual(std::ostream& os, const std::string& benchmark_name) {
    // Print the header if the file is empty
    os.seekp(0, std::ios::end);
    if (os.tellp() == 0) {
        // clang-format off
        os  << "benchmark,closure"
            << ",referenced,read,used"
            << ",referenced non-empty,read non-empty,used non-empty"
            << ",optimized away,dependent"
            << ",optimized away non-empty,dependent non-empty"
            << "\n";
        // clang-format on
    }

    auto out = [&os](const auto& x, bool last = false) {
        os << x << (last ? "\n" : ",");
    };

    auto qout = [&os](const auto& x, bool last = false) {
        os << "\"" << x << "\"" << (last ? "\n" : ",");
    };

    for (auto& cs : COMPILATION_SESSIONS) {
        for (auto& cv : cs.closuresVersionStats) {
            auto agg = cv.getFinalAgg(cs.functionsInfo);

            qout(benchmark_name);
            qout(cv.function->dispatchTable()->closureName);

            out(agg.referenced.value);
            out(agg.read.value);
            out(agg.used.value);

            out(agg.referencedNonEmpty.value);
            out(agg.readNonEmpty.value);
            out(agg.usedNonEmpty.value);

            out(agg.optimizedAway.value);
            out(agg.dependent.value);

            out(agg.optimizedAwayNonEmpty.value);
            out(agg.dependentNonEmpty.value, true);
        }
    }
}

} // namespace report
} // namespace rir
