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
    return os << agg.referenced << agg.referencedNonEmpty << agg.readNonEmpty
              << agg.used << agg.slotPresentNonEmpty
              << "\n"

              << StreamColor::blue << "Used\n" << StreamColor::clear
              << agg.exactMatch << agg.widened << agg.narrowed << agg.widenedNarrowed
              << "\n"

              << StreamColor::blue << "Unused\n" << StreamColor::clear
              << agg.unusedNonEmpty
              << agg.optimizedAway << agg.dependent << agg.unusedOther
              << "\n"

              << StreamColor::blue << "Polluted\n" << StreamColor::clear
              << agg.polluted << agg.pollutedUsed;
    // clang-format on
}

// ------------------------------------------------------------

Aggregate FeedbackStatsPerFunction::getAgg(const FunctionInfo& info) const {
    Aggregate agg;

    // Static info
    agg.referenced = info.allTypeSlots.size();
    agg.referencedNonEmpty =
        intersect(info.nonEmptySlots, keys(info.allTypeSlots)).size();
    agg.readNonEmpty = intersect(info.nonEmptySlots, slotsRead).size();

    // Used
    agg.used = slotsUsed.size();
    assert(agg.used.value ==
               intersect(info.nonEmptySlots, keys(slotsUsed)).size() &&
           "There is an empty used slot");

    for (const auto& i : slotsUsed) {
        const auto& usage = i.second;

        if (usage.exactMatch()) {
            agg.exactMatch++;
        } else if (usage.narrowedWithStaticType() && usage.widened()) {
            agg.widenedNarrowed++;
        } else if (usage.narrowedWithStaticType()) {
            agg.narrowed++;
        } else if (usage.widened()) {
            agg.widened++;
        } else {
            assert(false);
        }
    }

    // Unused
    auto unusedNonEmpty =
        intersect(difference(keys(info.allTypeSlots), keys(slotsUsed)),
                  info.nonEmptySlots);
    agg.unusedNonEmpty = unusedNonEmpty.size();

    auto slotPresentNonEmpty = intersect(keys(slotPresent), unusedNonEmpty);
    agg.slotPresentNonEmpty = slotPresentNonEmpty.size();

    auto usedFeedbackTypes = getUsedFeedbackTypes();
    for (auto slot : unusedNonEmpty) {
        if (!slotPresent.count(slot)) {
            agg.optimizedAway++;
        } else if (usedFeedbackTypes.count(info.allTypeSlots.at(slot))) {
            agg.dependent++;
        } else {
            agg.unusedOther++;
        }
    }

    // Polluted
    agg.polluted = info.pollutedSlots.size();
    agg.pollutedUsed = intersect(info.pollutedSlots, keys(slotsUsed)).size();

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

Aggregate ClosureVersionStats::getAgg(
    std::unordered_map<Function*, FunctionInfo>& functionsInfo) {
    Aggregate agg;

    agg.universe = keys(feedbackStats);

    for (auto& i : feedbackStats) {
        agg += i.second.getAgg(functionsInfo[i.first]);
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
            const auto& tf = feedback->types(i);

            if (tf.isEmpty()) {
                slotData.emptySlots.insert(idx);
            } else {
                slotData.nonEmptySlots.insert(idx);
            }

            if (tf.isPolluted) {
                slotData.pollutedSlots.insert(idx);
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

static std::vector<CompilationSession> COMPILATION_SESSIONS;

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

    assert(closureVersion->context() == compiledFunction->context());
    auto baseline = compiledFunction->dispatchTable()->baseline();
    const auto& context = compiledFunction->context();

    closureVersionStats.emplace_back(baseline, context,
                                     closureVersion->feedbackStatsByFunction);
}

FinalAggregate CompilationSession::getFinalAgg() {
    FinalAggregate res;
    std::vector<Aggregate> values;

    for (auto i : COMPILATION_SESSIONS) {
        for (auto j : i.closureVersionStats) {
            auto agg = j.getAgg(i.functionsInfo);
            res.sums += agg;
            values.push_back(agg);

            res.compiledClosureVersions++;
            if (agg.used.value > 0) {
                res.benefitedClosureVersions++;
            }
        }
    }

#define average(resultField, num, denom)                                       \
    {                                                                          \
        res.resultField.name =                                                 \
            Aggregate{}.num.name + " / " + Aggregate{}.denom.name;             \
                                                                               \
        for (auto& i : values) {                                               \
            res.resultField.add(i.num / i.denom);                              \
        }                                                                      \
    }

    average(referencedNonEmptyRatio, referencedNonEmpty, referenced);
    average(readRatio, readNonEmpty, referenced);
    average(usedRatio, used, referenced);

    average(optimizedAwayRatio, optimizedAway, unusedNonEmpty);
    average(dependentRatio, dependent, unusedNonEmpty);
    average(unusedOtherRatio, unusedOther, unusedNonEmpty);

    average(pollutedRatio, polluted, referencedNonEmpty);
    average(pollutedOutOfUsedRatio, pollutedUsed, used);
    average(pollutedUsedRatio, pollutedUsed, polluted);

    return res;
#undef average
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

            os << feedbackType;

            if (info.pollutedSlots.count(index)) {
                os << " (polluted)";
            }

            os << "\n";

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
        os << StreamColor::magenta
           << "*********************** Compilation session for: "
           << closureName(session.function) << " (" << session.context
           << ") ***********************\n"
           << StreamColor::clear;

        for (auto& cvstat : session.closureVersionStats) {
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

    auto final = CompilationSession::getFinalAgg();

    auto finalHeader = [&](const auto& name) {
        os << StreamColor::blue << name << "\n" << StreamColor::clear;
    };

    // clang-format off
    os  << Stat{"total functions (RIR compiled)", DTs.size()}
        << Stat{"compiled functions (PIR compiled)", final.sums.universe.size()}
        << final.compiledClosureVersions
        << final.benefitedClosureVersions
        << "\n";

    finalHeader("Slots");
    os  << final.sums.referenced
        << final.sums.referencedNonEmpty
        << final.sums.readNonEmpty
        << final.sums.used
        << "\n";

    os  << final.sums.referencedNonEmpty / final.sums.referenced
        << final.sums.readNonEmpty / final.sums.referenced
        << final.sums.used / final.sums.referenced
        << "\n";

    finalHeader("Slots - Averaged per closure version");
    os  << final.referencedNonEmptyRatio
        << final.readRatio
        << final.usedRatio
        << "\n";

    finalHeader("Used slots");
    os  << final.sums.exactMatch
        << final.sums.widened
        << final.sums.narrowed
        << final.sums.widenedNarrowed
        << "\n";

    finalHeader("Unused slots");
    os  << final.sums.unusedNonEmpty
        << final.sums.optimizedAway
        << final.sums.dependent
        << final.sums.unusedOther
        << "\n";

    os  << final.sums.optimizedAway / final.sums.unusedNonEmpty
        << final.sums.dependent / final.sums.unusedNonEmpty
        << final.sums.unusedOther / final.sums.unusedNonEmpty
        << "\n";

    finalHeader("Unused slots - Averaged per closure version");
    os  << final.optimizedAwayRatio
        << final.dependentRatio
        << final.unusedOtherRatio
        << "\n";

    finalHeader("Polluted slots");
    os  << final.sums.polluted
        << final.sums.pollutedUsed
        << "\n";

    os  << final.sums.polluted / final.sums.referencedNonEmpty
        << final.sums.pollutedUsed / final.sums.used
        << final.sums.pollutedUsed / final.sums.polluted
        << "\n";

    finalHeader("Polluted slots - Averaged per closure version");
    os  << final.pollutedRatio
        << final.pollutedOutOfUsedRatio
        << final.pollutedUsedRatio
        << "\n";
    // clang-format on
}

void printCsvHeader(std::ostream& os,
                    std::initializer_list<std::string> extraFields,
                    Aggregate* aggFields,
                    FinalAggregate* finalFields = nullptr) {
    assert(extraFields.size() != 0);

    // Print the header if the file is empty
    os.seekp(0, std::ios::end);
    if (os.tellp() != 0) {
        return;
    }

    bool first = true;

    for (const auto& i : extraFields) {
        if (first) {
            os << i;
            first = false;
        } else {
            os << "," << i;
        }
    }

    for (const auto& i : aggFields->stats()) {
        os << "," << i->name;
    }

    if (finalFields != nullptr) {
        for (const auto& i : finalFields->stats()) {
            os << "," << i->name;
        }

        for (auto i : finalFields->aggregates()) {
            os << "," << i->name;
        }
    }

    os << "\n";
}

// Assumes you have written something before (the extraFields)
void printCsvLine(std::ostream& os, Aggregate* aggFields,
                  FinalAggregate* finalFields = nullptr) {
    for (const auto& i : aggFields->stats()) {
        os << "," << i->value;
    }

    if (finalFields != nullptr) {
        for (const auto& i : finalFields->stats()) {
            os << "," << i->value;
        }

        for (const auto& i : finalFields->aggregates()) {
            os << "," << i->average();
        }
    }

    os << "\n";
}

void reportCsv(std::ostream& os, const std::string& program_name) {
    auto agg = CompilationSession::getFinalAgg();

    printCsvHeader(os, {"name"}, &agg.sums, &agg);
    os << "\"" << program_name << "\"";
    printCsvLine(os, &agg.sums, &agg);
}

void reportIndividual(std::ostream& os, const std::string& benchmark_name) {
    bool first = true;

    for (auto& cs : COMPILATION_SESSIONS) {
        for (auto& cv : cs.closureVersionStats) {
            auto agg = cv.getAgg(cs.functionsInfo);

            if (first) {
                printCsvHeader(os, {"benchmark", "closure"}, &agg);
                first = false;
            }

            os << "\"" << benchmark_name << "\"";
            os << ",\"" << cv.function->dispatchTable()->closureName << "\"";
            printCsvLine(os, &agg);
        }
    }
}

} // namespace report
} // namespace rir
