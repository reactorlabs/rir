#include "report.h"
#include "compiler/pir/closure_version.h"
#include "compiler/pir/instruction.h"
#include "runtime/DispatchTable.h"
#include "runtime/Function.h"
#include "utils/Terminal.h"

#include <cmath>
#include <iomanip>
#include <iostream>

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

// // Determine number of decimal places to show based on rules
// int getDisplayPrecision(double value) {
//     if (value >= 1.0) {
//         return 1;
//     } else {
//         int digits = 0;
//         int scale = 10;
//         while (std::trunc(value * scale) == 0 && scale <= 1e9) {
//             digits++;
//             scale *= 10;
//         }
//         return digits + 1; // one digit after first non-zero
//     }
// }
// std::string formatStatNumber(double value) {

//     int precision = getDisplayPrecision(value);
//     return report::streamToString([&](std::ostream& os) {
//         os << std::fixed << std::setprecision(precision);
//         os << value;
//     });

//     std::cout << std::fixed << std::setprecision(precision);

// }

std::string formatStatsNumber(double value) {
    // Convert to full-precision fixed string
    std::ostringstream oss;
    oss << std::fixed << std::setprecision(20) << value;
    std::string str = oss.str();

    // Trim trailing zeros and decimal point
    str.erase(str.find_last_not_of('0') + 1);
    if (str.back() == '.')
        str.pop_back();

    // ≥ 1.0 → keep up to one decimal place
    if (value >= 1.0) {
        size_t dot = str.find('.');
        if (dot != std::string::npos && dot + 2 < str.size()) {
            str = str.substr(0, dot + 2);
        }
        return str;
    }

    // < 1.0 → keep first non-zero decimal + 1 digit
    size_t dot = str.find('.');
    if (dot == std::string::npos)
        return str;

    size_t firstNonZero = dot + 1;
    while (firstNonZero < str.size() && str[firstNonZero] == '0') {
        firstNonZero++;
    }

    if (firstNonZero + 1 < str.size()) {
        str = str.substr(0, firstNonZero + 2);
    }

    return str;
}

void showPercent(double percent, std::ostream& ss) {
    ss << formatStatsNumber(percent * 100) << "%";
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

    os << StreamColor::bold << formatStatsNumber(st.value) << StreamColor::clear
       << "\n";

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

pir::PirType makeExpectedType(const pir::PirType& staticType,
                              const pir::PirType& feedbackType) {
    pir::PirType expected = staticType & feedbackType;

    // Reflecting what happens in TypeTest::Create
    if (staticType.maybeNAOrNaN() && !expected.maybeNAOrNaN() &&
        !expected.isSimpleScalar()) {
        expected = expected.orNAOrNaN();
    }

    return expected;
}

pir::PirType SlotUsed::expectedType() const {
    return makeExpectedType(*staticType, *feedbackType);
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

    if (slotUsed.hoistedForce) {
        os << "(hoisted force)\n";
    }

    return os;
}

// ------------------------------------------------------------

SlotPresent::Type SlotPresent::type() const {
    if (feedbackType->isA(*staticType)) {
        return FB_isA_ST;
    }

    if ((*staticType & *feedbackType).isVoid()) {
        return FB_ST_Disjoint;
    }

    auto expected = makeExpectedType(*staticType, *feedbackType);
    if (feedbackType->isA(expected)) {
        return FB_TooPolluted;
    } else {
        return FB_TooPolluted_Narrowed;
    }
}

// ------------------------------------------------------------

std::unordered_multiset<pir::PirType>
FunctionInfo::getFeedbackTypesBag() const {
    std::unordered_multiset<pir::PirType> result;
    for (const auto& kv : allTypeSlots) {
        result.insert(kv.second);
    }

    return result;
}

size_t
FunctionInfo::dependentsCountIn(std::unordered_set<FeedbackIndex> slots) const {

    auto allFeedbackTypesBag = getFeedbackTypesBag();
    size_t result = 0;
    std::unordered_set<rir::pir::PirType> types;

    for (auto s : slots) {
        auto t = allTypeSlots.at(s);
        if (allFeedbackTypesBag.count(t) > 1) {
            types.insert(t);
            result++;
        }
    }

    if (result > 0) {
        result = result - types.size();
    }

    return result;
}

// ------------------------------------------
std::ostream& operator<<(std::ostream& os, const Aggregate& agg) {
    // clang-format off
    //assert(agg.optimizedAway.value >= agg.optimizedAwayNonEmpty.value);
    return os << agg.referenced << agg.referencedNonEmpty << agg.readNonEmpty
              << agg.used << agg.presentNonEmpty
              << "\n"

              << StreamColor::blue << "Used\n" << StreamColor::clear
              << agg.exactMatch << agg.widened << agg.narrowed
              << "\n"

              << StreamColor::blue << "Unused\n" << StreamColor::clear
              << agg.unused
              << agg.optimizedAway
              << agg.unusedNonEmpty
              << agg.optimizedAwayNonEmpty
              << agg.dependent
              << agg.unusedOther
              << agg.preciseType
              << agg.preciseTypeNonEmpty
              << agg.preciseTypePresentNonEmpty
              << "\n"

              << StreamColor::blue << "Polymorphic\n" << StreamColor::clear
              << agg.polymorphic << agg.polymorphicUsed;
    // clang-format on
}

// ------------------------------------------------------------

template <typename MapType>
std::unordered_set<typename MapType::key_type> getKeys(const MapType& m) {
    std::unordered_set<typename MapType::key_type> keys;
    for (const auto& pair : m) {
        keys.insert(pair.first);
    }
    return keys;
}

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

        bool polymorphic = info.polymorphicSlots.count(i.first);

        if (usage.exactMatch()) {
            agg.exactMatch++;

            if (polymorphic) {
                agg.polymorphicExactMatch++;
            }
        } else {
            if (usage.narrowedWithStaticType()) {
                agg.narrowed++;

                if (polymorphic) {
                    agg.polymorphicNarrowed++;
                }
            }

            if (usage.widened()) {
                agg.widened++;

                if (polymorphic) {
                    agg.polymorphicWidened++;
                }
            }
        }
    }

    // Unused
    auto unusedSlots = difference(keys(info.allTypeSlots), keys(slotsUsed));
    agg.unused = unusedSlots.size();
    auto unusedNonEmpty = intersect(unusedSlots, info.nonEmptySlots);

    agg.unusedNonEmpty = unusedNonEmpty.size();

    auto presentNonEmpty = intersect(keys(slotPresent), unusedNonEmpty);
    agg.presentNonEmpty = presentNonEmpty.size();

    auto allFeedbackTypesBag = info.getFeedbackTypesBag();

    // precise type
    auto preciseTypeSlotsNonEmpty = intersect(preciseTypeSlots, unusedNonEmpty);
    auto preciseTypeSlotsPresentNonEmpty =
        intersect(preciseTypeSlotsNonEmpty, getKeys(slotPresent));

    agg.preciseType = preciseTypeSlots.size();
    agg.preciseTypeNonEmpty = preciseTypeSlotsNonEmpty.size();
    agg.preciseTypePresentNonEmpty = preciseTypeSlotsPresentNonEmpty.size();

    auto optimizedAwayNonEmptySlots =
        difference(unusedNonEmpty, getKeys(slotPresent));
    agg.optimizedAwayNonEmpty = optimizedAwayNonEmptySlots.size();

    auto optimizedAwaySlots =
        difference(getKeys(info.allTypeSlots), getKeys(slotPresent));
    agg.optimizedAway = optimizedAwaySlots.size();

    assert(optimizedAwaySlots.size() >= optimizedAwayNonEmptySlots.size());
    assert(agg.optimizedAway.value >= agg.optimizedAwayNonEmpty.value);

    agg.dependent = info.dependentsCountIn(unusedNonEmpty);
    agg.unusedOther.value = unusedNonEmpty.size() - agg.dependent.value;

    // polymorphic
    agg.polymorphic = info.polymorphicSlots.size();
    agg.polymorphicUsed =
        intersect(info.polymorphicSlots, keys(slotsUsed)).size();
    agg.polymorphicUnused =
        intersect(info.polymorphicSlots, unusedNonEmpty).size();

    return agg;
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

            if (tf.isPolymorphic) {
                slotData.polymorphicSlots.insert(idx);
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

    average(optimizedAwayRatio, optimizedAwayNonEmpty, unusedNonEmpty);
    average(dependentRatio, dependent, unusedNonEmpty);
    average(unusedOtherRatio, unusedOther, unusedNonEmpty);

    average(polymorphicRatio, polymorphic, referencedNonEmpty);
    average(polymorphicOutOfUsedRatio, polymorphicUsed, used);
    average(polymorphicOutOfUnusedRatio, polymorphicUnused, unusedNonEmpty);
    average(polymorphicUsedRatio, polymorphicUsed, polymorphic);

    average(polymorphicOutOfExactMatchRatio, polymorphicExactMatch, exactMatch);
    average(polymorphicOutOfNarrowedRatio, polymorphicNarrowed, narrowed);
    average(polymorphicOutOfWidenedRatio, polymorphicWidened, widened);

    average(usedNonemptyRatio, used, referencedNonEmpty);
#undef average

    for (auto f : res.sums.universe) {
        res.deoptsCount += f->allDeoptsCount;
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

            if (info.polymorphicSlots.count(index)) {
                os << " [polymorphic]";
            }

            os << StreamColor::bold << " <" << feedbackType << ">\n"
               << StreamColor::clear;

            if (used) {
                os << stats.slotsUsed.at(index);
            } else {
                bool otherReason = false;

                bool isDependency =
                    info.getFeedbackTypesBag().count(feedbackType) > 1;
                if (!stats.slotPresent.count(index)) {
                    os << "optimized away\n";
                    otherReason = false;
                } else {
                    auto present = stats.slotPresent.at(index);
                    os << StreamColor::bold << present.presentInstr
                       << StreamColor::clear << "\n";
                }

                if (isDependency) {
                    os << "dependent slot\n";
                    otherReason = false;
                }

                if (otherReason) {
                    os << "other unused reason\n";
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
        << "\n";

    finalHeader("Unused slots");
    os
        << final.sums.unused
        << final.sums.optimizedAway
        << final.sums.unusedNonEmpty
        << final.sums.optimizedAwayNonEmpty
        << final.sums.dependent
        << final.sums.unusedOther
        << final.sums.preciseType
        << final.sums.preciseTypeNonEmpty
        << final.sums.preciseTypePresentNonEmpty
        << "\n";

    os
        << final.sums.optimizedAway / final.sums.unused
        << final.sums.optimizedAwayNonEmpty / final.sums.unusedNonEmpty
        << final.sums.dependent / final.sums.unusedNonEmpty
        << final.sums.unusedOther / final.sums.unusedNonEmpty
        << final.sums.preciseTypeNonEmpty / final.sums.unusedNonEmpty

        << "\n";

    finalHeader("Unused slots - Averaged per closure version");
    os  << final.optimizedAwayRatio
        << final.dependentRatio
        << final.unusedOtherRatio
        << "\n";

    finalHeader("Polymorphic slots");
    os  << final.sums.polymorphic
        << final.sums.polymorphicUsed
        << "\n";

    os  << final.sums.polymorphic / final.sums.referencedNonEmpty
        << final.sums.polymorphicUsed / final.sums.used
        << final.sums.polymorphicUsed / final.sums.polymorphic
        << "\n";

    finalHeader("Polymorphic slots - Averaged per closure version");
    os  << final.polymorphicRatio
        << final.polymorphicOutOfUsedRatio
        << final.polymorphicUsedRatio
        << "\n";
    // clang-format on
}

void reportCsv(std::ostream& os, const std::string& program_name,
               const std::vector<DispatchTable*>& DTs) {
    os.seekp(0, std::ios::end);
    if (os.tellp() == 0) {
        // clang-format off
        os  << "benchmark"
            << ",closures"
            << ",compiled closures"
            << ",closure compilations"
            << ",benefited compilations"
            << ",deopts"
            << "\n";
        // clang-format on
    }

    auto agg = CompilationSession::getFinalAgg();

    // clang-format off
    os
        << "\"" << program_name << "\""
        << "," << DTs.size()
        << "," << agg.sums.universe.size()
        << "," << agg.compiledClosureVersions.value
        << "," << agg.benefitedClosureVersions.value
        << "," << agg.deoptsCount.value
        << "\n";
    // clang-format on
}

void reportPerSlot(std::ostream& os, const std::string& benchmark_name) {
    os.seekp(0, std::ios::end);
    if (os.tellp() == 0) {
        // clang-format off
        os  << "benchmark,compilation id,closure,slot idx"
            << ",non-empty,read,used,polymorphic"
            // How used
            << ",exact match,widened,narrowed"
            // Unused
            << ",optimized away,dependent"
            // Unused non-optimized away
            << ",too polluted,isA static,disjoint,unused narrowed,considered"
            // Types
            << ",staticT,feedbackT"
            // Used types
            << ",checkForT,expectedT,requiredT"
            << "\n";
        // clang-format on
    }

    auto out = [&](const auto& x, bool quote = false, bool first = false) {
        if (!first) {
            os << ",";
        }

        if (quote) {
            os << "\"";
        }

        os << x;

        if (quote) {
            os << "\"";
        }
    };

    auto qout = [&](const auto& x, bool first = false) { out(x, true, first); };

    auto out_bool = [&](bool b) {
        if (b) {
            os << "," << 1;
        } else {
            os << "," << 0;
        }
    };

    auto false_fields = [&](size_t count) {
        for (size_t i = 0; i < count; i++) {
            out_bool(false);
        }
    };

    auto empty_fields = [&](size_t count) {
        for (size_t i = 0; i < count; i++) {
            os << ",";
        }
    };

    size_t compilation_id = 0;

    for (auto& session : COMPILATION_SESSIONS) {
        auto& session_info = session.functionsInfo;

        for (auto& closure_compilation : session.closureVersionStats) {
            for (auto& i : closure_compilation.feedbackStats) {
                auto& closure = i.first;
                auto& feedback_info = i.second;
                auto& static_info = session_info[closure];

                auto feedback_types_bags = static_info.getFeedbackTypesBag();

                for (auto& j : sortByFeedbackIndex(static_info.allTypeSlots)) {
                    auto& slot = j.first;
                    auto& slot_type = j.second;

                    // ID
                    qout(benchmark_name, true);
                    out(compilation_id);
                    qout(closure->dispatchTable()->closureName);
                    out(slot.idx);

                    bool non_empty = !static_info.emptySlots.count(slot);
                    bool used = feedback_info.slotsUsed.count(slot);

                    // Info
                    out_bool(non_empty);
                    out_bool(feedback_info.slotsRead.count(slot));
                    out_bool(used);
                    out_bool(static_info.polymorphicSlots.count(slot));

                    if (used) {
                        auto& usage = feedback_info.slotsUsed[slot];

                        // How used
                        out_bool(usage.exactMatch());
                        out_bool(usage.widened());
                        out_bool(usage.narrowedWithStaticType());

                        // Unused
                        false_fields(2);

                        // Unused non-optimized away
                        false_fields(5);

                        // Types
                        qout(*usage.staticType);
                        qout(*usage.feedbackType);

                        // Used types
                        qout(*usage.checkFor);
                        qout(usage.expectedType());
                        qout(*usage.requiredType);
                    } else {
                        // How used
                        false_fields(3);

                        // Unused
                        auto present = feedback_info.slotPresent.count(slot);
                        out_bool(!present);
                        out_bool(non_empty &&
                                 feedback_types_bags.count(slot_type) > 1);

                        if (!present || !non_empty) {
                            // Unused non-optimized away
                            false_fields(5);

                            // Types, Used types
                            empty_fields(5);
                        } else {
                            bool tooPolluted = false;
                            bool isAStatic = false;
                            bool disjoint = false;
                            bool narrowed = false;

                            auto presentInfo = feedback_info.slotPresent[slot];

                            switch (presentInfo.type()) {
                            case SlotPresent::FB_isA_ST:
                                isAStatic = true;
                                break;

                            case SlotPresent::FB_ST_Disjoint:
                                disjoint = true;
                                break;

                            case SlotPresent::FB_TooPolluted:
                                tooPolluted = true;
                                break;

                            case SlotPresent::FB_TooPolluted_Narrowed:
                                narrowed = true;
                                tooPolluted = true;
                                break;
                            }

                            // Unused non-optimized away
                            out_bool(tooPolluted);
                            out_bool(isAStatic);
                            out_bool(disjoint);
                            out_bool(narrowed);
                            out_bool(presentInfo.considered);

                            // Types
                            qout(*presentInfo.staticType);
                            qout(*presentInfo.feedbackType);

                            // Used types
                            empty_fields(3);
                        }
                    }
                    os << "\n";
                }
            }
            compilation_id++;
        }
    }
}

} // namespace report
} // namespace rir
