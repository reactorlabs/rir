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

void SlotUsed::finalize(pir::Instruction* speculatedOn,
                        pir::Instruction* assumeInstr) {
    this->speculatedOn =
        streamToString([&](std::stringstream& ss) { speculatedOn->print(ss); });

    this->assumeInstr =
        streamToString([&](std::stringstream& ss) { assumeInstr->print(ss); });
}

SlotUsed::SlotUsed() {}
SlotUsed::SlotUsed(bool narrowedWithStaticType, SlotUsed::Kind kind,
                   const pir::PirType& checkFor, const pir::PirType& staticType,
                   const pir::PirType& feedbackType,
                   const pir::PirType& expectedType,
                   const pir::PirType& requiredType) {
    this->narrowedWithStaticType = narrowedWithStaticType;
    this->kind = kind;

    this->checkFor = new pir::PirType(checkFor);
    this->staticType = new pir::PirType(staticType);
    this->feedbackType = new pir::PirType(feedbackType);
    this->expectedType = new pir::PirType(expectedType);
    this->requiredType = new pir::PirType(requiredType);
}

std::ostream& operator<<(std::ostream& os, const SlotUsed& slotUsed) {
    using namespace StreamColor;

    os << bold << slotUsed.speculatedOn << clear << "\n";
    os << "narrowed with static type: "
       << boolToString(slotUsed.narrowedWithStaticType) << "\n";
    os << "exact match/widened: "
       << (slotUsed.kind == SlotUsed::exactMatch ? "exact match" : "widened")
       << "\n";

    // clang-format off
    os << bold << "checkFor: " << clear << *slotUsed.checkFor << ", "
       << bold << "static: "   << clear << *slotUsed.staticType << ", "
       << bold << "feedback: " << clear << *slotUsed.feedbackType << ", "
       << bold << "expected: " << clear << *slotUsed.expectedType << ", "
       << bold << "required: " << clear << *slotUsed.requiredType << "\n";
    // clang-format on
    return os;
}

// ------------------------------------------------------------

std::ostream& operator<<(std::ostream& os, const Aggregate& agg) {
    return os << agg.read << agg.readNonEmpty << agg.used << agg.usedNonEmpty;
}

// ------------------------------------------------------------

Aggregate FeedbackStatsPerFunction::getAgg(const FunctionInfo& info) const {
    Aggregate agg;

    agg.read += slotsRead.size();
    agg.readNonEmpty += intersect(info.nonEmptySlots, slotsRead).size();
    agg.used += slotsUsed.size();
    agg.usedNonEmpty += intersect(info.nonEmptySlots, keys(slotsUsed)).size();

    return agg;
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
    FinalAggregate res;

    auto agg = getAgg(functionsInfo);

    res.universe = universe();
    for (auto fun : res.universe) {
        auto& info = functionsInfo[fun];

        res.referenced += info.allTypeSlots.size();
        res.referencedNonEmpty += info.nonEmptySlots.size();
    }

    res.read += agg.read;
    res.readNonEmpty += agg.readNonEmpty;
    res.used += agg.used;
    res.usedNonEmpty += agg.usedNonEmpty;

    res.referencedNonEmptyRatio.add(res.referencedNonEmpty / res.referenced);
    res.readRatio.add(res.readNonEmpty / res.referenced);
    res.usedRatio.add(res.usedNonEmpty / res.referenced);

    res.compiledClosureVersions++;
    if (res.used.value != 0) {
        res.benefitedClosureVersions++;
    }

    return res;
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

            slotData.allTypeSlots.insert(idx);
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
    auto printFunctionInfo = [&](DispatchTable* dt,
                                 const FeedbackStatsPerFunction& stats,
                                 FunctionInfo& info, bool isInlinee = false) {
        // Header
        os << StreamColor::yellow;
        if (isInlinee) {
            os << "Inlinee: ";
        }
        os << dt->closureName << " [" << dt << "] " << StreamColor::clear;

        // Static slots info
        os << "(# of slots: " << info.allTypeSlots.size()
           << ", # of non-empty: " << info.nonEmptySlots.size() << ")\n";

        // Compilation slots info
        os << stats.getAgg(info) << "\n";

        if (breakdownInfo) {
            auto slotsUsed = sortByFeedbackIndex(stats.slotsUsed);
            for (auto& i : slotsUsed) {
                auto& index = i.first;
                auto& slotUsed = i.second;

                os << StreamColor::red << index << StreamColor::clear << "\n"
                   << slotUsed << "\n";
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

            // Sum info
            auto onUniverse =
                [&](std::function<size_t(const FunctionInfo&)> apply) {
                    size_t s = 0;
                    for (auto f : cvstat.universe()) {
                        s += apply(session.functionsInfo[f]);
                    }
                    return s;
                };

            size_t referenced = onUniverse(
                [](const FunctionInfo& i) { return i.allTypeSlots.size(); });
            size_t nonEmpty = onUniverse(
                [](const FunctionInfo& i) { return i.nonEmptySlots.size(); });

            // clang-format off
            os << Stat{"referenced slots: ", referenced}
               << Stat{"referenced non-empty slots: ", nonEmpty}
               << cvstat.getAgg(session.functionsInfo)
               << "\n";
            // clang-format on

            // Main
            auto& mainInfo = session.functionsInfo[mainFun];
            auto& mainFeedbackStats = cvstat.feedbackStats[mainFun];
            printFunctionInfo(mainFun->dispatchTable(), mainFeedbackStats,
                              mainInfo);
            os << "\n";

            // Rest
            for (auto& i : cvstat.feedbackStats) {
                auto fun = i.first;
                auto& info = session.functionsInfo[fun];
                auto& feedbackStats = i.second;

                if (fun == mainFun) {
                    continue;
                }

                os << "----------------\n";
                printFunctionInfo(fun->dispatchTable(), feedbackStats, info,
                                  true);
                os << "\n";
            }
        }
    }

    os << StreamColor::magenta
       << "----------------------- Summary ------------------------\n"
       << StreamColor::clear;

    auto agg = CompilationSession::getFinalAgg();

    // clang-format off
    os  << Stat{"total functions (RIR compiled)", DTs.size()}
        << Stat{"compiled functions (PIR compiled)", agg.universe.size()}
        << agg.compiledClosureVersions
        << agg.benefitedClosureVersions
        << "\n";

    os << StreamColor::blue << "Slots\n" << StreamColor::clear;
    os  << agg.referenced
        << agg.read
        << agg.used
        << "\n";

    os << StreamColor::blue << "Non-empty slots\n" << StreamColor::clear;
    os  << agg.referencedNonEmpty
        << agg.readNonEmpty
        << agg.usedNonEmpty
        << "\n";

    os  << agg.referencedNonEmpty / agg.referenced
        << agg.readNonEmpty / agg.referenced
        << agg.usedNonEmpty / agg.referenced
        << "\n";

    os << StreamColor::blue << "Averaged per closure version\n" << StreamColor::clear;
    os << agg.referencedNonEmptyRatio
       << agg.readRatio
       << agg.usedRatio;
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
    out(agg.usedRatio.average(), true);
}

} // namespace report
} // namespace rir
