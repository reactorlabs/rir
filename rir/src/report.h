#ifndef REPORT_H
#define REPORT_H

#include "runtime/Context.h"
#include "runtime/DispatchTable.h"
#include "runtime/TypeFeedback.h"
#include <numeric>
#include <string>
#include <unordered_map>
#include <unordered_set>

namespace rir {

namespace pir {
struct PirType;
struct Instruction;
} // namespace pir

namespace report {

// Helpers
std::string streamToString(std::function<void(std::stringstream&)> f);
pir::PirType getSlotPirType(size_t i, Function* baseline);
pir::PirType getSlotPirType(const FeedbackOrigin& origin);

// ------------------------------------------------------------

struct MetricPercent;

struct Stat {
    std::string name;
    size_t value = 0;

    void operator++(int) { value++; }
    void operator+=(size_t add) { value += add; }
    void operator+=(const Stat& other) { value += other.value; }

    Stat& operator=(size_t value) {
        this->value = value;
        return *this;
    }

    MetricPercent operator/(Stat& denom);
};

struct MetricPercent {
    Stat* numerator;
    Stat* denominator;
    std::string name = "";

    MetricPercent& named(const std::string& name) {
        this->name = name;
        return *this;
    }

    double value() const;
};

struct FunctionAggregate {
    std::string name;
    std::vector<double> values{};

    void add(const MetricPercent& metric) {
        if (metric.denominator->value) {
            values.push_back(metric.value());
        }
    }

    double average() const;
};

std::ostream& operator<<(std::ostream& os, const Stat& st);
std::ostream& operator<<(std::ostream& os, const MetricPercent& metric);
std::ostream& operator<<(std::ostream& os, const FunctionAggregate& agg);

// ------------------------------------------------------------

// TODO: strings to types
//
// struct SlotNotUsedSubsumedStaticTypeReason {
//     std::string staticType;
//     std::string feedbackType;
//     bool equalTypes; // equal types or more strict
//
//     bool fromContext;
//     Context ctx;
//     std::string fromInstruction;
// };
//
// struct SlotCandidateButNotUsedReason {
//     bool hasUsefulFeedbackInfo;
//     bool reqFulfilledWithoutSpec;
// };
//
// struct SlotOptimizedAway {};

struct SlotUsed {
    bool widened() const;
    bool narrowedWithStaticType() const;

    bool exactMatch() const { return !widened() && !narrowedWithStaticType(); }

    pir::PirType* checkFor = nullptr;
    pir::PirType* staticType = nullptr;
    pir::PirType* feedbackType = nullptr;
    pir::PirType* requiredType = nullptr;

    pir::PirType expectedType() const;

    std::string speculatedOn;
    std::string assumeInstr;

    SlotUsed() {}
};

struct SlotPresent {
    std::string presentInstr;

    SlotPresent() {}
};

std::ostream& operator<<(std::ostream& os, const SlotUsed& slotUsed);

// ------------------------------------------------------------

using Universe = std::unordered_set<Function*>;

// ------------------------------------------------------------

struct Aggregate {
    Universe universe;

    Stat referenced{"referenced"};
    Stat referencedNonEmpty{"referenced non-empty"};
    // Stat read{"read"};
    Stat readNonEmpty{"read non-empty"};

    Stat used{"used"};
    Stat unusedNonEmpty{"unused non-empty"};
    Stat slotPresentNonEmpty{"present non-empty"};

    Stat exactMatch{"exact match"};
    Stat widened{"widened"};
    Stat narrowed{"narrowed"};
    Stat widenedNarrowed{"widened narrowed"};

    Stat optimizedAway{"optimized away non-empty"};
    Stat dependent{"dependent"};
    Stat unusedOther{"other reasons unused non-empty"};

    Stat polluted{"polluted"};
    Stat pollutedUsed{"used polluted"};

    std::vector<Stat*> stats() {
        // clang-format off
        return {
            &referenced,
            &referencedNonEmpty,
            &readNonEmpty,

            &used,
            &unusedNonEmpty,
            &slotPresentNonEmpty,

            &exactMatch,
            &widened,
            &narrowed,
            &widenedNarrowed,

            &optimizedAway,
            &dependent,
            &unusedOther,

            &polluted,
            &pollutedUsed
        };
        // clang-format on
    }

    void operator+=(Aggregate other) {
        universe.insert(other.universe.begin(), other.universe.end());
        auto thisV = this->stats();
        auto otherV = other.stats();

        for (size_t i = 0; i < thisV.size(); i++) {
            *thisV[i] += *otherV[i];
        }
    }
};

std::ostream& operator<<(std::ostream& os, const Aggregate& agg);

struct FinalAggregate {
    Aggregate sums;

    Stat compiledClosureVersions{"closure version compilations"};
    Stat benefitedClosureVersions{
        "closure version compilations using some type feedback"};

    FunctionAggregate referencedNonEmptyRatio;
    FunctionAggregate readRatio;
    FunctionAggregate usedRatio;

    FunctionAggregate optimizedAwayRatio;
    FunctionAggregate dependentRatio;
    FunctionAggregate unusedOtherRatio;

    FunctionAggregate pollutedRatio;
    FunctionAggregate pollutedOutOfUsedRatio;
    FunctionAggregate pollutedUsedRatio;

    std::vector<Stat const*> stats() {
        return {&compiledClosureVersions, &benefitedClosureVersions};
    }

    std::vector<FunctionAggregate const*> aggregates() const {
        return {
            &referencedNonEmptyRatio,
            &readRatio,
            &usedRatio,

            &optimizedAwayRatio,
            &dependentRatio,
            &unusedOtherRatio,

            &pollutedRatio,
            &pollutedOutOfUsedRatio,
            &pollutedUsedRatio,
        };
    }
};

// ------------------------------------------------------------

/*
f <- function()
    g()

pir code:
...
assume [g#type1]

deopt on this assume

g.baseline()->inlinedSlotsDeopted.add(type1)
*/
struct FunctionInfo {
    std::unordered_map<FeedbackIndex, pir::PirType> allTypeSlots;
    std::unordered_set<FeedbackIndex> emptySlots;
    std::unordered_set<FeedbackIndex> nonEmptySlots;

    std::unordered_set<FeedbackIndex> pollutedSlots;

    std::unordered_set<FeedbackIndex> slotsDeopted;
    std::unordered_set<FeedbackIndex> inlinedSlotsDeopted;
    size_t deoptsCount;
};

// ------------------------------------------------------------

struct FeedbackStatsPerFunction {
    // std::unordered_map<FeedbackIndex, SlotNotUsedSubsumedStaticTypeReason>
    //     slotsReadNotUsedStaticTypeReason;
    //
    // std::unordered_map<FeedbackIndex, SlotCandidateButNotUsedReason>
    //     slotsReadCandidateNotUsedReason;
    //
    // std::unordered_map<FeedbackIndex, SlotOptimizedAway> slotsOptimizedAway;

    std::unordered_map<FeedbackIndex, SlotUsed> slotsUsed;
    std::unordered_set<FeedbackIndex> slotsRead;
    std::unordered_map<FeedbackIndex, SlotPresent> slotPresent;

    Aggregate getAgg(const FunctionInfo& info) const;
    std::unordered_multiset<pir::PirType>
    getUFeedbackTypesBag(const FunctionInfo& functionInfo) const;
};

// ------------------------------------------------------------

struct ClosureVersionStats {
    // Baseline of the compiled function
    Function* function;
    Context context;

    std::unordered_map<Function*, FeedbackStatsPerFunction> feedbackStats;

    ClosureVersionStats(
        Function* function, const Context& context,
        const std::unordered_map<Function*, FeedbackStatsPerFunction>&
            feedbackStats)
        : function(function), context(context), feedbackStats(feedbackStats) {}

    Aggregate
    getAgg(std::unordered_map<Function*, FunctionInfo>& functionsInfo);
};

// ------------------------------------------------------------

struct CompilationSession {
    // the info passed to PirCompile
    Function* function;
    Context context;

    std::unordered_map<Function*, FunctionInfo> functionsInfo;
    std::vector<ClosureVersionStats> closureVersionStats;

    CompilationSession(Function* function, const Context& context)
        : function(function), context(context) {}

    void addClosureVersion(pir::ClosureVersion* closureVersion,
                           Function* compiledFunction);

    static CompilationSession& getNew(Function* compiledFunction,
                                      const Context& compiledContext,
                                      const std::vector<DispatchTable*>& DTs);

    static FinalAggregate getFinalAgg();
};

// ------------------------------------------------------------

void report(std::ostream& os, bool breakdownInfo,
            const std::vector<DispatchTable*>& DTs);
void reportCsv(std::ostream& os, const std::string& name);
void reportIndividual(std::ostream& os, const std::string& benchmark_name);

} // namespace report
} // namespace rir

#endif
