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

    void set(const Stat& other) { this->value = other.value; }

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

    void add(double value) { values.push_back(value); }
    void add(const MetricPercent& metric) {
        if (metric.denominator->value) {
            values.push_back(metric.value());
        }
    }

    void operator+=(const FunctionAggregate& other) {
        for (const auto& i : other.values) {
            values.push_back(i);
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
    Stat referenced{"referenced slots"};
    Stat read{"read slots"};
    Stat used{"used slots"};

    Stat referencedNonEmpty{"referenced non-empty slots"};
    Stat readNonEmpty{"read non-empty slots"};
    Stat usedNonEmpty{"used non-empty slots"};

    Stat optimizedAway{"optimized away slots"};
    Stat dependent{"dependent slots"};
    Stat unusedOther{"other reasons unused slots"};

    Stat optimizedAwayNonEmpty{"optimized away non-empty slots"};
    Stat dependentNonEmpty{"dependent non-empty slots"};
    Stat unusedOtherNonEmpty{"other reasons non-empty slots"};

    void operator+=(const Aggregate& other) {
        referenced += other.referenced;
        read += other.read;
        used += other.used;

        referencedNonEmpty += other.referencedNonEmpty;
        readNonEmpty += other.readNonEmpty;
        usedNonEmpty += other.usedNonEmpty;

        optimizedAway += other.optimizedAway;
        dependent += other.dependent;
        unusedOther += other.unusedOther;

        optimizedAwayNonEmpty += other.optimizedAwayNonEmpty;
        dependentNonEmpty += other.dependentNonEmpty;
        unusedOtherNonEmpty += other.unusedOtherNonEmpty;
    }
};

std::ostream& operator<<(std::ostream& os, const Aggregate& agg);

struct FinalAggregate {
    Universe universe;
    Stat compiledClosureVersions{"closure version compilations"};
    Stat benefitedClosureVersions{
        "closure version compilations using some type feedback"};

    Stat referenced{"referenced"};
    Stat read{"read"};
    Stat used{"used"};

    Stat referencedNonEmpty{"referenced non-empty"};
    Stat readNonEmpty{"read non-empty"};
    Stat usedNonEmpty{"used non-empty"};

    FunctionAggregate referencedNonEmptyRatio{
        "referenced non-empty / referenced"};
    FunctionAggregate readRatio{"read non-empty / referenced"};
    FunctionAggregate usedRatio{"used non-empty / referenced"};

    Stat optimizedAway{"optimized away"};
    Stat dependent{"dependent"};
    Stat unusedOther{"other reasons"};

    Stat optimizedAwayNonEmpty{"optimized away non-empty"};
    Stat dependentNonEmpty{"dependent non-empty"};
    Stat unusedOtherNonEmpty{"other reasons non-empty"};

    void operator+=(const FinalAggregate& other) {
        universe.insert(other.universe.begin(), other.universe.end());
        compiledClosureVersions += other.compiledClosureVersions;
        benefitedClosureVersions += other.benefitedClosureVersions;

        referenced += other.referenced;
        read += other.read;
        used += other.used;

        referencedNonEmpty += other.referencedNonEmpty;
        readNonEmpty += other.readNonEmpty;
        usedNonEmpty += other.usedNonEmpty;

        referencedNonEmptyRatio += other.referencedNonEmptyRatio;
        readRatio += other.readRatio;
        usedRatio += other.usedRatio;

        optimizedAway += other.optimizedAway;
        dependent += other.dependent;
        unusedOther += other.unusedOther;

        optimizedAwayNonEmpty += other.optimizedAwayNonEmpty;
        dependentNonEmpty += other.dependentNonEmpty;
        unusedOtherNonEmpty += other.unusedOtherNonEmpty;
    }

    static FinalAggregate from(const Aggregate& agg) {
        FinalAggregate res;

        res.referenced.set(agg.referenced);
        res.read.set(agg.read);
        res.used.set(agg.used);

        res.referencedNonEmpty.set(agg.referencedNonEmpty);
        res.readNonEmpty.set(agg.readNonEmpty);
        res.usedNonEmpty.set(agg.usedNonEmpty);

        res.optimizedAway.set(agg.optimizedAway);
        res.dependent.set(agg.dependent);
        res.unusedOther.set(agg.unusedOther);

        res.optimizedAwayNonEmpty.set(agg.optimizedAwayNonEmpty);
        res.dependentNonEmpty.set(agg.dependentNonEmpty);
        res.unusedOtherNonEmpty.set(agg.unusedOtherNonEmpty);

        return res;
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
    std::unordered_set<pir::PirType> getUsedFeedbackTypes() const;
};

// ------------------------------------------------------------

struct ClosureVersionStats {
    // Baseline of the compiled function
    Function* function;
    Context context;

    std::unordered_map<Function*, FeedbackStatsPerFunction> feedbackStats;

    Universe universe() const;

    ClosureVersionStats(
        Function* function, const Context& context,
        const std::unordered_map<Function*, FeedbackStatsPerFunction>&
            feedbackStats)
        : function(function), context(context), feedbackStats(feedbackStats) {}

    Aggregate
    getAgg(std::unordered_map<Function*, FunctionInfo>& functionsInfo);

    FinalAggregate
    getFinalAgg(std::unordered_map<Function*, FunctionInfo>& functionsInfo);
};

// ------------------------------------------------------------

struct CompilationSession {
    // the info passed to PirCompile
    Function* function;
    Context context;

    std::unordered_map<Function*, FunctionInfo> functionsInfo;
    std::vector<ClosureVersionStats> closuresVersionStats;

    CompilationSession(Function* function, const Context& context)
        : function(function), context(context) {}

    void addClosureVersion(pir::ClosureVersion* closureVersion,
                           Function* compiledFunction);

    static CompilationSession& getNew(Function* compiledFunction,
                                      const Context& compiledContext,
                                      const std::vector<DispatchTable*>& DTs);

    Universe universe() const;

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
