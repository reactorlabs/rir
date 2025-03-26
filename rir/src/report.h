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

std::string streamToString(std::function<void(std::stringstream&)> f);

// ------------------------------------------------------------

struct MetricPercent;

struct Stat {
    std::string name;
    size_t value = 0;

    void operator++(int) { value++; }
    void operator+=(size_t add) { value += add; }
    void operator+=(const Stat& other) { value += other.value; }

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
struct SlotNotUsedSubsumedStaticTypeReason {
    std::string staticType;
    std::string feedbackType;
    bool equalTypes; // equal types or more strict

    bool fromContext;
    Context ctx;
    std::string fromInstruction;
};

struct SlotCandidateButNotUsedReason {
    bool hasUsefulFeedbackInfo;
    bool reqFulfilledWithoutSpec;
};

struct SlotOptimizedAway {};

struct SlotUsed {
    enum Kind { exactMatch, widened } kind;

    bool narrowedWithStaticType;

    pir::PirType* checkFor;
    pir::PirType* staticType;
    pir::PirType* feedbackType;
    pir::PirType* expectedType;
    pir::PirType* requiredType;

    std::string speculatedOn;
    std::string assumeInstr;

    void finalize(pir::Instruction* speculatedOn, pir::Instruction* assumeInstr);

    SlotUsed();
    SlotUsed(bool narrowedWithStaticType, SlotUsed::Kind kind,
             const pir::PirType& checkFor, const pir::PirType& staticType,
             const pir::PirType& feedbackType, const pir::PirType& expectedType,
             const pir::PirType& requiredType);
};

std::ostream& operator<<(std::ostream& os, const SlotUsed& slotUsed);

// ------------------------------------------------------------

using Universe = std::unordered_set<Function*>;

// ------------------------------------------------------------

struct Aggregate {
    Stat read{"read slots"};
    Stat readNonEmpty{"read non-empty slots"};
    Stat used{"used slots"};
    Stat usedNonEmpty{"used non-empty slots"};

    void operator+=(const Aggregate& other) {
        read += other.read;
        readNonEmpty += other.readNonEmpty;
        used += other.used;
        usedNonEmpty += other.usedNonEmpty;
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
    std::unordered_set<FeedbackIndex> allTypeSlots;
    std::unordered_set<FeedbackIndex> emptySlots;
    std::unordered_set<FeedbackIndex> nonEmptySlots;
    std::unordered_set<FeedbackIndex> slotsDeopted;
    std::unordered_set<FeedbackIndex> inlinedSlotsDeopted;
    size_t deoptsCount;
};

// ------------------------------------------------------------

struct FeedbackStatsPerFunction {
    std::unordered_map<FeedbackIndex, SlotNotUsedSubsumedStaticTypeReason>
        slotsReadNotUsedStaticTypeReason;

    std::unordered_map<FeedbackIndex, SlotCandidateButNotUsedReason>
        slotsReadCandidateNotUsedReason;

    std::unordered_map<FeedbackIndex, SlotOptimizedAway> slotsOptimizedAway;
    std::unordered_map<FeedbackIndex, SlotUsed> slotsUsed;
    std::unordered_set<FeedbackIndex> slotsRead;

    Aggregate getAgg(const FunctionInfo& info) const;
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

} // namespace report
} // namespace rir

#endif
