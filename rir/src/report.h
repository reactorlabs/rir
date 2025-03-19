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
    std::string instructionAsString;

    SlotUsed();
    SlotUsed(bool narrowedWithStaticType, SlotUsed::Kind kind,
             const pir::PirType& checkFor, const pir::PirType& staticType,
             const pir::PirType& feedbackType, const pir::PirType& expectedType,
             const pir::PirType& requiredType, pir::Instruction& instruction);

    void print(std::ostream&) const;
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
};

// ------------------------------------------------------------

using Universe = std::unordered_set<Function*>;

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
};

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
    size_t allTypeSlots; // TODO: as set of slots
    std::unordered_set<FeedbackIndex> emptySlots;
    std::unordered_set<FeedbackIndex> slotsDeopted;
    std::unordered_set<FeedbackIndex> inlinedSlotsDeopted;
    size_t deoptsCount;
};

struct CompilationSession {
    // ? the info passed to PirCompile
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
                                      std::vector<DispatchTable*> DTs);

    Universe universe() const;
};

// ------------------------------------------------------------

struct Stat {
    std::string name;
    size_t value = 0;

    void operator++(int) { value++; }
    void operator+=(size_t add) { value += add; }
    void operator+=(const Stat& other) { value += other.value; }
};

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

MetricPercent operator/(Stat& lhs, Stat& rhs);

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

std::ostream& operator<<(std::ostream& os, const Stat& st);
std::ostream& operator<<(std::ostream& os, const MetricPercent& metric);
std::ostream& operator<<(std::ostream& os, const FunctionAggregate& agg);

// ------------------------------------------------------------

void report(std::ostream& os, bool breakdownInfo);

} // namespace report
} // namespace rir

#endif
