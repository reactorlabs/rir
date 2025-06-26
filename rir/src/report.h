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

    bool hoistedForce;

    SlotUsed() {}
};

std::ostream& operator<<(std::ostream& os, const SlotUsed& slotUsed);

struct SlotPresent {
    std::string presentInstr;

    pir::PirType* staticType = nullptr;
    pir::PirType* feedbackType = nullptr;

    bool considered = false;
    bool create = false;
    bool emited = false;

    bool isReturned = false;

    enum Type { FB_isA_ST, ST_isA_FB, FB_ST_Disjoint, Narrowed };

    Type type() const;

    bool canBeSpeculated() const;

    SlotPresent() {}
};

std::ostream& operator<<(std::ostream& os, const SlotPresent& slotPresent);

// ------------------------------------------------------------

using Universe = std::unordered_set<Function*>;

// ------------------------------------------------------------

struct Aggregate {
    Universe universe{};

    size_t referenced = 0;
    size_t referencedNonEmpty = 0;
    size_t readNonEmpty = 0;
    size_t used = 0;

    void operator+=(Aggregate other) {
        universe.insert(other.universe.begin(), other.universe.end());

        this->referenced += other.referenced;
        this->referencedNonEmpty += other.referencedNonEmpty;
        this->readNonEmpty += other.readNonEmpty;
        this->used += other.used;
    }
};

std::ostream& operator<<(std::ostream& os, const Aggregate& agg);

struct FinalAggregate {
    Universe universe{};

    size_t compiledClosureVersions = 0;
    size_t benefitedClosureVersions = 0;
    size_t deoptsCount = 0;
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

    std::unordered_set<FeedbackIndex> polymorphicSlots;

    std::unordered_set<FeedbackIndex> slotsDeopted;
    std::unordered_set<FeedbackIndex> inlinedSlotsDeopted;
    size_t deoptsCount;

    std::unordered_multiset<pir::PirType> getFeedbackTypesBag() const;
    size_t dependentsCountIn(std::unordered_set<FeedbackIndex> slots) const;
};

// ------------------------------------------------------------

struct FeedbackStatsPerFunction {
    std::unordered_map<FeedbackIndex, SlotUsed> slotsUsed;
    std::unordered_set<FeedbackIndex> slotsRead;
    std::unordered_map<FeedbackIndex, SlotPresent> slotPresent;
    std::unordered_set<FeedbackIndex> preciseTypeSlots;

    Aggregate getAgg(const FunctionInfo& info) const;
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
void reportCsv(std::ostream& os, const std::string& name,
               const std::vector<DispatchTable*>& DTs);
void reportPerSlot(std::ostream& os, const std::string& benchmark_name);

} // namespace report
} // namespace rir

#endif
