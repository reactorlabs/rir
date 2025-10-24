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
std::string getClosureName(SEXP cls);

std::string streamToString(std::function<void(std::stringstream&)> f);
std::string typeToString(const pir::PirType& t);
std::string instrToString(pir::Instruction* instr);

pir::PirType getSlotPirType(size_t i, Function* baseline);
pir::PirType getSlotPirType(const FeedbackOrigin& origin);

std::unordered_set<FeedbackIndex> findAllSlots(Code* code);

// ------------------------------------------------------------

using Universe = std::unordered_set<Function*>;

// ------------------------------------------------------------

struct Aggregate {
    Universe universe{};

    size_t referenced = 0;
    size_t referencedNonEmpty = 0;
    size_t readNonEmpty = 0;
    size_t used = 0;

    void operator+=(const Aggregate& other) {
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

struct SlotInfo {
    // type, field name, field column descriptor
#define SLOT_INFOS(X)                                                          \
    /* ID */                                                                   \
    X(std::string, benchmark, "benchmark")                                     \
    X(size_t, compilation_id, "compilation id")                                \
    X(std::string, closure, "closure")                                         \
    X(size_t, slot_idx, "slot idx")                                            \
                                                                               \
    /* Info */                                                                 \
    X(bool, nonempty, "non-empty")                                             \
    X(bool, read, "read")                                                      \
    X(bool, used, "used")                                                      \
                                                                               \
    /* More info */                                                            \
    X(bool, inlinee, "inlinee")                                                \
    X(bool, inPromise, "in promise")                                           \
    /* X(bool, polymorphic, "polymorphic") */                                  \
                                                                               \
    /* Present info */                                                         \
    X(bool, widened, "widened")                                                \
                                                                               \
    /* How used */                                                             \
    X(bool, exactMatch, "exact match")                                         \
    X(bool, narrowed, "narrowed")                                              \
    X(std::string, slotUsedSource, "slot used source")                         \
                                                                               \
    /* Unused */                                                               \
    X(bool, notPresent, "not present")                                         \
    X(bool, promiseInlined, "promise inlined")                                 \
    /* X(bool, dependent, "dependent") */                                      \
                                                                               \
    /* Unused present non-empty */                                             \
    /* X(bool, expectedEmpty, "expected empty") */                             \
    /* X(bool, expectedIsStatic, "expected is static") */                      \
    /* X(bool, canBeSpeculated, "can be speculated") */                        \
    X(std::string, speculationPhase, "speculation phase")                      \
                                                                               \
    /* Types */                                                                \
    X(std::string, inferredT, "inferredT")                                     \
    X(std::string, observedT, "observedT")                                     \
    X(std::string, expectedT, "expectedT")                                     \
                                                                               \
    /* Used types */                                                           \
    X(std::string, checkForT, "checkForT")                                     \
    X(std::string, requiredT, "requiredT")                                     \
                                                                               \
    /* Unused types*/                                                          \
    X(std::string, widenedT, "widenedT")                                       \
                                                                               \
    /* Instruction */                                                          \
    X(std::string, instruction, "instruction")

#define X(type, name, _id) type name{};
    SLOT_INFOS(X)
#undef X

    void print(std::ostream& os) const;
    static void header(std::ostream& os);
};

// ------------------------------------------------------------

enum SlotUsedSource {
    FinalVersion,
    Proto,
    Patch,
};

std::ostream& operator<<(std::ostream& os, const SlotUsedSource source);

struct SlotUsed {
    bool widened() const;
    bool narrowedWithStaticType() const;
    bool exactMatch() const { return !widened() && !narrowedWithStaticType(); }

    pir::PirType* checkFor = nullptr;
    pir::PirType* inferredType = nullptr;
    pir::PirType* observedType = nullptr;
    pir::PirType* requiredType = nullptr;

    pir::PirType expectedType() const;

    std::string speculatedOn;
    std::string assumeInstr;

    bool hoistedForce;
    SlotUsedSource source;

    SlotUsed() {}
};

std::ostream& operator<<(std::ostream& os, const SlotUsed& slotUsed);

// Ordering matters -> weakest to strongest phases
// It might be that no place, non-type heuristic and early typecheck fail are
// uncomparable
enum SpeculationPhase {
    // The type speculation pass did not run
    NotRun,

    // The typeFeedbackUsed was set outside of type speculations
    ExternallySet,

    // Either static is feedback or speculation was considered (type related
    // heuristic could've failed)
    RunTypeObserved,

    // There is either no available checkpoint or no position for typecheck
    RunNoPlace,

    // Some non-type related heuristics failed
    RunNonTypeHeuristicFailed,

    // The early typecheck in TypeSpeculation has failed
    RunEarlyTypecheckFail,
};

std::ostream& operator<<(std::ostream& os, const SpeculationPhase speculation);

struct SlotPresent {
    bool canBeSpeculated() const;
    bool widened() const;

    // int compareExpectedTypeToStaticType() const;

    pir::PirType* inferredType = nullptr;
    pir::PirType* observedType = nullptr;

    pir::PirType expectedType() const;
    pir::PirType widenExpected() const;

    SpeculationPhase speculation;
    std::string presentInstr;

    SlotPresent() {}
};

std::ostream& operator<<(std::ostream& os, const SlotPresent& slotPresent);

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

    std::unordered_set<FeedbackIndex> promiseSlots;

    // TODO: not used probably remove
    std::unordered_set<FeedbackIndex> slotsDeopted;
    std::unordered_set<FeedbackIndex> inlinedSlotsDeopted;
    size_t deoptsCount;

    // TODO: not used by current analysis, probabluy refactor
    std::unordered_multiset<pir::PirType> getFeedbackTypesBag() const;
    size_t dependentsCountIn(std::unordered_set<FeedbackIndex> slots) const;
};

// ------------------------------------------------------------

struct FeedbackStatsPerFunction {
    std::unordered_map<FeedbackIndex, std::vector<SlotUsed>>
        finalVersionSlotsUsed;
    std::unordered_map<FeedbackIndex, std::vector<SlotUsed>> protoSlotsUsed;
    std::unordered_set<FeedbackIndex> slotsRead;
    std::unordered_map<FeedbackIndex, std::vector<SlotPresent>> slotsPresent;
    std::unordered_set<FeedbackIndex> slotsAssumeRemoved;
    std::unordered_set<FeedbackIndex> slotsPromiseInlined;

    // TODO: not used right now
    std::unordered_set<FeedbackIndex> preciseTypeSlots;

    size_t timesInlined = 0;

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

    void perSlotInfo(const std::string& benchmark_name, size_t compilation_id,
                     std::unordered_map<Function*, FunctionInfo>& session_info,
                     std::function<void(const SlotInfo&)> consume);
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

size_t currentSessionId();

// ------------------------------------------------------------

void report(std::ostream& os, bool breakdownInfo,
            const std::vector<DispatchTable*>& DTs);
void reportCsv(std::ostream& os, const std::string& name,
               const std::vector<DispatchTable*>& DTs);
void reportPerSlot(std::ostream& os, const std::string& benchmark_name);

// ------------------------------------------------------------

struct RecordedUsedSlots {
    /// Add all slots
    bool all;
    /// Indicies of slots to use
    const std::unordered_set<size_t>& slots;
};

RecordedUsedSlots getUsedSlotsFor(const std::string& closure_name);

// Only use the names that are identifyable at RIR compile time
bool useRIRNames();

} // namespace report
} // namespace rir

#endif
