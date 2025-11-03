#include "report.h"
#include "R/Protect.h"
#include "compiler/pir/closure_version.h"
#include "compiler/pir/instruction.h"
#include "interpreter/cache.h"
#include "runtime/DispatchTable.h"
#include "runtime/Function.h"
#include "utils/Terminal.h"

#include <iostream>
#include <queue>

extern "C" SEXP R_GetVarLocValue(R_varloc_t);

extern std::vector<rir::DispatchTable*> PreservedDispatchTables;

namespace rir {
namespace report {

// ------------------------------------------------------------
// CLOSURE NAME HELPER
// ------------------------------------------------------------

std::string getClosureName(SEXP cls) {
    std::string name = "";

    Protect p;

    auto env = p(CLOENV(cls));
    if (env == NULL || env == R_NilValue) {
        return name;
    }

    // 1. Look thru frames
    auto frame = p(FRAME(env));
    if (frame != NULL && frame != R_NilValue) {
        auto frameList = RList(frame);

        for (auto e = frameList.begin(); e != frameList.end(); ++e) {
            if (*e == cls) {
                name = CHAR(PRINTNAME(e.tag()));
                if (!name.empty()) {
                    return name;
                }
            }
        }
    }

    // 2. Try to look thru symbols
    auto symbols = p(R_lsInternal3(env, TRUE, FALSE));

    auto size = Rf_length(symbols);
    for (int i = 0; i < size; i++) {
        const char* symbol_char = CHAR(VECTOR_ELT(symbols, i));

        auto symbol = PROTECT(Rf_install(symbol_char));
        auto loc = R_findVarLocInFrame(env, symbol);

        SEXP cellValue;
        if (R_VARLOC_IS_NULL(loc)) {
            UNPROTECT(1);
            continue;
        } else if (IS_ACTIVE_BINDING(loc.cell)) {
            cellValue = loc.cell;
        } else {
            cellValue = R_GetVarLocValue(loc);
        }
        UNPROTECT(1);

        if (TYPEOF(cellValue) == PROMSXP) {
            cellValue = PRVALUE(cellValue);
        }

        if (cellValue == cls) {
            name = symbol_char;
            break;
        }
    }

    return name;
}

// ------------------------------------------------------------
// TYPE HELPERS
// ------------------------------------------------------------

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

pir::PirType makeExpectedType(const pir::PirType& inferredType,
                              const pir::PirType& observedType) {
    pir::PirType expected = inferredType & observedType;

    // Reflecting what happens in TypeTest::Create
    if (inferredType.maybeNAOrNaN() && !expected.maybeNAOrNaN() &&
        !expected.isSimpleScalar()) {
        expected = expected.orNAOrNaN();
    }

    return expected;
}

bool isSimpleNumericType(const pir::PirType& expectedType) {
    return !expectedType.maybeObj() &&
           (expectedType.noAttribsOrObject().isA(pir::RType::integer) ||
            expectedType.noAttribsOrObject().isA(pir::RType::real) ||
            expectedType.noAttribsOrObject().isA(pir::RType::logical));
}

pir::PirType makeWidenedType(const pir::PirType& inferredType,
                             const pir::PirType& expectedType,
                             bool earlyTypeCheckFail) {

    if (earlyTypeCheckFail) {
        return inferredType;
    }

    if (isSimpleNumericType(expectedType)) {
        return expectedType;
    }

    auto checkFor = inferredType.notLazy().noAttribsOrObject();
    if (expectedType.isA(checkFor)) {
        return checkFor;
    }

    checkFor = inferredType.notLazy().notObject();
    if (expectedType.isA(checkFor)) {
        return checkFor;
    }

    return inferredType;
}

bool isWidened(const pir::PirType& inferredType,
               const pir::PirType& observedType, bool earlyTypeCheckFail) {
    auto intersection = inferredType & observedType;

    // Widened by the NA check
    auto expected = makeExpectedType(inferredType, observedType);
    if (!expected.isA(intersection)) {
        return true;
    }

    if (isSimpleNumericType(expected)) {
        return false;
    }

    auto widened = makeWidenedType(inferredType, expected, earlyTypeCheckFail);
    if (!widened.isA(intersection)) {
        return true;
    }

    return false;
}

// ------------------------------------------------------------
// SLOT HELPERS
// ------------------------------------------------------------

std::unordered_set<FeedbackIndex> findAllSlots(Code* code) {
    std::unordered_set<FeedbackIndex> slots;

    for (auto pc = code->code(); pc < code->endCode(); pc = BC::next(pc)) {
        auto bc = BC::decode(pc, code);
        if (bc.bc == Opcode::record_type_) {
            slots.insert(FeedbackIndex::type(bc.immediate.i));
        }
    }

    return slots;
}

// ------------------------------------------------------------
// SET HELPERS
// ------------------------------------------------------------

template <typename K, typename V>
std::unordered_set<K> keys(const std::unordered_map<K, V>& map) {
    std::unordered_set<K> res;

    for (const auto& i : map) {
        res.insert(i.first);
    }

    return res;
}

template <typename K, typename V>
std::unordered_set<K> keys_nonempty(const std::unordered_map<K, V>& map) {
    std::unordered_set<K> res;

    for (const auto& i : map) {
        if (i.second.size()) {
            res.insert(i.first);
        }
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

template <typename T>
void printUnorderedSet(const std::unordered_set<T>& mySet) {
    for (const auto& item : mySet) {
        std::cerr << item << std::endl;
    }
}

// ------------------------------------------------------------
// SLOT USED
// ------------------------------------------------------------

pir::PirType SlotUsed::expectedType() const {
    return makeExpectedType(*inferredType, *observedType);
}

bool SlotUsed::widened() const {
    // The actual checkFor could be even more widened
    return isWidened(*inferredType, *observedType, false) ||
           (*checkFor != expectedType());
}

bool SlotUsed::narrowedWithStaticType() const {
    return !(observedType->isA(*inferredType));
}

// ------------------------------------------------------------
// SLOT PRESENT
// ------------------------------------------------------------

// int SlotPresent::compareExpectedTypeToStaticType() const {
//     // returns 0 is exp == st
//     /// return -1 if  exp < st
//     // fails otherwise. That should not happen
//     if (expectedType() == *inferredType) {
//         return 0;
//     }
//     assert(expectedType().isA( *inferredType));
//     return -1;
// }

pir::PirType SlotPresent::expectedType() const {
    return makeExpectedType(*inferredType, *observedType);
}

pir::PirType SlotPresent::widenExpected() const {
    return makeWidenedType(*inferredType, expectedType(),
                           speculation ==
                               SpeculationPhase::RunEarlyTypecheckFail);
}

bool SlotPresent::widened() const {
    return isWidened(*inferredType, *observedType,
                     speculation == SpeculationPhase::RunEarlyTypecheckFail);
}

bool SlotPresent::canBeSpeculated() const {
    auto expected = expectedType();

    if (expected.isVoid() || expected.maybeLazy()) {
        return false;
    }

    auto widened = widenExpected();
    return expected.isA(widened) && widened != *inferredType;
}

// ------------------------------------------------------------
// FUNCTION INFO
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

// ------------------------------------------------------------
// COLLECT INFO
// ------------------------------------------------------------

static std::vector<CompilationSession> COMPILATION_SESSIONS;

void computeFunctionInfo(
    std::unordered_map<Function*, FunctionInfo>& functionsInfo,
    DispatchTable* dt) {
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

    // Promise scan
    std::unordered_set<FeedbackIndex> codeSlots =
        findAllSlots(baseline->body());

    slotData.promiseSlots = difference(keys(slotData.allTypeSlots), codeSlots);

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

CompilationSession&
CompilationSession::getNew(Function* compiledFunction,
                           const Context& compiledContext,
                           const std::vector<DispatchTable*>& DTs) {
    COMPILATION_SESSIONS.emplace_back(compiledFunction, compiledContext);
    auto& session = COMPILATION_SESSIONS.back();
    for (auto dt : DTs) {
        computeFunctionInfo(session.functionsInfo, dt);
    }
    return session;
}

size_t currentSessionId() { return COMPILATION_SESSIONS.size() - 1; }

// ------------------------------------------------------------

void CompilationSession::addClosureVersion(pir::ClosureVersion* closureVersion,
                                           Function* compiledFunction) {

    assert(closureVersion->context() == compiledFunction->context());
    auto baseline = compiledFunction->dispatchTable()->baseline();
    const auto& context = compiledFunction->context();

    const auto& stats = closureVersion->feedbackStatsByFunction;

    for (auto& f : keys(stats)) {
        if (!functionsInfo.count(f)) {
            computeFunctionInfo(functionsInfo, f->dispatchTable());
        }
    }

    closureVersionStats.emplace_back(baseline, context, stats);
}

// ------------------------------------------------------------
// AGGREGATES
// ------------------------------------------------------------

Aggregate FeedbackStatsPerFunction::getAgg(const FunctionInfo& info) const {
    Aggregate agg;

    // Static info
    agg.referenced = info.allTypeSlots.size();
    agg.referencedNonEmpty =
        intersect(info.nonEmptySlots, keys(info.allTypeSlots)).size();
    agg.readNonEmpty = intersect(info.nonEmptySlots, slotsRead).size();
    agg.used = std::count_if(finalVersionSlotsUsed.begin(),
                             finalVersionSlotsUsed.end(),
                             [](auto& i) { return i.second.size(); });

    assert(keys_nonempty(finalVersionSlotsUsed) ==
               intersect(info.nonEmptySlots,
                         keys_nonempty(finalVersionSlotsUsed)) &&
           "There is an empty used slot");

    assert(slotsPromiseInlined ==
               intersect(info.promiseSlots, slotsPromiseInlined) &&
           "Inlined promise, not from promise");

    // if (slotsUsed.size() == 9) {
    //     std::cerr << "unused non empty: " <<  unusedNonEmpty.size();
    //     std::cerr << "\n";
    //     std::cerr << "slot present: " <<  keys(slotPresent).size() << "\n";
    //     //printUnorderedSet(keys(slotPresent));
    //     std::cerr << "\n";
    //     std::cerr << "slot present non-empty: " <<  presentNonEmpty.size();
    //     std::cerr << "\n";
    //        std::cerr << "slot present & unused non empty: " <<
    //            intersect(keys(slotPresent), unusedNonEmpty).size();
    //        std::cerr << "\n";
    //     std::cerr << "slotUsed: " <<  keys(slotsUsed).size();
    //     //printUnorderedSet(keys(slotsUsed));

    //     std::cerr << "\n";
    //     std::cerr << "presentNonEmpty & slotUsed: " <<
    //     intersect(keys(slotsUsed),presentNonEmpty).size();

    //     std::cerr << "\n";

    //     std::cerr << "\n";

    // assert(false);
    //}
    return agg;
}

Aggregate ClosureVersionStats::getAgg(
    std::unordered_map<Function*, FunctionInfo>& functionsInfo) {
    Aggregate agg;

    agg.universe = keys(feedbackStats);

    for (auto& i : feedbackStats) {
        agg += i.second.getAgg(functionsInfo[i.first]);
    }

    return agg;
}
FinalAggregate CompilationSession::getFinalAgg() {
    FinalAggregate res;

    for (auto i : COMPILATION_SESSIONS) {
        for (auto j : i.closureVersionStats) {
            auto agg = j.getAgg(i.functionsInfo);

            res.universe.insert(agg.universe.begin(), agg.universe.end());

            res.compiledClosureVersions++;
            if (agg.used > 0) {
                res.benefitedClosureVersions++;
            }
        }
    }

    for (auto f : res.universe) {
        res.deoptsCount += f->allDeoptsCount;
    }

    return res;
}

// ------------------------------------------------------------
// SLOTS INFOS
// ------------------------------------------------------------

void ClosureVersionStats::perSlotInfo(
    const std::string& benchmark_name, size_t compilation_id,
    std::unordered_map<Function*, FunctionInfo>& session_info,
    std::function<void(const SlotInfo&)> consume) {

    for (auto& i : this->feedbackStats) {
        auto& closure = i.first;
        auto& feedback_info = i.second;
        auto& static_info = session_info[closure];

        auto feedback_types_bags = static_info.getFeedbackTypesBag();

        for (auto& j : sortByFeedbackIndex(static_info.allTypeSlots)) {
            auto& slot = j.first;
            // auto& slot_type = j.second;

            SlotInfo res;

            // ID
            res.benchmark = benchmark_name;
            res.compilation_id = compilation_id;
            res.closure = closure->dispatchTable()->closureName;
            if (UseRIRNames::value) {
                assert(res.closure.size());
            }
            res.slot_idx = slot.idx;

            // Info
            res.nonempty = !static_info.emptySlots.count(slot);
            res.read = feedback_info.slotsRead.count(slot);

            bool hasSlotUsed = feedback_info.finalVersionSlotsUsed[slot].size();
            bool hasProtoSlotUsed = feedback_info.protoSlotsUsed[slot].size();
            res.used = hasSlotUsed || hasProtoSlotUsed;

            // More info
            res.inlinee = closure != this->function;
            res.inPromise = static_info.promiseSlots.count(slot);
            // res.polymorphic = static_info.polymorphicSlots.count(slot);

            if (res.used) {
                auto consumeSlotUsed =
                    [&](const std::vector<SlotUsed>& slotsUsed) {
                        for (const auto& usage : slotsUsed) {
                            // Present info
                            res.widened = usage.widened();

                            // How used
                            res.exactMatch = usage.exactMatch();
                            res.narrowed = usage.narrowedWithStaticType();
                            res.slotUsedSource = streamToString(
                                [&](std::ostream& os) { os << usage.source; });
                            res.speculation = usage.speculation;

                            // Unused defaults
                            res.promiseInlined =
                                feedback_info.slotsPromiseInlined.count(slot);

                            // (used && slot from promise) => promise inlined
                            assert(!res.inPromise || res.promiseInlined);

                            // Types
                            res.inferredT = typeToString(*usage.inferredType);
                            res.observedT = typeToString(*usage.observedType);
                            res.expectedT = typeToString(usage.expectedType());

                            // Used types
                            res.checkForT = typeToString(*usage.checkFor);
                            res.requiredT = typeToString(*usage.requiredType);

                            // Instruction
                            res.instruction = usage.speculatedOn;

                            consume(res);
                        }
                    };

                if (hasSlotUsed) {
                    consumeSlotUsed(feedback_info.finalVersionSlotsUsed[slot]);
                } else {
                    assert(hasProtoSlotUsed);
                    consumeSlotUsed(feedback_info.protoSlotsUsed[slot]);
                }

            } else {
                // Unused
                res.notPresent = !feedback_info.slotsPresent[slot].size();
                res.promiseInlined =
                    feedback_info.slotsPromiseInlined.count(slot);
                // res.dependent =
                //     res.nonempty && feedback_types_bags.count(slot_type) > 1;

                // Unused present non-empty
                if (!res.notPresent && res.nonempty) {
                    for (const auto& presentInfo :
                         feedback_info.slotsPresent[slot]) {
                        auto subRes = res;

                        auto expected = presentInfo.expectedType();

                        // Present info
                        subRes.widened = presentInfo.widened();

                        // Unused present non-empty
                        // subRes.expectedEmpty = expected.isVoid();
                        // subRes.expectedIsStatic =
                        //     expected == *presentInfo.inferredType;
                        assert(expected.isA(*presentInfo.inferredType) &&
                               "expected is not <= static");

                        // subRes.canBeSpeculated =
                        // presentInfo.canBeSpeculated();
                        subRes.speculationPhase =
                            streamToString([&](std::ostream& os) {
                                os << presentInfo.speculation;
                            });

                        // Types
                        subRes.inferredT =
                            typeToString(*presentInfo.inferredType);
                        subRes.observedT =
                            typeToString(*presentInfo.observedType);
                        subRes.expectedT = typeToString(expected);

                        // Unused types
                        subRes.widenedT =
                            typeToString(presentInfo.widenExpected());

                        // Instruction
                        subRes.instruction = presentInfo.presentInstr;
                        consume(subRes);
                    }
                } else {
                    // Unused not present non-empty
                    consume(res);
                }
            }
        }
    }
}

// ------------------------------------------------------------
// COLORS
// ------------------------------------------------------------

namespace StreamColor {
#define COLOR(color)                                                           \
    struct {                                                                   \
    } color;                                                                   \
                                                                               \
    std::ostream& operator<<(std::ostream& os, decltype(StreamColor::color)) { \
        auto forceColor = std::getenv("STATS_COLOR");                          \
        if ((forceColor != nullptr && std::string(forceColor) == "1") ||       \
            ConsoleColor::isTTY(os)) {                                         \
            ConsoleColor::color(os);                                           \
        }                                                                      \
        return os;                                                             \
    }

COLOR(red)
COLOR(yellow)
COLOR(blue)
COLOR(magenta)
COLOR(clear)
COLOR(bold)

#undef COLOR

}; // namespace StreamColor

// ------------------------------------------------------------
// PRINT FORMAT
// ------------------------------------------------------------

std::string streamToString(std::function<void(std::stringstream&)> f) {
    std::stringstream ss;
    f(ss);
    return ss.str();
};

std::string typeToString(const pir::PirType& t) {
    return streamToString([&](std::ostream& os) { os << t; });
}

std::string instrToString(pir::Instruction* instr) {
    return streamToString([&](std::ostream& os) { instr->print(os); });
}

std::string boolToString(bool b) { return b ? "yes" : "no"; }

void printStat(std::ostream& os, const std::string& name, size_t value) {
    os << name << ": " << StreamColor::bold << value << StreamColor::clear
       << "\n";
}

std::ostream& operator<<(std::ostream& os, const SpeculationPhase speculation) {
    switch (speculation) {
    case NotRun:
        os << "not run";
        break;
    case ExternallySet:
        os << "externally set";
        break;
    case RunTypeObserved:
        os << "type observed";
        break;
    case RunNoPlace:
        os << "no place";
        break;
    case RunNonTypeHeuristicFailed:
        os << "non-type heuristic fail";
        break;
    case RunEarlyTypecheckFail:
        os << "early type-check fail";
        break;
    default:
        assert(false);
    }
    return os;
}

std::ostream& operator<<(std::ostream& os, const SlotUsedSource source) {
    switch (source) {
    case FinalVersion:
        os << "final version";
        break;
    case Proto:
        os << "proto";
        break;
    case Patch:
        os << "patch";
        break;
    default:
        assert(false);
    }

    return os;
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
       << bold << "static: "   << clear << *slotUsed.inferredType << ", "
       << bold << "feedback: " << clear << *slotUsed.observedType << ", "
       << bold << "expected: " << clear << slotUsed.expectedType() << ", "
       << bold << "required: " << clear << *slotUsed.requiredType << "\n";
    // clang-format on

    if (slotUsed.hoistedForce) {
        os << "(hoisted force)\n";
    }
    return os;
}

std::ostream& operator<<(std::ostream& os, const SlotPresent& slotPresent) {
    using namespace StreamColor;

    os << bold << slotPresent.presentInstr << clear << "\n";
    os << slotPresent.speculation << "\n";

    // clang-format off
    os << bold << "static: "   << clear << *slotPresent.inferredType << ", "
       << bold << "feedback: " << clear << *slotPresent.observedType << ", "
       << bold << "expected: " << clear << slotPresent.expectedType() << ", "
       << bold << "widened: "  << clear << slotPresent.widenExpected() << "\n";
    // clang-format on

    return os;
}

std::ostream& operator<<(std::ostream& os, const Aggregate& agg) {
    printStat(os, "referenced", agg.referenced);
    printStat(os, "referenced non-empty", agg.referencedNonEmpty);
    printStat(os, "read non-empty", agg.readNonEmpty);
    printStat(os, "used", agg.used);
    return os;
}

// ------------------------------------------------------------
// OUTPUT
// ------------------------------------------------------------

void report(std::ostream& os, bool breakdownInfo,
            const std::vector<DispatchTable*>& DTs) {
    auto closureName = [](Function* fun) {
        return fun->dispatchTable()->closureName;
    };

    auto printSlotBreakdown =
        [&](const FeedbackIndex& index, const pir::PirType& observedType,
            FeedbackStatsPerFunction& stats, FunctionInfo& info) {
            bool used = stats.finalVersionSlotsUsed[index].size();

            os << StreamColor::red << index << StreamColor::clear;

            if (info.polymorphicSlots.count(index)) {
                os << " [polymorphic]";
            }

            if (info.getFeedbackTypesBag().count(observedType) > 1) {
                os << " [dependent]";
            }

            os << StreamColor::bold << " <" << observedType << ">\n"
               << StreamColor::clear;

            if (used) {
                for (const auto& i : stats.finalVersionSlotsUsed.at(index)) {
                    os << i;
                }
            } else {
                if (stats.slotsPresent[index].size()) {
                    for (const auto& i : stats.slotsPresent.at(index)) {
                        os << i;
                    }
                } else {
                    os << "not present\n";
                }
            }
        };

    auto printFunctionInfo = [&](DispatchTable* dt,
                                 FeedbackStatsPerFunction& stats,
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
                auto& observedType = i.second;
                printSlotBreakdown(index, observedType, stats, info);
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
            assert(session.functionsInfo.count(mainFun));
            auto& mainInfo = session.functionsInfo[mainFun];
            auto& mainFeedbackStats = cvstat.feedbackStats[mainFun];
            printFunctionInfo(mainFun->dispatchTable(), mainFeedbackStats,
                              mainInfo);

            // Rest
            for (auto& i : cvstat.feedbackStats) {
                auto fun = i.first;
                assert(session.functionsInfo.count(fun));
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

    printStat(os, "total functions (RIR compiled)", DTs.size());
    printStat(os, "compiled functions (PIR compiled)", final.universe.size());
    printStat(os, "closure version compilations",
              final.compiledClosureVersions);
    printStat(os, "closure version compilations using some type feedback",
              final.benefitedClosureVersions);
    printStat(os, "deoptimizations", final.deoptsCount);
}

// ------------------------------------------------------------
// CSV OUTPUT
// ------------------------------------------------------------

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
    os  << "\"" << program_name << "\""
        << "," << DTs.size()
        << "," << agg.universe.size()
        << "," << agg.compiledClosureVersions
        << "," << agg.benefitedClosureVersions
        << "," << agg.deoptsCount
        << "\n";
    // clang-format on
}

// ------------------------------------------------------------
// SLOT OUTPUT
// ------------------------------------------------------------

void SlotInfo::header(std::ostream& os) {
    bool first = true;

#define X(_type, _name, id)                                                    \
    {                                                                          \
        if (first) {                                                           \
            first = false;                                                     \
        } else {                                                               \
            os << ",";                                                         \
        }                                                                      \
        os << id;                                                              \
    }

    SLOT_INFOS(X)
#undef X
    os << "\n";
}

void SlotInfo::print(std::ostream& os) const {
    struct {
        bool first = true;

        void comma(std::ostream& os) {
            if (first) {
                first = false;
            } else {
                os << ",";
            }
        }

        void operator()(std::ostream& os, std::string str) {
            comma(os);
            if (str.size() == 0) {
                return;
            }

            for (size_t i = 0; i < str.size(); i++) {
                if (str[i] == '"') {
                    str[i] = '\'';
                } else if (str[i] == ',') {
                    str[i] = ';';
                }
            }

            // Replace newlines
            std::string::size_type pos;
            while ((pos = str.find('\n')) != std::string::npos) {
                str.replace(pos, 1, "\\n");
            }

            os << "\"" << str << "\"";
        }

        void operator()(std::ostream& os, size_t i) {
            comma(os);
            os << i;
        }

        void operator()(std::ostream& os, bool b) {
            comma(os);
            os << (b ? 1 : 0);
        }
    } out;

#define X(_type, name, _id) out(os, name);
    SLOT_INFOS(X)
#undef X
    os << "\n";
}

void reportPerSlot(std::ostream& os, const std::string& benchmark_name) {
    os.seekp(0, std::ios::end);
    if (os.tellp() == 0) {
        SlotInfo::header(os);
    }

    size_t compilation_id = 0;

    for (auto& session : COMPILATION_SESSIONS) {
        auto& session_info = session.functionsInfo;

        for (auto& closure_compilation : session.closureVersionStats) {
            closure_compilation.perSlotInfo(
                benchmark_name, compilation_id, session_info,
                [&](const SlotInfo& info) { info.print(os); });
            compilation_id++;
        }
    }
}

// ------------------------------------------------------------
// SUBSUMED SLOTS
// ------------------------------------------------------------

struct StronglyConnectedComponents {
    std::unordered_map<FeedbackOrigin, size_t> componentsForSlot;
    std::unordered_map<size_t, SmallSet<FeedbackOrigin>> components;
    std::unordered_map<size_t, SmallSet<size_t>> componentParents;
};

StronglyConnectedComponents stronglyConnectedComponents(
    const SmallSet<FeedbackOrigin>& allSlots,
    const std::unordered_map<FeedbackOrigin, SmallSet<FeedbackOrigin>>& parents,
    const std::unordered_map<FeedbackOrigin, SmallSet<FeedbackOrigin>>&
        children) {
    // Kosaraju's algorithm
    std::vector<FeedbackOrigin> stack;
    stack.reserve(allSlots.size());

    // Sort
    {
        SmallSet<FeedbackOrigin> visited;

        std::function<void(const FeedbackOrigin&)> visit =
            [&](const FeedbackOrigin& slot) {
                if (visited.includes(slot)) {
                    return;
                }

                visited.insert(slot);

                if (parents.count(slot)) {
                    for (const auto& i : parents.at(slot)) {
                        visit(i);
                    }
                }

                stack.push_back(slot);
            };

        for (const auto& slot : allSlots) {
            visit(slot);
        }
    }

    std::unordered_map<FeedbackOrigin, size_t> componentsForSlot;
    std::unordered_map<size_t, SmallSet<FeedbackOrigin>> components;
    std::unordered_map<size_t, SmallSet<size_t>> componentParents;

    // Collect components
    size_t currentComponent = 0;

    std::function<void(const FeedbackOrigin&)> visit =
        [&](const FeedbackOrigin& slot) {
            componentsForSlot[slot] = currentComponent;
            components[currentComponent].insert(slot);

            if (children.count(slot)) {
                for (const auto& child : children.at(slot)) {

                    // Child has already been visited
                    if (componentsForSlot.count(child)) {
                        auto childComponent = componentsForSlot[child];
                        if (childComponent != currentComponent) {
                            // Add an edge from the child component to this
                            // component
                            componentParents[childComponent].insert(
                                currentComponent);
                        }
                    } else {
                        visit(child);
                    }
                }
            }
        };

    for (auto i = stack.rbegin(); i != stack.rend(); ++i) {
        if (componentsForSlot.count(*i)) {
            continue;
        }

        visit(*i);
        currentComponent++;
    }

    return {componentsForSlot, components, componentParents};
}

void reportSubsumedSlots(std::ostream& os) {
    // First we need to create the graph vertices (parents, children)
    //
    // Since slots depend cyclically on each other a lot, we need to separate
    // them into strongly connected components (~ an equivalence class).
    //
    // If the component has some other components as a parent components, we
    // take the subsuming slot fromm them (one of them).
    // Otherwise we take one of the slots in the component.

    std::unordered_map<FeedbackOrigin, SmallSet<FeedbackOrigin>> parents;
    std::unordered_map<FeedbackOrigin, SmallSet<FeedbackOrigin>> children;
    SmallSet<FeedbackOrigin> allSlots;

    std::unordered_map<Function*, SmallSet<FeedbackIndex>> slotsByFunction;

    // Collect
    for (const auto& session : COMPILATION_SESSIONS) {
        for (const auto& stats : session.closureVersionStats) {
            for (const auto& fstat : stats.feedbackStats) {
                auto fun = fstat.first;

                for (const auto& i : fstat.second.slotSubsumedBy) {
                    auto idx = i.first;
                    auto origin = FeedbackOrigin(fun, idx);

                    allSlots.insert(origin);
                    slotsByFunction[fun].insert(idx);

                    for (const auto& parent : i.second) {
                        if (!(origin == parent)) {
                            parents[origin].insert(parent);
                            children[parent].insert(origin);

                            allSlots.insert(parent);
                            slotsByFunction[parent.function()].insert(
                                parent.index());
                        }
                    }
                }
            }
        }
    }

    // Get components
    auto c = stronglyConnectedComponents(allSlots, parents, children);

    // Create a representant for each component
    std::unordered_map<size_t, FeedbackOrigin> componentRepresentant;
    {
        std::function<FeedbackOrigin(size_t)> visit = [&](size_t component) {
            if (componentRepresentant.count(component)) {
                return componentRepresentant[component];
            }

            if (c.componentParents[component].size()) {
                // Take the first parent component
                auto representant =
                    visit(*c.componentParents[component].begin());
                componentRepresentant[component] = representant;
                return representant;
            } else {
                // Take the first slot in the component - does not matter,
                // they're equivalent
                auto representant = *c.components[component].begin();
                componentRepresentant[component] = representant;
                return representant;
            }
        };

        for (const auto& i : c.components) {
            visit(i.first);
        }
    }

    // Output
    for (const auto& funSlots : slotsByFunction) {
        auto fun = funSlots.first;

        bool nameEmited = false;

        for (const auto& slot : funSlots.second) {
            auto origin = FeedbackOrigin(fun, slot);
            auto component = c.componentsForSlot.at(origin);
            auto representant = componentRepresentant.at(component);

            if (!(origin == representant)) {
                if (!nameEmited) {
                    nameEmited = true;
                    os << fun->dispatchTable()->closureName;
                }

                os << "\t" << slot.idx << "-"
                   << representant.function()->dispatchTable()->closureName
                   << "," << representant.index().idx;
            }
        }

        if (nameEmited) {
            os << "\n";
        }
    }
}

bool SUBSUMED_SLOTS_TRIED_LOADING = false;

std::unordered_map<std::string, SubsumedSlots> SUBSUMED_SLOTS;

// Format is
// <name>(\t<idx>-<subsumerName>,<subsumerIdx>)*
void loadSubsumedSlots() {
    SUBSUMED_SLOTS_TRIED_LOADING = true;

    auto filepath = std::getenv("STATS_SUBSUMED_FROM");
    if (filepath == nullptr) {
        return;
    }

    std::ifstream ifs(filepath);
    if (!ifs.is_open()) {
        std::cerr << "Failed to open file: " << filepath << "\n";
        exit(EXIT_FAILURE);
        return;
    }

    std::string line;
    while (std::getline(ifs, line)) {
        if (line.empty())
            continue; // skip blank lines

        // find first tab
        auto comma_pos = line.find('\t');
        if (comma_pos == std::string::npos) {
            // whole line is the parent fun, skip
            continue;
        }

        auto parentFun = line.substr(0, comma_pos);
        std::istringstream line_ss{line.substr(comma_pos + 1)};

        auto& subsumers = SUBSUMED_SLOTS[parentFun];

        std::string token;
        while (std::getline(line_ss, token, '\t')) {
            auto dash = token.find('-');
            assert(dash != std::string::npos);

            auto comma = token.find(',');
            assert(comma != std::string::npos);

            auto subsumedIdx = std::stoul(token.substr(0, dash));
            auto subsumerName = token.substr(dash + 1, comma);
            auto subsumerIdx = std::stoul(token.substr(comma + 1));

            if (subsumerName == parentFun) {
                subsumerName.clear();
            }
            subsumers[subsumedIdx] = {subsumerName,
                                      static_cast<uint32_t>(subsumerIdx)};
        }
    }
}

const SubsumedSlots& getSubsumedSlots(const std::string& name) {
    if (!SUBSUMED_SLOTS_TRIED_LOADING) {
        loadSubsumedSlots();
    }

    return SUBSUMED_SLOTS[name];
}

std::tuple<bool, ObservedValues&, FeedbackOrigin>
getConcreteSubsumer(const SlotSubsumer& subsumer) {
    auto dt = std::find_if(
        PreservedDispatchTables.begin(), PreservedDispatchTables.end(),
        [&](DispatchTable* dt) { return dt->closureName == subsumer.name; });
    if (dt == PreservedDispatchTables.end()) {
        static ObservedValues dummy;
        return {false, dummy, FeedbackOrigin()};
    }

    auto baseline = (*dt)->baseline();
    auto origin = FeedbackOrigin(baseline, FeedbackIndex::type(subsumer.idx));
    auto& feedback = baseline->typeFeedback()->types(subsumer.idx);

    return {true, feedback, origin};
}

// ------------------------------------------------------------
// PRECOMPUTED USED SLOTS
// ------------------------------------------------------------

bool USED_SLOTS_TRIED_LOADING = false;

std::unordered_map<std::string, std::unordered_set<size_t>> USED_SLOTS;

void loadSlotsUsed() {
    USED_SLOTS_TRIED_LOADING = true;

    auto filepath = std::getenv("STATS_USED");
    if (filepath == nullptr) {
        return;
    }

    std::ifstream ifs(filepath);
    if (!ifs.is_open()) {
        std::cerr << "Failed to open file: " << filepath << "\n";
        exit(EXIT_FAILURE);
        return;
    }

    std::string line;
    while (std::getline(ifs, line)) {
        if (line.empty())
            continue; // skip blank lines

        // find first comma
        std::size_t comma_pos = line.find(',');
        if (comma_pos == std::string::npos) {
            // whole line is the key, empty set
            USED_SLOTS[line] = {};
            continue;
        }

        auto key = line.substr(0, comma_pos);
        auto rest = line.substr(comma_pos + 1);
        std::istringstream line_ss{rest};

        auto& values = USED_SLOTS[key];

        std::string token;
        while (std::getline(line_ss, token, ',')) {
            values.insert(std::stoull(token));
        }
    }
}

RecordedUsedSlots getUsedSlotsFor(const std::string& closure_name) {
    if (!USED_SLOTS_TRIED_LOADING) {
        loadSlotsUsed();
    }

    if (USED_SLOTS.count(closure_name)) {
        return {false, USED_SLOTS[closure_name]};
    }

    static const std::unordered_set<size_t> empty;
    return {true, empty};
}

const bool UseRIRNames::value =
    std::getenv("STATS_USE_RIR_NAMES") != nullptr &&
    std::string(std::getenv("STATS_USE_RIR_NAMES")) == "1";

const bool CollectStats::value =
    !(std::getenv("STATS_NO_USED") != nullptr &&
      std::string(std::getenv("STATS_NO_USED")) == "1");

} // namespace report
} // namespace rir
