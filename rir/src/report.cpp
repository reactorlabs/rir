#include "report.h"
#include "R/Protect.h"
#include "compiler/pir/closure_version.h"
#include "compiler/pir/instruction.h"
#include "runtime/DispatchTable.h"
#include "runtime/Function.h"
#include "utils/Terminal.h"

#include <iostream>

extern "C" SEXP R_GetVarLocValue(R_varloc_t);

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
        auto cellValue = R_GetVarLocValue(R_findVarLocInFrame(env, symbol));
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
    agg.used = std::count_if(slotsUsed.begin(), slotsUsed.end(),
                             [](auto& i) { return i.second.size(); });

    assert(keys_nonempty(slotsUsed) ==
               intersect(info.nonEmptySlots, keys_nonempty(slotsUsed)) &&
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
            assert(res.closure.size());
            res.slot_idx = slot.idx;

            // Info
            res.nonempty = !static_info.emptySlots.count(slot);
            res.read = feedback_info.slotsRead.count(slot);
            res.used = feedback_info.slotsUsed[slot].size();

            // More info
            res.inlinee = closure != this->function;
            res.inPromise = static_info.promiseSlots.count(slot);
            // res.polymorphic = static_info.polymorphicSlots.count(slot);

            if (res.used) {
                for (const auto& usage : feedback_info.slotsUsed[slot]) {
                    // Present info
                    res.widened = usage.widened();

                    // How used
                    res.exactMatch = usage.exactMatch();
                    res.narrowed = usage.narrowedWithStaticType();

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
            bool used = stats.slotsUsed[index].size();

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
                for (const auto& i : stats.slotsUsed.at(index)) {
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
// PRECOMPUTED USED SLOTS
// ------------------------------------------------------------


bool useRIRNames() {
    auto env = std::getenv("STATS_USE_RIR_NAMES");
    return (env != nullptr && std::string(env) == "1");
}

} // namespace report
} // namespace rir
