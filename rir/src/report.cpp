#include "report.h"
#include "compiler/pir/closure_version.h"
#include "compiler/pir/instruction.h"
#include "runtime/DispatchTable.h"
#include "runtime/Function.h"
#include "utils/Terminal.h"

#include <iostream>

namespace rir {
namespace report {

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

pir::PirType makeWidenedType(const pir::PirType& staticType,
                             const pir::PirType& expectedType) {
    auto checkFor = staticType.notLazy().noAttribsOrObject();
    if (expectedType.isA(checkFor)) {
        return checkFor;
    }

    checkFor = staticType.notLazy().notObject();
    if (expectedType.isA(checkFor)) {
        return checkFor;
    }

    return pir::PirType::voyd();
}

bool isWidened(const pir::PirType& staticType,
               const pir::PirType& feedbackType) {
    auto intersection = staticType & feedbackType;

    // Widened by the NA check
    auto expected = makeExpectedType(staticType, feedbackType);
    if (!expected.isA(intersection)) {
        return true;
    }

    if (!expected.maybeObj() &&
        (expected.noAttribsOrObject().isA(pir::RType::integer) ||
         expected.noAttribsOrObject().isA(pir::RType::real) ||
         expected.noAttribsOrObject().isA(pir::RType::logical))) {
        return false;
    }

    auto widened = makeWidenedType(staticType, expected);
    if (widened != pir::PirType::voyd() && !widened.isA(intersection)) {
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

// ------------------------------------------------------------
// SLOT USED
// ------------------------------------------------------------

pir::PirType SlotUsed::expectedType() const {
    return makeExpectedType(*staticType, *feedbackType);
}

bool SlotUsed::widened() const {
    // The actual checkFor could be even more widened
    return isWidened(*staticType, *feedbackType) ||
           (*checkFor != expectedType());
}

bool SlotUsed::narrowedWithStaticType() const {
    return !(feedbackType->isA(*staticType));
}

// ------------------------------------------------------------
// SLOT PRESENT
// ------------------------------------------------------------

// int SlotPresent::compareExpectedTypeToStaticType() const {
//     // returns 0 is exp == st
//     /// return -1 if  exp < st
//     // fails otherwise. That should not happen
//     if (expectedType() == *staticType) {
//         return 0;
//     }
//     assert(expectedType().isA( *staticType));
//     return -1;
// }

pir::PirType SlotPresent::expectedType() const {
    return makeExpectedType(*staticType, *feedbackType);
}

pir::PirType SlotPresent::widenExpected() const {
    return makeWidenedType(*staticType, expectedType());
}

bool SlotPresent::widened() const {
    return isWidened(*staticType, *feedbackType);
}

bool SlotPresent::canBeSpeculated() const {
    auto expected = expectedType();

    if (expected.isVoid() || expected.maybeLazy()) {
        return false;
    }

    auto widened = widenExpected();
    return expected.isA(widened);
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

        // Promise scan
        std::unordered_set<FeedbackIndex> codeSlots =
            findAllSlots(baseline->body());

        slotData.promiseSlots =
            difference(keys(slotData.allTypeSlots), codeSlots);

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

CompilationSession&
CompilationSession::getNew(Function* compiledFunction,
                           const Context& compiledContext,
                           const std::vector<DispatchTable*>& DTs) {
    COMPILATION_SESSIONS.emplace_back(compiledFunction, compiledContext);
    auto& session = COMPILATION_SESSIONS.back();
    computeFunctionsInfo(session.functionsInfo, DTs);
    return session;
}

CompilationSession& currentSession() {
    assert(!COMPILATION_SESSIONS.empty());
    return COMPILATION_SESSIONS.back();
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
    agg.used = slotsUsed.size();
    assert(agg.used == intersect(info.nonEmptySlots, keys(slotsUsed)).size() &&
           "There is an empty used slot");
    assert(agg.used == intersect(keys(slotsUsed), keys(slotsPresent)).size() &&
           "There is an non-present used slot");

    assert(slotsPromiseInlined ==
           intersect(info.promiseSlots, slotsPromiseInlined));

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
            auto& slot_type = j.second;

            SlotInfo res;

            // ID
            res.benchmark = benchmark_name;
            res.compilation_id = compilation_id;
            res.closure = closure->dispatchTable()->closureName;
            res.slot_idx = slot.idx;

            // Info
            res.nonempty = !static_info.emptySlots.count(slot);
            res.read = feedback_info.slotsRead.count(slot);
            res.used = feedback_info.slotsUsed.count(slot);

            // More info
            res.inlinee = closure != this->function;
            res.inPromise = static_info.promiseSlots.count(slot);
            res.polymorphic = static_info.polymorphicSlots.count(slot);

            if (res.used) {
                auto& usage = feedback_info.slotsUsed[slot];

                // Present info
                res.widened = usage.widened();

                // How used
                res.exactMatch = usage.exactMatch();
                res.narrowed = usage.narrowedWithStaticType();

                // Unused defaults
                res.promiseInlined =
                    feedback_info.slotsPromiseInlined.count(slot);

                // Types
                res.staticT = typeToString(*usage.staticType);
                res.feedbackT = typeToString(*usage.feedbackType);
                res.expectedT = typeToString(usage.expectedType());

                // Used types
                res.checkForT = typeToString(*usage.checkFor);
                res.requiredT = typeToString(*usage.requiredType);

                // Instruction
                res.instruction = usage.speculatedOn;
            } else {
                // Unused
                res.notPresent = !feedback_info.slotsPresent.count(slot);
                res.promiseInlined =
                    feedback_info.slotsPromiseInlined.count(slot);
                res.dependent =
                    res.nonempty && feedback_types_bags.count(slot_type) > 1;

                // Unused present non-empty
                if (!res.notPresent && res.nonempty) {
                    auto presentInfo = feedback_info.slotsPresent[slot];
                    auto expected = presentInfo.expectedType();

                    // Present info
                    res.widened = presentInfo.widened();

                    // Unused present non-empty
                    res.expectedEmpty = expected.isVoid();
                    res.expectedIsStatic = expected == *presentInfo.staticType;
                    assert(expected.isA(*presentInfo.staticType) &&
                           "expected is not <= static");

                    res.canBeSpeculated = presentInfo.canBeSpeculated();
                    res.speculationPhase =
                        streamToString([&](std::ostream& os) {
                            os << presentInfo.speculation;
                        });

                    // Types
                    res.staticT = typeToString(*presentInfo.staticType);
                    res.feedbackT = typeToString(*presentInfo.feedbackType);
                    res.expectedT = typeToString(expected);

                    // Unused types
                    res.widenedT = typeToString(presentInfo.widenExpected());

                    // Instruction
                    res.instruction = presentInfo.presentInstr;
                }
            }

            consume(res);
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

template <typename T>
void printUnorderedSet(const std::unordered_set<T>& mySet) {
    for (const auto& item : mySet) {
        std::cerr << item << std::endl;
    }
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

std::ostream& operator<<(std::ostream& os, const SlotPresent& slotPresent) {
    using namespace StreamColor;

    os << bold << slotPresent.presentInstr << clear << "\n";
    os << slotPresent.speculation;

    os << "\n";

    // clang-format off
    os << bold << "static: "   << clear << *slotPresent.staticType << ", "
       << bold << "feedback: " << clear << *slotPresent.feedbackType << ", "
       << bold << "expected: " << clear << slotPresent.expectedType() << "\n";
    // clang-format on

    if (slotPresent.canBeSpeculated()) {
        os << "(speculatable)\n";
    }

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
        [&](const FeedbackIndex& index, const pir::PirType& feedbackType,
            const FeedbackStatsPerFunction& stats, FunctionInfo& info) {
            bool used = stats.slotsUsed.count(index);

            os << StreamColor::red << index << StreamColor::clear;

            if (info.polymorphicSlots.count(index)) {
                os << " [polymorphic]";
            }

            if (info.getFeedbackTypesBag().count(feedbackType) > 1) {
                os << " [dependent]";
            }

            os << StreamColor::bold << " <" << feedbackType << ">\n"
               << StreamColor::clear;

            if (used) {
                os << stats.slotsUsed.at(index);
            } else {
                if (stats.slotsPresent.count(index)) {
                    os << stats.slotsPresent.at(index);
                } else {
                    os << "not present\n";
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

} // namespace report
} // namespace rir
