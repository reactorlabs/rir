#include "report.h"
#include "R/RList.h"
#include "compiler/pir/closure_version.h"
#include "compiler/pir/instruction.h"
#include "runtime/DispatchTable.h"
#include "runtime/Function.h"

namespace rir {
namespace report {

std::string streamToString(std::function<void(std::stringstream&)> f) {
    std::stringstream ss;
    f(ss);
    return ss.str();
};

std::string boolToString(bool b) { return b ? "yes" : "no"; }

SlotUsed::SlotUsed() {}
SlotUsed::SlotUsed(bool narrowedWithStaticType, SlotUsed::Kind kind,
                   const pir::PirType& checkFor, const pir::PirType& staticType,
                   const pir::PirType& feedbackType,
                   const pir::PirType& expectedType,
                   const pir::PirType& requiredType,
                   pir::Instruction& instruction) {
    this->narrowedWithStaticType = narrowedWithStaticType;
    this->kind = kind;

    this->checkFor = new pir::PirType(checkFor);
    this->staticType = new pir::PirType(staticType);
    this->feedbackType = new pir::PirType(feedbackType);
    this->expectedType = new pir::PirType(expectedType);
    this->requiredType = new pir::PirType(requiredType);
    this->instructionAsString = report::streamToString(
        [&](std::stringstream& ss) { instruction.print(ss); });
}

void SlotUsed::print(std::ostream& os) {
    os << instructionAsString << "\n";
    os << "narrowed with static type: " << boolToString(narrowedWithStaticType)
       << "\n";
    os << "exact match/widened: "
       << (kind == exactMatch ? "exact match" : "widened") << "\n";

    os << "checkFor: " << *checkFor << " ; "
       << "static: " << *staticType << " ; "
       << "feedback: " << *feedbackType << " ; "
       << "expected: " << *expectedType << " ; "
       << "required: " << *requiredType << "\n";
}

void computeFunctionsInfo(
    std::unordered_map<Function*, FunctionInfo>& functionsInfo,
    std::vector<DispatchTable*> DTs) {
    for (auto dt : DTs) {
        auto baseline = dt->baseline();
        auto& slotData = functionsInfo[baseline];

        // Types
        auto feedback = baseline->typeFeedback();
        slotData.allTypeSlots = feedback->types_size();

        for (size_t i = 0; i < feedback->types_size(); ++i) {
            if (feedback->types(i).isEmpty()) {
                slotData.emptySlots.insert(FeedbackIndex::type(i));
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

std::unordered_map<Function*, FunctionAggregateStats>
aggregateCompilationSession(const CompilationSession& session) {
    std::unordered_map<Function*, FunctionAggregateStats> stats;

    for (const auto& closure : session.closuresVersionStats) {
        auto& closureStats = stats[closure.function];

        for (const auto& i : closure.feedbackStats) {
            const auto& iStats = i.second;

            if (i.first == closure.function) {
                closureStats.slotsRead.value = iStats.slotsRead.size();
                closureStats.slotsUsed.value = iStats.slotsUsed.size();
            } else {
                // Inlinee info
                auto& inlineeStats = stats[i.first];

                inlineeStats.timesInlined++;
                inlineeStats.inlineeSlotsRead += iStats.slotsRead.size();
                inlineeStats.inlineeSlotsUsed += iStats.slotsUsed.size();

                closureStats.inlinedSlotsReferenced +=
                    session.functionsInfo.at(i.first).allTypeSlots;
                closureStats.inlinedSlotsRead += iStats.slotsRead.size();
                closureStats.inlinedSlotsUsed += iStats.slotsUsed.size();
            }
        }
    }

    for (auto& i : stats) {
        const auto& info = session.functionsInfo.at(i.first);
        i.second.slotsCount.value = info.allTypeSlots;
        i.second.slotsEmpty.value = info.emptySlots.size();
    }

    return stats;
}

// ------------------------------------------------------------

std::vector<CompilationSession> sessions;

CompilationSession& CompilationSession::getNew(Function* compiledFunction,
                                               const Context& compiledContext,
                                               std::vector<DispatchTable*> DTs) {
    sessions.emplace_back(compiledFunction, compiledContext);
    auto& session = sessions.back();
    computeFunctionsInfo(session.functionsInfo, DTs);
    return session;
}

void CompilationSession::addClosureVersion(pir::ClosureVersion* closureVersion,
                                           Function* compiledFunction) {
    auto baseline = compiledFunction->dispatchTable()->baseline();
    const auto& context = compiledFunction->context();

    closuresVersionStats.emplace_back(baseline, context,
                                      closureVersion->feedbackStatsByFunction);
}

// ------------------------------------------------------------

MetricPercent operator/(Stat& lhs, Stat& rhs) {
    return MetricPercent{.numerator = &lhs, .denominator = &rhs};
}

void showPercent(double percent, std::ostream& ss) {
    ss << percent * 100 << "%";
}

std::ostream& operator<<(std::ostream& os, const Stat& st) {
    if (st.name != "") {
        os << st.name;
        os << ": ";
    }

    os << st.value;
    os << "\n";

    return os;
}

std::ostream& operator<<(std::ostream& os, const MetricPercent& metric) {
    if (metric.name != "") {
        os << metric.name << " ";
    }

    os << "(" << metric.numerator->name << " / " << metric.denominator->name
       << "): " << metric.numerator->value << " / " << metric.denominator->value
       << " (";

    if (metric.denominator->value) {
        showPercent(metric.value(), os);
    } else {
        os << "-";
    }

    os << ")\n";

    return os;
};

std::ostream& operator<<(std::ostream& os, const FunctionAggregate& agg) {
    if (agg.values.empty()) {
        return os;
    }

    os << agg.name << " (on average of " << agg.values.size()
       << " functions): ";

    showPercent(agg.average(), os);

    os << "\n";

    return os;
}

void report(std::ostream& os, bool breakdownInfo) {

    std::function<void(std::string&, FunctionInfo&)> printFunctionInfo =
        [&](std::string& functionName, FunctionInfo& info) {
            os << functionName << " ("
               << "# of slots: " << info.allTypeSlots << " - "
               << "#non-empty: " << info.allTypeSlots - info.emptySlots.size()
               << ")"
               << "\n";
        };

    std::function<void(FeedbackStatsPerFunction&)> printFeedbackStats =
        [&](FeedbackStatsPerFunction& stats) {
            os << "read slots: " << stats.slotsRead.size() << "\n";
            if (breakdownInfo) {

                // for (auto& sr : stats.slotsRead) {
                //     os << sr << "\n";
                // }

                os << "\n";
            }

            os << "used slots: " << stats.slotsUsed.size() << "\n";
            if (breakdownInfo) {
                for (auto& su : stats.slotsUsed) {
                    os << su.first << ": \n";
                    su.second.print(os);
                    os << "\n\n";
                }
            }

            os << "\n";
        };

    for (auto& session : sessions) {
        os << "*********************** Compilation: "
           << session.function->dispatchTable()->closureName << " ("
           << session.context << ")"
           << " ***********************\n\n";

        for (auto& cvstat : session.closuresVersionStats) {
            auto& mainInfo = session.functionsInfo[cvstat.function];
            printFunctionInfo(cvstat.function->dispatchTable()->closureName,
                              mainInfo);

            auto& mainFeedbackStats = cvstat.feedbackStats[cvstat.function];
            printFeedbackStats(mainFeedbackStats);

            for (auto& i : cvstat.feedbackStats) {
                if (i.first == cvstat.function) {
                    continue;
                }
                auto fun = i.first;
                auto feedbackStats = i.second;
                auto info = session.functionsInfo[fun];
                os << "\n ---------------- \n";
                os << "Inlinee: ";
                printFunctionInfo(fun->dispatchTable()->closureName, info);
                printFeedbackStats(feedbackStats);
            }

            os << "===============================================\n";
        }

        // auto agg = aggregateCompilationSession(session);
        // for ( const auto& i : agg ) {
        //     const auto& info = i.second;
        //     os << "Name: " << i.first->dispatchTable()->closureName << "\n";
        //     os << info.slotsCount << info.slotsEmpty << info.slotsRead <<
        //     info.slotsUsed << "\n"; os << info.inlinedSlotsReferenced <<
        //     info.inlinedSlotsRead << info.inlinedSlotsUsed << "\n"; os <<
        //     info.timesInlined << info.inlineeSlotsRead <<
        //     info.inlineeSlotsUsed << "\n"; os << "----------------\n";
        // }

        os << "\n\n";
    }
}

//     std::ofstream null_stream("/dev/null");
//     std::ostream& defaultOutput = std::cerr;
//
//     std::ostream& outputInFunction = defaultOutput;
//
//     SEXP DTs = Rf_findVar(DTsSymbol, R_GlobalEnv);
//
//     Stat totalSlots{"total"};
//     Stat referencedSlots{"referenced"};
//     Stat readSlots{"read"};
//     Stat readNonEmptySlots{"read non-empty"};
//     Stat usedSlots{"used"};
//     Stat usedNonEmptySlots{"used non-empty"};
//
//     Stat usedSlotsOfKindType{"used (kind type)"};
//     Stat exactMatchUsedSlots{"exact match"};
//     Stat widenedUsedSlots{"widened"};
//     Stat narrowedSlots{"narrowed"};
//
//     Stat compiledFunctions{"compiled functions"};
//     Stat functionsUsingFeedback{"functions using feedback"};
//
//     Stat emptySlots{"empty"};
//     Stat emptyReferencedSlots{"empty referenced"};
//
//     auto list = RList(DTs);
//     Stat totalFunctions = {"Total functions (RIR compiled)",
//     list.length()}; Stat totalCompiledVersions = {"Total compiled
//     versions", 0}; Stat totalDeopts = {"Total deopts", 0};
//
//     FunctionAggregate emptySlotsOverTotalSlots{"empty slots"};
//     FunctionAggregate nonEmptySlotsOverTotalSlots{"non-empty slots"};
//
//     FunctionAggregate slotsReadOverReferencedPerFunction{
//         "read / referenced slots"};
//     FunctionAggregate slotsUsedOverReadNonEmptyPerFunction{
//         "used / read non-empty slots"};
//     FunctionAggregate slotsUsedOverNonEmptyPerFunction{
//         "used / non-empty slots"};
//
//     FunctionAggregate typeSlotsPerFunction{"% of slots beign type"};
//
//     for (auto a = list.begin(); a != list.end(); ++a) {
//         DispatchTable* dt = DispatchTable::unpack(*a);
//         auto baseline = dt->baseline();
//         auto feedback = baseline->typeFeedback();
//
//         outputInFunction << "---------\nname: " << dt->closureName
//                          << (baseline->involvedInCompilation ? "
//                          (compiled)"
//                                                              : "")
//                          << "\n";
//
//         outputInFunction << "baseline function: " << dt->baseline() <<
//         "\n";
//         // ---------
//         // Versions
//         // ---------
//         Stat compiledVersions{"compiled versions", dt->size() - 1};
//         outputInFunction << compiledVersions;
//
//         totalCompiledVersions += compiledVersions;
//
//         outputInFunction << "\n";
//
//         // ---------
//         // Slots count
//         // ---------
//         Stat slotsInFunction = {"slots in function",
//         feedback->types_size()}; totalSlots += slotsInFunction;
//
//         {
//             Stat allSlots = {"", feedback->types_size() +
//                                      feedback->callees_size() +
//                                      feedback->tests_size()};
//             if (allSlots.value != 0) {
//                 typeSlotsPerFunction.add(slotsInFunction / allSlots);
//             }
//         }
//
//         Stat slotsReadInFunction{"read", baseline->slotsRead.size()};
//         readSlots += slotsReadInFunction;
//
//         Stat slotsUsedInFunction = {"used", baseline->slotsUsed.size()};
//         usedSlots += slotsUsedInFunction;
//
//         Stat slotsUsedInInlinedFunction = {"used as inline",
//                                            baseline->slotsUsedInlined.size()};
//
//         // ---------
//         // Read non-empty
//         // ---------
//         Stat readNonEmptySlotsInFunction{"read non-empty"};
//         for (auto& slot : baseline->slotsRead) {
//             switch (slot.kind) {
//             case FeedbackKind::Type:
//                 if (!feedback->types(slot.idx).isEmpty())
//                     readNonEmptySlotsInFunction++;
//                 break;
//
//             default:
//                 assert(false);
//             }
//         }
//         readNonEmptySlots += readNonEmptySlotsInFunction;
//
//         // ---------
//         // Used
//         // ---------
//         Stat usedNonEmptySlotsInFunction{"used non-empty"};
//         for (auto& slot : baseline->slotsUsed) {
//             switch (slot.kind) {
//             case FeedbackKind::Type:
//                 if (!feedback->types(slot.idx).isEmpty())
//                     usedNonEmptySlotsInFunction++;
//                 break;
//
//             case FeedbackKind::Call:
//                 assert(false);
//                 if (!feedback->callees(slot.idx).isEmpty())
//                     usedNonEmptySlotsInFunction++;
//                 break;
//
//             case FeedbackKind::Test:
//                 assert(false);
//                 if (!feedback->test(slot.idx).isEmpty())
//                     usedNonEmptySlotsInFunction++;
//                 break;
//
//             default:
//                 assert(false);
//             }
//         }
//         usedNonEmptySlots += usedNonEmptySlotsInFunction;
//
//         // ---------
//         // Slots of kind type
//         // ---------
//         narrowedSlots += baseline->slotsNarrowedWithStaticType.size();
//         exactMatchUsedSlots += baseline->slotsUsedExactMatch.size();
//         widenedUsedSlots += baseline->slotsUsedWidened.size();
//
//         for (auto& s : baseline->slotsUsed) {
//             assert(s.kind == FeedbackKind::Type);
//             usedSlotsOfKindType++;
//         }
//
//         // ---------
//         // Empty slots
//         // ---------
//         Stat emptySlotsCountInFunction{"empty slots in function"};
//
//         // for (size_t i = 0; i < feedback->tests_size(); i++) {
//         //     if (feedback->test(i).isEmpty())
//         //         emptySlotsCountInFunction++;
//         // }
//
//         // for (size_t i = 0; i < feedback->callees_size(); i++) {
//         //     if (feedback->callees(i).isEmpty())
//         //         emptySlotsCountInFunction++;
//         // }
//
//         for (size_t i = 0; i < feedback->types_size(); i++) {
//             if (feedback->types(i).isEmpty())
//                 emptySlotsCountInFunction++;
//         }
//         emptySlots += emptySlotsCountInFunction;
//
//         if (baseline->involvedInCompilation) {
//             emptyReferencedSlots += emptySlotsCountInFunction;
//         }
//
//         if (slotsInFunction.value != 0) {
//             emptySlotsOverTotalSlots.add(emptySlotsCountInFunction /
//                                          slotsInFunction);
//         }
//
//         // ---------
//         // Non empty slots
//         // ---------
//         Stat nonEmptySlotsCountInFunction = {
//             "non-empty slots",
//             slotsInFunction.value - emptySlotsCountInFunction.value};
//
//         if (slotsInFunction.value != 0) {
//             auto p = (nonEmptySlotsCountInFunction / slotsInFunction)
//                          .named("non-empty slots");
//
//             nonEmptySlotsOverTotalSlots.add(p);
//         }
//
//         // ---------
//         // Print
//         // ---------
//         outputInFunction << slotsInFunction;
//         outputInFunction << nonEmptySlotsCountInFunction;
//         outputInFunction << slotsReadInFunction;
//         outputInFunction << slotsUsedInFunction;
//         outputInFunction << slotsUsedInInlinedFunction;
//
//         Stat assumeEmited{"assume emited",
//         baseline->assumeEmited.size()}; outputInFunction << assumeEmited;
//
//         // ---------
//         // Compiled stats
//         // ---------
//         if (baseline->involvedInCompilation) {
//             compiledFunctions++;
//             if (baseline->slotsUsed.size()) {
//                 functionsUsingFeedback++;
//             }
//
//             referencedSlots += slotsInFunction;
//
//             if (slotsInFunction.value != 0) {
//                 auto p =
//                     (slotsReadInFunction / slotsInFunction).named("slots
//                     read");
//
//                 slotsReadOverReferencedPerFunction.add(p);
//             }
//
//             if (nonEmptySlotsCountInFunction.value != 0) {
//                 slotsUsedOverNonEmptyPerFunction.add(
//                     slotsUsedInFunction / nonEmptySlotsCountInFunction);
//             }
//
//             if (readNonEmptySlotsInFunction.value != 0) {
//                 slotsUsedOverReadNonEmptyPerFunction.add(
//                     slotsUsedInFunction / readNonEmptySlotsInFunction);
//             }
//
//             // ---------
//             // Speculation and Inlines
//             // ---------
//             // Stat speculationWithinInline{
//             //     "speculation within inlines",
//             //     baseline->speculationWithinInlines.size()};
//             // outputInFunction << speculationWithinInline;
//             //
//             // Stat speculationInFunctions{
//             //     "speculation in functions",
//             //     baseline->speculationInFunctions.size()};
//             // outputInFunction << speculationInFunctions;
//
//             // ---------
//             // Deopts
//             // ---------
//             Stat deoptedSlots{"deopted", baseline->slotsDeopted.size()};
//             outputInFunction << deoptedSlots;
//
//             Stat deopts{"deopt count", baseline->otherVersionDeopted};
//             outputInFunction << deopts;
//
//             totalDeopts += deopts;
//         }
//
//         outputInFunction << "\n";
//     }
//
//     Stat nonEmptySlots{"non-empty", totalSlots.value - emptySlots.value};
//
//     // std::ofstream fileStream("summary.txt", std::ios::app);
//     // std::ostream& ss = fileStream;
//
//     std::ostream& ss = defaultOutput;
//
//     ss << "\n\n********** SUMMARY *************\n\n";
//     ss << totalFunctions;
//     ss << "Compiled functions (PIR compiled): " <<
//     compiledFunctions.value
//        << "\n";
//     ss << totalCompiledVersions;
//     ss << totalDeopts;
//
//     ss << "Total slots: " << totalSlots.value << "\n";
//     ss << "\n";
//
//     ss << (emptySlots / totalSlots).named("empty slots (never filled)");
//     ss << emptySlotsOverTotalSlots;
//     ss << nonEmptySlotsOverTotalSlots;
//     ss << "\n";
//
//     ss << (referencedSlots / totalSlots)
//               .named("referenced slots in compilation");
//     ss << (readSlots / referencedSlots).named("slots read");
//     ss << slotsReadOverReferencedPerFunction;
//     ss << "\n";
//
//     // benefit
//     ss << (functionsUsingFeedback / compiledFunctions)
//               .named("benefited from feedback");
//     ss << "\n";
//
//     // used slots
//     ss << "--- USED SLOTS ---\n";
//
//     // ss << (usedSlots / totalSlots).named("slots used in speculation");
//     ss << (usedSlots / referencedSlots)
//               .named("referenced slots used in speculation");
//     ss << (usedSlots / readSlots).named("read slots used in
//     speculation"); ss << "\n";
//
//     // USED / READ non-empty
//     ss << (usedSlots / readNonEmptySlots)
//               .named("read non-empty slots used in speculation ");
//     ss << slotsUsedOverReadNonEmptyPerFunction;
//     ss << "\n";
//
//     // USED / non-empty
//     ss << (usedSlots / nonEmptySlots)
//               .named("non-empty slots used in speculation");
//     ss << slotsUsedOverNonEmptyPerFunction;
//     ss << "\n";
//
//     // USED MATCH
//     ss << "--- KIND TYPE ---\n";
//     ss << "used (of kind type): " << usedSlotsOfKindType << "\n";
//
//     ss << (narrowedSlots / usedSlotsOfKindType)
//               .named("narrowed with static type");
//     ss << (exactMatchUsedSlots / usedSlotsOfKindType).named("exact
//     match"); ss << (widenedUsedSlots /
//     usedSlotsOfKindType).named("widened"); ss << "\n";
//
//     ss.flush();
//
//     auto csv_file = getenv("STATS_CSV");
//     if (csv_file != nullptr) {
//         std::ofstream ofs{csv_file, std::ios::out | std::ios::app};
//
//         ofs.seekp(0, std::ios::end);
//         if (ofs.tellp() == 0) {
//             // clang-format off
//             ofs << "name,total functions,compiled functions,benefited
//             functions,total versions,total deopts"
//                 << ",total slots,referenced slots,empty slots,empty
//                 referenced slots"
//                 << ",read slots,read non-empty slots,used slots,used
//                 non-empty slots"
//                 << ",type slots perc"
//                 // << ",used type slots,narrowed,exact match,widened"
//                 << "\n";
//             // clang-format on
//         }
//
//         const char* stats_name = getenv("STATS_NAME");
//         if (stats_name == nullptr) {
//             stats_name = "?";
//         }
//
//         auto out = [&ofs](Stat x, bool last = false) {
//             ofs << x.value << (last ? "\n" : ",");
//         };
//
//         auto qout = [&ofs](auto x, bool last = false) {
//             ofs << "\"" << x << "\"" << (last ? "\n" : ",");
//         };
//
//         qout(stats_name);
//
//         out(totalFunctions);
//         out(compiledFunctions);
//         out(functionsUsingFeedback);
//         out(totalCompiledVersions);
//         out(totalDeopts);
//
//         out(totalSlots);
//         out(referencedSlots);
//         out(emptySlots);
//         out(emptyReferencedSlots);
//
//         out(readSlots);
//         out(readNonEmptySlots);
//         out(usedSlots);
//         /*out(usedNonEmptySlots, true);*/
//         out(usedNonEmptySlots);
//
//         if (typeSlotsPerFunction.values.size() != 0) {
//             ofs << typeSlotsPerFunction.average();
//         }
//
//         ofs << "\n";
//
//         // out(usedSlotsOfKindType);
//         // out(narrowedSlots);
//         // out(exactMatchUsedSlots);
//         // out(widenedUsedSlots);
//     }
//
//     assert(usedSlotsOfKindType.value == usedSlots.value);

} // namespace report
} // namespace rir
