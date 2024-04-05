#ifndef RIR_RELAXCONTEXTS_H
#define RIR_RELAXCONTEXTS_H

#include "R/Serialize.h"
#include "Rinternals.h"
#include "bc/BC.h"
#include "bc/Compiler.h"
#include "compiler/backend.h"
#include "compiler/compiler.h"
#include "compiler/log/debug.h"
#include "compiler/parameter.h"
#include "compiler/pir/closure.h"
#include "compiler/pir/closure_version.h"
#include "compiler/pir/instruction.h"
#include "compiler/pir/type.h"
#include "compiler/test/PirCheck.h"
#include "compiler/test/PirTests.h"
#include "interpreter/interp_incl.h"
#include "runtime/Context.h"
#include "runtime/DispatchTable.h"
#include "utils/measuring.h"

#include "compiler/util/bb_transform.h"
#include <cassert>
#include <cstddef>
#include <cstdio>
#include <list>
#include <memory>
#include <string>

using namespace rir;

class RelaxContexts {
  public:
    static bool similarVersions(pir::ClosureVersion* v1,
                                pir::ClosureVersion* v2) {

        auto ret = v1->numInstrs() == v2->numInstrs() &&
                   v1->numNonDeoptInstrs() == v2->numNonDeoptInstrs() &&
                   v1->nextBBId == v2->nextBBId;

        if (!ret)
            return false;

        std::vector<pir::Instruction*> instructionsV1;
        pir::Visitor::run(v1->entry, [&](pir::Instruction* instr) {
            instructionsV1.push_back(instr);
        });

        std::vector<pir::Instruction*> instructionsV2;
        pir::Visitor::run(v2->entry, [&](pir::Instruction* instr) {
            instructionsV2.push_back(instr);
        });

        for (size_t i = 0; i < instructionsV1.size(); i++) {
            if (instructionsV1[i]->tag != instructionsV2[i]->tag)
                return false;
        }

        return true;
    }

    class TestAndRelaxContext {
      public:
        TestAndRelaxContext(
            std::function<bool(const Context& ctx, int index)> a,
            std::function<void(Context& ctx, int index)> b)
            : testPropertyToRelax(a), relaxContext(b) {}

        std::function<bool(const Context& ctx, int index)> testPropertyToRelax;
        std::function<void(Context& ctx, int index)> relaxContext;
    };

    static void tryRelaxContext(SEXP what, const Context& assumptions,
                                const std::string& name,
                                const pir::DebugOptions& debug,
                                pir::ClosureVersion*& originalVersion) {

        struct CompilationStats {

            int topLevelCompilationsCount = 0;
            int functionToRelaxCount = 0;
            int similarCount = 0;

            void printStats() {

                std::cerr << "\n\n ************************ \n";
                std::cerr << "topLevelCompilationsCount: "
                          << topLevelCompilationsCount << " \n";
                std::cerr << "functionToRelaxCount: " << functionToRelaxCount
                          << " \n";
                if (functionToRelaxCount > 0)
                    std::cerr << "SIMILAR count: " << similarCount << " ("
                              << round((double)similarCount * 100 /
                                       functionToRelaxCount)
                              << " %) \n";
            }
        };

        static std::map<SEXP, CompilationStats> statsPerClosure;

        static int topLevelCompilationsCount = 0;
        topLevelCompilationsCount++;
        statsPerClosure[what].topLevelCompilationsCount++;

        std::vector<TestAndRelaxContext> conditions;

        auto condNotObj = TestAndRelaxContext(
            [&](const Context& ctx, int index) {
                return ctx.isNotObj(index) && !ctx.isSimpleInt(index) &&
                       !ctx.isSimpleReal(index);
            },
            [&](Context& ctx, int index) { ctx.resetNotObj(index); });

        // SimpleInt  *****************************************
        auto condSimpleInt = TestAndRelaxContext(
            [&](const Context& ctx, int index) {
                return ctx.isSimpleInt(index);
            },
            [&](Context& ctx, int index) {
                ctx.resetNotObj(index);
                ctx.resetSimpleInt(index);
            });

        // SimpleReal  *****************************************
        auto condSimpleReal = TestAndRelaxContext(
            [&](const Context& ctx, int index) {
                return ctx.isSimpleReal(index);
            },
            [&](Context& ctx, int index) {
                ctx.resetNotObj(index);
                ctx.resetSimpleReal(index);
            });

        // NotRefl  *****************************************
        auto condNotRefl = TestAndRelaxContext(
            [&](const Context& ctx, int index) {
                return !ctx.isEager(index) && ctx.isNonRefl(index);
            },
            [&](Context& ctx, int index) { ctx.resetNonRefl(index); });

        // Eager  *****************************************

        auto condEager = TestAndRelaxContext(
            [&](const Context& ctx, int index) { return ctx.isEager(index); },
            [&](Context& ctx, int index) {
                ctx.resetEager(index);
                ctx.resetNonRefl(index);
            });

        // *********************************************

        conditions.push_back(condNotObj);
        conditions.push_back(condSimpleInt);
        conditions.push_back(condSimpleReal);
        conditions.push_back(condNotRefl);
        conditions.push_back(condEager);

        if (originalVersion && assumptions.numMissing() == 0 &&
            assumptions.includes(Assumption::NoExplicitlyMissingArgs)) {

            bool anyArgsRelaxed = false;
            bool anyArgsToRelax = false;
            Context finalRelaxedContext = assumptions;

            for (int i = 0; i <= 5; i++) {

                for (auto& cond : conditions) {

                    if (cond.testPropertyToRelax(assumptions, i)) {

                        anyArgsToRelax = true;

                        pir::Module* m2 = new pir::Module;
                        pir::Log logger2(debug);
                        logger2.title("Compiling " + name);
                        assert(originalVersion->context() == assumptions);

                        auto relaxedAssumptions = assumptions;
                        cond.relaxContext(relaxedAssumptions, i);

                        rir::pir::ClosureVersion* relaxedVersion = nullptr;

                        pir::Compiler cmp2(m2, logger2);

                        auto compileRelaxed = [&](pir::ClosureVersion* c) {
                            relaxedVersion = c;
                            logger2.flushAll();
                            cmp2.optimizeModule();
                        };

                        cmp2.compileClosure(
                            what, name, relaxedAssumptions, true,
                            compileRelaxed,
                            [&]() {
                                std::cerr << "Compilation failed for relaxed\n";
                            },
                            {});

                        logger2.flushAll();
                        assert(relaxedVersion != nullptr &&
                               "relaxedVersion not compiled");

                        assert(relaxedVersion->context() == relaxedAssumptions);

                        auto similar =
                            similarVersions(originalVersion, relaxedVersion);
                        if (similar) {
                            anyArgsRelaxed = true;
                            cond.relaxContext(finalRelaxedContext, i);
                        }

                        delete m2;
                    }
                }
            }

            static int functionToRelaxCount = 0;
            if (anyArgsToRelax) {
                functionToRelaxCount++;
                statsPerClosure[what].functionToRelaxCount++;
            }

            static int similarCount = 0;

            std::cerr << "\n\n ************************ \n";
            std::cerr << "original VERSION: \n";
            std::cerr << originalVersion->context() << "\n";
            // originalVersion->printStandard(std::cerr, true, false);

            if (anyArgsRelaxed) {
                similarCount++;
                statsPerClosure[what].similarCount++;

                std::cerr << "\n\n ------------------------ \n";
                std::cerr << "relaxed VERSION: \n";
                std::cerr << finalRelaxedContext << "\n";
                // relaxedVersion->printStandard(std::cerr, true, false);

                // std::cerr
                //         << "\n **** " << relaxedIndices.size() << " args
                //         relaxed ("
                //           << argsToRelaxCount
                //           << " total ArgsToRelax). \n"
                //           << "SIMILAR count: " << similarCount
                //           << " (out of " << functionToRelaxCount
                //           << ") ************************ \n\n ";
            }

            CompilationStats globalStats;
            globalStats.topLevelCompilationsCount = topLevelCompilationsCount;
            globalStats.functionToRelaxCount = functionToRelaxCount;
            globalStats.similarCount = similarCount;
            globalStats.printStats();

            // std::cerr << "*** STATS PER CLOSURE ******************";
            // for (auto& st : statsPerClosure) {
            //     std::cerr << "closure: " << st.first << "\n";
            //     printStats(st.second.topLevelCompilationsCount,
            //     st.second.functionToRelaxCount, st.second.similarCount);
            //     std::cerr << "\n";
            // }
            // std::cerr << "*** END - STATS PER CLOSURE ******************";
        }
    }
};

#endif
