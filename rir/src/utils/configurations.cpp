#include <iostream>
#include <sstream>

#include "configurations.h"

#include "../compiler/opt/pass_definitions.h"
#include <deque>

namespace rir {

using namespace std;

void Configurations::parseINIFile() {
    INIReader reader(".pir/configuration.ini");
    if (reader.ParseError() < 0) {
        // std::cout << "Can't load optimizations.ini file, we resort to default
        // "
        //             "optimizations'\n";
        return defaultOptimizations();
    }

    std::map<short, std::deque<pir::PirTranslator*>> opts;
#define PARSE_OPT(Name, where, Klass)                                          \
    if (short order = reader.GetInteger("optimizations", Name, 0))             \
        opts[order].push_##where(new pir::Klass);

    PARSE_OPT("forceDominance", front, ForceDominance());
    PARSE_OPT("scopeResolution", front, ScopeResolution());
    PARSE_OPT("delayInstructions", front, DelayInstr());
    PARSE_OPT("elideEnvironments", front, ElideEnv());
    PARSE_OPT("delayEnvironments", front, DelayEnv());
    PARSE_OPT("inline", front, Inline());
    PARSE_OPT("cleanup", back, Cleanup());
    PARSE_OPT("cleanupCheckpoints", back, CleanupCheckpoints());
#undef PARSE_OPT
    for (auto& optlist : opts)
        for (auto& opt : optlist.second)
            optimizations.push_back(std::move(opt));
}

void Configurations::defaultOptimizations() {
    auto phasemarker = [&](const std::string& name) {
        optimizations.push_back(new pir::PhaseMarker(name));
    };
    auto addDefaultOpt = [&]() {
        optimizations.push_back(new pir::OptimizeVisibility());
        optimizations.push_back(new pir::ForceDominance());
        optimizations.push_back(new pir::ScopeResolution());
        optimizations.push_back(new pir::DeadStoreRemoval());
        optimizations.push_back(new pir::EagerCalls());
        optimizations.push_back(new pir::Constantfold());
        optimizations.push_back(new pir::Cleanup());
        optimizations.push_back(new pir::DelayInstr());
        optimizations.push_back(new pir::ElideEnv());
        optimizations.push_back(new pir::DelayEnv());
        optimizations.push_back(new pir::Cleanup());
        optimizations.push_back(new pir::Inline());
        optimizations.push_back(new pir::OptimizeContexts());
        optimizations.push_back(new pir::LoadElision());
        optimizations.push_back(new pir::GVN());
        optimizations.push_back(new pir::OptimizeAssumptions());
        optimizations.push_back(new pir::Cleanup());
        optimizations.push_back(new pir::TypeInference());
    };

    phasemarker("Initial");

    // ==== Phase 1) Run the default passes a couple of times
    for (size_t i = 0; i < 3; ++i)
        addDefaultOpt();

    phasemarker("Phase 1");

    // ==== Phase 2) Speculate away environments
    //
    // This pass is scheduled second, since we want to first try to do this
    // statically in Phase 1
    optimizations.push_back(new pir::TypeSpeculation());
    optimizations.push_back(new pir::ElideEnvSpec());
    addDefaultOpt();
    optimizations.push_back(new pir::TypeSpeculation());
    optimizations.push_back(new pir::ElideEnvSpec());

    phasemarker("Phase 2: Env speculation");

    // ==== Phase 3) Remove checkpoints we did not use
    //
    // This pass removes unused checkpoints.
    // We schedule this pass here, since it might unblock optimizations.
    // Since for example even unused checkpoints keep variables live.
    //
    // After this phase it is no longer possible to add assumptions at any point
    optimizations.push_back(new pir::CleanupCheckpoints());
    for (size_t i = 0; i < 2; ++i)
        addDefaultOpt();

    // ==== Phase 3.1) Remove Framestates we did not use
    //
    // Framestates can be used by call instructions. This pass removes this
    // dependency and the framestates will subsequently be cleaned.
    //
    // After this pass it is no longer possible to inline callees with deopts
    optimizations.push_back(new pir::CleanupFramestate());
    optimizations.push_back(new pir::CleanupCheckpoints());

    phasemarker("Phase 3: Cleanup Checkpoints");

    // ==== Phase 4) Final round of default opts
    for (size_t i = 0; i < 2; ++i)
        addDefaultOpt();

    // Our backend really does not like unused checkpoints, so be sure to remove
    // all of them here already.
    optimizations.push_back(new pir::CleanupCheckpoints());
    optimizations.push_back(new pir::LoopInvariant());

    phasemarker("Phase 4: finished");
}

} // namespace rir
