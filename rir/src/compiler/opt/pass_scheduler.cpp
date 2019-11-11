#include "pass_scheduler.h"
#include "pass_definitions.h"

namespace rir {
namespace pir {

static std::regex getPassBlacklist() {
    auto filter = getenv("PIR_PASS_BLACKLIST");
    if (filter)
        return std::regex(filter);
    return std::regex("");
}

static const std::regex PIR_PASS_BLACKLIST = getPassBlacklist();

void PassScheduler::add(std::unique_ptr<const PirTranslator>&& t) {
    auto name = t->getName();
    if (std::regex_match(name.begin(), name.end(), PIR_PASS_BLACKLIST))
        return;
    schedule_.push_back(std::move(t));
}

PassScheduler::PassScheduler() {
    auto addDefaultOpt = [&]() {
        add<DotDotDots>();
        add<EagerCalls>();

        add<Inline>();
        add<OptimizeContexts>();

        add<ForceDominance>();
        add<ScopeResolution>();
        add<GVN>();
        add<Constantfold>();
        add<DeadStoreRemoval>();

        add<Inline>();
        add<OptimizeContexts>();

        add<OptimizeAssumptions>();
        add<Cleanup>();

        add<ElideEnv>();
        add<DelayEnv>();
        add<DelayInstr>();
        add<Cleanup>();

        add<OptimizeAssumptions>();
        add<Cleanup>();

        add<TypeInference>();
    };
    auto addDefaultPrePhaseOpt = [&]() { add<OptimizeVisibility>(); };
    auto addDefaultPostPhaseOpt = [&]() {
        add<HoistInstruction>();
        add<LoopInvariant>();
        add<LoadElision>();
    };

    auto addDefaultspecPhaseOpt = [&]() {
        add<TypeSpeculation>();
        add<ElideEnv>();
        add<ElideEnvSpec>();
    };

    add<PhaseMarker>("Initial");

    addDefaultPrePhaseOpt();
    // ==== Phase 1) Run the default passes a couple of times
    for (size_t i = 0; i < 2; ++i)
        addDefaultOpt();
    addDefaultPostPhaseOpt();

    add<PhaseMarker>("Phase 1");

    // ==== Phase 2) Speculate away environments
    //
    // This pass is scheduled second, since we want to first try to do this
    // statically in Phase 1
    addDefaultPrePhaseOpt();
    addDefaultspecPhaseOpt();
    addDefaultOpt();
    addDefaultspecPhaseOpt();
    addDefaultOpt();
    addDefaultPostPhaseOpt();

    add<PhaseMarker>("Phase 2: Env speculation");

    // ==== Phase 3) Remove checkpoints we did not use
    //
    // This pass removes unused checkpoints.
    // We schedule this pass here, since it might unblock optimizations.
    // Since for example even unused checkpoints keep variables live.
    //
    // After this phase it is no longer possible to add assumptions at any point
    addDefaultPrePhaseOpt();
    add<CleanupCheckpoints>();
    for (size_t i = 0; i < 2; ++i)
        addDefaultOpt();
    addDefaultPostPhaseOpt();

    // ==== Phase 3.1) Remove Framestates we did not use
    //
    // Framestates can be used by call instructions. This pass removes this
    // dependency and the framestates will subsequently be cleaned.
    //
    // After this pass it is no longer possible to inline callees with deopts
    add<CleanupFramestate>();
    add<CleanupCheckpoints>();

    add<PhaseMarker>("Phase 3: Cleanup Checkpoints");

    // ==== Phase 4) Final round of default opts
    addDefaultPrePhaseOpt();
    for (size_t i = 0; i < 3; ++i) {
        addDefaultOpt();
        add<CleanupCheckpoints>();
    }
    addDefaultPostPhaseOpt();
    add<CleanupCheckpoints>();
    add<PhaseMarker>("Phase 4: finished");
}
}
}
