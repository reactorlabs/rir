#include <iostream>
#include <sstream>

#include "configurations.h"

#include "../compiler/opt/cleanup.h"
#include "../compiler/opt/cleanup_safepoint.h"
#include "../compiler/opt/delay_env.h"
#include "../compiler/opt/delay_instr.h"
#include "../compiler/opt/elide_env.h"
#include "../compiler/opt/force_dominance.h"
#include "../compiler/opt/inline.h"
#include "../compiler/opt/scope_resolution.h"

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
    PARSE_OPT("cleanupSafepoints", back, CleanupSafepoint());
#undef PARSE_OPT
    for (auto& optlist : opts)
        for (auto& opt : optlist.second)
            optimizations.push_back(std::move(opt));
}

void Configurations::defaultOptimizations() {
    auto addDefaultOpt = [&]() {
        optimizations.push_back(new pir::ForceDominance());
        optimizations.push_back(new pir::ScopeResolution());
        optimizations.push_back(new pir::Cleanup());
        optimizations.push_back(new pir::DelayInstr());
        optimizations.push_back(new pir::ElideEnv());
        optimizations.push_back(new pir::DelayEnv());
    };

    for (int j = 0; j < 2; ++j) {
        for (int i = 0; i < 3; ++i) {
            addDefaultOpt();
            optimizations.push_back(new pir::Cleanup());
            optimizations.push_back(new pir::Inline());
        }
        optimizations.push_back(new pir::CleanupSafepoint());
    }
}

} // namespace rir
