#include <iostream>
#include <sstream>

#include "configurations.h"

#include "../compiler/opt/cleanup.h"
#include "../compiler/opt/delay_env.h"
#include "../compiler/opt/delay_instr.h"
#include "../compiler/opt/elide_env.h"
#include "../compiler/opt/force_dominance.h"
#include "../compiler/opt/inline.h"
#include "../compiler/opt/scope_resolution.h"

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
    read(reader, "globalValueNumber");
    read(reader, "escapeAnalysis");
    read(reader, "forceDominance");
    read(reader, "delayInstructions");
    read(reader, "elideEnvironments");
    read(reader, "delayEnvironments");
    string cleanups = reader.Get("optimizations", "cleanup", "UNKNOWN");
    if (cleanups != "UNKNOWN") {
        std::stringstream data(cleanups);
        std::string order;
        while (std::getline(data, order, ',')) {
            optimizations.insert(
                new Optimization(new pir::Cleanup(), std::stoi(order)));
        }
    }
}

void Configurations::defaultOptimizations() {
    optimizations.insert(new Optimization(new pir::ForceDominance(), 1));
    optimizations.insert(new Optimization(new pir::ScopeResolution(), 2));
    optimizations.insert(new Optimization(new pir::Cleanup(), 3));
    optimizations.insert(new Optimization(new pir::Cleanup(), 4));
    optimizations.insert(new Optimization(new pir::DelayInstr(), 5));
    optimizations.insert(new Optimization(new pir::ElideEnv(), 6));
    optimizations.insert(new Optimization(new pir::DelayEnv(), 7));
    optimizations.insert(new Optimization(new pir::Cleanup(), 8));
}

void Configurations::read(INIReader& reader, string optimizationName) {
    short order = reader.GetInteger("optimizations", optimizationName, 0);
    if (order) {
        if (optimizationName == "globalValueNumber") {
            // optimizations.insert(new Optimization(new pir::Cleanup(), order
            // ));
        } else if (optimizationName == "forceDominance") {
            optimizations.insert(
                new Optimization(new pir::ForceDominance(), order));
        } else if (optimizationName == "escapeAnalysis") {
            optimizations.insert(
                new Optimization(new pir::ScopeResolution(), order));
        } else if (optimizationName == "delayInstructions") {
            optimizations.insert(
                new Optimization(new pir::DelayInstr(), order));
        } else if (optimizationName == "elideEnvironments") {
            optimizations.insert(new Optimization(new pir::ElideEnv(), order));
        } else if (optimizationName == "delayEnvironments") {
            optimizations.insert(new Optimization(new pir::DelayEnv(), order));
        }
    }
}

} // namespace rir
