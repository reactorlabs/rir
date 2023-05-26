//
// Created by Jakob Hain on 5/25/23.
//

#include "compiler/log/debug.h"
#include "compiler/pir/closure_version.h"
#include <string>

#pragma once

namespace rir {

static std::string printClosureVersionForCompilerServerComparison(pir::ClosureVersion* version) {
    std::stringstream pir;
    version->print(pir::DebugStyle::Standard, pir, true, false);
    return pir.str();
}

} // namespace rir
