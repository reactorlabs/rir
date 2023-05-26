//
// Created by Jakob Hain on 5/25/23.
//

#include "compiler/log/debug.h"
#include "compiler/pir/closure_version.h"
#include <string>

#pragma once

namespace rir {

const uint64_t PIR_COMPILE_MAGIC = 0x217A25432A462D4A;

static std::string printClosureVersionForCompilerServerComparison(pir::ClosureVersion* version) {
    std::stringstream pir;
    version->print(pir::DebugStyle::Standard, pir, true, false);
    return pir.str();
}

/// Alternative to `throw error_t()` in zeromq, since we don't allow exceptions.
/// Used by external/zeromq so it may be unused before setup.
__attribute__((unused)) NORET void zeromq_error();

} // namespace rir
