//
// Created by Jakob Hain on 5/25/23.
//

#include "compiler/pir/closure_version.h"
#include <string>

#pragma once

namespace rir {

const uint64_t PIR_COMPILE_MAGIC = 0x217A25432A462D4A;
const uint64_t PIR_COMPILE_RESPONSE_MAGIC = 0x9BEEB1E5356F1A36;

std::string printClosureVersionForCompilerServerComparison(pir::ClosureVersion* version);

/// Alternative to `throw error_t()` in zeromq, since we don't allow exceptions.
/// Used by external/zeromq so it may be unused before setup.
__attribute__((unused)) NORET void zeromq_error(const char* func);

} // namespace rir
