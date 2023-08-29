//
// Created by Jakob Hain on 5/25/23.
//

#include "compiler/pir/closure_version.h"
#include "compiler/parameter.h"
#include <string>

#pragma once

#define COMPARE_COMPILER_CLIENT_SENT_BYTECODE_WITH_SOURCE 0

namespace rir {

enum class Request : uint64_t {
    /// For large requests, we send the hash. If the server already received
    /// the same request it will serve the cached response. Otherwise it will
    /// send `Response::NeedsFull`
    Memoize = 0x217A25432A462D4B,
    /// Compile a function with assumptions and debug options
    Compile = 0x217A25432A462D4A,
    /// Retrieve an SEXP on the server referenced by an SEXP on the client
    Retrieve = 0x217A25432A462D4D,
    /// Kill the server
    Kill = 0x217A25432A462D4C,
    /// Retrieved SEXP
    Retrieved = 0x217A25432A462D4E,
    /// SEXP isn't in client
    RetrieveFailed = 0x217A25432A462D4F,
};

enum class Response : uint64_t {
    /// Memoized request - needs the full response
    NeedsFull = 0x9BEEB1E5356F1A37,
    /// Compiled closure
    Compiled = 0x9BEEB1E5356F1A36,
    /// Retrieved SEXP
    Retrieved = 0x9BEEB1E5356F1A3D,
    /// SEXP isn't in server
    RetrieveFailed = 0x9BEEB1E5356F1A3E,
    /// Acknowledge that the server has been killed
    Killed = 0x9BEEB1E5356F1A38,
    /// Retrieve an SEXP on the client referenced by an SEXP on the server
    NeedsRetrieve = 0x9BEEB1E5356F1A3C,
};

/// If set, we still compile on the client and only compare the compiler server
/// and client results, instead of replacing the SEXP with the compiled version.
extern bool PIR_CLIENT_DRY_RUN;
extern size_t PIR_CLIENT_COMPILE_SIZE_TO_HASH_ONLY;

std::string printClosureVersionForCompilerServerComparison(pir::ClosureVersion* version);

/// Alternative to `throw error_t()` in zeromq, since we don't allow exceptions.
/// Used by external/zeromq so it may be unused before setup.
__attribute__((unused)) NORET void zeromq_error(const char* func);

} // namespace rir
