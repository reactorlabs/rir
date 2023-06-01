//
// Created by Jakob Hain on 5/25/23.
//

#pragma once

#include "R/r_incl.h"
#include "compiler/log/debug.h"
#include "compiler/log/loggers.h"
#include "compiler/log/log.h"
#include "compiler/pir/closure_version.h"
#include "runtime/Context.h"
#include <future>

namespace rir {

/**
 * Compiler server client.
 * On startup, attempts to connect to a compile-server on PIR_CLIENT_ADDR (weird
 * naming because we reserve PIR_SERVER_ADDR to be set only for the compiler-
 * server) with a zeromq "request" socket. If successful, it will use the server t
 * compile RIR to PIR (currently just compares to check for discrepancies).
 */
class CompilerClient {
    struct ResponseData {
        SEXP sexp;
        std::string finalPir;
    };
  public:
    class Handle {
        friend class CompilerClient;
        std::future<ResponseData> response;
        explicit Handle(std::future<ResponseData> response)
            : response(std::move(response)) {}
      public:
        /// When we get response PIR, compares it with given locally-compiled
        /// closure PIR and logs any discrepancies.
        void compare(pir::ClosureVersion* version);
    };

    /// Initializes if PIR_CLIENT_ADDR is set
    static void tryInit();
    /// Asynchronously sends the closure to the compile server and returns a
    /// handle to use the result.
    static Handle* pirCompile(SEXP what, const Context& assumptions,
                              const std::string& name,
                              const pir::DebugOptions& debug);
};

} // namespace rir
