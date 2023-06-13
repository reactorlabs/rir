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

    static bool _isRunning;
  public:
    class Handle {
        friend class CompilerClient;
#ifdef MULTI_THREADED_COMPILER_CLIENT
        std::shared_ptr<int> socketIndexRef;
        std::future<ResponseData> response;
        Handle(const std::shared_ptr<int>& socketIndexRef,
               std::future<ResponseData> response)
            : socketIndexRef(socketIndexRef), response(std::move(response)) {}
#else
        ResponseData response;
        Handle(ResponseData response) : response(std::move(response)) {}
#endif
      public:
        /// When we get response PIR, compares it with given locally-compiled
        /// closure PIR and logs any discrepancies.
        void compare(pir::ClosureVersion* version) const;
    };

    /// Returns if the client was initialized
    static bool isRunning() { return _isRunning; }

    /// Initializes if PIR_CLIENT_ADDR is set
    static void tryInit();
    /// Asynchronously sends the closure to the compile server and returns a
    /// handle to use the result.
    static Handle* pirCompile(SEXP what, const Context& assumptions,
                              const std::string& name,
                              const pir::DebugOptions& debug);

    /// Send a message from the compiler client (this) to each connected
    /// compiler server, which kills the server (exit 0) on receive. Then stops
    /// the client for the remainder of the session
    static void killServers();
};

} // namespace rir
