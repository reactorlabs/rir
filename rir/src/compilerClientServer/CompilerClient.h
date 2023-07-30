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
#include <utility>

class ByteBuffer;

namespace rir {

class UUID;

/**
 * Compiler server client.
 * On startup, attempts to connect to a compile-server on PIR_CLIENT_ADDR (weird
 * naming because we reserve PIR_SERVER_ADDR to be set only for the compiler-
 * server) with a zeromq "request" socket. If successful, it will use the server t
 * compile RIR to PIR (currently just compares to check for discrepancies).
 */
class CompilerClient {
    struct CompiledResponseData {
        SEXP sexp;
        std::string finalPir;

        CompiledResponseData(SEXP sexp, const std::string&& finalPir)
            : sexp(sexp), finalPir(finalPir) {
            R_PreserveObject(sexp);
        }

        ~CompiledResponseData() {
            R_ReleaseObject(sexp);
        }
    };
    template<typename T>
    class Handle {
        friend class CompilerClient;
#ifdef MULTI_THREADED_COMPILER_CLIENT
        std::shared_ptr<int> socketIndexRef;
        std::future<T> response;
        CompiledHandle(const std::shared_ptr<int>& socketIndexRef,
                       std::future<T> response)
            : socketIndexRef(socketIndexRef), response(std::move(response)) {}
        /// Block and get the response data
        T getResponse() const;
#else
        T response;
        explicit Handle(T response) : response(std::move(response)) {}
#endif
    };

    static bool _isRunning;

    template<typename T>
    static Handle<T>* request(
            const std::function<void(ByteBuffer&)>&& makeRequest,
            const std::function<T(ByteBuffer&)>&& makeResponse);
  public:
    class CompiledHandle {
        friend class CompilerClient;
        Handle<CompiledResponseData>* inner;
        explicit CompiledHandle(Handle<CompiledResponseData>* inner)
            : inner(inner) {}
      public:
        ~CompiledHandle() { delete inner; }

        /// When we get response PIR, compares it with given locally-compiled
        /// closure PIR and logs any discrepancies.
        void compare(pir::ClosureVersion* version) const;
        /// Block and get the SEXP
        SEXP getSexp() const;
        /// Block and get the final PIR debug print
        const std::string& getFinalPir() const;
    };

    /// Returns if the client was initialized
    static bool isRunning() { return _isRunning; }

    /// Initializes if PIR_CLIENT_ADDR is set
    static void tryInit();
    /// "Asynchronously" (not currently, maybe in the future) sends the closure
    /// to the compile server and returns a handle to use the result.
    /// Then interns the result,
    static CompiledHandle* pirCompile(SEXP what, const Context& assumptions,
                                      const std::string& name,
                                      const pir::DebugOptions& debug);
    /// Synchronously retrieves the closure with the given hash from the server.
    /// If in the future we make this asynchronous, should still return a
    /// closure SEXP but make it block while we're waiting for the response.
    ///
    /// The SEXP is also interned. It must actually be interned before we finish
    /// deserializing for recursive retrievals (a -> retrieve b -> retrieve a ->
    /// ...).
    ///
    /// Returns `nullptr` if the server doesn't have the closure.
    static SEXP retrieve(const UUID& hash);

    /// Send a message from the compiler client (this) to each connected
    /// compiler server, which kills the server (exit 0) on receive. Then stops
    /// the client for the remainder of the session
    static void killServers();
};

} // namespace rir
