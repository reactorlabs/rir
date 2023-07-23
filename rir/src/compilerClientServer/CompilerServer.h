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
 * Compiler server.
 * On startup, attempts to bind to PIR_SERVER_ADDR with a zeromq "reply" socket.
 * If successful, it will wait for incoming "compile" requests and process them
 * by calling pirCompile.
 */
class CompilerServer {
    static bool _isRunning;

  public:
    /// Is this Ř instance a compiler server?
    static bool isRunning() { return _isRunning; }
    /// If PIR_SERVER_ADDR is set, initializes and starts handling requests
    static void tryRun();

    /// Synchronously retrieves the closure with the given hash from the client.
    /// If in the future we make this asynchronous, should still return a
    /// closure SEXP but make it block while we're waiting for the response.
    ///
    /// The SEXP is also interned. It must actually be interned before we finish
    /// deserializing for recursive retrievals (a -> retrieve b -> retrieve a ->
    /// ...).
    ///
    /// Returns `nullptr` if the client doesn't have the closure.
    static SEXP retrieve(const UUID& hash);
};

} // namespace rir