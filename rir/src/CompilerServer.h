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
  public:
    /// If PIR_SERVER_ADDR is set, initializes and starts handling requests
    static void tryRun();
};

} // namespace rir