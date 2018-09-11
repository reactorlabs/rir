#pragma once

#include "../debugging/debugging.h"
#include "../debugging/stream_logger.h"
#include "../pir/module.h"
#include "../pir/pir.h"
#include "runtime/Function.h"

#include <sstream>
#include <unordered_set>

namespace rir {
namespace pir {

class Pir2RirCompiler {
  public:
    Pir2RirCompiler(const DebugOptions& debug, StreamLogger& logger)
        : debug(debug), logger(logger) {}

    const DebugOptions debug;

    void compile(Closure* cls, SEXP origin);
    StreamLogger& getLogger() { return logger; }

  private:
    StreamLogger& logger;
    std::unordered_set<Closure*> done;
};

} // namespace pir
} // namespace rir
