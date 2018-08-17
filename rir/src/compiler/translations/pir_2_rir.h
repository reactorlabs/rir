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
    Pir2RirCompiler(const DebugOptions& debug, StreamLogger& log)
        : debug(debug), log(log) {}

    const DebugOptions debug;

    void compile(Closure* cls, SEXP origin);
    StreamLogger& getLog() { return log; }

  private:
    StreamLogger& log;
    std::unordered_set<Closure*> done;
};

} // namespace pir
} // namespace rir
