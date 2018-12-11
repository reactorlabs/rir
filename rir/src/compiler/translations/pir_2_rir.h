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
    explicit Pir2RirCompiler(StreamLogger& logger) : logger(logger) {}
    Pir2RirCompiler(const Pir2RirCompiler&) = delete;
    Pir2RirCompiler& operator=(const Pir2RirCompiler&) = delete;

    void compile(Closure* cls, SEXP origin, bool dryRun);

    StreamLogger& logger;

  private:
    std::unordered_set<Closure*> done;
};

} // namespace pir
} // namespace rir
