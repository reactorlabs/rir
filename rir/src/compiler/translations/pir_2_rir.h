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

    rir::Function* compile(Closure* cls, SEXP origin, bool dryRun);

    StreamLogger& logger;

    Function* alreadyCompiled(Closure* cls) {
        return done.count(cls) ? done.at(cls) : nullptr;
    }
    bool isCompiling(Closure* cls) { return done.count(cls); }

    void needsPatching(Closure* c, size_t i) { fixup[c].insert(i); }

  private:
    std::unordered_map<Closure*, Function*> done;
    std::unordered_map<Closure*, std::unordered_set<size_t>> fixup;
};

} // namespace pir
} // namespace rir
