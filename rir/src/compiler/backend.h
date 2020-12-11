#pragma once

#include "compiler/log/debug.h"
#include "compiler/log/stream_logger.h"
#include "compiler/pir/module.h"
#include "compiler/pir/pir.h"
#include "runtime/Function.h"

#include <sstream>
#include <unordered_set>

namespace rir {
namespace pir {

class Backend {
  public:
    explicit Backend(StreamLogger& logger) : logger(logger) {}
    Backend(const Backend&) = delete;
    Backend& operator=(const Backend&) = delete;

    rir::Function* compile(ClosureVersion* cls);

    void needsPatching(ClosureVersion* c, size_t i) { fixup[c].insert(i); }

  private:
    std::unordered_map<ClosureVersion*, Function*> done;
    std::unordered_map<ClosureVersion*, std::unordered_set<size_t>> fixup;

    StreamLogger& logger;

    rir::Function* doCompile(ClosureVersion* cls, ClosureStreamLogger& log);
};

} // namespace pir
} // namespace rir
