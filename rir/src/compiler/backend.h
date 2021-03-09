#pragma once

#include "R/Preserve.h"
#include "compiler/log/debug.h"
#include "compiler/log/stream_logger.h"
#include "compiler/native/pir_debug_info.h"
#include "compiler/native/pir_jit_llvm.h"
#include "compiler/pir/module.h"
#include "compiler/pir/pir.h"
#include "runtime/Function.h"

#include <sstream>
#include <unordered_set>

namespace rir {
namespace pir {

class Backend {
  public:
#ifdef PIR_GDB_SUPPORT
    Backend(StreamLogger& logger, const std::string& name)
        : jit(name), logger(logger) {}
#else
    explicit Backend(StreamLogger& logger) : logger(logger) {}
#endif
    Backend(const Backend&) = delete;
    Backend& operator=(const Backend&) = delete;

    rir::Function* getOrCompile(ClosureVersion* cls);

  private:
    Preserve preserve;
    PirJitLLVM jit;
    std::unordered_map<ClosureVersion*, Function*> done;
    StreamLogger& logger;

    rir::Function* doCompile(ClosureVersion* cls, ClosureStreamLogger& log);
};

} // namespace pir
} // namespace rir
