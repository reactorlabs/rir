#pragma once

#include "R/Preserve.h"
#include "compiler/log/debug.h"
#include "compiler/log/loggers.h"
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
    Backend(Module* m, Log& logger, const std::string& name)
        : module(m), jit(name), logger(logger) {}
    Backend(const Backend&) = delete;
    Backend& operator=(const Backend&) = delete;

    rir::Function* getOrCompile(ClosureVersion* cls);

  private:
    struct LastDestructor {
        LastDestructor();
        ~LastDestructor();
    };
    LastDestructor firstMember_;
    Preserve preserve;

    Module* module;
    PirJitLLVM jit;
    std::unordered_map<ClosureVersion*, Function*> done;
    Log& logger;

    rir::Function* doCompile(ClosureVersion* cls, ClosureLog& log);
};

} // namespace pir
} // namespace rir
