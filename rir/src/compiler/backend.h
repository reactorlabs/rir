#pragma once

#include "R/Preserve.h"
#include "compiler/log/debug.h"
#include "compiler/log/stream_logger.h"
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
    Backend(Module* m, StreamLogger& logger, const std::string& name)
        : module(m), jit(name), logger(logger) {}
    Backend(const Backend&) = delete;
    Backend& operator=(const Backend&) = delete;

    rir::Function* getOrCompile(ClosureVersion* cls);

    void deserialize(
      std::vector<std::vector<std::vector<size_t>>> & argOrderingData,
      size_t hast, Context context,
      int & envCreation, int & optimization, unsigned int & numArguments, size_t & dotsPosition, // for function signature
      std::string bcPath, std::string poolPath, std::string startingHandle, std::string promiseData, std::string srcData, std::string argData,
      size_t & cPoolEntriesSize, size_t & srcPoolEntriesSize, size_t & ePoolEntriesSize, size_t & promiseSrcPoolEntriesSize
      );

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
    StreamLogger& logger;

    rir::Function* doCompile(ClosureVersion* cls, ClosureStreamLogger& log);
};

} // namespace pir
} // namespace rir
