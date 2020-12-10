#pragma once

#include "compiler/log/debug.h"
#include "compiler/log/stream_logger.h"
#include "compiler/native/lower_llvm.h"
#include "compiler/pir/module.h"
#include "compiler/pir/pir.h"
#include "runtime/Function.h"

#include <sstream>
#include <unordered_set>

namespace rir {
namespace pir {

class Pir2RirCompiler {
  public:
    explicit Pir2RirCompiler(StreamLogger& logger) : logger(logger), native() {}
    Pir2RirCompiler(const Pir2RirCompiler&) = delete;
    Pir2RirCompiler& operator=(const Pir2RirCompiler&) = delete;

    rir::Function* compile(ClosureVersion* cls);

    StreamLogger& logger;

    Function* alreadyCompiled(ClosureVersion* cls) {
        return done.count(cls) ? done.at(cls) : nullptr;
    }
    bool isCompiling(ClosureVersion* cls) { return done.count(cls); }

    void needsPatching(ClosureVersion* c, size_t i) { fixup[c].insert(i); }

  private:
    std::unordered_map<ClosureVersion*, Function*> done;
    std::unordered_map<ClosureVersion*, std::unordered_set<size_t>> fixup;
    LowerLLVM native;
    rir::Function* doCompile(ClosureVersion* cls, ClosureStreamLogger& log);
};

} // namespace pir
} // namespace rir
