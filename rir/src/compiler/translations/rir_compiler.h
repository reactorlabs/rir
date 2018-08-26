#ifndef RIR__PIR_COMPILER_H
#define RIR__PIR_COMPILER_H

#include "../debugging/debugging.h"
#include "../pir/closure.h"
#include "../pir/module.h"
#include "../pir/pir.h"
#include "../pir/value.h"
#include "pir_translator.h"
#include "runtime/Function.h"
#include <vector>

namespace rir {
namespace pir {

class RirCompiler {
  public:
    RirCompiler(Module* module, const DebugOptions& debug)
        : debug(debug), module(module) {}
    RirCompiler(const RirCompiler&) = delete;
    void operator=(const RirCompiler&) = delete;

    typedef std::function<void()> Maybe;
    typedef std::function<void(Closure*)> MaybeCls;

    virtual void compileClosure(SEXP, MaybeCls, Maybe) = 0;
    void compileClosure(SEXP cls, MaybeCls success) {
        return compileClosure(cls, success, []() {});
    }

    const DebugOptions debug;

  protected:
    std::vector<PirTranslator*> translations;
    Module* module;
};
} // namespace pir
} // namespace rir

#endif
