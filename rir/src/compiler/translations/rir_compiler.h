#ifndef RIR__PIR_COMPILER_H
#define RIR__PIR_COMPILER_H

#include "../debugging/debugging.h"
#include "../pir/closure.h"
#include "../pir/module.h"
#include "../pir/pir.h"
#include "../pir/value.h"
#include "R/Preserve.h"
#include "pir_translator.h"
#include "runtime/Function.h"
#include <vector>

namespace rir {
namespace pir {

class RirCompiler {
  public:
    explicit RirCompiler(Module* module) : module(module) {}
    RirCompiler(const RirCompiler&) = delete;
    RirCompiler& operator=(const RirCompiler&) = delete;

    typedef std::function<void()> Maybe;
    typedef std::function<void(Closure*)> MaybeCls;

    void preserve(SEXP c) { preserve_(c); }

  protected:
    std::vector<const PirTranslator*> translations;
    Module* module;
    Preserve preserve_;
};
} // namespace pir
} // namespace rir

#endif
