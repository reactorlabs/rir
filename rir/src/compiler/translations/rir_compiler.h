#ifndef RIR__PIR_COMPILER_H
#define RIR__PIR_COMPILER_H

#include "../pir/closure.h"
#include "../pir/module.h"
#include "../pir/value.h"
#include "pir_translator.h"
#include "runtime/Function.h"
#include <vector>

namespace rir {
namespace pir {

class RirCompiler {
  public:
    RirCompiler(Module* module) : module(module) {}
    RirCompiler(const RirCompiler&) = delete;
    void operator=(const RirCompiler&) = delete;

    typedef std::function<void()> Maybe;
    typedef std::function<void(Closure*)> MaybeVal;

    virtual void compileClosure(SEXP, MaybeVal, Maybe) = 0;
    void compileClosure(SEXP cls, MaybeVal success) {
        return compileClosure(cls, success, []() {});
    }

    bool isVerbose() { return verbose; }
    void setVerbose(bool v) { verbose = v; }

  private:
    bool verbose = false;

  protected:
    std::vector<PirTranslator*> translations;
    Module* module;
};
} // namespace pir
} // namespace rir

#endif
