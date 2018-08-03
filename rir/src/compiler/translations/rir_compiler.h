#ifndef RIR__PIR_COMPILER_H
#define RIR__PIR_COMPILER_H

#include "../pir/pir.h"
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
    typedef std::function<void(Closure*)> MaybeCls;

    virtual void compileClosure(SEXP, MaybeCls, Maybe) = 0;
    void compileClosure(SEXP cls, MaybeCls success) {
        return compileClosure(cls, success, []() {});
    }

    bool isVerbose() { return verbose; }
    bool shouldPrintOriginalVersion() { return verbose & PRINT_ORIGINAL_MASK; }
    bool shouldPrintCompiledVersion() { return verbose & PRINT_RAW_PIR_MASK; }
    bool shouldPrintOptimizations() { return verbose & PRINT_OPT_PHASES_MASK; }
    bool shouldPrintInliningVersions() { return verbose & PRINT_INLINIG_MASK; }
    void setVerbose(uint v) { verbose = v; }

  private:
    uint32_t verbose = 0;

  protected:
    std::vector<PirTranslator*> translations;
    Module* module;
};
} // namespace pir
} // namespace rir

#endif
