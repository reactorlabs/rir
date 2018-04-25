#ifndef RIR__PIR_COMPILER_H
#define RIR__PIR_COMPILER_H

#include "../pir/module.h"
#include "../pir/value.h"
#include "../util/builder.h"
#include "ir_translator.h"
#include "runtime/Function.h"
#include <vector>

namespace rir {
namespace pir {

struct RirInput {
    RirInput(rir::Function* function, std::vector<SEXP>* args, Env* env)
        : function(function), args(args), env(env) {}
    rir::Function* function;
    std::vector<SEXP>* args;
    Env* env;
};

class RirCompiler {
  public:
    RirCompiler(Module* module) : module(module) {}
    RirCompiler(const RirCompiler&) = delete;
    void operator=(const RirCompiler&) = delete;

    virtual IRCode compile(IRCode) = 0;
    virtual void optimizeModule() = 0;
    // virtual IREntryPoint decompile(IRCode) = 0;

    bool isVerbose() { return verbose; }
    void setVerbose(bool v) { verbose = v; }
    bool optimizationsEnabled() { return runOptimizations; }
    void enableOptimizations() { runOptimizations = true; }
    Env* getEnv(SEXP sexp) { return module->getEnv(CLOENV(sexp)); }

    ~RirCompiler() {
        delete compileToOptimizingIR;
        for (auto translation : optimizations) {
            delete translation;
        }
    }

    void setBuilder(Builder* bldr) { insert = bldr; }
    Builder* getBuilder() { return insert; }
    Module* getModule() { return module; }

  private:
    bool verbose = false;
    bool runOptimizations = false;

  protected:
    Module* module;
    Builder* insert;

    IRTranslator* compileToOptimizingIR;
    std::vector<IRTranslator*> optimizations;
    // IRTranslator* decompileToRunningIR;
};
} // namespace pir
} // namespace rir

#endif
