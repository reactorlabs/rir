#ifndef RIR__PIR_COMPILER_H
#define RIR__PIR_COMPILER_H

#include "../pir/module.h"
#include "../pir/value.h"
#include "runtime/Function.h"
#include "pir_translator.h"
#include "../pir/closure.h"
#include <vector>

namespace rir {
namespace pir {

class RirCompiler {
  public:
    RirCompiler(Module* module) : module(module) {}
    virtual Closure* compileClosure(SEXP) = 0;
    //virtual Closure* compileClosure(rir::Function*, const std::vector<SEXP>&,
    //                        Value* closureEnv);
    //virtual Closure* compileFunction(rir::Function*, const std::vector<SEXP>&);
    
    Module* getModule() { return module; }
    bool isVerbose() { return verbose; }
    void setVerbose(bool v) { verbose = v; }
  private:
    std::vector<PirTranslator> translations;
    bool verbose = false;
  protected:
    Module* module;
};
}
}

#endif
