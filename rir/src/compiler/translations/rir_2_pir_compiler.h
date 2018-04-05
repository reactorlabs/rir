#ifndef RIR_2_PIR_COMPILER_H
#define RIR_2_PIR_COMPILER_H

#include "rir_compiler.h"

namespace rir {
namespace pir {

class Rir2PirCompiler : public RirCompiler {
  public:
    Rir2PirCompiler(Module* module) : RirCompiler(module) {}
    Closure* compileClosure(SEXP);
    Closure* compileClosure(rir::Function*, const std::vector<SEXP>&,
                            Value* closureEnv);
    Closure* compileFunction(rir::Function*, const std::vector<SEXP>&);
    void optimizeModule();
};
} // namespace pir
} // namespace rir

#endif
