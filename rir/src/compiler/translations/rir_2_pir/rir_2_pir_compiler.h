#ifndef RIR_2_PIR_COMPILER_H
#define RIR_2_PIR_COMPILER_H

#include "../rir_compiler.h"

namespace rir {
namespace pir {

class Rir2PirCompiler : public RirCompiler {
  public:
    Rir2PirCompiler(Module* module);

    void compileClosure(SEXP, MaybeVal success, Maybe fail) override;
    void compileFunction(rir::Function*, const std::vector<SEXP>&,
                         MaybeVal success, Maybe fail);
    void optimizeModule();
    void printAfterPass(const std::string&, const std::string&, Closure*,
                        size_t);
  private:
    void compileClosure(rir::Function*, const std::vector<SEXP>&,
                        Env* closureEnv, MaybeVal success, Maybe fail);
    void applyOptimizations(Closure*, const std::string&);
};
} // namespace pir
} // namespace rir

#endif
