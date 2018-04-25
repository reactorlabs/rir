#ifndef PIR_COMPILER_H
#define PIR_COMPILER_H

#include "../rir_compiler.h"

namespace rir {
namespace pir {

class PirCompiler : public RirCompiler {
  public:
    PirCompiler(Module* module);

    IRCode compile(IRCode);
    void optimizeModule();
    
    static RirInput createRirInputFromSEXP(SEXP, std::vector<SEXP>&, Env* env);
    static RirInput createRirInputFromFunction(rir::Function*, std::vector<SEXP>&);
    
  private:
    Closure* compileClosure(rir::Function*, const std::vector<SEXP>&,
                            Env* closureEnv);
    void optimize(rir::Function*, Closure*);
    void applyOptimizations(Closure*, const std::string&);
    void printAfterPass(const std::string&, const std::string&, Closure*,
                        size_t);

};
} // namespace pir
} // namespace rir

#endif
