#ifndef RIR_2_PIR_COMPILER_H
#define RIR_2_PIR_COMPILER_H

#include "../opt/cleanup.h"
#include "../opt/delay_env.h"
#include "../opt/delay_instr.h"
#include "../opt/elide_env.h"
#include "../opt/force_dominance.h"
#include "../opt/inline.h"
#include "../opt/scope_resolution.h"
#include "rir_compiler.h"

namespace rir {
namespace pir {

class Rir2PirCompiler : public RirCompiler {
  public:
    Rir2PirCompiler(Module* module) : RirCompiler(module) {
        translations.push_back(new ForceDominance());
        translations.push_back(new ScopeResolution());
        translations.push_back(new Cleanup());
        translations.push_back(new DelayInstr());
        translations.push_back(new ElideEnv());
        translations.push_back(new DelayEnv());
    }

    Closure* compileClosure(SEXP);
    Closure* compileFunction(rir::Function*, const std::vector<SEXP>&);
    void optimizeModule();
    void printAfterPass(const std::string&, const std::string&, Closure*,
                        size_t);
    
    ~Rir2PirCompiler() {
        for (auto translation : translations) {
            delete translation;
        }
    }

  private:
    Closure* compileClosure(rir::Function*, const std::vector<SEXP>&,
                            Value* closureEnv);
    void applyOptimizations(Closure*, const std::string&);
};
} // namespace pir
} // namespace rir

#endif
