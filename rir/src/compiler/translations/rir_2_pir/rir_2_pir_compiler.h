#ifndef RIR_2_PIR_COMPILER_H
#define RIR_2_PIR_COMPILER_H

#include "../rir_compiler.h"

namespace rir {
namespace pir {

class Rir2PirCompiler : public RirCompiler {
  public:
    Rir2PirCompiler(Module* module);

    Closure* compileClosure(SEXP) override;
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
                            Env* closureEnv);
    void applyOptimizations(Closure*, const std::string&);
};
} // namespace pir
} // namespace rir

#endif
