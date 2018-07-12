#ifndef RIR_2_PIR_COMPILER_H
#define RIR_2_PIR_COMPILER_H

#include "../../../utils/FormalArgs.h"
#include "../rir_compiler.h"

namespace rir {
namespace pir {

class Rir2PirCompiler : public RirCompiler {
  public:
    Rir2PirCompiler(Module* module);

    void compileClosure(SEXP, MaybeCls success, Maybe fail) override;
    void compileFunction(rir::Function*, FormalArgs const&, MaybeCls success,
                         Maybe fail);
    void optimizeModule();
    void printAfterPass(const std::string&, const std::string&, Closure*,
                        size_t);

  private:
    void compileClosure(rir::Function*, FormalArgs const&, Env* closureEnv,
                        MaybeCls success, Maybe fail);
    void applyOptimizations(Closure*, const std::string&);
};
} // namespace pir
} // namespace rir

#endif
