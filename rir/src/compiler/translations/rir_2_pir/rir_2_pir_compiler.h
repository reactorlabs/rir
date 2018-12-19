#ifndef RIR_2_PIR_COMPILER_H
#define RIR_2_PIR_COMPILER_H

#include "../../../utils/FormalArgs.h"
#include "../../debugging/stream_logger.h"
#include "../rir_compiler.h"
#include <stack>

namespace rir {
namespace pir {

class Rir2PirCompiler : public RirCompiler {
  public:
    static const Assumptions minimalAssumptions;

    Rir2PirCompiler(Module* module, StreamLogger& logger);

    void compileClosure(SEXP, const std::string& name, const Assumptions& ctx,
                        MaybeCls success, Maybe fail);
    void compileFunction(rir::Function*, const std::string& name,
                         FormalArgs const&, const Assumptions& ctx,
                         MaybeCls success, Maybe fail);
    void optimizeModule();

  private:
    StreamLogger& logger;
    void compileClosure(rir::Function*, const std::string& name,
                        FormalArgs const&, const OptimizationContext& ctx,
                        MaybeCls success, Maybe fail);
    void applyOptimizations(Closure*, const std::string&);
};
} // namespace pir
} // namespace rir

#endif
