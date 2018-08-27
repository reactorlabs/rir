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
    Rir2PirCompiler(Module* module, const DebugOptions& debug);

    void compileClosure(SEXP, MaybeCls success, Maybe fail) override;
    void compileFunction(rir::Function*, FormalArgs const&, MaybeCls success,
                         Maybe fail);
    void optimizeModule();

    StreamLogger& getLog() { return log; }

  private:
    void compileClosure(rir::Function*, FormalArgs const&, Env* closureEnv,
                        MaybeCls success, Maybe fail);
    void applyOptimizations(Closure*, const std::string&);

    StreamLogger log;
};
} // namespace pir
} // namespace rir

#endif
