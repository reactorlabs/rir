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
    Rir2PirCompiler(Module* module, StreamLogger& logger);

    void compileClosure(SEXP, const std::string& name, MaybeCls success,
                        Maybe fail);
    void compileFunction(rir::Function*, const std::string& name,
                         FormalArgs const&, MaybeCls success, Maybe fail);
    void optimizeModule(StreamLogger& logger, bool preserveVersions = false);

  private:
    StreamLogger& logger;
    void compileClosure(rir::Function*, const std::string& name,
                        FormalArgs const&, Env* closureEnv, MaybeCls success,
                        Maybe fail);
    void applyOptimizations(Closure*, const std::string&);
};
} // namespace pir
} // namespace rir

#endif
