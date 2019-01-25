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
    static const Assumption minimalAssumptions[];
    static const Assumptions defaultAssumptions;

    Rir2PirCompiler(Module* module, StreamLogger& logger);

    void compileClosure(SEXP cls, const std::string& name, MaybeCls success,
                        Maybe fail) {
        compileClosure(cls, name, defaultAssumptions, success, fail);
    }
    void compileClosure(SEXP, const std::string& name, const Assumptions& ctx,
                        MaybeCls success, Maybe fail);
    void compileFunction(rir::Function* f, const std::string& name,
                         SEXP formals, SEXP srcRef, MaybeCls success,
                         Maybe fail) {
        compileFunction(f, name, formals, srcRef, defaultAssumptions, success,
                        fail);
    }
    void compileFunction(rir::Function*, const std::string& name, SEXP formals,
                         SEXP srcRef, const Assumptions& ctx, MaybeCls success,
                         Maybe fail);
    void optimizeModule();

  private:
    StreamLogger& logger;
    void compileClosure(Closure* closure, const OptimizationContext& ctx,
                        MaybeCls success, Maybe fail);
};
} // namespace pir
} // namespace rir

#endif
