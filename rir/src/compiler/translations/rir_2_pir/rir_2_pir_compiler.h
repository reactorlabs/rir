#ifndef RIR_2_PIR_COMPILER_H
#define RIR_2_PIR_COMPILER_H

#include "../../../utils/FormalArgs.h"
#include "../../debugging/stream_logger.h"
#include "../rir_compiler.h"
#include <stack>
namespace rir {
struct DispatchTable;
namespace pir {

class Rir2PirCompiler : public RirCompiler {
  public:
    static constexpr Assumptions::Flags minimalAssumptions =
        Assumptions::Flags(Assumption::CorrectOrderOfArguments) |
        Assumption::NotTooManyArguments;
    static constexpr Assumptions defaultAssumptions =
        Assumptions(Assumptions::Flags(Assumption::CorrectOrderOfArguments) |
                        Assumption::NotTooManyArguments,
                    0);

    Rir2PirCompiler(Module* module, StreamLogger& logger)
        : RirCompiler(module), logger(logger){};

    void compileClosure(SEXP, const std::string& name, const Assumptions& ctx,
                        MaybeCls success, Maybe fail);
    void compileFunction(rir::DispatchTable*, const std::string& name,
                         SEXP formals, SEXP srcRef, const Assumptions& ctx,
                         MaybeCls success, Maybe fail);
    void optimizeModuleFor(const ClosureVersion* version);
    void optimizeModule();

    bool seenC = false;

  private:
    StreamLogger& logger;
    void compileClosure(Closure* closure, rir::Function* optFunction,
                        const OptimizationContext& ctx, MaybeCls success,
                        Maybe fail);
};

} // namespace pir
} // namespace rir

#endif
