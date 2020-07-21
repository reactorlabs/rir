#ifndef RIR_2_PIR_COMPILER_H
#define RIR_2_PIR_COMPILER_H

#include "../../../utils/FormalArgs.h"
#include "../../debugging/stream_logger.h"
#include "../rir_compiler.h"
#include <list>
#include <stack>

namespace rir {
struct DispatchTable;
namespace pir {

class Rir2PirCompiler : public RirCompiler {
  public:
    static constexpr Context::Flags minimalContext =
        Context::Flags(Assumption::CorrectOrderOfArguments) |
        Assumption::NotTooManyArguments;
    static constexpr Context defaultContext =
        Context(Context::Flags(Assumption::CorrectOrderOfArguments) |
                    Assumption::NotTooManyArguments,
                0);

    Rir2PirCompiler(Module* module, StreamLogger& logger)
        : RirCompiler(module), logger(logger){};

    void compileClosure(SEXP, const std::string& name, const Context& ctx,
                        MaybeCls success, Maybe fail,
                        std::list<PirTypeFeedback*> outerFeedback);
    void compileFunction(rir::DispatchTable*, const std::string& name,
                         SEXP formals, SEXP srcRef, const Context& ctx,
                         MaybeCls success, Maybe fail,
                         std::list<PirTypeFeedback*> outerFeedback);
    void optimizeModule();

    bool seenC = false;

  private:
    StreamLogger& logger;
    void compileClosure(Closure* closure, rir::Function* optFunction,
                        const Context& ctx, MaybeCls success, Maybe fail,
                        std::list<PirTypeFeedback*> outerFeedback);
};

} // namespace pir
} // namespace rir

#endif
