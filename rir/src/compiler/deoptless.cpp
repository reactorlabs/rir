#include "deoptless.h"

#include "compiler/backend.h"
#include "compiler/compiler.h"
#include "pir/deopt_context.h"
#include "pir/pir_impl.h"

namespace rir {
namespace pir {

Function* DeoptLess::dispatch(SEXP closure, rir::Code* c,
                              const DeoptContext& ctx) {
    DeoptlessDispatchTable* dispatchTable = nullptr;
    if (c->extraPoolSize > 0) {
        dispatchTable = DeoptlessDispatchTable::check(
            c->getExtraPoolEntry(c->extraPoolSize - 1));
    }
    if (!dispatchTable) {
        dispatchTable = DeoptlessDispatchTable::create();
        c->addExtraPoolEntry(dispatchTable->container());
    }

    Function* fun = dispatchTable->dispatch(ctx);

    if (!fun && !dispatchTable->full()) {
        // compile to pir
        pir::Module* module = new pir::Module;

        pir::StreamLogger logger(DebugOptions::DefaultDebugOptions);
        logger.title("Compiling continuation");
        pir::Compiler cmp(module, logger);

        pir::Backend backend(module, logger, "continuation");

        cmp.compileContinuation(
            closure, ctx,
            [&](Continuation* cnt) {
                cmp.optimizeModule();

                fun = backend.getOrCompile(cnt);
                dispatchTable->insert(ctx, fun);
            },
            [&]() { std::cerr << "Continuation compilation failed\n"; });
    }
    return fun;
}

} // namespace pir
} // namespace rir
