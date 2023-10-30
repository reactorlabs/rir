#include "osr.h"

#include "compiler/backend.h"
#include "compiler/compiler.h"
#include "pir/deopt_context.h"
#include "pir/pir_impl.h"
#include "runtime/DispatchTable.h"

namespace rir {
namespace pir {

Function* OSR::compile(SEXP closure, rir::Code* c,
                       const ContinuationContext& ctx) {
    Function* fun = nullptr;

    // compile to pir
    pir::Module* module = new pir::Module;

    pir::Log logger(DebugOptions::DefaultDebugOptions);
    logger.title("Compiling continuation");
    pir::Compiler cmp(module, logger);

    pir::Backend backend(module, logger, "continuation",
                         // Right now, serial options aren't important here,
                         // because OSR and serialization are completely
                         // separate. So we could probably pass anything. What
                         // would we pass if they were used? idk
                         SerialOptions::DeepCopy);

    cmp.compileContinuation(
        closure, c->function(), &ctx,
        [&](Continuation* cnt) {
            cmp.optimizeModule();
            fun = backend.getOrCompile(cnt);
        },
        [&]() { std::cerr << "Continuation compilation failed\n"; });

    delete module;

    auto dt = DispatchTable::unpack(BODY(closure));
    fun->dispatchTable(dt);

    return fun;
}

Function* OSR::deoptlessDispatch(SEXP closure, rir::Code* c,
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

    auto res = dispatchTable->dispatch(ctx);

    auto fun = res.second;
    // If the context differs recompile in the hope we get a better version
    if (fun && !(res.first == ctx) &&
        dispatchTable->size() < (dispatchTable->capacity() / 2))
        fun = nullptr;

    if (!fun && !dispatchTable->full()) {
        assert(ctx.asDeoptContext());
        fun = compile(closure, c, ctx);
        if (fun)
            dispatchTable->insert(ctx, fun);
    }
    return fun;
}

} // namespace pir
} // namespace rir
