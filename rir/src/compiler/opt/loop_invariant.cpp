#include "../analysis/loop_detection.h"
#include "../pir/pir_impl.h"
#include "../util/cfg.h"
#include "../util/safe_builtins_list.h"
#include "pass_definitions.h"

namespace rir {
namespace pir {

bool isSafeToHoistLoads(const LoopDetection::Loop& loop) {
    auto noEnvironmentTainting = [&](Instruction* i) {
        // For these instructions we test later they don't change the particular
        // binding
        if (StVar::Cast(i) || StVarSuper::Cast(i) || MkEnv::Cast(i))
            return true;

        if (auto call = CallBuiltin::Cast(i)) {
            if (SafeBuiltinsList::nonObject(call->builtinId)) {
                auto safe = true;
                call->eachCallArg([&](Value* arg) {
                    if (arg->type.maybeObj())
                        safe = false;
                });
                return safe;
            }
        }

        return !i->changesEnv();
    };
    return loop.check(noEnvironmentTainting);
}

bool overwritesBinding(LoopDetection::Loop& loop, SEXP binding) {
    auto noOverwriting = [&](Instruction* i) {
        SEXP varName = nullptr;
        if (auto store = StVar::Cast(i))
            varName = store->varName;
        else if (auto store = StVarSuper::Cast(i))
            varName = store->varName;

        if (varName && varName == binding)
            return false;

        // An environment overwrites the binding
        if (auto env = MkEnv::Cast(i)) {
            bool envOverwrites = false;
            env->eachLocalVar([&](SEXP name, Value*) {
                if (name == binding) {
                    envOverwrites = true;
                    return;
                }
            });
            return !envOverwrites;
        }

        return true;
    };
    return !loop.check(noOverwriting);
}

void LoopInvariant::apply(RirCompiler&, ClosureVersion* function,
                          LogStream& log) const {
    LoopDetection loops(function);
    CFG cfg(function);

    for (auto& loop : loops) {
        BB* targetBB = loop.preheader(cfg);
        if (targetBB && isSafeToHoistLoads(loop)) {
            for (auto bb : loop) {
                auto ip = bb->begin();
                while (ip != bb->end()) {
                    Instruction* i = *ip;
                    auto next = ip + 1;

                    SEXP binding = nullptr;
                    if (auto ldFun = LdFun::Cast(i)) {
                        binding = ldFun->varName;
                    } else if (auto ldVar = LdVar::Cast(i)) {
                        binding = ldVar->varName;
                    }

                    if (binding && !overwritesBinding(loop, binding))
                        next = bb->moveToEnd(ip, targetBB);

                    ip = next;
                }
            }
        }
    }
}
} // namespace pir
} // namespace rir
