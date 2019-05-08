#include "../analysis/loop_detection.h"
#include "../pir/pir_impl.h"
#include "../util/cfg.h"
#include "../util/safe_builtins_list.h"
#include "pass_definitions.h"

namespace rir {
namespace pir {

bool isSafeToHoistLoads(const LoopDetection::Loop& loop) {
    auto noEnvironmentTainting = [](Instruction* i) {
        // For this instructions we test later they not change the particular
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

        if (CallSafeBuiltin::Cast(i))
            return true;

        if (CallInstruction::CastCall(i))
            return false;

        return !(i->changesEnv());
    };
    return loop.check(noEnvironmentTainting);
}

bool overwritesBinding(LoopDetection::Loop& loop, SEXP binding) {
    return loop.check(
        [binding](Instruction* i) {
            SEXP varName = nullptr;
            if (auto store = StVar::Cast(i))
                varName = store->varName;
            else if (auto store = StVarSuper::Cast(i))
                varName = store->varName;

            if (varName && varName == binding)
                return true;

            // An environment overwrites the binding
            if (auto env = MkEnv::Cast(i)) {
                bool ows = false;
                env->eachLocalVar([&](SEXP name, Value* v) {
                    if (name == binding) {
                        ows = true;
                        return;
                    }
                });
                return ows;
            }

            return false;
        });
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
