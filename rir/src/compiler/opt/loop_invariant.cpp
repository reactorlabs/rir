#include "../analysis/loop_detection.h"
#include "../pir/pir_impl.h"
#include "../transform/bb.h"
#include "../util/cfg.h"
#include "../util/safe_builtins_list.h"
#include "../util/visitor.h"
#include "R/r.h"
#include "pass_definitions.h"

namespace rir {
namespace pir {

typedef std::pair<BB*, BB*> BackEdge;
typedef std::function<bool(Instruction*)> InstrActionPredicate;

auto noEnvironmentTainting = [](Instruction* i) {
    if (LdFun::Cast(i))
        return true;

    if (auto call = CallBuiltin::Cast(i))
        return SafeBuiltinsList::nonObject(call->builtinId);

    if (CallSafeBuiltin::Cast(i))
        return true;

    if (CallInstruction::CastCall(i))
        return false;

    return !(i->mayUseReflection());
};

bool isSafeToHoistLoads(LoopDetection::Loop& loop) {
    return loop.holdsPropery(noEnvironmentTainting);
}

bool overwritesBinding(LoopDetection::Loop& loop, SEXP binding) {
    auto overwrites =
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
        };

    return loop.holdsPropery(overwrites);
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
