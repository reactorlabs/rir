#include "../analysis/available_checkpoints.h"
#include "../analysis/force_dominance.h"
#include "../parameter.h"
#include "../pir/pir_impl.h"
#include "compiler/util/bb_transform.h"
#include "compiler/util/safe_builtins_list.h"
#include "pass_definitions.h"
#include "utils/Map.h"
#include "utils/Set.h"
#include <functional>
#include <unordered_map>
#include <unordered_set>

/*
For each call, if we can statically know the function at a call and the function
is annotated as depromise, we add a Force instruction for each argument right
before the call, and we pass the forced values instead. By adding a force, we
are sure that the corresponding MkArgs are in scope. It means that promises will
be inlined by a future pass (promise inliner). Corresponding MkArg instructions
will be later on removed by dead code elimination.

*/

namespace rir {
namespace pir {

BB::Instrs::iterator iteratorAt(BB* bb, Instruction* ins) {
    auto ip = bb->begin();
    while (ip != bb->end()) {
        if (*ip == ins)
            return ip;
        ip++;
    }
    assert(false && "instruction not found in bb");
    // return nullptr;
};

bool InlineForcePromises::apply(Compiler&, ClosureVersion* cls, Code* code,
                                LogStream& log) const {

    bool anyChange = false;

    Visitor::run(code->entry, [&](BB* bb) {
        auto ip = bb->begin();
        while (ip != bb->end()) {
            auto next = ip + 1;

            if (auto call = CallInstruction::CastCall(*ip)) {
                auto callAsInstr = *ip;
                auto clsCallee = call->tryGetCls();

                if (clsCallee) {

                    auto functionVersion = clsCallee->rirFunction();
                    if (functionVersion->flags.contains(
                            rir::Function::Flag::DepromiseArgs)) {

                        bool anyChangeLocal;
                        std::function<void(InstrArg&)> updateMkArg =
                            [&](InstrArg& v) {
                                if (auto mkarg = MkArg::Cast(v.val())) {

                                    anyChange = true;
                                    anyChangeLocal = true;

                                    auto cast = new CastType(
                                        mkarg, CastType::Kind::Upcast,
                                        RType::prom, PirType::valOrLazy());

                                    auto forced =
                                        new Force(cast, mkarg->env(),
                                                  Tombstone::framestate());
                                    v.val() = forced;
                                    // v.type() = PirType::val();
                                    ip = bb->insert(ip, cast) + 1;
                                    ip = bb->insert(ip, forced) + 1;
                                }
                            };

                        call->eachCallArg([&](InstrArg& v) {
                            updateMkArg(v);

                            if (auto dots = DotsList::Cast(v.val())) {

                                anyChangeLocal = false;
                                dots->eachArg([&](InstrArg& vDot) {
                                    updateMkArg(vDot);
                                    vDot.type() = PirType::val();
                                });

                                if (anyChangeLocal) {

                                    bb->remove(dots);
                                    ip = iteratorAt(bb, callAsInstr);
                                    ip = bb->insert(ip, dots) + 1;

                                    // auto clone = dots->clone();
                                    // ip = bb->insert(ip, clone) + 1;
                                    // dots->replaceUsesWith(clone);
                                    // bb->remove(dots);
                                    // ip = iteratorAt(bb, callAsInstr);
                                }
                            }
                        });
                        next = ip + 1;
                    }
                }
            }
            ip = next;
        }
    });

    return anyChange;
}

} // namespace pir
} // namespace rir
