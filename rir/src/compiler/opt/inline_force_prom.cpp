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

bool InlineForcePromises::apply(Compiler&, ClosureVersion* cls, Code* code,
                                LogStream& log) const {

    bool anyChange = false;

    Visitor::run(code->entry, [&](BB* bb) {
        auto ip = bb->begin();
        while (ip != bb->end()) {
            auto next = ip + 1;

            if (auto call = CallInstruction::CastCall(*ip)) {

                auto cls = call->tryGetCls();

                if (cls) {

                    auto functionVersion = cls->rirFunction();
                    if (functionVersion->flags.contains(
                            rir::Function::Flag::Annotated)) {

                        call->eachCallArg([&](InstrArg& v) {
                            if (auto mkarg = MkArg::Cast(v.val())) {

                                anyChange = true;

                                auto cast = new CastType(
                                    mkarg, CastType::Kind::Upcast, RType::prom,
                                    PirType::valOrLazy());

                                auto forced =
                                    new Force(cast, mkarg->env(),
                                              Tombstone::framestate());
                                v.val() = forced;
                                ip = bb->insert(ip, cast) + 1;
                                ip = bb->insert(ip, forced) + 1;
                                next = ip + 1;


                            }
                        });
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
