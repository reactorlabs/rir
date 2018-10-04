#include "../pir/pir_impl.h"
#include "../transform/bb.h"
#include "../util/cfg.h"
#include "../util/visitor.h"
#include "R/r.h"
#include "pass_definitions.h"

#include <unordered_map>

namespace rir {
namespace pir {

void ElideEnvSpec::apply(Closure* function) const {

    // Elide environments of binary operators in which both operators are
    // primitive values
    Visitor::run(function->entry, [&](BB* bb) {
        auto ip = bb->begin();
        Safepoint* lastSafepoint = nullptr;
        while (ip != bb->end()) {
            Instruction* i = *ip;

            // Initially before a binop that enables speculation there should
            // always be a safepoint inserted only for that purpose. However,
            // interleavings with other optimization passes may change this.
            if (Safepoint::Cast(i)) {
                lastSafepoint = Safepoint::Cast(i);
                ip++;
                continue;
            } else {
                if (i->hasEffect()) {
                    lastSafepoint = nullptr;
                    ip++;
                    continue;
                }
            }

            if ((Lte::Cast(i) || Gte::Cast(i) || Lt::Cast(i) || Gt::Cast(i) ||
                 Mod::Cast(i) || Add::Cast(i) || Div::Cast(i) ||
                 IDiv::Cast(i) || Colon::Cast(i) || Pow::Cast(i) ||
                 Sub::Cast(i) || Mul::Cast(i) || Neq::Cast(i) || Eq::Cast(i)) &&
                (i->env() != Env::elided()) && lastSafepoint != nullptr) {

                ProfiledValues* profile = &function->runtimeFeedback;
                Value* opLeft = i->arg(0).val();
                Value* opRight = i->arg(1).val();

                if (profile->hasTypesFor(opLeft) &&
                    profile->hasTypesFor(opRight) &&
                    !profile->types.at(opLeft).observedObject() &&
                    !profile->types.at(opRight).observedObject()) {
                    i->elideEnv();
                    BBTransform::addConditionalDeopt(
                        function, bb, ip, new IsObject(opLeft), lastSafepoint);
                    BBTransform::addConditionalDeopt(
                        function, bb, ip, new IsObject(opRight), lastSafepoint);
                }
            }

            ip++;
        }
    });
}
} // namespace pir
} // namespace rir
