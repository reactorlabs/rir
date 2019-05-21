#include "../analysis/available_checkpoints.h"
#include "../pir/pir_impl.h"
#include "../transform/bb.h"
#include "../util/cfg.h"
#include "../util/visitor.h"
#include "R/r.h"
#include "pass_definitions.h"

#include <unordered_map>

namespace rir {
namespace pir {

void TypeSpeculation::apply(RirCompiler&, ClosureVersion* function,
                            LogStream& log) const {

    AvailableCheckpoints checkpoint(function, log);

    std::unordered_map<Checkpoint*, std::unordered_map<Instruction*, PirType>>
        speculate;

    Visitor::run(function->entry, [&](BB* bb) {
        auto ip = bb->begin();
        while (ip != bb->end()) {
            auto next = ip + 1;
            auto i = *ip;
            bool trigger = false;
            if (Force::Cast(i)) {
                if (!i->type.isA(i->typeFeedback)) {
                    auto arg = i->arg(0).val()->followCasts();
                    // Blacklist of where it is not worthwhile
                    if (!LdConst::Cast(arg) &&
                        // leave this to the promise inliner
                        !MkArg::Cast(arg) && !Force::Cast(arg) &&
                        // leave this to scope analysis
                        !LdVar::Cast(arg) && !LdVarSuper::Cast(arg)) {
                        trigger = true;
                    }
                }
            }
            if (trigger) {
                PirType type = i->typeFeedback;
                if (!type.isVoid() && !i->type.isA(type)) {
                    if (auto cp = checkpoint.next(i)) {
                        if (!type.maybeObj()) {
                            PirType specType = (type.isA(RType::integer) ||
                                                type.isA(RType::real))
                                                   ? type
                                                   : i->type.notObject();
                            bool redundant = !Visitor::check(
                                function->entry, [&](Instruction* i2) {
                                    if (auto c2 = CastType::Cast(i2)) {
                                        if (c2->arg<0>().val() == i &&
                                            c2->type == specType)
                                            return false;
                                    }
                                    return true;
                                });
                            if (!redundant)
                                speculate[cp][i] = specType;
                        }
                    }
                }
            }
            ip = next;
        }
    });

    Visitor::run(function->entry, [&](BB* bb) {
        if (bb->isEmpty())
            return;
        auto cp = Checkpoint::Cast(bb->last());
        if (!cp)
            return;
        if (!speculate.count(cp))
            return;

        bb = bb->trueBranch();
        auto ip = bb->begin();

        for (auto sp : speculate[cp]) {
            auto i = sp.first;
            PirType type = sp.second;

            Instruction* condition = nullptr;
            bool assumeTrue = true;

            if (type == i->type.notObject()) {
                condition = new IsObject(i);
                assumeTrue = false;
            } else {
                condition = new IsType(type, i);
            }

            auto cast = new CastType(i, PirType::any(), type);
            i->replaceUsesWithLimits(cast, bb, i);

            ip = bb->insert(ip, condition);
            ip++;
            auto assume = new Assume(condition, cp);
            assume->assumeTrue = assumeTrue;
            ip = bb->insert(ip, assume);
            ip++;
            ip = bb->insert(ip, cast);
            ip++;
        }
    });
}
} // namespace pir
} // namespace rir
