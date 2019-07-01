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
            if (!i->type.maybePromiseWrapped() && !i->typeFeedback.isVoid() &&
                !i->type.isA(i->typeFeedback)) {
                switch (i->tag) {
                case Tag::Force: {
                    auto arg = i->arg(0).val()->followCasts();
                    // Blacklist of where it is not worthwhile
                    if (!LdConst::Cast(arg) &&
                        // leave this to the promise inliner
                        !MkArg::Cast(arg) && !Force::Cast(arg) &&
                        // leave this to scope resolution
                        !LdVar::Cast(arg) && !LdVarSuper::Cast(arg)) {
                        trigger = true;
                    }
                    if (auto ld = LdVar::Cast(arg))
                        if (!Env::isPirEnv(ld->env()))
                            trigger = true;
                    if (auto ld = LdVarSuper::Cast(arg))
                        if (!Env::isPirEnv(ld->env()))
                            trigger = true;
                    break;
                }
                default: {}
                }
            }
            if (trigger) {
                PirType seen = i->typeFeedback;
                if (!seen.isVoid() && !seen.maybeObj() && !i->type.isA(seen)) {
                    if (auto cp = checkpoint.next(i)) {
                        auto assume =
                            (seen.isA(RType::integer) || seen.isA(RType::real))
                                ? seen
                                : i->type.notObject();
                        if (!i->type.isA(assume))
                            speculate[cp][i] = assume;
                        // Prevent redundant speculation
                        i->typeFeedback = i->type;
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
            BBTransform::insertAssume(condition, cp, bb, ip, assumeTrue);

            auto cast =
                new CastType(i, CastType::Downcast, PirType::val(), type);
            ip = bb->insert(ip, cast);
            ip++;
            i->replaceReachableUses(cast);
        }
    });
}
} // namespace pir
} // namespace rir
