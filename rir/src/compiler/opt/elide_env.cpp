#include "../pir/pir_impl.h"
#include "../util/cfg.h"
#include "../util/visitor.h"
#include "R/r.h"
#include "pass_definitions.h"

#include <unordered_map>

namespace rir {
namespace pir {

void ElideEnv::apply(RirCompiler&, ClosureVersion* function,
                     ClosureStreamLogger&) const {
    std::unordered_set<Value*> envNeeded;
    std::unordered_map<Value*, Value*> envDependency;

    Visitor::run(function->entry, [&](BB* bb) {
        for (auto ip = bb->begin(); ip != bb->end(); ++ip) {
            auto i = *ip;
            if (i->hasEnv()) {
                bool envIsNeeded = i->hasEnv();

                if (envIsNeeded && i->envOnlyForObj()) {
                    envIsNeeded = i->anyArg([&](Value* v) {
                        return v != i->env() && v->type.maybeObj();
                    });
                    if (!envIsNeeded) {
                        i->elideEnv();
                        i->updateTypeAndEffects();
                    }
                }

                if (envIsNeeded) {
                    if (!StVar::Cast(i))
                        envNeeded.insert(i->env());
                    if (!Env::isPirEnv(i))
                        envDependency[i] = i->env();
                }

                if (auto force = Force::Cast(i)) {
                    if (!force->input()->type.maybeLazy())
                        force->elideEnv();
                }
            }
        }
    });

    Visitor::run(function->entry, [&](Instruction* i) {
        if (i->hasEffect() || i->type != PirType::voyd() || Return::Cast(i) ||
            Deopt::Cast(i)) {
            i->eachArg([&](Value* v) {
                if (envDependency.count(v))
                    envNeeded.insert(envDependency.at(v));
            });
        }
    });

    Visitor::run(function->entry, [&](BB* bb) {
        auto ip = bb->begin();
        while (ip != bb->end()) {
            Instruction* i = *ip;
            if (Env::isPirEnv(i)) {
                if (envNeeded.find(i) == envNeeded.end())
                    ip = bb->remove(ip);
                else
                    ip++;
            } else if (i->hasEnv() && Env::isPirEnv(i->env()) &&
                       envNeeded.find(i->env()) == envNeeded.end()) {
                ip = bb->remove(ip);
            } else {
                ip++;
            }
        }
    });
}
} // namespace pir
} // namespace rir
