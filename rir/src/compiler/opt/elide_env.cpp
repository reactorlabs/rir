#include "../pir/pir_impl.h"
#include "../util/visitor.h"
#include "R/r.h"
#include "compiler/analysis/cfg.h"
#include "compiler/analysis/query.h"
#include "compiler/util/env_stub_info.h"
#include "pass_definitions.h"

#include <unordered_map>
#include <unordered_set>

namespace rir {
namespace pir {

bool ElideEnv::apply(Compiler&, ClosureVersion* cls, Code* code, LogStream&,
                     size_t) const {
    bool anyChange = false;
    std::unordered_set<Value*> envNeeded;
    std::unordered_set<Value*> envStubNeeded;
    std::unordered_map<Value*, Value*> envDependency;

    Visitor::run(code->entry, [&](BB* bb) {
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
                    auto mk = MkEnv::Cast(i->env());
                    if (!StVar::Cast(i) && !IsEnvStub::Cast(i)) {
                        if (mk && !mk->neverStub &&
                            EnvStubInfo::of(i->tag).allowedNotMaterializing) {
                            envStubNeeded.insert(i->env());
                        } else {
                            envNeeded.insert(i->env());
                        }
                    }
                    if (!Env::isPirEnv(i))
                        envDependency[i] = i->env();
                }

                if (auto force = Force::Cast(i)) {
                    if (!force->input()->type.maybeLazy()) {
                        force->elideEnv();
                        force->clearFrameState();
                    }
                }

                if (auto mk = MkArg::Cast(i)) {
                    if (!mk->prom()->env()) {
                        mk->elideEnv();
                    }
                }
            }
        }
    });

    Visitor::run(code->entry, [&](Instruction* i) {
        if (i->hasEffect() || i->type != PirType::voyd() || Return::Cast(i) ||
            Deopt::Cast(i)) {
            i->eachArg([&](Value* v) {
                if (envDependency.count(v)) {
                    auto d = envDependency.at(v);
                    auto vi = Instruction::Cast(v);
                    if (vi && vi->hasEnv() && MkEnv::Cast(vi->env()) &&
                        EnvStubInfo::of(vi->tag).allowedNotMaterializing) {
                        envStubNeeded.insert(d);
                    } else {
                        envNeeded.insert(d);
                    }
                }
            });
        }
    });

    std::unordered_set<MkEnv*> newStub;
    Visitor::run(code->entry, [&](BB* bb) {
        auto ip = bb->begin();
        while (ip != bb->end()) {
            Instruction* i = *ip;
            if (Env::isPirEnv(i)) {
                if (envNeeded.find(i) == envNeeded.end()) {
                    if (envStubNeeded.find(i) != envStubNeeded.end()) {
                        auto mk = MkEnv::Cast(i);
                        if (!mk->neverStub && !mk->stub) {
                            mk->stub = true;
                            newStub.insert(mk);
                            anyChange = true;
                        }
                        ip++;
                    } else {
                        ip = bb->remove(ip);
                        anyChange = true;
                    }
                } else {
                    ip++;
                }
            } else if (i->hasEnv() && Env::isPirEnv(i->env()) &&
                       envNeeded.find(i->env()) == envNeeded.end() &&
                       envStubNeeded.find(i->env()) == envStubNeeded.end()) {
                ip = bb->remove(ip);
                anyChange = true;
            } else {
                ip++;
            }
        }
    });
    if (!newStub.empty()) {
        Visitor::run(code->entry, [&](Instruction* i) {
            if (auto st = StVar::Cast(i)) {
                if (auto mk = MkEnv::Cast(st->env())) {
                    if (mk->stub && newStub.count(mk)) {
                        if (!mk->contains(st->varName)) {
                            mk->pushArg(UnboundValue::instance(),
                                        PirType::any());
                            mk->varName.push_back(st->varName);
                        }
                    }
                }
            }
        });
    }

    return anyChange;
}
} // namespace pir
} // namespace rir
