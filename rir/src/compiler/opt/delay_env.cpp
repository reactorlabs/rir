#include "../pir/pir_impl.h"
#include "../util/visitor.h"
#include "compiler/analysis/cfg.h"
#include "pass_definitions.h"

#include <unordered_set>

namespace rir {
namespace pir {

static bool envInterference(Value* e1, Value* e2) {
    while (e1 && Env::isAnyEnv(e1) && e1 != Env::nil()) {
        if (e1 == e2) {
            return true;
            break;
        }
        e1 = Env::parentEnv(e1);
    }
    return false;
}

bool DelayEnv::apply(Compiler&, ClosureVersion* cls, Code* code,
                     LogStream&) const {
    bool anyChange = false;
    Visitor::run(code->entry, [&](BB* bb) {
        std::unordered_set<MkEnv*> done;
        MkEnv* envInstr;

        while (true) {
            envInstr = nullptr;

            auto it = bb->end();
            while (it != bb->begin()) {
                it--;
                auto t = MkEnv::Cast(*it);
                if (t && done.find(t) == done.end()) {
                    envInstr = t;
                    break;
                }
            }

            if (!envInstr)
                break;
            done.insert(envInstr);

            while (it != bb->end() && (it + 1) != bb->end()) {
                assert(*it == envInstr);

                auto next = *(it + 1);

                if (Branch::Cast(next) || Return::Cast(next) ||
                    Deopt::Cast(next) || Checkpoint::Cast(next))
                    break;

                // Otherwise when we remove an update-promise we will wrongly
                // update this env
                if (UpdatePromise::Cast(next)) {
                    break;
                }

                auto consumeStVar = [&](StVar* st) {
                    bool exists = false;
                    envInstr->eachLocalVar(
                        [&](SEXP name, InstrArg& arg, bool& missing) {
                            if (name == st->varName) {
                                exists = true;
                                arg.val() = st->val();
                                if (!st->isStArg) {
                                    missing = false;
                                }
                            }
                        });

                    if (!exists) {
                        assert(!st->isStArg);
                        envInstr->pushArg(st->val(), PirType::any());
                        envInstr->varName.push_back(st->varName);
                    }
                    return true;
                };

                {
                    auto st = StVar::Cast(next);
                    if (st && st->env() == envInstr) {
                        if (consumeStVar(st)) {
                            it = bb->remove(it + 1);
                            it--;
                            continue;
                        } else {
                            break;
                        }
                    }
                }

                if (next->hasEnv()) {
                    if (envInterference(envInstr, next->env()))
                        break;
                }

                if (PushContext::Cast(next))
                    envInstr->context++;
                if (PopContext::Cast(next))
                    envInstr->context--;

                bb->swapWithNext(it);
                it++;
            }

            if (it == bb->end() || (it + 1) == bb->end())
                break;

            // Duplicate the mkEnv and stick the copied version in the deopt
            // branch. This removes the deopt as a dependency from the actual
            // mkenv.
            auto copyMkEnvToDeoptBranch = [&](BB* deoptBranch,
                                              BB* fastPathBranch) {
                anyChange = true;
                auto newEnvInstr = envInstr->clone();
                deoptBranch->insert(deoptBranch->begin(), newEnvInstr);
                envInstr->replaceUsesIn(newEnvInstr, deoptBranch);
                it = bb->moveToBegin(it, fastPathBranch);
            };

            assert(envInstr);
            auto branch = (*(it + 1))->branches();
            if (branch) {
                if (!bb->falseBranch()->isEmpty() &&
                    Deopt::Cast(bb->falseBranch()->last())) {
                    copyMkEnvToDeoptBranch(bb->falseBranch(), bb->trueBranch());
                } else if (!bb->trueBranch()->isEmpty() &&
                           Deopt::Cast(bb->trueBranch()->last())) {
                    copyMkEnvToDeoptBranch(bb->trueBranch(), bb->falseBranch());
                }
            }
        }
    });
    return anyChange;
}
} // namespace pir
} // namespace rir
