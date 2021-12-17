#include "../pir/pir_impl.h"
#include "../util/visitor.h"
#include "compiler/analysis/cfg.h"
#include "pass_definitions.h"

#include <unordered_set>

namespace rir {
namespace pir {

bool DelayEnv::apply(Compiler&, ClosureVersion* cls, Code* code, LogStream&,
                     size_t) const {
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

                if (next->exits() || next->branches())
                    break;

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

                if (next->hasEnv() && next->env() == envInstr)
                    break;

                if (PushContext::Cast(next))
                    envInstr->context++;
                else if (PopContext::Cast(next))
                    envInstr->context--;
                else if (envInstr->context > 0 &&
                         next->effects.contains(Effect::Reflection))
                    break;

                anyChange = true;
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
