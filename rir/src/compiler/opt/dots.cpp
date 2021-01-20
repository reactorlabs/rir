#include "../analysis/available_checkpoints.h"
#include "../pir/pir_impl.h"
#include "../util/safe_builtins_list.h"
#include "../util/visitor.h"
#include "R/Funtab.h"
#include "R/Symbols.h"
#include "R/r.h"
#include "compiler/analysis/cfg.h"
#include "pass_definitions.h"

#include <unordered_map>

namespace rir {
namespace pir {

// Search for ExpandDots(Dotlist(...)) pairs and statically expand them
bool DotDotDots::apply(Compiler& cmp, ClosureVersion* cls, Code* code,
                       LogStream& log) const {
    bool anyChange = false;
    Visitor::run(code->entry, [&](BB* bb) {
        auto ip = bb->begin();
        while (ip != bb->end()) {
            auto next = ip + 1;

            if (auto calli = CallInstruction::CastCall(*ip)) {
                // Not sure how this would work, since this optimization changes
                // the number of arguments...
                if (StaticCall::Cast(*ip))
                    break;
                auto i = Instruction::Cast(*ip);
                auto namedCall = NamedCall::Cast(i);

                std::vector<Value*> args;
                std::vector<SEXP> names;
                bool hasNames = false;
                bool hasDots = false;
                size_t pos = 0;

                calli->eachCallArg([&](Value* v) {
                    auto splat = ExpandDots::Cast(v);
                    if (splat && DotsList::Cast(splat->arg(0).val())) {
                        auto d = DotsList::Cast(splat->arg(0).val());
                        hasDots = true;
                        size_t listPos = 0;
                        d->eachArg([&](Value* v) {
                            auto n = d->names[listPos++];
                            if (n != R_NilValue)
                                hasNames = true;
                            names.push_back(n);
                            args.push_back(v);
                        });
                    } else if (splat &&
                               splat->arg(0).val() == MissingArg::instance()) {
                        hasDots = true;
                    } else {
                        args.push_back(v);
                        if (namedCall) {
                            auto n = namedCall->names[pos];
                            if (n != R_NilValue)
                                hasNames = true;
                            names.push_back(n);
                        } else {
                            names.push_back(R_NilValue);
                        }
                    }
                    pos++;
                });

                if (hasDots) {
                    // TODO support named call builtin
                    if (hasNames && !(Call::Cast(i) || NamedCall::Cast(i)))
                        return;

                    bool eager =
                        CallBuiltin::Cast(*ip) || CallSafeBuiltin::Cast(*ip);

                    // Evaluate all args if call is eager (in particular this
                    // will force the expanded args from the dotlist).
                    for (auto& a : args) {
                        if (eager) {
                            if (auto mk = MkArg::Cast(a)) {
                                if (mk->isEager())
                                    a = mk->eagerArg();
                            }
                            auto mk = MkArg::Cast(a);
                            if (a->type.maybeLazy() || mk) {
                                Value* env = nullptr;
                                if (mk && mk->noReflection)
                                    env = Env::elided();
                                if (!env && i->hasEnv())
                                    env = i->env();
                                if (!env)
                                    env = Env::elided();

                                auto cast = new CastType(
                                    a, CastType::Upcast, RType::prom,
                                    PirType::any().notMissing());
                                ip = bb->insert(ip, cast);
                                ip++;
                                ip = bb->insert(
                                    ip, new Force(cast, env,
                                                  Tombstone::framestate()));
                                a = *ip;
                                ip++;
                                next = ip + 1;
                            }
                        }
                    }

                    anyChange = true;
                    if (hasNames) {
                        if (auto c = Call::Cast(i)) {
                            auto nc =
                                new NamedCall(i->env(), c->cls(), args, names,
                                              c->frameStateOrTs(), i->srcIdx);
                            i->replaceUsesAndSwapWith(nc, ip);
                        } else if (auto c = NamedCall::Cast(i)) {
                            auto nc =
                                new NamedCall(i->env(), c->cls(), args, names,
                                              c->frameStateOrTs(), i->srcIdx);
                            i->replaceUsesAndSwapWith(nc, ip);
                        } else {
                            assert(false);
                        }
                    } else {
                        if (auto c = Call::Cast(i)) {
                            auto nc = new Call(i->env(), c->cls(), args,
                                               c->frameStateOrTs(), i->srcIdx);
                            i->replaceUsesAndSwapWith(nc, ip);
                        } else if (auto c = NamedCall::Cast(i)) {
                            auto nc = new Call(i->env(), c->cls(), args,
                                               c->frameStateOrTs(), i->srcIdx);
                            i->replaceUsesAndSwapWith(nc, ip);
                        } else if (auto b = CallBuiltin::Cast(i)) {
                            auto nc = BuiltinCallFactory::New(
                                i->env(), b->builtinSexp, args, i->srcIdx);
                            i->replaceUsesAndSwapWith(nc, ip);
                        } else if (auto b = CallSafeBuiltin::Cast(i)) {
                            auto nc = BuiltinCallFactory::New(
                                Env::elided(), b->builtinSexp, args, i->srcIdx);
                            i->replaceUsesAndSwapWith(nc, ip);
                        } else {
                            assert(false);
                        }
                    }

                    for (; pos < calli->nCallArgs(); ++pos)
                        i->popArg();
                }
            }

            ip = next;
        }
    });
    return anyChange;
}
} // namespace pir
} // namespace rir
