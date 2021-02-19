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

namespace rir {
namespace pir {

/*

void InlineForcePromises::aa(Code* code, ClosureVersion* cls, BB* bb,
BB::Instrs::iterator& ip,  MkArg* mkarg) { auto anyChange = false;

    if (mkarg->isEager()) {
        anyChange = true;
        Value* eager = mkarg->eagerArg();
        f->replaceUsesWith(eager);
        next = bb->remove(ip);

    } else if (toInline.count(f)) {
        anyChange = true;
        Promise* prom = mkarg->prom();
        BB* split =
            BBTransform::split(code->nextBBId++, bb, ip, code);

        // Note that the entry block is empty and jumps to the
        // next block; this is to ensure that it has no
        // predecessors.
        auto entry = prom->entry;
        assert(entry->isEmpty() && entry->isJmp() &&
                !entry->next()->isEmpty());
        BB* prom_copy =
            BBTransform::clone(prom->entry->next(), code, cls);
        bb->overrideNext(prom_copy);

        // Patch framestates  *******************  REVIEW
        // Visitor::runPostChange(prom_copy, [&](BB* bb) {
        //     auto it = bb->begin();
        //     while (it != bb->end()) {
        //         auto next = it + 1;
        //         if (f->frameState()) {
        //             if (auto sp = FrameState::Cast(*it)) {
        //                 if (!sp->next()) {
        //                     auto copyFromFs = f->frameState();
        //                     auto cloneSp = FrameState::Cast(
        //                         copyFromFs->clone());

        //                     it = bb->insert(it, cloneSp);
        //                     sp->next(cloneSp);

        //                     size_t created = 1;
        //                     while (copyFromFs->next()) {
        //                         assert(copyFromFs->next() ==
        //                                 cloneSp->next());
        //                         copyFromFs = copyFromFs->next();
        //                         auto prevClone = cloneSp;
        //                         cloneSp = FrameState::Cast(
        //                             copyFromFs->clone());

        //                         it = bb->insert(it, cloneSp);
        //                         created++;

        //                         prevClone->updateNext(cloneSp);
        //                     }

        //                     next = it + created + 1;
        //                 }
        //             }
        //         } else {
        //             if (FrameState::Cast(*it))
        //                 next = bb->remove(it);
        //             // TODO: don't copy this to start with
        //             if ((*it)->frameState())
        //                 (*it)->clearFrameState();
        //             if (auto cp = Checkpoint::Cast(*it)) {
        //                 auto n = cp->nextBB();
        //                 auto d = cp->deoptBranch();
        //                 bb->eraseLast();
        //                 bb->overrideSuccessors({n});
        //                 delete d;
        //                 next = bb->end();
        //             }
        //         }
        //         it = next;
    //     }
        // });


        // **************  How many times is this in the code???? SK
        // Search for the env instruction of the promise. We
        // replace its usages with the caller environment.
        BB::Instrs::iterator promenv;
        BB* promenvBB = nullptr;
        Visitor::run(prom_copy, [&](BB* bb) {
    #ifndef ENABLE_SLOWASSERT
            if (promenvBB)
                return;
    #endif
            for (auto it = bb->begin(); it != bb->end(); ++it) {
                if (LdFunctionEnv::Cast(*it)) {
                    assert(!promenvBB);
                    promenv = it;
                    promenvBB = (*it)->bb();
                }
            }
        });

        if (promenvBB) {
            prom_copy->replaceUsesOfValue(*promenv,
                                            mkarg->promEnv());
            promenvBB->remove(promenv);
        }
// ********************************


        // Update environment dependency of inlined forces:
        // the inlined forces can see local env of this
        // function if it is stored on the context.
        if (auto mkenv = MkEnv::Cast(f->env())) {
            if (mkenv->context) {
                Visitor::run(prom_copy, [&](Instruction* i) {
                    if (auto fi = Force::Cast(i)) {
                        if (fi->hasEnv()) {
                            fi->env(f->env());
                        }
                    }
                });
            }
        }
 //       ******************

        // Create a return value phi of the promise   /// **** It doesnt create
phi auto promRet = BBTransform::forInline(prom_copy, split, f->env()); auto
promRes = promRet.first;

        assert(!promRes->type.maybePromiseWrapped());
        f = Force::Cast(*split->begin());
        // Ensure we don't loose inferred type information
        promRes->type = promRes->type & f->type;
        assert(f);
        f->replaceUsesWith(promRes);
        split->remove(split->begin());

        auto pos = split->begin();
        MkArg* fixedMkArg =
            new MkArg(mkarg->prom(), promRes, mkarg->promEnv());
        pos = split->insert(pos, fixedMkArg);
        pos++;
        CastType* upcast = new CastType(
            fixedMkArg, CastType::Upcast, RType::prom,
            promRes->type.orPromiseWrapped());
        pos = split->insert(pos, upcast);
        pos++;

        auto u = toInline.at(f);
        if (u.kind == ForcedBy::PromiseInlineable::
                            SafeToInlineWithUpdate) {
            if (auto m = MkEnv::Cast(u.escapedAt())) {
                m->eachLocalVar([&](SEXP name, Value* a, bool) {
                    if (a->followCasts() == mkarg) {
                        pos = split->insert(
                            pos, new StArg(name, upcast, m));
                        pos++;
                    }
                });
            } else if (PushContext::Cast(u.escapedAt())) {
                // Escapes to non-environment, let's update
                // existing promise
                pos = split->insert(
                    pos, new UpdatePromise(mkarg, promRes));
                pos++;
            } else {
                assert(false && "Cannot deal with this leak");
            }
        }
        next = pos;

        inlinedPromise[f] = promRes;

        if (promRet.second->isNonLocalReturn())
            dead.insert(split);
        break;
    }



}


*/

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
                    // cls->print(std::cout,true);
                    // assert(false &&  "aaa");
                    // ??? this is just one function
                    // version , I don't like relying on it. Maybe it's just to
                    // start
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

                                printf("forced! \n");
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
