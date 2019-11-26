#include "../analysis/dead.h"
#include "../pir/pir_impl.h"
#include "../transform/bb.h"
#include "../translations/pir_translator.h"
#include "../util/cfg.h"
#include "../util/visitor.h"

#include "R/r.h"
#include "pass_definitions.h"

#include <unordered_map>
#include <unordered_set>

namespace {

using namespace rir::pir;

class TheCleanup {
  public:
    explicit TheCleanup(ClosureVersion* function) : function(function) {}
    ClosureVersion* function;
    void operator()() {
        std::unordered_set<size_t> used_p;
        std::unordered_map<BB*, std::unordered_set<Phi*>> usedBB;
        std::deque<Promise*> todo;

        DeadInstructions dead(function, 3, Effect::Visibility,
                              DeadInstructions::IgnoreUpdatePromise);

        Visitor::run(function->entry, [&](BB* bb) {
            auto ip = bb->begin();
            while (ip != bb->end()) {
                Instruction* i = *ip;
                auto next = ip + 1;
                bool removed = false;
                bool isDead = dead.isDead(i);
                // unused ldfun is a left over from a guard where ldfun was
                // converted into ldvar.
                if (isDead && !Visible::Cast(i) && !Invisible::Cast(i)) {
                    if (i->getObservableEffects().includes(
                            Effect::Visibility) &&
                        i->visibilityFlag() != VisibilityFlag::Unknown) {
                        switch (i->visibilityFlag()) {
                        case VisibilityFlag::On:
                            bb->replace(ip, new Visible());
                            break;
                        case VisibilityFlag::Off:
                            bb->replace(ip, new Invisible());
                            break;
                        default:
                            assert(false);
                        }
                    } else {
                        next = bb->remove(ip);
                    }
                    removed = true;
                } else if (auto force = Force::Cast(i)) {
                    Value* arg = force->input();
                    // Missing args produce error.
                    if (!arg->type.maybePromiseWrapped() &&
                        !arg->type.maybeMissing()) {
                        removed = true;
                        force->replaceUsesWith(arg);
                        next = bb->remove(ip);
                    }
                } else if (auto chkcls = ChkClosure::Cast(i)) {
                    Value* arg = chkcls->arg<0>().val();
                    if (arg->type.isA(RType::closure)) {
                        removed = true;
                        chkcls->replaceUsesWith(arg);
                        next = bb->remove(ip);
                    }
                } else if (auto b = CallBuiltin::Cast(i)) {
                    if (!i->hasEnv()) {
                        std::vector<Value*> args;
                        b->eachCallArg([&](Value* v) { args.push_back(v); });
                        i->replaceUsesAndSwapWith(
                            new CallSafeBuiltin(b->blt, args, b->srcIdx), ip);
                    }
                } else if (auto missing = ChkMissing::Cast(i)) {
                    Value* arg = missing->arg<0>().val();
                    if (!arg->type.maybeMissing()) {
                        removed = true;
                        missing->replaceUsesWith(arg);
                        next = bb->remove(ip);
                    }
                } else if (auto phi = Phi::Cast(i)) {
                    std::unordered_set<Value*> phin;
                    phi->eachArg([&](BB*, Value* v) {
                        if (v != phi)
                            phin.insert(v);
                    });
                    if (phin.size() == 1) {
                        removed = true;
                        phi->replaceUsesWith(*phin.begin());
                        next = bb->remove(ip);
                    } else {
                        for (auto curBB : phi->inputs()) {
                            assert(!curBB->isDeopt());
                            usedBB[curBB].insert(phi);
                        }
                    }
                } else if (auto arg = MkArg::Cast(i)) {
                    if (dead.isDead(arg)) {
                        removed = true;
                        next = bb->remove(ip);
                    } else {
                        used_p.insert(arg->prom()->id);
                        todo.push_back(arg->prom());
                    }
                } else if (auto upd = UpdatePromise::Cast(i)) {
                    if (dead.isDead(upd->arg(0).val())) {
                        removed = true;
                        next = bb->remove(ip);
                    }
                } else if (auto tt = IsType::Cast(i)) {
                    auto arg = tt->arg<0>().val();
                    if (arg->type.isA(tt->typeTest)) {
                        tt->replaceUsesWith(True::instance());
                        removed = true;
                        next = bb->remove(ip);
                    }
                } else if (auto tt = CastType::Cast(i)) {
                    auto arg = tt->arg<0>().val();
                    if ((tt->kind == CastType::Upcast &&
                         tt->type.isA(arg->type)) ||
                        (tt->kind == CastType::Downcast &&
                         arg->type.isA(tt->type))) {
                        tt->replaceUsesWith(arg);
                        removed = true;
                        next = bb->remove(ip);
                    }
                } else if (auto asInt = AsInt::Cast(i)) {
                    auto arg = asInt->arg<0>().val();
                    if (arg->type.isA(
                            PirType(RType::integer).scalar().notObject())) {
                        asInt->replaceUsesWith(arg);
                        removed = true;
                        next = bb->remove(ip);
                    }
                } else if (auto env = MkEnv::Cast(i)) {
                    static std::unordered_set<Tag> tags{Tag::IsEnvStub};
                    if (env->stub && env->usesAreOnly(function->entry, tags)) {
                        env->replaceUsesWith(Env::elided());
                        removed = true;
                        next = bb->remove(ip);
                    }
                } else if (auto test = IsEnvStub::Cast(i)) {
                    if (test->env() == Env::elided()) {
                        // Assuming only env stubs are elided, see case above
                        i->replaceUsesWith(True::instance());
                        removed = true;
                        next = bb->remove(ip);
                    }
                }

                if (!removed) {
                    i->updateTypeAndEffects();
                }
                ip = next;
            }
        });

        while (!todo.empty()) {
            Promise* p = todo.back();
            todo.pop_back();
            Visitor::run(p->entry, [&](Instruction* i) {
                if (auto mk = MkArg::Cast(i)) {
                    size_t id = mk->prom()->id;
                    if (used_p.find(id) == used_p.end()) {
                        // found a new used promise...
                        todo.push_back(mk->prom());
                        used_p.insert(mk->prom()->id);
                    }
                }
            });
        }

        for (size_t i = 0; i < function->promises().size(); ++i)
            if (function->promise(i) && used_p.find(i) == used_p.end())
                function->erasePromise(i);

        // Consider:
        //
        //  loop:
        //    p1 = phi(...)
        //    p = phi(a, c)
        //    c = cast(p1)
        //    goto loop
        //
        //  If cleanup removes the cast, then p1 will be input to p in the same
        //  block. So we need an additional pass to fix those cases and merge
        //  the appropriate branch of p1 into p.
        Visitor::run(function->entry, [&](BB* bb) {
            for (auto ip = bb->begin(); ip != bb->end(); ++ip) {
                if (auto p = Phi::Cast(*ip)) {
                    p->eachArg([&](BB* in, InstrArg& arg) {
                        if (auto p2 = Phi::Cast(arg.val())) {
                            if (p->bb() != p2->bb())
                                return;
                            if (bb->atPosition(p2) > ip)
                                return;
                            p2->eachArg([&](BB* in2, Value* arg2) {
                                if (in == in2)
                                    arg.val() = arg2;
                            });
                        }
                    });
                }
            }
        });

        auto fixupPhiInput = [&](BB* old, BB* n) {
            if (!usedBB.count(old))
                return;
            for (auto phi : usedBB[old]) {
                for (size_t i = 0; i < phi->nargs(); ++i)
                    if (phi->inputAt(i) == old)
                        phi->updateInputAt(i, n);
            }
            assert(!n->isDeopt());
            usedBB[n].insert(usedBB[old].begin(), usedBB[old].end());
            usedBB.erase(usedBB.find(old));
        };

        std::unordered_map<BB*, BB*> toDel;
        Visitor::run(function->entry, [&](BB* bb) {
            // If bb is a jump to non-merge block, we merge it with the next
            if (bb->isJmp() && bb->next()->hasSinglePred()) {
                bool block = false;
                // Prevent this removal from merging a phi input block with the
                // block the phi resides in
                if (usedBB.count(bb))
                    for (auto phi : usedBB[bb]) {
                        phi->eachArg([&](BB* in, Value*) {
                            if (in == bb && bb->next() == phi->bb())
                                block = true;
                        });
                    }
                if (block)
                    return;
                BB* d = bb->next();
                while (!d->isEmpty())
                    d->moveToEnd(d->begin(), bb);
                bb->overrideSuccessors(d->succsessors());
                d->deleteSuccessors();
                fixupPhiInput(d, bb);
                toDel[d] = nullptr;
            }
        });

        // Merge blocks
        Visitor::runPostChange(function->entry, [&](BB* bb) {
            if (bb->isJmp() && bb->hasSinglePred() &&
                bb->next()->hasSinglePred()) {
                BB* d = bb->next();
                while (!d->isEmpty()) {
                    d->moveToEnd(d->begin(), bb);
                }
                bb->overrideSuccessors(d->succsessors());
                d->deleteSuccessors();
                fixupPhiInput(d, bb);
                toDel[d] = nullptr;
            }
        });

        Visitor::runPostChange(function->entry, [&](BB* bb) {
            // Remove empty branches
            if (bb->isBranch()) {
                if (bb->trueBranch()->isEmpty() &&
                    bb->falseBranch()->isEmpty() && bb->trueBranch()->isJmp() &&
                    bb->falseBranch()->isJmp() &&
                    bb->trueBranch()->next() == bb->falseBranch()->next() &&
                    usedBB.find(bb->trueBranch()) == usedBB.end() &&
                    usedBB.find(bb->falseBranch()) == usedBB.end()) {
                    toDel[bb->trueBranch()] = bb->trueBranch()->next();
                    toDel[bb->falseBranch()] = nullptr;
                    bb->convertBranchToJmp(true);
                    bb->remove(bb->end() - 1);
                }
            }
        });

        if (function->entry->isJmp() &&
            function->entry->next()->hasSinglePred()) {
            BB* bb = function->entry;
            BB* d = bb->next();
            while (!d->isEmpty()) {
                d->moveToEnd(d->begin(), bb);
            }
            bb->overrideSuccessors(d->succsessors());
            d->deleteSuccessors();
            fixupPhiInput(d, bb);
            toDel[d] = nullptr;
        }
        Visitor::run(function->entry, [&](BB* bb) {
            while (bb->isJmp() && toDel.count(bb->next()))
                bb->overrideNext(toDel[bb->next()]);
        });
        for (auto e : toDel) {
            BB* bb = e.first;
            bb->deleteSuccessors();
            delete bb;
        }

        BBTransform::renumber(function);
        function->eachPromise(BBTransform::renumber);

    }
};
} // namespace

namespace rir {
namespace pir {

void Cleanup::apply(RirCompiler&, ClosureVersion* function, LogStream&) const {
    TheCleanup s(function);
    s();
}

} // namespace pir
} // namespace rir
