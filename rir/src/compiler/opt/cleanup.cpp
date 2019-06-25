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

        Visitor::run(function->entry, [&](BB* bb) {
            auto ip = bb->begin();
            while (ip != bb->end()) {
                Instruction* i = *ip;
                auto next = ip + 1;
                bool removed = false;
                bool dead = i->unused() && !i->branchOrExit();
                if (dead && !i->hasObservableEffects()) {
                    removed = true;
                    next = bb->remove(ip);
                } else if (dead &&
                           i->getObservableEffects() == Effect::Visibility &&
                           i->visibilityFlag() != VisibilityFlag::Unknown &&
                           !Visible::Cast(i) && !Invisible::Cast(i)) {
                    removed = true;
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
                        for (auto curBB : phi->inputs())
                            usedBB[curBB].insert(phi);
                    }
                } else if (auto arg = MkArg::Cast(i)) {
                    if (arg->unused()) {
                        removed = true;
                        next = bb->remove(ip);
                    } else {
                        used_p.insert(arg->prom()->id);
                        todo.push_back(arg->prom());
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
                    if (arg->type == tt->type) {
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
                        i->replaceUsesWith(False::instance());
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

        auto fixupPhiInput = [&](BB* old, BB* n) {
            for (auto phi : usedBB[old]) {
                for (size_t i = 0; i < phi->nargs(); ++i)
                    if (phi->inputAt(i) == old)
                        phi->updateInputAt(i, n);
            }
            usedBB[n].insert(usedBB[old].begin(), usedBB[old].end());
            usedBB.erase(usedBB.find(old));
        };

        CFG cfg(function);
        std::unordered_map<BB*, BB*> toDel;
        Visitor::run(function->entry, [&](BB* bb) {
            // If bb is a jump to non-merge block, we merge it with the next
            if (bb->isJmp() && cfg.hasSinglePred(bb->next0)) {
                bool block = false;
                // Prevent this removal from merging a phi input block with the
                // block the phi resides in
                for (auto phi : usedBB[bb]) {
                    phi->eachArg([&](BB* in, Value*) {
                        if (in == bb && bb->next0 == phi->bb())
                            block = true;
                    });
                }
                if (block)
                    return;
                BB* d = bb->next0;
                while (!d->isEmpty())
                    d->moveToEnd(d->begin(), bb);
                bb->next0 = d->next0;
                bb->next1 = d->next1;
                d->next0 = nullptr;
                d->next1 = nullptr;
                fixupPhiInput(d, bb);
                toDel[d] = nullptr;
            }
        });

        // Merge blocks
        Visitor::runPostChange(function->entry, [&](BB* bb) {
            if (bb->isJmp() && cfg.hasSinglePred(bb) &&
                cfg.hasSinglePred(bb->next0)) {
                BB* d = bb->next0;
                while (!d->isEmpty()) {
                    d->moveToEnd(d->begin(), bb);
                }
                bb->next0 = d->next0;
                bb->next1 = d->next1;
                d->next0 = nullptr;
                d->next1 = nullptr;
                fixupPhiInput(d, bb);
                toDel[d] = nullptr;
            }
        });

        Visitor::runPostChange(function->entry, [&](BB* bb) {
            // Remove empty branches
            if (bb->next0 && bb->next1) {
                if (bb->next0->isEmpty() && bb->next1->isEmpty() &&
                    bb->next0->next0 == bb->next1->next0 &&
                    usedBB.find(bb->next0) == usedBB.end() &&
                    usedBB.find(bb->next1) == usedBB.end()) {
                    toDel[bb->next0] = bb->next0->next0;
                    toDel[bb->next1] = bb->next0->next0;
                    bb->next1 = nullptr;
                    bb->remove(bb->end() - 1);
                }
            }
        });

        if (function->entry->isJmp() &&
            cfg.hasSinglePred(function->entry->next0)) {
            BB* bb = function->entry;
            BB* d = bb->next0;
            while (!d->isEmpty()) {
                d->moveToEnd(d->begin(), bb);
            }
            bb->next0 = d->next0;
            bb->next1 = d->next1;
            d->next0 = nullptr;
            d->next1 = nullptr;
            fixupPhiInput(d, bb);
            toDel[d] = nullptr;
        }
        Visitor::run(function->entry, [&](BB* bb) {
            while (toDel.count(bb->next0))
                bb->next0 = toDel[bb->next0];
            assert(!toDel.count(bb->next1));
        });
        for (auto e : toDel) {
            BB* bb = e.first;
            bb->next0 = nullptr;
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
