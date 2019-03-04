#include "../pir/pir_impl.h"
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
        std::unordered_map<Instruction*, int> isShared;
        std::deque<Promise*> todo;

        Visitor::run(function->entry, [&](BB* bb) {
            auto ip = bb->begin();
            while (ip != bb->end()) {
                Instruction* i = *ip;
                auto next = ip + 1;
                bool removed = false;
                if (!i->hasEffect() && !i->changesEnv() && i->unused() &&
                    !i->branchOrExit()) {
                    removed = true;
                    next = bb->remove(ip);
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
                    phi->eachArg([&](BB*, Value* v) { phin.insert(v); });
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
                } else if (SetShared::Cast(i) || EnsureNamed::Cast(i)) {
                    if (i->unused()) {
                        removed = true;
                        next = bb->remove(ip);
                    } else {
                        if (!isShared.count(i))
                            isShared[i] = 0;
                    }
                } else if (auto lgl = AsLogical::Cast(i)) {
                    auto arg = lgl->arg<0>().val();
                    if (arg->type.isA(RType::logical)) {
                        lgl->replaceUsesWith(arg);
                        removed = true;
                        next = bb->remove(ip);
                    }
                } else if (auto asInt = AsInt::Cast(i)) {
                    auto arg = asInt->arg<0>().val();
                    if (arg->type.isA(PirType(RType::integer).scalar().notObject())) {
                        asInt->replaceUsesWith(arg);
                } else if (auto env = MkEnv::Cast(i)) {
                    static std::unordered_set<Tag> tags{Tag::FrameState,
                                                        Tag::IsEnvStub};
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
                    if (!Phi::Cast(i)) {
                        i->eachArg([&](Value* arg) {
                            auto argi = Instruction::Cast(arg);
                            if (argi && (SetShared::Cast(argi) ||
                                         EnsureNamed::Cast(argi))) {
                                // Count how many times a shared value is used.
                                isShared[argi]++;
                                // If it leaks, it really needs to be marked
                                // shared.
                                if (i->leaksArg(argi))
                                    isShared[argi]++;
                                // if a value is created before and used in a
                                // loop, then this represents of course also
                                // multiple uses.
                                // For now we are very conservative and only
                                // support use and creation in the same BB.
                                // TODO: make a real static named count pass!
                                if (argi->bb() != bb)
                                    isShared[argi]++;
                            }
                        });
                    }
                    i->updateType();
                }
                ip = next;
            }
        });

        // SetShared instructions with only one use, do not need to be set to
        // shared
        for (auto shared : isShared) {
            if (shared.second <= 1)
                shared.first->replaceUsesWith(shared.first->arg(0).val());
        }

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
        Visitor::run(function->entry, [&](BB* bb) {
            // Remove empty jump-through blocks
            if (bb->isJmp() && bb->next0->isEmpty() && bb->next0->isJmp() &&
                cfg.hasSinglePred(bb->next0->next0) &&
                usedBB.find(bb->next0) == usedBB.end()) {
                toDel[bb->next0] = bb->next0->next0;
            }
        });
        Visitor::run(function->entry, [&](BB* bb) {
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
        // if (function->entry->isJmp() && function->entry->empty()) {
        //     toDel[function->entry] = function->entry->next0;
        //     function->entry = function->entry->next0;
        // }
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

        auto renumberBBs = [&](Code* code) {
            // Renumber in dominance order. This ensures that controlflow always
            // goes from smaller id to bigger id, except for back-edges.
            DominanceGraph dom(code);
            code->nextBBId = 0;
            DominatorTreeVisitor<VisitorHelpers::PointerMarker>(dom).run(
                code, [&](BB* bb) {
                    bb->unsafeSetId(code->nextBBId++);
                    bb->gc();
                });
        };
        renumberBBs(function);
        function->eachPromise(renumberBBs);
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
