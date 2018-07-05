#include "cleanup.h"
#include "../pir/pir_impl.h"
#include "../util/cfg.h"
#include "../util/visitor.h"
#include "R/r.h"

#include <unordered_map>
#include <unordered_set>

namespace {
using namespace rir::pir;

class TheCleanup {
  public:
    TheCleanup(Closure* function) : function(function) {}
    Closure* function;
    void operator()() {
        std::unordered_set<size_t> used_p;
        std::unordered_map<BB*, std::unordered_set<Phi*>> used_bb;

        Visitor::run(function->entry, [&](BB* bb) {
            auto ip = bb->begin();
            while (ip != bb->end()) {
                Instruction* i = *ip;
                auto next = ip + 1;

                // Convert lazy calls to eager calls, if all args are eager.
                {
                    auto call = Call::Cast(i);
                    if (call) {
                        bool allEager = true;
                        std::vector<Value*> args;
                        call->eachCallArg([&](Value* v) {
                            auto arg = MkArg::Cast(v);
                            if (arg && arg->eagerArg() == Missing::instance())
                                allEager = false;
                            if (allEager)
                                args.push_back(arg->eagerArg());
                        });
                        if (allEager) {
                            auto eagerCall = new EagerCall(
                                call->env(), call->cls(), args, call->srcIdx);
                            call->replaceUsesWith(eagerCall);
                            bb->replace(ip, eagerCall);
                        }
                    }
                }

                {
                    auto call = StaticCall::Cast(i);
                    if (call) {
                        bool allEager = true;
                        std::vector<Value*> args;
                        call->eachCallArg([&](Value* v) {
                            auto arg = MkArg::Cast(v);
                            if (arg && arg->eagerArg() == Missing::instance())
                                allEager = false;
                            if (allEager)
                                args.push_back(arg->eagerArg());
                        });
                        if (allEager) {
                            auto eagerCall = new StaticEagerCall(
                                call->env(), call->cls(), args, call->srcIdx,
                                call->origin());
                            call->replaceUsesWith(eagerCall);
                            bb->replace(ip, eagerCall);
                        }
                    }
                }

                ip = next;
            }
        });

        Visitor::run(function->entry, [&](BB* bb) {
            auto ip = bb->begin();
            while (ip != bb->end()) {
                Instruction* i = *ip;
                auto next = ip + 1;
                Force* force = Force::Cast(i);
                ChkClosure* chkcls = ChkClosure::Cast(i);
                ChkMissing* missing = ChkMissing::Cast(i);
                Phi* phi = Phi::Cast(i);
                MkArg* arg = MkArg::Cast(i);
                if (!i->mightIO() && !i->changesEnv() && i->unused()) {
                    next = bb->remove(ip);
                } else if (force) {
                    Value* arg = force->arg<0>().val();
                    if (PirType::valOrMissing().isSuper(arg->type)) {
                        force->replaceUsesWith(arg);
                        next = bb->remove(ip);
                    }
                } else if (chkcls) {
                    Value* arg = chkcls->arg<0>().val();
                    if (arg->type.isA(RType::closure)) {
                        chkcls->replaceUsesWith(arg);
                        next = bb->remove(ip);
                    }
                } else if (missing) {
                    Value* arg = missing->arg<0>().val();
                    if (PirType::val().isSuper(arg->type)) {
                        missing->replaceUsesWith(arg);
                        next = bb->remove(ip);
                    }
                } else if (phi) {
                    std::unordered_set<Value*> phin;
                    phi->eachArg([&](BB*, Value* v) { phin.insert(v); });
                    if (phin.size() == 1) {
                        phi->replaceUsesWith(*phin.begin());
                        next = bb->remove(ip);
                    } else {
                        phi->updateType();
                        for (auto bb : phi->input)
                            used_bb[bb].insert(phi);
                    }
                } else if (arg) {
                    used_p.insert(arg->prom->id);
                }
                ip = next;
            }
        });

        // Recursively serach promises for referencs to other promises
        std::deque<Promise*> todo;
        for (size_t i = 0; i < function->promises.size(); ++i) {
            Promise* p = function->promises[i];
            if (p && used_p.find(i) != used_p.end()) {
                todo.push_back(p);
            }
        }
        while (!todo.empty()) {
            Promise* p = todo.back();
            todo.pop_back();
            Visitor::run(p->entry, [&](Instruction* i) {
                MkArg* mk = MkArg::Cast(i);
                if (mk) {
                    size_t id = mk->prom->id;
                    if (used_p.find(id) == used_p.end()) {
                        // found a new used promise...
                        todo.push_back(mk->prom);
                        used_p.insert(mk->prom->id);
                    }
                }
            });
        }

        for (size_t i = 0; i < function->promises.size(); ++i) {
            if (function->promises[i] && used_p.find(i) == used_p.end()) {
                delete function->promises[i];
                function->promises[i] = nullptr;
            }
        }

        auto fixupPhiInput = [&](BB* old, BB* n) {
            for (auto phi : used_bb[old]) {
                for (auto& in : phi->input)
                    if (in == old)
                        in = n;
            }
        };
        CFG cfg(function);
        std::unordered_map<BB*, BB*> toDel;
        Visitor::run(function->entry, [&](BB* bb) {
            // Remove unnecessary splits
            if (bb->isJmp() && cfg.hasSinglePred(bb->next0) &&
                /* this condition keeps graph in split-edge: */
                cfg.hasSinglePred(bb) &&
                cfg.immediatePredecessors(bb)[0]->isJmp()) {
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
        Visitor::run(function->entry, [&](BB* bb) {
            // Remove empty jump-through blocks
            if (bb->isJmp() && bb->next0->isEmpty() && bb->next0->isJmp() &&
                cfg.hasSinglePred(bb->next0->next0)) {
                assert(used_bb.find(bb->next0) == used_bb.end());
                toDel[bb->next0] = bb->next0->next0;
            }
        });
        Visitor::run(function->entry, [&](BB* bb) {
            // Remove empty branches
            if (bb->next0 && bb->next1) {
                if (bb->next0->isEmpty() && bb->next1->isEmpty() &&
                    bb->next0->next0 == bb->next1->next0 &&
                    used_bb.find(bb->next0) == used_bb.end() &&
                    used_bb.find(bb->next1) == used_bb.end()) {
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
        function->eachDefaultArg(renumberBBs);
    }
};

} // namespace

namespace rir {
namespace pir {

void Cleanup::apply(Closure* function) {
    TheCleanup s(function);
    s();
}

} // namespace pir
} // namespace rir
