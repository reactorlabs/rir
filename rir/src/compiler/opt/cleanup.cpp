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
    TheCleanup(Function* function) : function(function) {}
    Function* function;
    void operator()() {
        std::unordered_set<size_t> used_p;
        std::unordered_map<BB*, std::unordered_set<Phi*>> used_bb;

        Visitor::run(function->entry, [&](BB* bb) {
            auto ip = bb->begin();
            while (ip != bb->end()) {
                Instruction* i = *ip;
                auto next = ip + 1;
                Force* force = Force::Cast(i);
                ChkMissing* missing = ChkMissing::Cast(i);
                ChkClosure* closure = ChkClosure::Cast(i);
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
                } else if (missing) {
                    Value* arg = missing->arg<0>().val();
                    if (PirType::val().isSuper(arg->type)) {
                        missing->replaceUsesWith(arg);
                        next = bb->remove(ip);
                    }
                } else if (closure) {
                    Value* arg = closure->arg<0>().val();
                    if (PirType::val().isSuper(arg->type)) {
                        closure->replaceUsesWith(arg);
                        next = bb->remove(ip);
                    }
                } else if (phi) {
                    std::unordered_set<Value*> phin;
                    phi->eachArg([&](Value* v) { phin.insert(v); });
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
            Visitor::run(p->entry, [&](BB* bb) {
                for (auto i : *bb) {
                    MkArg* mk = MkArg::Cast(i);
                    if (mk) {
                        size_t id = mk->prom->id;
                        if (used_p.find(id) == used_p.end()) {
                            // found a new used promise...
                            todo.push_back(mk->prom);
                            used_p.insert(mk->prom->id);
                        }
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

        CFG cfg(function->entry);
        std::unordered_map<BB*, BB*> toDel;

        Visitor::run(function->entry, [&](BB* bb) {
            // Remove unnecessary splits
            if (bb->isJmp() && cfg.preds[bb->next0->id].size() == 1) {
                BB* d = bb->next0;
                while (!d->isEmpty()) {
                    d->moveToEnd(d->begin(), bb);
                }
                bb->next0 = d->next0;
                bb->next1 = d->next1;
                d->next0 = nullptr;
                d->next1 = nullptr;
                for (auto phi : used_bb[d]) {
                    for (auto& in : phi->input)
                        if (in == d)
                            in = bb;
                }
                toDel[d] = nullptr;
            }
        });
        Visitor::run(function->entry, [&](BB* bb) {
            // Remove empty jump-through blocks
            if (bb->isJmp() && bb->next0->isEmpty() && bb->next0->isJmp() &&
                cfg.preds[bb->next0->next0->id].size() == 1) {
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
            cfg.preds[function->entry->next0->id].size() == 1) {
            BB* bb = function->entry;
            BB* d = bb->next0;
            while (!d->isEmpty()) {
                d->moveToEnd(d->begin(), bb);
            }
            bb->next0 = d->next0;
            bb->next1 = d->next1;
            d->next0 = nullptr;
            d->next1 = nullptr;
            assert(used_bb.find(d) == used_bb.end());
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

        // Renumber
        function->maxBBId = 0;
        Visitor::runWithChangingIds(function->entry, [&](BB* bb) {
            bb->unsafeSetId(function->maxBBId++);
            bb->gc();
        });
        function->maxBBId--;
    }
};
}

namespace rir {
namespace pir {

void Cleanup::apply(Function* function) {
    TheCleanup s(function);
    s();
}
}
}
