#include "bb.h"
#include "../analysis/dead.h"
#include "../pir/pir_impl.h"
#include "../util/visitor.h"
#include "R/Funtab.h"
#include "R/r.h"

#include <unordered_map>
#include <unordered_set>

namespace rir {
namespace pir {

BB* BBTransform::clone(BB* src, Code* target, ClosureVersion* targetClosure) {
    std::vector<BB*> bbs;

    // Copy instructions and remember old -> new instruction map.
    std::unordered_map<Value*, Instruction*> relocation_table;
    Visitor::run(src, [&](BB* bb) {
        BB* theClone = BB::cloneInstrs(bb, target->nextBBId++, target);
        assert(bb->size() == theClone->size());
        if (bb->id >= bbs.size())
            bbs.resize(bb->id + 5);
        bbs[bb->id] = theClone;
        for (size_t i = 0; i < bb->size(); ++i)
            relocation_table[bb->at(i)] = theClone->at(i);
    });

    // Fixup CFG: next pointers of copied BB's need to be filled in.
    Visitor::run(src, [&](BB* bb) {
        bbs[bb->id]->next0 = bbs[bb->id]->next1 = nullptr;
        if (bb->next0)
            bbs[bb->id]->next0 = bbs[bb->next0->id];
        if (bb->next1)
            bbs[bb->id]->next1 = bbs[bb->next1->id];
    });

    std::unordered_map<Promise*, Promise*> promMap;
    // Relocate argument pointers using old -> new map
    BB* newEntry = bbs[src->id];
    Visitor::run(newEntry, [&](Instruction* i) {
        auto phi = Phi::Cast(i);
        if (phi) {
            for (size_t j = 0; j < phi->nargs(); ++j)
                phi->updateInputAt(j, bbs[phi->inputAt(j)->id]);
        }
        i->eachArg([&](InstrArg& arg) {
            if (arg.val()->isInstruction()) {
                assert(relocation_table.count(arg.val()));
                arg.val() = relocation_table.at(arg.val());
            }
        });
        if (auto mk = MkArg::Cast(i)) {
            Promise* p = mk->prom();
            if (p->owner != targetClosure) {
                if (promMap.count(p)) {
                    mk->updatePromise(promMap.at(p));
                } else {
                    auto c = targetClosure->createProm(p->rirSrc());
                    c->entry = clone(p->entry, c, targetClosure);
                    mk->updatePromise(c);
                }
            }
        }
    });

    return newEntry;
}

BB* BBTransform::splitEdge(size_t next_id, BB* from, BB* to, Code* target) {
    BB* split = new BB(target, next_id);

    split->next0 = to;
    split->next1 = nullptr;

    if (from->next0 == to)
        from->next0 = split;
    else
        from->next1 = split;

    Visitor::run(split, [&](Instruction* i) {
        if (auto phi = Phi::Cast(i)) {
            for (size_t j = 0; j < phi->nargs(); ++j)
                if (phi->inputAt(j) == from)
                    phi->updateInputAt(j, split);
        }
    });

    return split;
}

BB* BBTransform::split(size_t next_id, BB* src, BB::Instrs::iterator it,
                       Code* target) {
    BB* split = new BB(target, next_id);
    split->next0 = src->next0;
    split->next1 = src->next1;
    while (it != src->end()) {
        it = src->moveToEnd(it, split);
    }
    src->next0 = split;
    src->next1 = nullptr;
    Visitor::run(split, [&](Instruction* i) {
        if (auto phi = Phi::Cast(i)) {
            for (size_t j = 0; j < phi->nargs(); ++j)
                if (phi->inputAt(j) == src)
                    phi->updateInputAt(j, split);
        }
    });
    return split;
}

void BBTransform::splitCriticalEdges(Code* fun) {
    std::vector<std::pair<BB*, BB*>> edges;
    CFG cfg(fun);

    // pred->bb is a critical edge if pred has multiple successors and bb
    // has multiple predecessors
    Visitor::run(fun->entry, [&](BB* bb) {
        if (cfg.isMergeBlock(bb)) {
            for (const auto& pred : cfg.immediatePredecessors(bb)) {
                if (pred->isBranch()) {
                    // Don't split edges while iterating over the CFG!
                    edges.emplace_back(std::make_pair(pred, bb));
                }
            }
        }
    });

    for (const auto& e : edges) {
        BBTransform::splitEdge(fun->nextBBId++, e.first, e.second, fun);
    }
}

std::pair<Value*, BB*> BBTransform::forInline(BB* inlinee, BB* splice) {
    Value* found = nullptr;
    Return* ret;
    Visitor::run(inlinee, [&](BB* bb) {
        if (bb->next0 != nullptr)
            return;

        assert(bb->next1 == nullptr);
        if (bb->isDeopt())
            return;

        ret = Return::Cast(bb->last());
        assert(ret);

        // This transformation assumes that we have just one reachable return.
        // Assert that we do not find a second one.
        assert(!found);

        found = ret->arg<0>().val();
        assert(ret->bb() == bb);
        bb->next0 = splice;
        bb->remove(bb->end() - 1);
    });
    assert(found);
    return {found, ret->bb()};
}

BB* BBTransform::lowerExpect(Code* code, BB* src, BB::Instrs::iterator position,
                             Assume* assume, bool condition, BB* deoptBlock,
                             const std::string& debugMessage,
                             bool triggerAnyway) {
    auto split = BBTransform::split(code->nextBBId++, src, position + 1, code);

    static SEXP print = Rf_findFun(Rf_install("cat"), R_GlobalEnv);

    if (debugMessage.size() != 0) {
        BB* debug = new BB(code, code->nextBBId++);
        SEXP msg = Rf_mkString(debugMessage.c_str());
        auto ldprint = new LdConst(print);
        Instruction* ldmsg = new LdConst(msg);
        debug->append(ldmsg);
        debug->append(ldprint);
        // Hack to silence the verifier.
        ldmsg = new CastType(ldmsg, CastType::Downcast, PirType::any(),
                             RType::prom);
        debug->append(ldmsg);
        debug->append(new Call(Env::global(), ldprint, {ldmsg},
                               Tombstone::framestate(), 0));
        debug->setNext(deoptBlock);
        deoptBlock = debug;
    }

    if (!assume->feedbackOrigin.empty()) {
        BB* record = new BB(code, code->nextBBId++);
        for (auto& origin : assume->feedbackOrigin) {
            Value* src = nullptr;
            auto cond = assume->condition();
            auto r = DeoptReason::Typecheck;
            if (auto t = IsObject::Cast(cond)) {
                src = t->arg<0>().val();
            } else if (auto t = IsType::Cast(cond)) {
                src = t->arg<0>().val();
            } else if (auto t = Identical::Cast(cond)) {
                src = t->arg<0>().val();
                if (LdConst::Cast(src))
                    src = t->arg<0>().val();
                assert(!LdConst::Cast(src));
                r = DeoptReason::Calltarget;
            } else if (IsEnvStub::Cast(cond)) {
                // TODO
            } else {
                if (auto c = Instruction::Cast(cond)) {
                    c->print(std::cerr);
                    assert(src && "Don't know how to report deopt reason");
                }
            }
            if (src) {
                auto offset =
                    (uintptr_t)origin.second - (uintptr_t)origin.first;
                auto o = *((Opcode*)origin.first + offset);
                assert(o == Opcode::record_call_ || o == Opcode::record_type_);
                assert((uintptr_t)origin.second > (uintptr_t)origin.first);
                auto rec = new RecordDeoptReason(
                    {r, origin.first, (uint32_t)offset}, src);
                record->append(rec);
            }
        }
        record->setNext(deoptBlock);
        deoptBlock = record;
    }

    Value* test = assume->condition();
    if (triggerAnyway) {
        test = condition ? (Value*)False::instance() : (Value*)True::instance();
    }

    src->replace(position, new Branch(test));
    if (condition) {
        src->next1 = deoptBlock;
        src->next0 = split;
    } else {
        src->next0 = deoptBlock;
        src->next1 = split;
    }

    splitEdge(code->nextBBId++, src, deoptBlock, code);

    return split;
}

void BBTransform::insertAssume(Value* condition, Checkpoint* cp, BB* bb,
                               BB::Instrs::iterator& position,
                               bool assumePositive, rir::Code* srcCode,
                               Opcode* origin) {
    position = bb->insert(position, (Instruction*)condition);
    auto assume = new Assume(condition, cp);
    if (srcCode) {
        assert(origin);
        assume->feedbackOrigin.push_back({srcCode, origin});
    }
    if (!assumePositive)
        assume->Not();
    position = bb->insert(position + 1, assume);
    position++;
};

void BBTransform::insertAssume(Value* condition, Checkpoint* cp,
                               bool assumePositive, rir::Code* srcCode,
                               Opcode* origin) {
    auto contBB = cp->bb()->trueBranch();
    auto contBegin = contBB->begin();
    insertAssume(condition, cp, contBB, contBegin, assumePositive, srcCode,
                 origin);
}

void BBTransform::mergeRedundantBBs(Code* closure) {
    // Aggregate all BBs that have 1 successor and that successor
    // only has 1 predecessor
    CFG cfg(closure);
    std::unordered_set<BB*> merge;
    Visitor::run(closure->entry, [&](BB* bb) {
        if (bb->isJmp() && cfg.hasSinglePred(bb->next()))
            merge.insert(bb);
    });

    // Take each such BB and insert all its successor's instructions
    // at the end
    while (!merge.empty()) {
        auto it = merge.begin();
        auto bb = *it;
        auto next = bb->next();

        // Remove the current BB from the todo list, but if its successor is
        // also to be merged, remove that one instead
        if (merge.count(next))
            merge.erase(merge.find(next));
        else
            merge.erase(it);

        // Fixup phis
        Visitor::run(next, [&](Instruction* i) {
            if (auto phi = Phi::Cast(i)) {
                for (size_t j = 0; j < phi->nargs(); ++j)
                    if (phi->inputAt(j) == next)
                        phi->updateInputAt(j, bb);
            }
        });

        // Merge
        auto instr = next->begin();
        while (instr != next->end()) {
            instr = next->moveToEnd(instr, bb);
        }
        bb->next0 = next->next0;
        bb->next1 = next->next1;
        next->next0 = next->next1 = nullptr;
        delete next;
    }
}

void BBTransform::renumber(Code* fun) {
    DominanceGraph dom(fun);
    fun->nextBBId = 0;
    DominatorTreeVisitor<VisitorHelpers::PointerMarker>(dom).run(
        fun->entry, [&](BB* bb) {
            bb->unsafeSetId(fun->nextBBId++);
            bb->gc();
        });
}

void BBTransform::removeDeadInstrs(Code* fun, uint8_t maxBurstSize) {
    // Map of phis to other instructions that use them
    // key -> {val_1, ..., val_n} means val_1 ... val_n have key as an input
    std::unordered_map<Phi*, std::unordered_set<Instruction*>> phiUses;

    DeadInstructions dead(fun, maxBurstSize);

    Visitor::run(fun->entry, [&](BB* bb) {
        auto ip = bb->begin();
        while (ip != bb->end()) {
            Instruction* i = *ip;
            auto next = ip + 1;
            if (!i->hasObservableEffects() && dead.isDead(i)) {
                next = bb->remove(ip);
            } else {
                i->eachArg([&](Value* v) {
                    if (auto phi = Phi::Cast(v)) {
                        auto& uses = phiUses[phi];
                        uses.insert(i);
                    }
                });
            }
            ip = next;
        }
    });

    // Now see which phis are dead and can be removed. At this point, a phi is
    // dead if it is only used by itself or dead phis.
    // Alternatively, a phi is live if any of its (transitive) uses is a
    // non-phi instruction.
    auto isDeadPhi = [&phiUses](Phi* phi) -> bool {
        // Idea is that phiUses is a dependency graph, so we use DFS to find a
        // non-phi instruction.
        std::unordered_set<Phi*> seen;
        std::vector<Phi*> todo;
        todo.push_back(phi);

        while (!todo.empty()) {
            Phi* cur = todo.back();
            todo.pop_back();
            if (!seen.count(cur)) {
                seen.insert(cur);
                const auto& uses = phiUses[cur];

                // Phis not used by any other instruction have already been
                // removed.
                assert(!uses.empty());

                for (const auto& i : uses) {
                    if (auto p = Phi::Cast(i)) {
                        todo.push_back(p);
                    } else {
                        // Found a non-phi instruction that uses cur, so phi is
                        // not dead
                        return false;
                    }
                }
            }
        }

        return true;
    };

    // Iterate through phiUses, check if the phi is dead. If so, remove the
    // instruction from the cfg.
    for (const auto& kv : phiUses) {
        const auto& phi = kv.first;
        if (isDeadPhi(phi)) {
            phi->bb()->remove(phi);
        }
    }
}

} // namespace pir
} // namespace rir
