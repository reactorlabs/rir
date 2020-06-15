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
        bbs[bb->id]->setSuccessors(
            bb->successors().map([&](BB* suc) { return bbs[suc->id]; }));
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

    split->setNext(to);

    from->replaceSuccessor(to, split);

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
    while (it != src->end())
        it = src->moveToEnd(it, split);
    split->setSuccessors(src->successors());
    src->overrideSuccessors({split});
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

    // pred->bb is a critical edge if pred has multiple successors and bb
    // has multiple predecessors
    Visitor::run(fun->entry, [&](BB* bb) {
        if (bb->isMerge()) {
            for (const auto& pred : bb->predecessors()) {
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

std::pair<Value*, BB*> BBTransform::forInline(BB* inlinee, BB* splice,
                                              Value* context) {
    Value* found = nullptr;
    Instruction* ret;
    BB* retBlock = nullptr;
    Visitor::run(inlinee, [&](BB* bb) {
        if (!bb->isExit())
            return;

        if (bb->isDeopt())
            return;

        // non-local returns become local returs through inlining only if the
        // caller context matches the context of the non-local return.
        // (contexts are identified by envs)
        auto nlr = NonLocalReturn::Cast(bb->last());
        if (nlr) {
            if (nlr->env() == context) {
                ret = nlr;
            } else {
                retBlock = bb;
                return;
            }
        } else {
            ret = Return::Cast(bb->last());
        }
        assert(ret);
        retBlock = bb;

        // This transformation assumes that we have just one reachable return.
        // Assert that we do not find a second one.
        assert(!found);

        found = ret->arg(0).val();
        assert(ret->bb() == bb);
        bb->setNext(splice);
        bb->remove(bb->end() - 1);
    });
    if (!found) {
        // can happen if we have only non local returns
        return {Nil::instance(), retBlock};
    }
    assert(found);
    return {found, retBlock};
}

BB* BBTransform::lowerExpect(Code* code, BB* src, BB::Instrs::iterator position,
                             Assume* assume, bool condition, BB* deoptBlock_,
                             const std::string& debugMessage,
                             bool triggerAnyway) {
    auto split = BBTransform::split(code->nextBBId++, src, position + 1, code);

    static SEXP print = Rf_findFun(Rf_install("cat"), R_GlobalEnv);

    BB* deoptBlock = new BB(code, code->nextBBId++);
    deoptBlock->setNext(deoptBlock_);

    if (debugMessage.size() != 0) {
        SEXP msg = Rf_mkString(debugMessage.c_str());
        auto ldprint = new LdConst(print);
        Instruction* ldmsg = new LdConst(msg);
        deoptBlock->append(ldmsg);
        deoptBlock->append(ldprint);
        // Hack to silence the verifier.
        ldmsg = new CastType(ldmsg, CastType::Downcast, PirType::any(),
                             RType::prom);
        deoptBlock->append(ldmsg);
        deoptBlock->append(new Call(Env::global(), ldprint, {ldmsg},
                                    Tombstone::framestate(), 0));
    }

    if (!assume->feedbackOrigin.empty()) {
        for (auto& origin : assume->feedbackOrigin) {
            Value* src = nullptr;
            auto cond = assume->condition();
            auto r = DeoptReason::None;
            if (auto t = IsType::Cast(cond)) {
                r = DeoptReason::Typecheck;
                src = t->arg<0>().val();
            } else if (auto t = Identical::Cast(cond)) {
                src = t->arg<0>().val();
                if (LdConst::Cast(src))
                    src = t->arg<0>().val();
                assert(!LdConst::Cast(src));
                r = DeoptReason::Calltarget;
            } else if (auto t = IsEnvStub::Cast(cond)) {
                src = t->arg(0).val();
                r = DeoptReason::EnvStubMaterialized;
            } else {
                if (auto c = Instruction::Cast(cond)) {
                    c->print(std::cerr);
                    assert(src && "Don't know how to report deopt reason");
                }
            }
            switch (r) {
            case DeoptReason::Typecheck:
            case DeoptReason::Calltarget: {
                auto offset =
                    (uintptr_t)origin.second - (uintptr_t)origin.first;
                auto o = *((Opcode*)origin.first + offset);
                assert(o == Opcode::record_call_ || o == Opcode::record_type_ ||
                       o == Opcode::record_test_);
                assert((uintptr_t)origin.second > (uintptr_t)origin.first);
                auto rec = new RecordDeoptReason(
                    {r, origin.first, (uint32_t)offset}, src);
                deoptBlock->append(rec);
                break;
            }
            case DeoptReason::EnvStubMaterialized: {
                auto rec = new RecordDeoptReason({r, origin.first, 0}, src);
                deoptBlock->append(rec);
                break;
            }
            case DeoptReason::DeadBranchReached: {
                assert(false);
                break;
            }
            case DeoptReason::None:
                break;
            }
        }
    }

    Value* test = assume->condition();
    if (triggerAnyway) {
        test = condition ? (Value*)False::instance() : (Value*)True::instance();
    }

    src->replace(position, new Branch(test));
    if (condition)
        src->overrideSuccessors({split, deoptBlock});
    else
        src->overrideSuccessors({deoptBlock, split});

    // If visibility was tainted between the last checkpoint and the bailout,
    // we try (best-effort) to recover the correct setting, by scanning for the
    // last known-good setting.
    bool wrongViz = false;
    for (auto i : *src) {
        if (i->effects.contains(Effect::Visibility))
            wrongViz = true;
    }
    auto rollbackBlock = assume->checkpoint()->bb();
    while (wrongViz) {
        for (auto it = rollbackBlock->rbegin(); it != rollbackBlock->rend();
             ++it) {
            if (!(*it)->effects.includes(Effect::Visibility))
                continue;

            auto viz = (*it)->visibilityFlag();
            if (viz != VisibilityFlag::Unknown) {
                deoptBlock->append(viz == VisibilityFlag::On
                                       ? (Instruction*)new Visible
                                       : (Instruction*)new Invisible);
                wrongViz = false;
                break;
            }
        }
        if (!rollbackBlock->hasSinglePred())
            break;

        rollbackBlock = *rollbackBlock->predecessors().begin();
    }

    return split;
}

void BBTransform::insertAssume(Instruction* condition, Checkpoint* cp, BB* bb,
                               BB::Instrs::iterator& position,
                               bool assumePositive, rir::Code* srcCode,
                               Opcode* origin) {
    position = bb->insert(position, condition);
    auto assume = new Assume(condition, cp);
    if (srcCode)
        assume->feedbackOrigin.push_back({srcCode, origin});
    if (!assumePositive)
        assume->Not();
    position = bb->insert(position + 1, assume);
    position++;
};

void BBTransform::insertAssume(Instruction* condition, Checkpoint* cp,
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
    std::unordered_set<BB*> merge;
    Visitor::run(closure->entry, [&](BB* bb) {
        if (bb->isJmp() && bb->next()->predecessors().size() == 1)
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
        bb->overrideSuccessors(next->successors());
        next->deleteSuccessors();
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

    DeadInstructions dead(fun, maxBurstSize, Effects());

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
                if (auto ldf = LdFun::Cast(i)) {
                    if (auto guess = ldf->guessedBinding()) {
                        auto gi = Instruction::Cast(guess);
                        if (!gi->hasObservableEffects() && dead.isDead(gi))
                            ldf->clearGuessedBinding();
                    }
                }
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

void BBTransform::removeDeadBlocks(Code* fun,
                                   const std::unordered_set<BB*>& dead) {
    if (dead.empty())
        return;
    {
        std::unordered_set<BB*> toDelete;
        CFG cfg(fun);
        for (auto d : dead) {
            if (toDelete.count(d))
                continue;
            toDelete.insert(d);
            Visitor::run(d, [&](BB* bb) {
                if (!cfg.isPredecessor(fun->entry, bb))
                    toDelete.insert(bb);
            });
        }
        Visitor::run(fun->entry, [&](Instruction* i) {
            if (auto phi = Phi::Cast(i)) {
                phi->removeInputs(toDelete);
            }
        });
        for (const auto& bb : toDelete)
            bb->deleteSuccessors();
        for (const auto& bb : toDelete)
            delete bb;
    }
}

} // namespace pir
} // namespace rir
