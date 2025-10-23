#include "bb_transform.h"

#include "R/Funtab.h"
#include "R/Symbols.h"
#include "R/r.h"
#include "compiler/analysis/dead.h"
#include "compiler/pir/pir_impl.h"
#include "compiler/util/visitor.h"

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
            if (Instruction::Cast(arg.val())) {
                auto val = arg.val();
                assert(relocation_table.count(val));
                arg.val() = relocation_table.at(val);
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

Value* BBTransform::forInline(BB* inlinee, BB* splice, Value* context,
                              Checkpoint* entryCp) {
    Value* found = nullptr;
    Instruction* ret;

    if (entryCp) {
        auto pos = inlinee;
        while (true) {
            for (auto i : *pos)
                if (i->isDeoptBarrier())
                    entryCp = nullptr;

            // EntryCp no longer valid, giving up
            if (!entryCp)
                break;

            // Only one successor, go there
            if (pos->isJmp()) {
                pos = pos->next();
                continue;
            }

            // This is the first CP of the inlinee, let's replace it with the
            // outer CP
            if (pos->isCheckpoint()) {
                auto cp = Checkpoint::Cast(pos->last());
                cp->replaceUsesWith(entryCp);
                auto del = pos->deoptBranch();
                std::vector<BB*> toDel = {del};
                while (del->successors().size()) {
                    assert(del->successors().size() == 1);
                    toDel.push_back(*del->successors().begin());
                    del = *del->successors().begin();
                }
                pos->overrideSuccessors(pos->nonDeoptSuccessors());
                pos->remove(pos->end() - 1);
                for (auto d : toDel)
                    delete d;
            }
            break;
        }
    }

    Visitor::run(inlinee, [&](BB* bb) {
        if (!bb->isExit())
            return;

        if (bb->isDeopt() || bb->isEndUnreachable() ||
            NonLocalReturn::Cast(bb->last()))
            return;

        ret = Return::Cast(bb->last());

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
        return Tombstone::unreachable();
    }
    return found;
}

BB* BBTransform::lowerAssume(Module* m, Code* code, BB* srcBlock,
                             BB::Instrs::iterator position, Assume* assume,
                             size_t nDropContexts, bool condition,
                             BB* deoptBlock_, const std::string& debugMessage) {

    auto split =
        BBTransform::split(code->nextBBId++, srcBlock, position + 1, code);

    BB* deoptBlock = new BB(code, code->nextBBId++);
    deoptBlock->setNext(deoptBlock_);

    if (debugMessage.size() != 0) {
        static SEXP print = Rf_findFun(Rf_install("cat"), R_GlobalEnv);

        SEXP msg = Rf_mkString(debugMessage.c_str());
        auto ldprint = m->c(print);
        auto ldmsg = m->c(msg);
        // Hack to silence the verifier.
        Instruction* ldmsg2 = new CastType(ldmsg, CastType::Downcast,
                                           PirType::any(), RType::prom);
        deoptBlock->append(ldmsg2);
        deoptBlock->append(new Call(Env::global(), ldprint, {ldmsg2},
                                    Tombstone::framestate(), 0));
    }

    while (nDropContexts--) {
        deoptBlock->append(new DropContext());
    }

    auto deoptReason = m->deoptReasonValue(assume->reason);
    auto deoptTrigger = assume->valueUnderTest();
    if (!deoptTrigger)
        deoptTrigger = UnknownDeoptTrigger::instance();

    auto d = Deopt::Cast(deoptBlock_->last());
    if (d->deoptReason() == DeoptReasonWrapper::unknown()) {
        auto newDr = new Phi(NativeType::deoptReason);
        auto newDt = new Phi;
        deoptBlock_->insert(deoptBlock_->begin(), newDr);
        deoptBlock_->insert(deoptBlock_->begin(), newDt);
        d->setDeoptReason(newDr, newDt);
    }
    Phi::Cast(d->deoptReason())->addInput(deoptBlock, deoptReason);
    Phi::Cast(d->deoptTrigger())->addInput(deoptBlock, deoptTrigger);

    Value* test = assume->condition();

    auto br = new Branch(test);
    br->deoptTrigger = true;
    srcBlock->replace(position, br);
    if (condition)
        srcBlock->overrideSuccessors({split, deoptBlock});
    else
        srcBlock->overrideSuccessors({deoptBlock, split});

    // If visibility was tainted between the last checkpoint and the bailout,
    // we try (best-effort) to recover the correct setting, by scanning for the
    // last known-good setting.
    bool wrongViz = false;
    for (auto i : *srcBlock) {
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

void BBTransform::insertAssume(Instruction* condition, bool assumePositive,
                               Checkpoint* cp, const FeedbackOrigin& origin,
                               DeoptReason::Reason reason, BB* bb,
                               BB::Instrs::iterator& position,
                               TypeTest::Info* info) {
    position = bb->insert(position, condition);
    auto assume =
        new Assume(condition, cp, DeoptReason(origin, reason), assumePositive);

    if (info) {
        info->updateAssume(*assume);
    }

    position = bb->insert(position + 1, assume);
    position++;

    bb->owner->getClosureVersion()->registerProtoSlotUsed(assume, false);
}

void BBTransform::insertAssume(Instruction* condition, bool assumePositive,
                               Checkpoint* cp, const FeedbackOrigin& origin,
                               DeoptReason::Reason reason) {
    auto contBB = cp->bb()->trueBranch();
    auto contBegin = contBB->begin();
    insertAssume(condition, assumePositive, cp, origin, reason, contBB,
                 contBegin, nullptr);
}

Value* BBTransform::insertCalleeGuard(Compiler& compiler,
                                      const CallFeedback& fb,
                                      const DeoptReason& dr, Value* callee,
                                      bool stableEnv, Value* overrideExpect,
                                      Checkpoint* cp, BB* bb,
                                      BB::Instrs::iterator& pos) {
    // We use ldvar instead of ldfun for the guard. The reason is that
    // ldfun can force promises, which is a pain for our optimizer to
    // deal with. If we use a ldvar here, the actual ldfun will be
    // delayed into the deopt branch. Note that ldvar is conservative.
    // If we find a non-function binding with the same name, we will
    // deopt unneccessarily. In the case of `c` this is guaranteed to
    // cause problems, since many variables are called "c". Therefore if
    // we have seen any variable c we keep the ldfun in this case.
    // TODO: Implement this with a dependency on the binding cell
    // instead of an eager check.

    auto calleeForGuard = callee;
    if (auto ldfun = LdFun::Cast(callee)) {
        if (ldfun->varName != symbol::c || !compiler.seenC) {
            auto ldvar = new LdVar(ldfun->varName, ldfun->env());
            pos = bb->insert(pos, ldvar) + 1;
            calleeForGuard = ldvar;
        }
    }
    auto guardedCallee = calleeForGuard;

    assert(fb.monomorphic);

    if (!stableEnv) {
        static SEXP b = nullptr;
        if (!b) {
            auto idx = blt("bodyCode");
            b = Rf_allocSExp(BUILTINSXP);
            b->u.primsxp.offset = idx;
            R_PreserveObject(b);
        }

        // The "bodyCode" builtin will return R_NilValue for promises.
        // It is therefore safe (ie. conservative with respect to the
        // guard) to avoid forcing the result by casting it to a value.
        if (calleeForGuard->type.maybeLazy()) {
            auto casted = new CastType(calleeForGuard, CastType::Downcast,
                                       PirType::any(), PirType::function());
            calleeForGuard = casted;
            pos = bb->insert(pos, casted) + 1;
        }

        auto body = new CallSafeBuiltin(b, {calleeForGuard}, 0);
        body->effects.reset();
        pos = bb->insert(pos, body) + 1;

        calleeForGuard = body;
    }

    auto expected =
        overrideExpect ? overrideExpect
                       : (stableEnv ? compiler.module->c(fb.monomorphic)
                                    : compiler.module->c(BODY(fb.monomorphic)));

    auto t = new Identical(calleeForGuard, expected, PirType::any());
    pos = bb->insert(pos, t) + 1;

    auto assumption = new Assume(t, cp, dr);
    pos = bb->insert(pos, assumption) + 1;

    if (stableEnv)
        return expected;

    // The guard also ensures that this closure is not a promise thus we
    // can force for free.
    if (guardedCallee->type.maybePromiseWrapped()) {
        auto forced =
            new Force(guardedCallee, Env::elided(), Tombstone::framestate());
        forced->effects.reset();
        forced->effects.set(Effect::DependsOnAssume);
        pos = bb->insert(pos, forced) + 1;

        guardedCallee = forced;
    }

    return guardedCallee;
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
    size_t nextBBId = 0;
    DepthFirstVisitor<VisitorHelpers::PointerMarker>::run(
        fun->entry, [&](BB* bb) {
            bb->unsafeSetId(nextBBId++);
            bb->gc();
        });
    fun->nextBBId = nextBBId;
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
            if (dead.isDead(i)) {
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
                        if (!gi ||
                            (!gi->hasObservableEffects() && dead.isDead(gi)))
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
