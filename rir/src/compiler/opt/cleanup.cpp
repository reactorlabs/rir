#include "../analysis/dead.h"
#include "../pir/pir_impl.h"
#include "../util/visitor.h"
#include "compiler/analysis/cfg.h"
#include "compiler/util/bb_transform.h"
#include "pass_definitions.h"

#include "R/r.h"
#include "pass_definitions.h"

#include <unordered_map>
#include <unordered_set>

namespace rir {
namespace pir {

bool Cleanup::apply(Compiler&, ClosureVersion* cls, Code* code, AbstractLog&,
                    size_t iter) const {
    std::unordered_set<size_t> usedProms;
    std::unordered_map<BB*, std::unordered_set<Phi*>> usedBB;
    std::deque<Promise*> todoUsedProms;

    DeadInstructions dead(code, 3, Effects(Effect::Visibility),
                          DeadInstructions::IgnoreUpdatePromise);
    bool anyChange = false;

    if (iter == 300)
        assert(false);

    Visitor::run(
        code->entry, [&](BB* bb) {
            auto ip = bb->begin();
            while (ip != bb->end()) {
                Instruction* i = *ip;
                auto next = ip + 1;
                bool removed = false;
                bool isDead = dead.isDead(i);
                // unused ldfun is a left over from a guard where ldfun was
                // converted into ldvar.
                // std::ofstream ost;
                // ost.open("iters.txt", std::ios_base::app); // append instead
                // of overwrite

                if (isDead && !Visible::Cast(i) && !Invisible::Cast(i)) {

                    if (i->getObservableEffects().includes(
                            Effect::Visibility) &&
                        i->visibilityFlag() != VisibilityFlag::Unknown) {

                        switch (i->visibilityFlag()) {
                        case VisibilityFlag::On:
                            // ost<< " von ";
                            bb->replace(ip, new Visible());
                            break;
                        case VisibilityFlag::Off:
                            // ost<< " voff ";
                            bb->replace(ip, new Invisible());
                            break;
                        default:
                            assert(false);
                        }
                    } else {
                        // ost<< " velse ";

                        // ost << "inst: " << i << "\n";
                        // i->print(ost, false);
                        // ost << i->name() << "\n";
                        // ost << "bb: " << "\n";
                        // bb->print(ost,false);

                        // ost << "instr: ";
                        // i->print(ost, false);
                        // ost << " iter: ";
                        //(*ip)->print(ost, false);
                        // ost << "bb: ";
                        // bb->print(ost,false);

                        next = bb->remove(ip);
                    }
                    removed = true;
                    // ost<< " removed1 \n";

                } else if (auto force = Force::Cast(i)) {
                    Value* arg = force->input();
                    assert(!MkArg::Cast(arg));
                    auto mkArg =
                        MkArg::Cast(force->input()->followCastsAndForce());
                    if (!arg->type.maybePromiseWrapped()) {
                        removed = true;
                        force->replaceUsesWith(arg);
                        next = bb->remove(ip);
                    } else if (mkArg && mkArg->isEager()) {
                        removed = true;
                        force->replaceUsesWith(mkArg->eagerArg());
                        next = bb->remove(ip);
                    } else if (auto a =
                                   LdArg::Cast(arg->followCastsAndForce())) {
                        if (force->hasEnv() &&
                            cls->context().isNonRefl(a->pos)) {
                            force->elideEnv();
                            force->effects.reset(Effect::Reflection);
                        }
                    }
                } else if (auto chkfun = ChkFunction::Cast(i)) {
                    Value* arg = chkfun->arg<0>().val();
                    if (arg->type.isA(PirType::function())) {
                        removed = true;
                        chkfun->replaceUsesWith(arg);
                        next = bb->remove(ip);
                    }
                } else if (auto b = CallBuiltin::Cast(i)) {
                    if (!i->hasEnv()) {
                        // ost<< " removed2 \n";
                        std::vector<Value*> args;
                        b->eachCallArg([&](Value* v) { args.push_back(v); });
                        i->replaceUsesAndSwapWith(
                            BuiltinCallFactory::New(i->env(), b->builtinSexp,
                                                    args, b->srcIdx),
                            ip);
                    }
                } else if (auto tst = CheckTrueFalse::Cast(i)) {
                    if (tst->arg(0).val()->type.isA(PirType::test())) {
                        removed = true;
                        tst->replaceUsesWith(tst->arg(0).val());
                        next = bb->remove(ip);
                    }
                } else if (auto lgl = AsLogical::Cast(i)) {
                    if (lgl->arg(0).val()->type.isA(
                            PirType::anySimpleScalar().notNAOrNaN())) {
                        removed = true;
                        lgl->replaceUsesWith(lgl->arg(0).val());
                        next = bb->remove(ip);
                    }
                } else if (auto idx = AsSwitchIdx::Cast(i)) {
                    if (idx->arg(0).val()->type.isA(
                            PirType::simpleScalarInt().notNAOrNaN())) {
                        removed = true;
                        idx->replaceUsesWith(idx->val());
                        next = bb->remove(ip);
                    }
                } else if (auto seq = ToForSeq::Cast(i)) {
                    Value* arg = seq->arg<0>().val();
                    if (!arg->type.maybeObj()) {
                        removed = true;
                        seq->replaceUsesWith(arg);
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
                        usedProms.insert(arg->prom()->id);
                        todoUsedProms.push_back(arg->prom());
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
                    } else if (!arg->type.maybe(tt->typeTest)) {
                        tt->replaceUsesWith(False::instance());
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
                } else if (auto br = Branch::Cast(i)) {
                    if (auto n = Not::Cast(br->arg(0).val())) {
                        if (n->arg(0).val()->type.isA(PirType::test())) {
                            br->arg(0).val() = n->arg(0).val();
                            auto a = bb->getBranch(true);
                            auto b = bb->getBranch(false);
                            bb->deleteSuccessors();
                            bb->setSuccessors({b, a});
                        }
                    }
                } else if (auto env = MkEnv::Cast(i)) {
                    static std::unordered_set<Tag> tags{Tag::IsEnvStub};
                    if (env->stub && env->usesAreOnly(code->entry, tags)) {
                        env->replaceUsesWith(Env::elided());
                        removed = true;
                        next = bb->remove(ip);
                    }
                } else if (auto m = MaterializeEnv::Cast(i)) {
                    if (auto mk = MkEnv::Cast(m->env())) {
                        // We un-stub envs which moved to deopt branches. Thus
                        // we need to also remove the materialize instr.
                        if (!mk->stub) {
                            i->replaceUsesWith(mk);
                            removed = true;
                            next = bb->remove(ip);
                        }
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
                } else {
                    anyChange = true;
                }
                // ost.close();
                ip = next;
            }
        });

    if (cls == code) {
        while (!todoUsedProms.empty()) {
            Promise* p = todoUsedProms.back();
            todoUsedProms.pop_back();
            Visitor::run(p->entry, [&](Instruction* i) {
                if (auto mk = MkArg::Cast(i)) {
                    size_t id = mk->prom()->id;
                    if (!usedProms.count(id)) {
                        // found a new used promise...
                        todoUsedProms.push_back(mk->prom());
                        usedProms.insert(mk->prom()->id);
                    }
                }
            });
        }

        for (size_t i = 0; i < cls->promises().size(); ++i)
            if (cls->promise(i) && !usedProms.count(i))
                cls->erasePromise(i);
    }

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
    Visitor::run(
        code->entry, [&](BB* bb) {
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
    Visitor::run(
        code->entry, [&](BB* bb) {
            // Prevent this removal from merging the entry block with its
            // successor. We always want an empty, separate entry block, so
            // that it will never have predecessors.
            if (code->entry == bb)
                return;
            // If bb is a jump to non-merge block, we merge it with the next
            if (bb->isJmp() && bb->next()->hasSinglePred()) {
                // Prevent this removal from merging a phi input block with the
                // block the phi resides in
                bool block = false;
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
                bb->overrideSuccessors(d->successors());
                d->deleteSuccessors();
                fixupPhiInput(d, bb);
                toDel[d] = nullptr;
            }
        });

    // Merge blocks
    Visitor::runPostChange(code->entry, [&](BB* bb) {
        // Prevent this removal from merging the entry block with its
        // successor. We always want an empty, separate entry block, so
        // that it will never have predecessors.
        if (code->entry == bb)
            return;

        if (bb->isJmp() && bb->hasSinglePred() && bb->next()->hasSinglePred()) {
            BB* d = bb->next();
            while (!d->isEmpty()) {
                d->moveToEnd(d->begin(), bb);
            }
            bb->overrideSuccessors(d->successors());
            d->deleteSuccessors();
            fixupPhiInput(d, bb);
            toDel[d] = nullptr;
        }
    });

    Visitor::runPostChange(
        code->entry, [&](BB* bb) {
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

    // There used to be code here to merge an entry block with its next
    // block, if the entry had only one successor and the next block had
    // only one predecessor. We want to avoid this kind of merge, because
    // we want to ensure that there is always an empty, separate entry
    // block with no predecessors.

    if (!toDel.empty())
        anyChange = true;

    Visitor::run(code->entry, [&](BB* bb) {
        while (bb->isJmp() && toDel.count(bb->next()))
            bb->overrideNext(toDel[bb->next()]);
    });
    for (auto e : toDel) {
        BB* bb = e.first;
        bb->deleteSuccessors();
        delete bb;
    }

    BBTransform::renumber(code);

    return anyChange;
}

} // namespace pir
} // namespace rir
