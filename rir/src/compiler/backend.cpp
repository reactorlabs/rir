#include "backend.h"
#include "R/BuiltinIds.h"
#include "analysis/dead.h"
#include "compiler/analysis/cfg.h"
#include "compiler/analysis/last_env.h"
#include "compiler/analysis/reference_count.h"
#include "compiler/analysis/verifier.h"
#include "compiler/log/perf_counter.h"
#include "compiler/native/lower_llvm.h"
#include "compiler/parameter.h"
#include "compiler/pir/pir_impl.h"
#include "compiler/pir/value_list.h"
#include "compiler/util/bb_transform.h"
#include "compiler/util/lowering/allocators.h"
#include "compiler/util/visitor.h"
#include "event_counters.h"
#include "interpreter/instance.h"
#include "ir/CodeStream.h"
#include "ir/CodeVerifier.h"
#include "runtime/DispatchTable.h"
#include "simple_instruction_list.h"
#include "utils/FunctionWriter.h"

#include <algorithm>
#include <chrono>
#include <iomanip>
#include <list>
#include <sstream>

namespace rir {
namespace pir {

static void approximateRefcount(ClosureVersion* cls, Code* code,
                                NeedsRefcountAdjustment& refcount,
                                ClosureStreamLogger& log) {
    StaticReferenceCount refcountAnalysis(cls, code, log.out());
    refcountAnalysis();
    refcount = refcountAnalysis.getGlobalState();
}

static void approximateNeedsLdVarForUpdate(
    Code* code, std::unordered_set<Instruction*>& needsLdVarForUpdate) {
    Visitor::run(code->entry, [&](Instruction* i) {
        switch (i->tag) {
        case Tag::Subassign1_1D:
        case Tag::Subassign2_1D:
        case Tag::Subassign1_2D:
        case Tag::Subassign2_2D:
            // Subassigns override the vector, even if the named count
            // is 1. This is only valid, if we are sure that the vector
            // is local, ie. vector and subassign operation come from
            // the same lexical scope.
            if (auto vec =
                    Instruction::Cast(i->arg(1).val()->followCastsAndForce())) {
                if (auto ld = LdVar::Cast(vec)) {
                    if (auto su = i->hasSingleUse()) {
                        if (auto st = StVar::Cast(su)) {
                            if (ld->env() != st->env())
                                needsLdVarForUpdate.insert(vec);
                            break;
                        }
                        if (StVarSuper::Cast(su)) {
                            break;
                        }
                    }
                    if (auto mk = MkEnv::Cast(ld->env()))
                        if (mk->stub &&
                            mk->arg(mk->indexOf(ld->varName)).val() !=
                                UnboundValue::instance())
                            break;

                    needsLdVarForUpdate.insert(vec);
                }
            }
            break;
        default: {}
        }
    });
}

static bool coinFlip() {
    static std::mt19937 gen(Parameter::DEOPT_CHAOS_SEED);
    static std::bernoulli_distribution coin(
        Parameter::DEOPT_CHAOS ? 1.0 / Parameter::DEOPT_CHAOS : 0);
    return coin(gen);
};

static void lower(Code* code) {
    DeadInstructions representAsReal(
        code, 1, Effects::Any(),
        DeadInstructions::IgnoreUsesThatDontObserveIntVsReal);

    Visitor::runPostChange(code->entry, [&](BB* bb) {
        auto it = bb->begin();
        while (it != bb->end()) {
            auto next = it + 1;
            if ((*it)->frameState() && !Deopt::Cast(*it))
                (*it)->clearFrameState();

            auto t = (*it)->type;
            if (t.isA(PirType::simpleScalar())) {
                // In the case we have an instruction that might statically
                // return int or double, but there is no instruction that could
                // observe the difference we might as well set the type to
                // double to avoid boxing.
                if (t.maybe(PirType::simpleScalarInt()) &&
                    t.maybe(PirType::simpleScalarReal()) &&
                    representAsReal.isDead(*it)) {
                    (*it)->type = t & PirType::simpleScalarReal();
                }
            }

            if (auto b = CallSafeBuiltin::Cast(*it)) {
                if (b->builtinId == blt("length") && next != bb->end()) {
                    if (auto t = IsType::Cast(*(it + 1))) {
                        if (t->typeTest.isA(
                                PirType::simpleScalarInt().notNAOrNaN()) &&
                            t->arg(0).val() == b) {
                            // Type test follows, let's cheat and load this as
                            // an integer already. this avoids boxing in the
                            // native backend. NOTE: don't move this to an
                            // earlier pass, since otherwise the check will be
                            // optimized away!
                            b->type = PirType::simpleScalarInt().notNAOrNaN();
                            break;
                        }
                    }
                }
            } else if (auto ldfun = LdFun::Cast(*it)) {
                // The guessed binding in ldfun is just used as a temporary
                // store. If we did not manage to resolve ldfun by now, we
                // have to remove the guess again, since apparently we
                // were not sure it is correct.
                if (ldfun->guessedBinding())
                    ldfun->clearGuessedBinding();
            } else if (auto ld = LdVar::Cast(*it)) {
                while (true) {
                    auto mk = MkEnv::Cast(ld->env());
                    if (mk && mk->stub && !mk->contains(ld->varName))
                        ld->env(mk->lexicalEnv());
                    else
                        break;
                }
            } else if (auto id = Identical::Cast(*it)) {
                if (auto ld1 = LdConst::Cast(id->arg(0).val())) {
                    if (auto ld2 = LdConst::Cast(id->arg(1).val())) {
                        id->replaceUsesWith(ld1->c() == ld2->c()
                                                ? (Value*)True::instance()
                                                : (Value*)False::instance());
                        next = bb->remove(it);
                    }
                }
            } else if (auto st = StVar::Cast(*it)) {
                auto mk = MkEnv::Cast(st->env());
                if (mk && mk->stub)
                    assert(mk->contains(st->varName));
            } else if (auto deopt = Deopt::Cast(*it)) {
                // Lower Deopt instructions + their FrameStates to a
                // ScheduledDeopt.
                auto newDeopt = new ScheduledDeopt();
                newDeopt->consumeFrameStates(deopt);

                if (Parameter::DEBUG_DEOPTS) {
                    std::stringstream msgs;
                    msgs << "DEOPT:\n";
                    deopt->printRecursive(msgs, 3);
                    static std::vector<std::string> leak;
                    leak.push_back(msgs.str());
                    SEXP msg = Rf_mkString(leak.back().c_str());
                    static SEXP print =
                        Rf_findFun(Rf_install("cat"), R_GlobalEnv);
                    auto ldprint = new LdConst(print);
                    Instruction* ldmsg = new LdConst(msg);
                    it = bb->insert(it, ldmsg) + 1;
                    it = bb->insert(it, ldprint) + 1;
                    // Hack to silence the verifier.
                    ldmsg = new CastType(ldmsg, CastType::Downcast,
                                         PirType::any(), RType::prom);
                    it = bb->insert(it, ldmsg) + 1;
                    it =
                        bb->insert(it, new Call(Env::global(), ldprint, {ldmsg},
                                                Tombstone::framestate(), 0));
                    it++;
                }
                bb->replace(it, newDeopt);
                next = it + 1;
            } else if (auto expect = Assume::Cast(*it)) {
                if (expect->arg(0).val() == True::instance()) {
                    next = bb->remove(it);
                } else {
                    auto expectation = expect->assumeTrue;
                    std::string debugMessage;
                    if (Parameter::DEBUG_DEOPTS) {
                        debugMessage = "DEOPT, assumption ";
                        {
                            std::stringstream dump;
                            if (auto i =
                                    Instruction::Cast(expect->condition())) {
                                dump << "\n";
                                i->printRecursive(dump, 4);
                                dump << "\n";
                            } else {
                                expect->condition()->printRef(dump);
                            }
                            debugMessage += dump.str();
                        }
                        debugMessage += " failed\n";
                    }
                    BBTransform::lowerExpect(
                        code, bb, it, expect, expectation,
                        expect->checkpoint()->bb()->falseBranch(), debugMessage,
                        Parameter::DEOPT_CHAOS && coinFlip());
                    // lowerExpect splits the bb from current position. There
                    // remains nothing to process. Breaking seems more robust
                    // than trusting the modified iterator.
                    break;
                }
            }

            it = next;
        }
    });

    Visitor::run(code->entry, [&](BB* bb) {
        auto it = bb->begin();
        while (it != bb->end()) {
            auto next = it + 1;
            if (FrameState::Cast(*it)) {
                next = bb->remove(it);
            } else if (Checkpoint::Cast(*it)) {
                next = bb->remove(it);
                bb->convertBranchToJmp(true);
            }
            it = next;
        }
    });

    BBTransform::mergeRedundantBBs(code);

    // Insert Nop into all empty blocks to make life easier
    Visitor::run(code->entry, [&](BB* bb) {
        if (bb->isEmpty())
            bb->append(new Nop());
    });
}

static void toCSSA(Code* code) {

    // For each Phi, insert copies
    BreadthFirstVisitor::run(code->entry, [&](BB* bb) {
        // TODO: move all phi's to the beginning, then insert the copies not
        // after each phi but after all phi's?
        for (auto it = bb->begin(); it != bb->end(); ++it) {
            auto instr = *it;
            if (auto phi = Phi::Cast(instr)) {

                for (size_t i = 0; i < phi->nargs(); ++i) {
                    BB* pred = phi->inputAt(i);
                    // If pred is branch insert a new split block
                    if (!pred->isJmp()) {
                        assert(pred->isBranch());
                        BB* split = nullptr;
                        if (pred->trueBranch() == phi->bb())
                            split = pred->trueBranch();
                        else if (pred->falseBranch() == phi->bb())
                            split = pred->falseBranch();
                        assert(split &&
                               "Don't know where to insert a phi input copy.");
                        pred = BBTransform::splitEdge(code->nextBBId++, pred,
                                                      split, code);
                    }
                    if (Instruction* iav =
                            Instruction::Cast(phi->arg(i).val())) {
                        auto copy = pred->insert(pred->end(), new PirCopy(iav));
                        phi->arg(i).val() = *copy;
                    } else {

                        auto val = phi->arg(i).val()->asRValue();
                        auto copy = pred->insert(pred->end(), new LdConst(val));

                        phi->arg(i).val() = *copy;

                        if (phi->arg(i).type() == NativeType::test) {
                            (*copy)->type = phi->arg(i).type();
                        }
                    }
                }
                auto phiCopy = new PirCopy(phi);
                phi->replaceUsesWith(phiCopy);
                it = bb->insert(it + 1, phiCopy);
            }
        }
    });
}

rir::Function* Backend::doCompile(ClosureVersion* cls,
                                  ClosureStreamLogger& log) {
    // TODO: keep track of source ast indices in the source pool
    // (for now, calls, promises and operators do)
    // + how to deal with inlined stuff?

    Preserve preserve;
    FunctionWriter function;

    FunctionSignature signature(
        FunctionSignature::Environment::CalleeCreated,
        FunctionSignature::OptimizationLevel::Optimized);

    auto arg = cls->owner()->formals().original();
    for (size_t i = 0; i < cls->nargs(); ++i) {
        // In PIR default args are callee-handled.
        function.addArgWithoutDefault();
        signature.pushFormal(CAR(arg), TAG(arg));
        arg = CDR(arg);
    }

    assert(signature.formalNargs() == cls->nargs());
    std::unordered_map<Code*,
                       std::unordered_map<Code*, std::pair<unsigned, MkArg*>>>
        promMap;
    std::function<void(Code*)> scan = [&](Code* c) {
        if (promMap.count(c))
            return;
        // cppcheck-suppress variableScope
        auto& pm = promMap[c];
        auto addProm = [&](Instruction* i) {
            if (auto mk = MkArg::Cast(i)) {
                auto p = mk->prom();
                if (!pm.count(p)) {
                    scan(p);
                    pm[p] = {pm.size(), mk};
                }
            }
        };
        lower(c);
        toCSSA(c);
        log.CSSA(c);
#ifdef FULLVERIFIER
        Verify::apply(cls, "Error after lowering", true);
#else
#ifdef ENABLE_SLOWASSERT
        Verify::apply(cls, "Error after lowering");
#endif
#endif
        Visitor::run(c->entry, addProm);
    };
    scan(cls);

    std::unordered_map<Code*, rir::Code*> done;
    LowerLLVM lowerLlvm;
    std::function<rir::Code*(Code*)> compile = [&](Code* c) {
        if (done.count(c))
            return done.at(c);
        NeedsRefcountAdjustment refcount;
        approximateRefcount(cls, c, refcount, log);
        std::unordered_set<Instruction*> needsLdVarForUpdate;
        approximateNeedsLdVarForUpdate(c, needsLdVarForUpdate);
        auto res = done[c] = rir::Code::New(c->rirSrc()->src);
        preserve(res->container());
        lowerLlvm.compile(res, cls, c, promMap.at(c), refcount,
                          needsLdVarForUpdate, log.out());
        auto& pm = promMap.at(c);
        // Order of prms in the extra pool must equal id in promMap
        std::vector<Code*> proms(pm.size());
        for (auto p : pm)
            proms.at(p.second.first) = p.first;
        for (auto p : proms) {
            auto code = compile(p);
            if (pm.at(p).second->noReflection)
                res->flags.set(rir::Code::NoReflection);
            res->addExtraPoolEntry(code->container());
        }
        return res;
    };
    auto body = compile(cls);

    log.finalPIR(cls);
    function.finalize(body, signature, cls->context());

    function.function()->inheritFlags(cls->owner()->rirFunction());
    return function.function();
}

rir::Function* Backend::compile(ClosureVersion* cls) {
    auto res = done.find(cls);
    if (res != done.end())
        return res->second;

    auto& log = logger.get(cls);
    done[cls] = nullptr;
    auto fun = doCompile(cls, log);
    done[cls] = fun;
    log.flush();

    if (fixup.count(cls)) {
        auto fixups = fixup.find(cls);
        for (auto idx : fixups->second)
            Pool::patch(idx, fun->container());
        fixup.erase(fixups);
    }
    return fun;
}

bool Parameter::DEBUG_DEOPTS = getenv("PIR_DEBUG_DEOPTS") &&
                               0 == strncmp("1", getenv("PIR_DEBUG_DEOPTS"), 1);
int Parameter::DEOPT_CHAOS =
    getenv("PIR_DEOPT_CHAOS") ? atoi(getenv("PIR_DEOPT_CHAOS")) : 0;
int Parameter::DEOPT_CHAOS_SEED =
    getenv("PIR_DEOPT_CHAOS_SEED") ? atoi(getenv("PIR_DEOPT_CHAOS_SEED")) : 42;

} // namespace pir
} // namespace rir
