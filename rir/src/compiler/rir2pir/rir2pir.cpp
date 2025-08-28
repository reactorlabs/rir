#include "rir2pir.h"
#include "R/BuiltinIds.h"
#include "R/Funtab.h"
#include "R/RList.h"
#include "R/Symbols.h"
#include "bc/BC.h"
#include "bc/Compiler.h"
#include "compiler/analysis/cfg.h"
#include "compiler/analysis/query.h"
#include "compiler/analysis/verifier.h"
#include "compiler/opt/pass_definitions.h"
#include "compiler/parameter.h"
#include "compiler/pir/builder.h"
#include "compiler/pir/pir_impl.h"
#include "compiler/util/arg_match.h"
#include "compiler/util/bb_transform.h"
#include "compiler/util/visitor.h"
#include "insert_cast.h"
#include "runtime/ArglistOrder.h"
#include "runtime/TypeFeedback.h"
#include "simple_instruction_list.h"
#include "utils/FormalArgs.h"

#include <sstream>
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace {

using namespace rir::pir;
typedef rir::Function Function;
typedef rir::Opcode Opcode;
typedef rir::BC BC;
typedef rir::RList RList;

typedef std::pair<BB*, Value*> ReturnSite;

template <size_t SIZE>
struct Matcher {
    const std::array<Opcode, SIZE> seq;

    typedef std::function<void(Opcode*)> MatcherMaybe;

    bool operator()(Opcode* pc, const Opcode* end, MatcherMaybe m) const {
        for (size_t i = 0; i < SIZE; ++i) {
            if (*pc != seq[i])
                return false;
            pc = BC::next(pc);
            if (pc == end)
                return false;
        }
        m(pc);
        return true;
    }
};

struct State {
    bool seen = false;
    BB* entryBB = nullptr;
    Opcode* entryPC = 0;

    State() {}
    State(State&&) = default;
    State(const State&) = delete;
    State(const State& other, bool seen, BB* entryBB, Opcode* entryPC)
        : seen(seen), entryBB(entryBB), entryPC(entryPC), stack(other.stack) {}

    void operator=(const State&) = delete;
    State& operator=(State&&) = default;

    void mergeIn(const State& incom, BB* incomBB);
    void createMergepoint(Builder&);

    void clear() {
        stack.clear();
        entryBB = nullptr;
        entryPC = nullptr;
    }

    RirStack stack;
};

void State::createMergepoint(Builder& insert) {
    BB* oldBB = insert.getCurrentBB();
    insert.createNextBB();
    for (size_t i = 0; i < stack.size(); ++i) {
        auto v = stack.at(i);
        auto p = insert(new Phi);
        p->addInput(oldBB, v);
        stack.at(i) = p;
    }
}

void State::mergeIn(const State& incom, BB* incomBB) {
    assert(stack.size() == incom.stack.size());

    for (size_t i = 0; i < stack.size(); ++i) {
        Phi* p = Phi::Cast(stack.at(i));
        assert(p);
        Value* in = incom.stack.at(i);
        if (in != Tombstone::unreachable())
            p->addInput(incomBB, in);
    }
    incomBB->setNext(entryBB);
}

std::unordered_set<Opcode*> findMergepoints(rir::Code* srcCode) {
    std::unordered_map<Opcode*, std::vector<Opcode*>> incom;
    Opcode* first = srcCode->code();

    // Mark incoming jmps
    for (auto pc = srcCode->code(); pc != srcCode->endCode();) {
        BC bc = BC::decodeShallow(pc);
        if (bc.isJmp()) {
            incom[bc.jmpTarget(pc)].push_back(pc);
        }
        pc = BC::next(pc);
    }
    // Mark falltrough to label
    for (auto pc = srcCode->code(); pc != srcCode->endCode();) {
        BC bc = BC::decodeShallow(pc);
        if (!bc.isUncondJmp() && !bc.isExit()) {
            Opcode* next = BC::next(pc);
            if (incom.count(next))
                incom[next].push_back(pc);
        }
        pc = BC::next(pc);
    }

    std::unordered_set<Opcode*> mergepoints;
    // Create mergepoints
    for (auto m : incom)
        // The first position must also be considered a mergepoint in case it
        // has only one incoming (a jump)
        if (std::get<0>(m) == first || std::get<1>(m).size() > 1)
            mergepoints.insert(m.first);
    return mergepoints;
}

} // namespace

namespace rir {
namespace pir {

Rir2Pir::Rir2Pir(Compiler& cmp, ClosureVersion* cls, ClosureLog& log,
                 const std::string& name,
                 const std::list<PirTypeFeedback*>& outerFeedback,
                 rir::TypeFeedback* typeFeedback)
    : compiler(cmp), cls(cls), log(log), name(name),
      outerFeedback(outerFeedback), typeFeedback(typeFeedback) {
    if (cls->optFunction && cls->optFunction->body()->pirTypeFeedback())
        this->outerFeedback.push_back(
            cls->optFunction->body()->pirTypeFeedback());
}

Checkpoint* Rir2Pir::addCheckpoint(rir::Code* srcCode, Opcode* pos,
                                   const RirStack& stack,
                                   Builder& insert) const {
    if (inlining())
        return nullptr;
    for (auto i : stack)
        if (ExpandDots::Cast(i))
            return nullptr;
    return insert.emitCheckpoint(srcCode, pos, stack, inPromise());
}

Value* Rir2Pir::tryCreateArg(rir::Code* promiseCode, Builder& insert,
                             bool eager) {
    Promise* prom = insert.function->createProm(promiseCode);
    {
        Builder promiseBuilder(insert.function, prom);
        if (!tryCompilePromise(promiseCode, promiseBuilder)) {
            log.warn("Failed to compile a promise for call");
            return nullptr;
        }
    }

    Value* eagerVal = UnboundValue::instance();
    if (eager || Query::pureExceptDeopt(prom)) {
        eagerVal = tryInlinePromise(promiseCode, insert);
        if (!eagerVal) {
            log.warn("Failed to inline a promise");
            return nullptr;
        }
        cls->promiseInlined(prom);
    }

    if (eager) {
        assert(eagerVal != UnboundValue::instance());
        return eagerVal;
    }

    return insert(new MkArg(prom, eagerVal, insert.env));
}

bool Rir2Pir::compileBC(const BC& bc, Opcode* pos, Opcode* nextPos,
                        rir::Code* srcCode, RirStack& stack, Builder& insert,
                        CallTargetCheckpoints& callTargetCheckpoints) {
    Value* env = insert.env;

    unsigned srcIdx = srcCode->getSrcIdxAt(pos, true);

    srcCode->function()->involvedInCompilation = true;

    Value* v;
    Value* x;
    Value* y;

    auto push = [&stack](Value* v) { stack.push(v); };
    auto pop = [&stack]() { return stack.pop(); };
    auto popn = [&stack](size_t n) {
        for (size_t i = 0; i < n; ++i)
            stack.pop();
    };
    auto at = [&stack](unsigned i) { return stack.at(i); };
    auto top = [&stack]() { return stack.at(0); };
    auto set = [&stack](unsigned i, Value* v) { stack.at(i) = v; };

    auto forceIfPromised = [&](unsigned i) {
        if (stack.at(i)->type.maybePromiseWrapped()) {
            stack.at(i) =
                insert(new Force(at(i), env, Tombstone::framestate()));
        }
    };

    static bool TRANSFER_FEEDBACK = getenv("PIR_TRANSFER_FEEDBACK")
                                        ? atoi(getenv("PIR_TRANSFER_FEEDBACK"))
                                        : true;
    static bool DEFAULT_SPECULATION =
        getenv("PIR_DEFAULT_SPECULATION")
            ? atoi(getenv("PIR_DEFAULT_SPECULATION"))
            : true;

    switch (bc.bc) {

    case Opcode::push_: {
        auto c = bc.immediateConst();
        if (c == R_UnboundValue) {
            push(UnboundValue::instance());
        } else if (c == R_MissingArg) {
            push(MissingArg::instance());
        } else if (c == R_NilValue) {
            push(Nil::instance());
        } else if (c == R_TrueValue ||
                   (IS_SIMPLE_SCALAR(c, LGLSXP) && *LOGICAL(c) == 1)) {
            push(True::instance());
        } else if (c == R_FalseValue ||
                   (IS_SIMPLE_SCALAR(c, LGLSXP) && *LOGICAL(c) == 0)) {
            push(False::instance());
        } else if (c == symbol::expandDotsTrigger) {
            auto d = insert(new LdDots(insert.env));
            push(insert(new ExpandDots(d)));
        } else {
            push(compiler.module->c(bc.immediateConst()));
        }
        break;
    }

    case Opcode::ldvar_:
    case Opcode::ldvar_cached_:
    case Opcode::ldvar_for_update_:
    case Opcode::ldvar_for_update_cache_: {
        if (bc.immediateConst() == symbol::c)
            compiler.seenC = true;
        auto ld = new LdVar(bc.immediateConst(), env);
        if (bc.bc == Opcode::ldvar_for_update_ ||
            bc.bc == Opcode::ldvar_for_update_cache_)
            ld->forUpdate = true;
        v = insert(ld);
        // PIR LdVar corresponds to ldvar_noforce_ which does not change
        // visibility
        insert(new Visible());
        auto fs = inlining() ? (Value*)Tombstone::framestate()
                             : insert.registerFrameState(srcCode, nextPos,
                                                         stack, inPromise());
        push(insert(new Force(v, env, fs)));
        break;
    }

    case Opcode::ldvar_noforce_: {
        if (bc.immediateConst() == symbol::c)
            compiler.seenC = true;
        v = insert(new LdVar(bc.immediateConst(), env));

        push(v);
        break;
    }

    case Opcode::stvar_:
    case Opcode::stvar_cached_:
        if (bc.immediateConst() == symbol::c)
            compiler.seenC = true;
        forceIfPromised(0);
        v = pop();
        if (auto mk = MkCls::Cast(v)) {
            if (localFuns.count(bc.immediateConst()))
                localFuns.at(bc.immediateConst()) = nullptr;
            else
                localFuns[bc.immediateConst()] = mk;
        }
        insert(new StVar(bc.immediateConst(), v, env));
        break;

    case Opcode::ldvar_super_:
        if (bc.immediateConst() == symbol::c)
            compiler.seenC = true;
        push(insert(new LdVarSuper(bc.immediateConst(), env)));
        break;

    case Opcode::stvar_super_:
        if (bc.immediateConst() == symbol::c)
            compiler.seenC = true;
        forceIfPromised(0);
        v = pop();
        insert(new StVarSuper(bc.immediateConst(), v, env));
        break;

    case Opcode::asbool_:
        push(insert(new CheckTrueFalse(pop())));
        break;

    case Opcode::aslogical_:
        push(insert(new AsLogical(pop(), srcIdx)));
        break;

    case Opcode::colon_input_effects_:
        push(insert(new ColonInputEffects(at(1), at(0), srcIdx)));
        break;

    case Opcode::colon_cast_lhs_:
        push(insert(new ColonCastLhs(pop(), srcIdx)));
        break;

    case Opcode::colon_cast_rhs_:
        push(insert(new ColonCastRhs(at(1), pop(), srcIdx)));
        break;

    case Opcode::ldfun_: {
        // Speculative inlining is too important, so let's ensure we have a cp
        // here.
        addCheckpoint(srcCode, pos, stack, insert);
        auto ld = insert(new LdFun(bc.immediateConst(), env));
        // Add early checkpoint for efficient speculative inlining. The goal is
        // to be able do move the ldfun into the deoptbranch later.
        if (auto cp = addCheckpoint(srcCode, pos, stack, insert))
            callTargetCheckpoints[ld] = cp;
        push(ld);
        break;
    }

    case Opcode::guard_fun_:
        log.unsupportedBC("Guard ignored", bc);
        break;

    case Opcode::swap_:
        x = pop();
        y = pop();
        push(x);
        push(y);
        break;

    case Opcode::dup_:
        push(top());
        break;

    case Opcode::dup2_:
        push(at(1));
        push(at(1));
        break;

    case Opcode::nop_:
        break;

    case Opcode::pop_:
        pop();
        break;

    case Opcode::popn_: {
        for (int i = bc.immediate.i; i > 0; --i)
            pop();
        break;
    }

    case Opcode::record_test_: {
        uint32_t idx = bc.immediate.i;
        auto& feedback = typeFeedback->test(idx);

        if (feedback.seen == ObservedTest::OnlyTrue ||
            feedback.seen == ObservedTest::OnlyFalse) {
            if (auto i = Instruction::Cast(at(0))) {
                auto v = feedback.seen == ObservedTest::OnlyTrue
                             ? (Value*)True::instance()
                             : (Value*)False::instance();
                if (!i->typeFeedback(false).value) {
                    auto& t = i->updateTypeFeedback(false);
                    if (TRANSFER_FEEDBACK) {
                        t.value = v;
                    }
                    t.feedbackOrigin = FeedbackOrigin(srcCode->function(),
                                                      FeedbackIndex::test(idx));
                } else if (i->typeFeedback(false).value != v) {
                    assert(false && "aa");
                    i->updateTypeFeedback().value = nullptr;
                }
            }
        } else if (feedback.seen == ObservedTest::None) {
            // To communicate to the backend that feedback is missing that
            // should still be collected.
            if (auto i = Instruction::Cast(at(0)))
                i->updateTypeFeedback(false);
        }
        break;
    }

    case Opcode::record_type_: {
        uint32_t idx = bc.immediate.i;
        auto& feedback = typeFeedback->types(idx);

        if (auto i = Instruction::Cast(at(0))) {
            // Search for the most specific feedback for this location
            for (auto fb : outerFeedback) {
                bool found = false;
                // TODO: implement with a find method on register map
                fb->forEachSlot(
                    [&](size_t i, const PirTypeFeedback::MDEntry& mdEntry) {
                        found = true;
                        auto origin = fb->rirIdx(i);
                        if (origin == FeedbackIndex::type(idx) &&
                            mdEntry.readyForReopt) {
                            feedback = mdEntry.feedback;
                        }
                    });
                if (found)
                    break;
            }
            // TODO: deal with multiple locations
            auto& t = i->updateTypeFeedback(false);
            t.feedbackOrigin =
                FeedbackOrigin(srcCode->function(), FeedbackIndex::type(idx));
            if (feedback.numTypes) {

                if (TRANSFER_FEEDBACK) {
                    t.type.merge(feedback);
                    if (auto force = Force::Cast(i)) {
                        force->observed = static_cast<Force::ArgumentKind>(
                            feedback.stateBeforeLastForce);

                        if (t.type.maybeLazy()) {
                            std::cerr << "maybelazy: \n\n";
                            force->print(std::cerr, true);
                            std::cerr << "\n";
                            std::cerr << t.type << "\n";

                            assert(false && "maybe lazy");
                        }
                    }
                }
            } else if (t.type.isVoid() &&
                       (!insert.function->optFunction->isOptimized() ||
                        insert.function->optFunction->deoptCount() == 0)) {

                if (DEFAULT_SPECULATION) {
                    t.type = PirType::val().notObject().fastVecelt();
                    t.defaultFeedback = true;
                }
            }
        }
        break;
    }

    case Opcode::record_call_: {
        uint32_t idx = bc.immediate.i;
        Value* target = top();

        auto& feedback = typeFeedback->callees(bc.immediate.i);

        // If this call was never executed we might as well compile an
        // unconditional deopt.
        if (TRANSFER_FEEDBACK && !inPromise() && !inlining() &&
            feedback.taken == 0 &&
            insert.function->optFunction->invocationCount() > 1 &&
            srcCode->function()->deadCallReached() < 3) {
            auto sp =
                insert.registerFrameState(srcCode, pos, stack, inPromise());

            DeoptReason reason = DeoptReason(
                FeedbackOrigin(srcCode->function(), FeedbackIndex::call(idx)),
                DeoptReason::DeadCall);

            auto d = insert(new Deopt(sp));
            d->setDeoptReason(compiler.module->deoptReasonValue(reason),
                              target);

            // FEEDBACKINFO
            // d->deadCall = true;
            // d->deadCallOrigin = srcCode->function();

            // auto fo =
            //     FeedbackOrigin(srcCode->function(),
            //     FeedbackIndex::call(idx));

            // fo.function()->slotsRead.insert(fo.index());
            // fo.function()->slotsUsed.insert(fo.index());
            // assert(feedback.isEmpty() && "non empty slot in branch!");

            stack.clear();
        } else if (auto i = Instruction::Cast(target)) {
            // See if the call feedback suggests a monomorphic target
            // TODO: Deopts in promises are not supported by the promise
            // inliner. So currently it does not pay off to put any deopts
            // in there.
            //
            auto& f = i->updateCallFeedback(false);
            f.feedbackOrigin =
                FeedbackOrigin(srcCode->function(), FeedbackIndex::call(idx));

            if (TRANSFER_FEEDBACK) {

                f.taken = feedback.taken;
                if (feedback.numTargets == 1) {
                    assert(!feedback.invalid &&
                           "feedback can't be invalid if numTargets is 1");
                    f.monomorphic = feedback.getTarget(srcCode->function(), 0);
                    f.type = TYPEOF(f.monomorphic);
                    f.stableEnv = true;
                } else if (feedback.numTargets > 1) {
                    SEXP first = nullptr;
                    bool stableType = !feedback.invalid;
                    bool stableBody = !feedback.invalid;
                    bool stableEnv = !feedback.invalid;
                    for (size_t i = 0; i < feedback.numTargets; ++i) {
                        SEXP b = feedback.getTarget(srcCode->function(), i);
                        if (!first) {
                            first = b;
                        } else {
                            if (TYPEOF(b) != TYPEOF(first))
                                stableType = stableBody = stableEnv = false;
                            else if (TYPEOF(b) == CLOSXP) {
                                if (BODY(first) != BODY(b))
                                    stableBody = false;
                                if (CLOENV(first) != CLOENV(b))
                                    stableEnv = false;
                            } else {
                                stableBody = stableEnv = false;
                            }
                        }
                    }

                    if (auto c = cls->isContinuation()) {
                        if (auto d = c->continuationContext->asDeoptContext()) {
                            if (d->reason().reason == DeoptReason::CallTarget) {
                                if (d->reason().origin.idx() == idx) {
                                    auto deoptCallTarget =
                                        d->callTargetTrigger();
                                    for (size_t i = 0; i < feedback.numTargets;
                                         ++i) {
                                        SEXP b = feedback.getTarget(
                                            srcCode->function(), i);
                                        if (b != deoptCallTarget)
                                            deoptedCallTargets.insert(b);
                                    }
                                    if (feedback.numTargets == 2) {
                                        assert(
                                            !feedback.invalid &&
                                            "Feedback should not be invalid");
                                        first = deoptCallTarget;
                                        stableBody = stableEnv = stableType =
                                            true;
                                        if (TYPEOF(deoptCallTarget) == CLOSXP &&
                                            !isValidClosureSEXP(
                                                deoptCallTarget))
                                            rir::Compiler::compileClosure(
                                                deoptCallTarget);
                                        deoptedCallReplacement =
                                            deoptCallTarget;
                                    }
                                }
                            }
                        }
                    }

                    if (stableType)
                        f.type = TYPEOF(first);
                    if (stableBody)
                        f.monomorphic = first;
                    if (stableEnv)
                        f.stableEnv = true;
                }
            }
        }
        break;
    }

    case Opcode::mk_eager_promise_:
    case Opcode::mk_promise_: {
        unsigned promi = bc.immediate.i;
        rir::Code* promiseCode = srcCode->getPromise(promi);
        Value* val = UnboundValue::instance();
        if (bc.bc == Opcode::mk_eager_promise_)
            val = pop();
        Promise* prom = insert.function->createProm(promiseCode);
        {
            Builder promiseBuilder(insert.function, prom);
            if (!tryCompilePromise(promiseCode, promiseBuilder)) {
                log.warn("Failed to compile a promise");
                return false;
            }
        }
        if (val == UnboundValue::instance() && Query::pureExceptDeopt(prom)) {
            val = tryInlinePromise(promiseCode, insert);
            if (!val) {
                log.warn("Failed to inline a promise");
                return false;
            }
            cls->promiseInlined(prom);
        }
        push(insert(new MkArg(prom, val, env)));
        break;
    }

    case Opcode::call_dots_:
    case Opcode::named_call_:
    case Opcode::call_: {
        long nargs = bc.immediate.callFixedArgs.nargs;
        auto toPop = nargs + 1;
        std::vector<Value*> args(nargs);
        for (long i = 0; i < nargs; ++i)
            args[nargs - i - 1] = at(i);

        std::vector<BC::PoolIdx> callArgumentNames;
        bool namedArguments = false;
        if (bc.bc == Opcode::named_call_) {
            callArgumentNames = bc.callExtra().callArgumentNames;
            namedArguments = true;
        } else if (bc.bc == Opcode::call_dots_) {
            for (auto n : bc.callExtra().callArgumentNames) {
                // The dots symbol as a name is used as a marker symbol for
                // call_dots_ and is not an actual name of the argument.
                auto name = Pool::get(n);
                if (name == R_DotsSymbol) {
                    callArgumentNames.push_back(Pool::insert(R_NilValue));
                } else {
                    if (name != R_NilValue)
                        namedArguments = true;
                    callArgumentNames.push_back(n);
                }
            }
        }

        auto callee = at(nargs);

        while (auto phi = Phi::Cast(callee)) {
            if (phi->nargs() == 1)
                callee = phi->arg(0).val();
            else
                break;
        }
        const auto dummyCallFeedback = CallFeedback();
        auto ti = Instruction::Cast(callee)
                      ? Instruction::Cast(callee)->callFeedback()
                      : dummyCallFeedback;
        if (ti.monomorphic && deoptedCallTargets.count(ti.monomorphic)) {
            if (deoptedCallReplacement)
                ti.monomorphic = deoptedCallReplacement;
            else
                ti.monomorphic = nullptr;
        }

        auto ldfun = LdFun::Cast(callee);
        if (ldfun) {
            if (ti.monomorphic) {
                ldfun->hint(ti.monomorphic, ti.feedbackOrigin);
                if (ti.stableEnv)
                    ldfun->hintHasStableEnv = true;
            } else {
                ldfun->hint(symbol::ambiguousCallTarget, {});
            }
        }

        bool monomorphicClosure =
            ti.monomorphic && isValidClosureSEXP(ti.monomorphic);
        bool stableEnv = ti.stableEnv;
        if (monomorphicClosure)
            if (auto dt = DispatchTable::check(BODY(ti.monomorphic)))
                if (dt->baseline()->flags.includes(
                        Function::Flag::InnerFunction))
                    stableEnv = false;

        bool monomorphicBuiltin = ti.monomorphic &&
                                  TYPEOF(ti.monomorphic) == BUILTINSXP &&
                                  // TODO implement support for call_builtin_
                                  // with names
                                  bc.bc == Opcode::call_;
        SEXP staticCallee = nullptr;
        if (auto ld = Const::Cast(callee))
            staticCallee = ld->c();
        bool staticMonomorphicBuiltin = staticCallee &&
                                        TYPEOF(staticCallee) == BUILTINSXP &&
                                        // TODO implement support for
                                        // call_builtin_ with names
                                        bc.bc == Opcode::call_;

        auto checkArity = [&](SEXP builtin) {
            int arity = getBuiltinArity(builtin);
            if (arity != -1 && arity != nargs)
                return false;
            return true;
        };
        if (monomorphicBuiltin)
            monomorphicBuiltin = checkArity(ti.monomorphic);
        if (staticMonomorphicBuiltin)
            staticMonomorphicBuiltin = checkArity(staticCallee);

        const std::unordered_set<int> supportedSpecials = {blt("forceAndCall")};
        bool monomorphicSpecial =
            ti.monomorphic && TYPEOF(ti.monomorphic) == SPECIALSXP &&
            supportedSpecials.count(ti.monomorphic->u.primsxp.offset);
        if (monomorphicClosure) {
            auto dt = DispatchTable::unpack(BODY(ti.monomorphic));
            // Let's not re-translate already optimized functions if they are
            // huge.
            // TODO: this is more of a temporary measure. Long term we should
            // have static calls with lazily compiled PIR targtets, so we can
            // defer compilation to the point where we e.g. want to analyze or
            // inline the callee...
            if (dt->baseline()->body()->codeSize >
                Parameter::RECOMPILE_THRESHOLD) {
                auto cls = insert.function->owner();
                // exclude recursive calls
                if (!cls->hasOriginClosure() ||
                    ti.monomorphic != cls->rirClosure())
                    monomorphicClosure = false;
            }
        }

        auto guardedCallee = callee;
        auto ast = bc.immediate.callFixedArgs.ast;
        // Insert a guard if we want to speculate
        if (!staticMonomorphicBuiltin &&
            (monomorphicBuiltin || monomorphicClosure || monomorphicSpecial)) {
            // Can't speculate in promises
            if (inPromise()) {
                monomorphicBuiltin = monomorphicClosure = monomorphicSpecial =
                    false;
            } else {
                auto cp = callTargetCheckpoints.count(callee)
                              ? callTargetCheckpoints.at(callee)
                              : nullptr;
                if (!cp)
                    cp = addCheckpoint(srcCode, pos, stack, insert);
                if (cp) {
                    auto bb = cp->nextBB();
                    auto dummyPos = bb->begin();

                    Value* expection = nullptr;
                    if (ldfun && localFuns.count(ldfun->varName)) {
                        auto mk = localFuns.at(ldfun->varName);
                        if (mk && mk->originalBody->container() ==
                                      BODY(ti.monomorphic)) {
                            expection = mk;
                            // Even though we statically know the env, we must
                            // compile an Env::unclosed() closure here, since we
                            // cannot pass the pir env from the host function to
                            // the inner function -- currently it is not
                            // possible to refer to values outside the
                            // function...
                            stableEnv = false;
                        }
                    }
                    guardedCallee = BBTransform::insertCalleeGuard(
                        compiler, ti,
                        DeoptReason(ti.feedbackOrigin, DeoptReason::CallTarget),
                        callee, stableEnv || expection, expection, cp, bb,
                        dummyPos);
                }
            }
        }

        auto eagerEval = [&](Value*& arg, size_t i, bool promiseWrapped) {
            if (auto mk = MkArg::Cast(arg)) {
                if (mk->isEager()) {
                    arg = mk->eagerArg();
                } else {
                    auto original = arg;
                    arg = tryCreateArg(mk->prom()->rirSrc(), insert, true);
                    if (promiseWrapped)
                        arg = insert(new MkArg(mk->prom(), arg, mk->env()));
                    if (!arg) {
                        log.warn("Failed to compile a promise");
                        return false;
                    }
                    if (i != (size_t)-1 && at(nargs - 1 - i) == original) {
                        // Inlined argument evaluation might have side effects.
                        // Let's have a checkpoint here. This checkpoint needs
                        // to capture the so far evaluated promises.
                        stack.at(nargs - 1 - i) =
                            promiseWrapped
                                ? arg
                                : insert(new MkArg(mk->prom(), arg, mk->env()));
                        addCheckpoint(srcCode, pos, stack, insert);
                    }
                }
            }
            return true;
        };

        auto emitGenericCall = [&]() {
            popn(toPop);
            Value* fs = inlining() ? (Value*)Tombstone::framestate()
                                   : (Value*)insert.registerFrameState(
                                         srcCode, nextPos, stack, inPromise());
            Instruction* res;
            if (namedArguments) {
                res = insert(new NamedCall(env, guardedCallee, args,
                                           callArgumentNames, fs,
                                           bc.immediate.callFixedArgs.ast));
            } else {
                res = insert(new Call(env, guardedCallee, args, fs,
                                      bc.immediate.callFixedArgs.ast));
            }
            if (monomorphicSpecial)
                res->effects.set(Effect::DependsOnAssume);
            push(res);
        };

        if (monomorphicBuiltin || staticMonomorphicBuiltin) {
            for (size_t i = 0; i < args.size(); ++i)
                if (!eagerEval(args[i], i, false))
                    return false;

            if (auto calli = Instruction::Cast(callee)) {
                calli->typeFeedbackUsed = true;
                calli->updateTypeFeedback(false).setSpeculationPhase(
                    report::SpeculationPhase::ExternallySet);
            }
            popn(toPop);
            auto bt = insert(BuiltinCallFactory::New(
                env, staticMonomorphicBuiltin ? staticCallee : ti.monomorphic,
                args, ast));
            if (!staticMonomorphicBuiltin)
                bt->effects.set(Effect::DependsOnAssume);
            push(bt);
        } else if (monomorphicClosure) {
            // (1) Argument Matching
            //
            size_t missingArgs = 0;
            auto matchedArgs(args);
            ArglistOrder::CallArglistOrder argOrderOrig;
            // Static argument name matching
            // Currently we only match callsites with the correct number of
            // arguments passed. Thus, we set those given assumptions below.
            auto formals = RList(FORMALS(ti.monomorphic));
            size_t needed = 0;
            bool hasDotsFormals = false;
            for (auto a = formals.begin(); a != formals.end(); ++a) {
                needed++;
                hasDotsFormals =
                    hasDotsFormals || (a.hasTag() && a.tag() == R_DotsSymbol);
            }

            bool correctOrder = !namedArguments && !hasDotsFormals &&
                                bc.bc != Opcode::call_dots_;

            if (!correctOrder) {
                correctOrder = ArgumentMatcher::reorder(
                    [&](DotsList* d) { insert(d); }, FORMALS(ti.monomorphic),
                    {[&]() { return nargs; },
                     [&](size_t i) {
                         SLOWASSERT(i < args.size());
                         return args[i];
                     },
                     [&](size_t i) {
                         SLOWASSERT(!namedArguments ||
                                    i < callArgumentNames.size());
                         return namedArguments ? Pool::get(callArgumentNames[i])
                                               : R_NilValue;
                     }},
                    matchedArgs, argOrderOrig);
            }

            if (!correctOrder || needed < matchedArgs.size()) {
                emitGenericCall();
                break;
            }

            // Specialcase for calling usemethod, the first argument is eager.
            // This helps determine the object type of the caller.
            {
                auto dt = DispatchTable::unpack(BODY(ti.monomorphic));
                auto calleeAst = src_pool_at(dt->baseline()->body()->src);
                auto isUseMethod = CAR(calleeAst) == symbol::UseMethod &&
                                   TYPEOF(CADR(calleeAst)) == STRSXP &&
                                   CDDR(calleeAst) == R_NilValue;
                static std::unordered_set<SEXP> usemethodNoReflectionList({
                    Rf_install("seq"),
                    Rf_install("unique"),
                    Rf_install("as.list"),
                    Rf_install("rev"),
                    Rf_install("anyDuplicated"),
                });
                if (isUseMethod) {
                    auto usemethodName =
                        Rf_install(CHAR(STRING_ELT(CADR(calleeAst), 0)));
                    bool maybeReflection =
                        !usemethodNoReflectionList.count(usemethodName);
                    if (auto d = DotsList::Cast(matchedArgs[0])) {
                        if (d->nargs() > 0) {
                            if (eagerEval(d->arg(0).val(), 0,
                                          maybeReflection)) {
                                d->arg(0).type() = d->arg(0).val()->type;
                                // creation of dots list must come after eager
                                // evaluation of content...
                                auto clone = d->clone();
                                matchedArgs[0] = clone;
                                d->eraseAndRemove();
                                insert(clone);
                            } else {
                                return false;
                            }
                        }
                    } else {
                        if (!eagerEval(matchedArgs[0], -1, maybeReflection))
                            return false;
                    }
                }
            }

            // Special case for the super nasty match.arg(x) pattern where the
            // arguments being matched are read reflectively from the default
            // promises in the formals...
            static SEXP argmatchFun =
                Rf_findFun(Rf_install("match.arg"), R_BaseNamespace);
            if (ti.monomorphic == argmatchFun && matchedArgs.size() == 1) {
                if (auto mk = MkArg::Cast(matchedArgs[0])) {
                    auto varName = mk->prom()->rirSrc()->trivialExpr;
                    if (TYPEOF(varName) == SYMSXP) {
                        auto& formals = cls->owner()->formals();
                        auto f = std::find(formals.names().begin(),
                                           formals.names().end(), varName);
                        if (f != formals.names().end()) {
                            auto pos = f - formals.names().begin();
                            if (formals.hasDefaultArgs() &&
                                formals.defaultArgs()[pos] != R_NilValue) {
                                if (auto options = rir::Code::check(
                                        formals.defaultArgs()[pos])) {
                                    auto ast = src_pool_at(options->src);
                                    if (CAR(ast) == symbol::c) {
                                        bool allStrings = true;
                                        for (auto c : RList(CDR(ast))) {
                                            if (TYPEOF(c) != STRSXP)
                                                allStrings = false;
                                        }
                                        if (allStrings) {
                                            auto optionList =
                                                Rf_eval(ast, R_GlobalEnv);
                                            auto opt =
                                                compiler.module->c(optionList);
                                            matchedArgs.push_back(opt);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }

            missingArgs = needed - matchedArgs.size();

            // (2)
            // Emit Static Call
            std::string name = "";
            if (ldfun)
                name = CHAR(PRINTNAME(ldfun->varName));

            Context given;
            // Make some optimistic assumptions, they might be reset below...
            given.add(Assumption::NoExplicitlyMissingArgs);
            given.numMissing(missingArgs);
            given.add(Assumption::NotTooManyArguments);
            given.add(Assumption::CorrectOrderOfArguments);
            given.add(Assumption::StaticallyArgmatched);

            {
                size_t i = 0;
                for (const auto& arg : matchedArgs) {

                    if (auto j = Instruction::Cast(arg))
                        j->updateTypeAndEffects();
                    arg->callArgTypeToContext(given, i);
                    i++;
                }
            }

            auto apply = [&](ClosureVersion* f) {
                if (auto calli = Instruction::Cast(callee)) {
                    calli->typeFeedbackUsed = true;
                    calli->updateTypeFeedback(false).setSpeculationPhase(
                        report::SpeculationPhase::ExternallySet);
                }
                popn(toPop);
                assert(!inlining());
                auto fs = insert.registerFrameState(srcCode, nextPos, stack,
                                                    inPromise());
                auto cl = insert(
                    new StaticCall(insert.env, f, given, matchedArgs,
                                   std::move(argOrderOrig), fs, ast,
                                   f->owner()->closureEnv() == Env::notClosed()
                                       ? guardedCallee
                                       : Tombstone::closure()));
                cl->effects.set(Effect::DependsOnAssume);
                push(cl);

                auto innerc = MkCls::Cast(guardedCallee->followCastsAndForce());
                if (!innerc)
                    return;
                auto delayed = delayedCompilation.find(innerc);
                if (delayed == delayedCompilation.end())
                    return;
                delayed->first->setCls(f->owner());
                delayedCompilation.erase(delayed);
            };

            if (!stableEnv) {
                compiler.compileFunction(
                    DispatchTable::unpack(BODY(ti.monomorphic)), name,
                    FORMALS(ti.monomorphic),
                    Rf_getAttrib(ti.monomorphic, symbol::srcref), given, apply,
                    emitGenericCall, outerFeedback);
            } else {
                compiler.compileClosure(ti.monomorphic, name, given, false,
                                        apply, emitGenericCall, outerFeedback);
            }
        } else {
            emitGenericCall();
        }

        if (ti.taken != (size_t)-1 &&
            // the reason to take the baseline version is that we only
            // increment the taken type feedback while running baseline
            // FIXME: refactor
            insert.function->owner()->rirFunction()->invocationCount()) {
            if (auto c = CallInstruction::CastCall(top())) {
                // invocation count is already incremented before calling jit
                c->taken = (double)ti.taken / (double)(insert.function->owner()
                                                           ->rirFunction()
                                                           ->invocationCount() -
                                                       1);
            }
        }
        break;
    }

    case Opcode::call_builtin_: {
        unsigned n = bc.immediate.callBuiltinFixedArgs.nargs;
        auto ast = bc.immediate.callBuiltinFixedArgs.ast;
        SEXP target = rir::Pool::get(bc.immediate.callBuiltinFixedArgs.builtin);

        std::vector<Value*> args(n);
        for (size_t i = 0; i < n; ++i) {
            args[n - i - 1] = pop();
        }

        assert(TYPEOF(target) == BUILTINSXP);
        push(insert(BuiltinCallFactory::New(env, target, args, ast)));

        if (target->u.primsxp.offset == blt("stop")) {
            insert(new Unreachable());
            stack.clear();
        }
        break;
    }

    case Opcode::for_seq_size_: {
        push(insert(new ToForSeq(pop())));
        push(insert(new Length(top())));
        break;
    }

    case Opcode::length_:
        push(insert(new Length(pop())));
        break;

    case Opcode::extract1_1_: {
        forceIfPromised(1); // <- ensure forced captured in framestate
        addCheckpoint(srcCode, pos, stack, insert);
        Value* idx = pop();
        Value* vec = pop();
        push(insert(new Extract1_1D(vec, idx, env, srcIdx)));
        break;
    }

    case Opcode::extract2_1_: {
        forceIfPromised(1); // <- forced version are captured in framestate
        addCheckpoint(srcCode, pos, stack, insert);
        Value* idx = pop();
        Value* vec = pop();
        push(insert(new Extract2_1D(vec, idx, env, srcIdx)));
        break;
    }

    case Opcode::extract1_2_: {
        forceIfPromised(2);
        addCheckpoint(srcCode, pos, stack, insert);
        Value* idx2 = pop();
        Value* idx1 = pop();
        Value* vec = pop();
        push(insert(new Extract1_2D(vec, idx1, idx2, env, srcIdx)));
        break;
    }

    case Opcode::extract2_2_: {
        forceIfPromised(2);
        addCheckpoint(srcCode, pos, stack, insert);
        Value* idx2 = pop();
        Value* idx1 = pop();
        Value* vec = pop();
        push(insert(new Extract2_2D(vec, idx1, idx2, env, srcIdx)));
        break;
    }

    case Opcode::extract1_3_: {
        forceIfPromised(3);
        addCheckpoint(srcCode, pos, stack, insert);
        Value* idx3 = pop();
        Value* idx2 = pop();
        Value* idx1 = pop();
        Value* vec = pop();
        push(insert(new Extract1_3D(vec, idx1, idx2, idx3, env, srcIdx)));
        break;
    }

    case Opcode::subassign1_1_: {
        forceIfPromised(1);
        addCheckpoint(srcCode, pos, stack, insert);
        Value* idx = pop();
        Value* vec = pop();
        Value* val = pop();
        push(insert(new Subassign1_1D(val, vec, idx, env, srcIdx)));
        break;
    }

    case Opcode::set_vec_elt_: {
        forceIfPromised(1);
        addCheckpoint(srcCode, pos, stack, insert);
        Value* idx = pop();
        Value* vec = pop();
        Value* val = pop();
        push(insert(new SetVecElt(val, vec, idx)));
        break;
    }

    case Opcode::subassign2_1_: {
        forceIfPromised(1);
        addCheckpoint(srcCode, pos, stack, insert);
        Value* idx = pop();
        Value* vec = pop();
        Value* val = pop();
        push(insert(new Subassign2_1D(val, vec, idx, env, srcIdx)));
        break;
    }

    case Opcode::subassign1_2_: {
        forceIfPromised(2);
        addCheckpoint(srcCode, pos, stack, insert);
        Value* idx2 = pop();
        Value* idx1 = pop();
        Value* vec = pop();
        Value* val = pop();
        push(insert(new Subassign1_2D(val, vec, idx1, idx2, env, srcIdx)));
        break;
    }

    case Opcode::subassign2_2_: {
        forceIfPromised(2);
        addCheckpoint(srcCode, pos, stack, insert);
        Value* idx2 = pop();
        Value* idx1 = pop();
        Value* vec = pop();
        Value* val = pop();
        push(insert(new Subassign2_2D(val, vec, idx1, idx2, env, srcIdx)));
        break;
    }

    case Opcode::subassign1_3_: {
        forceIfPromised(3);
        addCheckpoint(srcCode, pos, stack, insert);
        Value* idx3 = pop();
        Value* idx2 = pop();
        Value* idx1 = pop();
        Value* vec = pop();
        Value* val = pop();
        push(
            insert(new Subassign1_3D(val, vec, idx1, idx2, idx3, env, srcIdx)));
        break;
    }

#define BINOP_NOENV(Name, Op)                                                  \
    case Opcode::Op: {                                                         \
        auto rhs = pop();                                                      \
        auto lhs = pop();                                                      \
        push(insert(new Name(lhs, rhs)));                                      \
        break;                                                                 \
    }
        BINOP_NOENV(LOr, lgl_or_);
        BINOP_NOENV(LAnd, lgl_and_);
#undef BINOP_NOENV

        // Explicit force below to ensure that framestate contains the forced
        // version
        // Forcing of both args is ok here, even if lhs is an object, because
        // binop dispatch in R always forces both arguments before deciding on
        // a dispatch strategy.

#define BINOP(Name, Op)                                                        \
    case Opcode::Op: {                                                         \
        forceIfPromised(1);                                                    \
        forceIfPromised(0);                                                    \
        addCheckpoint(srcCode, pos, stack, insert);                            \
        auto lhs = at(1);                                                      \
        auto rhs = at(0);                                                      \
        pop();                                                                 \
        pop();                                                                 \
        push(insert(new Name(lhs, rhs, env, srcIdx)));                         \
        break;                                                                 \
    }

        BINOP(Lt, lt_);
        BINOP(Gt, gt_);
        BINOP(Gte, ge_);
        BINOP(Lte, le_);
        BINOP(Mod, mod_);
        BINOP(Div, div_);
        BINOP(IDiv, idiv_);
        BINOP(Add, add_);
        BINOP(Mul, mul_);
        BINOP(Colon, colon_);
        BINOP(Pow, pow_);
        BINOP(Sub, sub_);
        BINOP(Eq, eq_);
        BINOP(Neq, ne_);
#undef BINOP

    case Opcode::identical_noforce_: {
        auto rhs = pop();
        auto lhs = pop();
        push(insert(new Identical(lhs, rhs, PirType::any())));
        break;
    }

#define UNOP(Name, Op)                                                         \
    case Opcode::Op: {                                                         \
        v = pop();                                                             \
        push(insert(new Name(v, env, srcIdx)));                                \
        break;                                                                 \
    }
        UNOP(Plus, uplus_);
        UNOP(Minus, uminus_);
        UNOP(Not, not_);
#undef UNOP

#define UNOP_NOENV(Name, Op)                                                   \
    case Opcode::Op: {                                                         \
        push(insert(new Name(pop())));                                         \
        break;                                                                 \
    }
        UNOP_NOENV(Inc, inc_);
#undef UNOP_NOENV

    case Opcode::missing_:
        push(insert(new Missing(Pool::get(bc.immediate.pool), env)));
        break;

    case Opcode::as_switch_idx_:
        push(insert(new AsSwitchIdx(pop())));
        break;

    case Opcode::is_:
        if (bc.immediate.typecheck == BC::RirTypecheck::isNonObject) {
            push(insert(new IsType(PirType::val().notMissing().notObject(),
                                   pop(), FeedbackOrigin())));
        } else if (bc.immediate.typecheck == BC::RirTypecheck::isVector) {
            std::vector<Instruction*> tests;
            for (auto type : BC::isVectorTypes) {
                tests.push_back(insert(new Is(type, top())));
            }
            pop();
            auto res = tests.back();
            tests.pop_back();
            while (!tests.empty()) {
                res = insert(new LOr(res, tests.back()));
                tests.pop_back();
            }
            push(res);
        } else {
            push(insert(new Is(bc.immediate.typecheck, pop())));
        }
        break;

    case Opcode::pull_: {
        size_t i = bc.immediate.i;
        push(at(i));
        break;
    }

    case Opcode::pick_: {
        x = at(bc.immediate.i);
        for (int i = bc.immediate.i; i > 0; --i)
            set(i, at(i - 1));
        set(0, x);
        break;
    }

    case Opcode::put_: {
        x = top();
        for (size_t i = 0; i < bc.immediate.i; ++i)
            set(i, at(i + 1));
        set(bc.immediate.i, x);
        break;
    }

    case Opcode::ensure_named_:
    case Opcode::set_shared_:
        // Recomputed automatically in the backend
        break;

    case Opcode::invisible_:
        insert(new Invisible());
        break;

    case Opcode::visible_:
        insert(new Visible());
        break;

    case Opcode::force_: {
        auto v = pop();
        auto fs = inlining() ? (Value*)Tombstone::framestate()
                             : insert.registerFrameState(srcCode, nextPos,
                                                         stack, inPromise());
        push(insert(new Force(v, env, fs)));
        break;
    }

    case Opcode::names_:
        push(insert(new Names(pop())));
        break;

    case Opcode::set_names_: {
        Value* names = pop();
        Value* vec = pop();
        push(insert(new SetNames(vec, names)));
        break;
    }

    case Opcode::check_function_:
        push(insert(new ChkFunction(pop())));
        break;

#define V(_, name, Name)                                                       \
    case Opcode::name##_:                                                      \
        insert(new Name());                                                    \
        break;
        SIMPLE_INSTRUCTIONS(V, _)
#undef V

    // Silently ignored
    case Opcode::clear_binding_cache_:
        break;

    // Invalid opcodes:
    case Opcode::invalid_:
    case Opcode::num_of:

    // Opcodes handled elsewhere
    case Opcode::brtrue_:
    case Opcode::brfalse_:
    case Opcode::br_:
    case Opcode::close_:
    case Opcode::ret_:
    case Opcode::return_:
        assert(false);

    // Unsupported opcodes:
    case Opcode::beginloop_:
    case Opcode::endloop_:
    case Opcode::ldddvar_:
        log.unsupportedBC("Unsupported BC", bc);
        return false;
    }

    return true;
}

bool Rir2Pir::tryCompileContinuation(Builder& insert, Opcode* start,
                                     const std::vector<PirType>& initialStack) {
    return tryCompile(cls->owner()->rirFunction()->body(), insert, start,
                      initialStack);
}

bool Rir2Pir::tryCompile(Builder& insert) {
    return tryCompile(cls->owner()->rirFunction()->body(), insert);
}

bool Rir2Pir::tryCompile(rir::Code* srcCode, Builder& insert) {
    return tryCompile(srcCode, insert, srcCode->code(), {});
}

bool Rir2Pir::tryCompile(rir::Code* srcCode, Builder& insert, Opcode* start,
                         const std::vector<PirType>& initialStack) {
    if (auto mk = MkEnv::Cast(insert.env)) {
        mk->eachLocalVar([&](SEXP name, Value*, bool) {
            if (name == symbol::c)
                compiler.seenC = true;
        });
    }
    if (auto res = tryTranslate(srcCode, insert, start, initialStack)) {
        finalize(res, insert);
        return true;
    }
    return false;
}

bool Rir2Pir::tryCompilePromise(rir::Code* prom, Builder& insert) {
    return PromiseRir2Pir(compiler, cls, log, name, outerFeedback, typeFeedback,
                          false)
        .tryCompile(prom, insert);
}

Value* Rir2Pir::tryInlinePromise(rir::Code* srcCode, Builder& insert) {
    return PromiseRir2Pir(compiler, cls, log, name, outerFeedback, typeFeedback,
                          true)
        .tryTranslate(srcCode, insert);
}

Value* Rir2Pir::tryTranslate(rir::Code* srcCode, Builder& insert) {
    return tryTranslate(srcCode, insert, srcCode->code(), {});
}

Value* Rir2Pir::tryTranslate(rir::Code* srcCode, Builder& insert, Opcode* start,
                             const std::vector<PirType>& initialStack) {
    assert(!finalized);

    auto firstBB = insert.getCurrentBB();
    insert.createNextBB();
    CallTargetCheckpoints callTargetCheckpoints;
    std::vector<ReturnSite> results;

    std::unordered_map<Opcode*, State> mergepoints;
    for (auto p : findMergepoints(srcCode))
        mergepoints.emplace(p, State());

    std::deque<State> worklist;
    State cur;
    {
        size_t num = 0;
        for (auto t : initialStack) {
            auto n = insert(new LdArg(num++));
            n->type = t;
            cur.stack.push(n);
        }
    }
    cur.seen = true;

    Opcode* end = srcCode->endCode();
    Opcode* finger = start;

    auto popWorklist = [&]() {
        assert(!worklist.empty());
        cur = std::move(worklist.back());
        worklist.pop_back();
        insert.enterBB(cur.entryBB);
        return cur.entryPC;
    };
    auto pushWorklist = [&](BB* bb, Opcode* pos) {
        assert(pos != end);
        worklist.push_back(State(cur, false, bb, pos));
    };

    bool anyReflective = false;
    for (size_t i = 0; i < cls->nargs(); ++i)
        if (!cls->context().isNonRefl(i) && !cls->context().isEager(i))
            anyReflective = true;
    // If there are args that might be reflective it is helpful to have a
    // checkpoint before forcing the first arg. Otherwise it is typically just
    // inhibiting.
    if (cls->rirSrc() == srcCode && anyReflective && start == srcCode->code()) {
        addCheckpoint(srcCode, finger, cur.stack, insert);
    }

    while (finger != end || !worklist.empty()) {
        if (finger == end)
            finger = popWorklist();
        assert(finger != end);

        if (mergepoints.count(finger)) {
            State& other = mergepoints.at(finger);
            if (other.seen) {
                other.mergeIn(cur, insert.getCurrentBB());
                cur.clear();
                if (worklist.empty())
                    break;
                finger = popWorklist();
                continue;
            }
            cur.createMergepoint(insert);
            other = State(cur, true, insert.getCurrentBB(), finger);
            addCheckpoint(srcCode, finger, cur.stack, insert);
        }
        const auto pos = finger;
        BC bc = BC::advance(&finger, srcCode);
        // cppcheck-suppress variableScope
        const auto nextPos = finger;

        assert(pos != end);
        if (bc.isJmp()) {
            auto trg = bc.jmpTarget(pos);
            if (bc.isUncondJmp()) {
                finger = trg;
                continue;
            }

            if (bc.bc == Opcode::beginloop_) {
                log.warn("Cannot compile Function. Unsupported beginloop bc");
                return nullptr;
            }

            // Conditional jump
            assert(bc.isCondJmp());
            auto branchCondition = cur.stack.top();
            auto branchReason = bc.bc == Opcode::brtrue_
                                    ? (Value*)True::instance()
                                    : (Value*)False::instance();
            auto asBool = insert(
                new Identical(branchCondition, branchReason, PirType::val()));

            if (!inPromise()) {
                if (auto c = Instruction::Cast(branchCondition)) {
                    auto likely = c->typeFeedback().value;
                    if (likely == True::instance() ||
                        likely == False::instance()) {
                        if (auto cp = addCheckpoint(srcCode, pos, cur.stack,
                                                    insert)) {
                            bool expectBranch = likely == branchReason;
                            if (expectBranch)
                                finger = trg;

                            auto assume = new Assume(
                                asBool, cp,
                                DeoptReason(c->typeFeedback().feedbackOrigin,
                                            DeoptReason::DeadBranchReached),
                                expectBranch);
                            insert(assume);

                            // If we deopt on a typecheck, then we should record
                            // that information by casting the value.
                            // TODO: also do this for negative type checks
                            if (expectBranch)
                                if (auto tt = IsType::Cast(branchCondition)) {
                                    auto checkedValue = tt->arg<0>().val();
                                    auto checkedType = checkedValue->type;

                                    for (auto& e : cur.stack) {
                                        if (e == checkedValue) {
                                            if (!e->type.isA(checkedType)) {
                                                bool block = false;
                                                if (auto j =
                                                        Instruction::Cast(e)) {
                                                    // In case the typefeedback
                                                    // is more precise than the
                                                    if (!j->typeFeedback()
                                                             .type.isVoid() &&
                                                        !checkedType.isA(
                                                            j->typeFeedback()
                                                                .type))
                                                        block = true;
                                                }
                                                if (!block) {
                                                    auto cast =
                                                        insert(new CastType(
                                                            e,
                                                            CastType::Downcast,
                                                            PirType::any(),
                                                            checkedType));
                                                    cast->effects.set(
                                                        Effect::
                                                            DependsOnAssume);
                                                    e = cast;
                                                }
                                            }
                                        }
                                    }
                                }
                            cur.stack.pop();
                            continue;
                        }
                    }
                }
            }

            cur.stack.pop();
            insert(new Branch(asBool));

            BB* branch = insert.createBB();
            BB* fall = insert.createBB();

            insert.setBranch(branch, fall);

            pushWorklist(branch, trg);
            insert.enterBB(fall);
            continue;
        }

        if (bc.isExit()) {
            Value* tos;
            bool localReturn = true;
            switch (bc.bc) {
            case Opcode::ret_:
                tos = cur.stack.pop();
                break;
            case Opcode::return_:
                // Return bytecode as top-level statement cannot cause non-local
                // return. Therefore we can treat it as normal local return
                // instruction. We just need to make sure to empty the stack.
                tos = cur.stack.pop();
                if (inPromise()) {
                    insert(new NonLocalReturn(tos, insert.env));
                    localReturn = false;
                }
                cur.stack.clear();
                break;
            default:
                assert(false);
            }
            assert(cur.stack.empty());
            if (localReturn)
                results.push_back(ReturnSite(insert.getCurrentBB(), tos));
            // Setting the position to end, will either terminate the loop, or
            // pop from the worklist
            finger = end;
            continue;
        }

        assert(pos != end);
        const static Matcher<4> ifFunctionLiteral(
            {{{Opcode::push_, Opcode::push_, Opcode::push_, Opcode::close_}}});

        bool skip = false;

        ifFunctionLiteral(pos, end, [&](Opcode* next) {
            Opcode* pc = pos;
            BC ldfmls = BC::advance(&pc, srcCode);
            BC ldcode = BC::advance(&pc, srcCode);
            BC ldsrc = BC::advance(&pc, srcCode);
            pc = BC::next(pc); // close

            SEXP formals = ldfmls.immediateConst();
            SEXP code = ldcode.immediateConst();
            SEXP srcRef = ldsrc.immediateConst();

            DispatchTable* dt = DispatchTable::unpack(code);

            std::stringstream inner;
            inner << name;
            // Try to find the name of this inner function by peeking for the
            // stvar
            {
                auto n = pc;
                for (int i = 0; i < 2 && n < end; ++i, n = BC::next(n)) {
                    auto nextbc = BC::decodeShallow(n);
                    if (nextbc.bc == Opcode::stvar_ ||
                        nextbc.bc == Opcode::stvar_cached_) {
                        inner << ">"
                              << CHAR(PRINTNAME(nextbc.immediateConst()));
                        break;
                    }
                }
            }
            inner << "@";
            if (srcCode != cls->owner()->rirFunction()->body()) {
                size_t i = 0;
                for (auto c : insert.function->promises()) {
                    if (c == insert.code) {
                        inner << "Prom(" << i << ")";
                        break;
                    }
                    i++;
                }
            }
            inner << (pos - srcCode->code());

            auto mk = new MkCls(nullptr, formals, srcRef, dt, insert.env);
            firstBB->append(mk);
            cur.stack.push(mk);

            delayedCompilation[mk] = {dt,      inner.str(),
                                      formals, srcRef,
                                      false,   Compiler::defaultContext};

            finger = pc;
            skip = true;
        });

        if (!skip) {
            auto oldStack = cur.stack;
            if (!compileBC(bc, pos, nextPos, srcCode, cur.stack, insert,
                           callTargetCheckpoints)) {
                log.failed("Abort r2p due to unsupported bc");
                return nullptr;
            }

            if (!insert.getCurrentBB()->isEmpty()) {
                auto last = insert.getCurrentBB()->last();

                if (Deopt::Cast(last) || Unreachable::Cast(last)) {
                    finger = end;
                    continue;
                }

                // Here we iterate over the arguments to the last instruction
                // and insert forces where required. This is actually done later
                // by the insert_cast helper. However there we cannot create
                // checkpoints. Here we ensure that between every force
                // we will have a checkpoint with the updated forced value.
                if (!cur.stack.empty() && cur.stack.top() == last) {
                    insert.getCurrentBB()->eraseLast();
                    last->eachArg([&](InstrArg& arg) {
                        if (!arg.type().maybePromiseWrapped() &&
                            arg.val()->type.maybePromiseWrapped()) {
                            size_t idx = 0;
                            while (idx < oldStack.size() &&
                                   oldStack.at(idx) != arg.val())
                                idx++;
                            if (idx < oldStack.size()) {
                                arg.val() = oldStack.at(idx) =
                                    insert(new Force(arg.val(), insert.env,
                                                     Tombstone::framestate()));
                                addCheckpoint(srcCode, pos, oldStack, insert);
                            }
                        }
                    });
                    insert(last);
                }

                if (last->isDeoptBarrier() && finger != end)
                    addCheckpoint(srcCode, nextPos, cur.stack, insert);
            }

            if (cur.stack.size() !=
                oldStack.size() - bc.popCount() + bc.pushCount()) {
                srcCode->print(std::cerr);
                std::cerr << "After interpreting '";
                bc.print(std::cerr);
                std::cerr << "' which is supposed to pop " << bc.popCount()
                          << " and push " << bc.pushCount() << " we got from "
                          << oldStack.size() << " to " << cur.stack.size()
                          << "\n";
                assert(false);
                return nullptr;
            }
        }
    }
    assert(cur.stack.empty());

    Value* res;
    if (results.size() == 0) {
        res = Tombstone::unreachable();
        insert.clearCurrentBB();
    } else if (results.size() == 1) {
        res = results.back().second;
        insert.reenterBB(results.back().first);
    } else {
        BB* merge = insert.createBB();
        insert.enterBB(merge);
        auto phi = insert(new Phi);
        for (auto r : results) {
            r.first->setNext(merge);
            phi->addInput(r.first, r.second);
        }
        phi->updateTypeAndEffects();
        res = phi;
    }

    if (!inPromise()) {
        assert(cls == insert.code);

        // The return is only added for the early opt passes to update the
        // result value. Now we need to remove it again, because we don't know
        // if it is needed (e.g. when we compile an inline promise it is not).
        if (insert.getCurrentBB()) {
            insert(new Return(res));
        }

        log.forPass(0, "rir2pir").compilationEarlyPir();

        // EarlyConstantfold is used to expand specials such as forceAndCall
        // which can be expressed in PIR.
        {
            static EarlyConstantfold ecf;
            auto passLog = log.forPass(1, "earlyCF");
            ecf.apply(compiler, cls, cls, log, 0);
            passLog.pirOptimizations(&ecf);
        }

        // This early pass of scope resolution helps to find local call targets
        // and thus leads to better assumptions in the delayed compilation
        // below.
        {
            static ScopeResolution sr;
            auto passLog = log.forPass(2, "earlySR");
            sr.apply(compiler, cls, cls, log, 0);
            passLog.pirOptimizations(&sr);
        }

        if (auto last = insert.getCurrentBB()) {
            res = Return::Cast(last->last())->arg(0).val();
            last->remove(last->end() - 1);
        }
    }

    Visitor::run(insert.code->entry, [&](Instruction* i) {
        Value* callee = nullptr;
        Context asmpt;
        if (auto ci = Call::Cast(i)) {
            callee = ci->cls();
            asmpt = ci->inferAvailableAssumptions();
        } else if (auto ci = NamedCall::Cast(i)) {
            callee = ci->cls();
            asmpt = ci->inferAvailableAssumptions();
        }
        if (!callee)
            return;
        auto innerFCallee = MkCls::Cast(callee);
        auto f = delayedCompilation.find(innerFCallee);
        if (f == delayedCompilation.end())
            return;
        auto& d = f->second;
        if (d.seen) {
            d.context = d.context & asmpt;
        } else {
            d.seen = true;
            d.context = asmpt;
        }
    });

    for (auto& delayed : delayedCompilation) {
        auto d = delayed.second;
        compiler.compileFunction(
            d.dt, d.name, d.formals, d.srcRef,
            d.context | Compiler::minimalContext,
            [&](ClosureVersion* innerF) {
                delayed.first->setCls(innerF->owner());
            },
            [&]() { log.warn("Failed to compile inner function" + name); },
            outerFeedback);
    }
    results.clear();

    return res;
}

void Rir2Pir::finalize(Value* ret, Builder& insert) {
    assert(!finalized);
    assert(ret);

    bool changed = true;
    while (changed) {
        changed = false;
        // Remove excessive Phis
        Visitor::run(insert.code->entry, [&](BB* bb) {
            auto it = bb->begin();
            while (it != bb->end()) {
                Phi* p = Phi::Cast(*it);
                if (!p) {
                    it++;
                    continue;
                }
                if (p->nargs() == 1) {
                    if (p == ret)
                        ret = p->arg(0).val();
                    p->replaceUsesWith(p->arg(0).val());
                    it = bb->remove(it);
                    changed = true;
                    continue;
                }
                // Phi where all inputs are the same value (except the phi
                // itself), then we can remove it.
                Value* allTheSame = p->arg(0).val();
                p->eachArg([&](BB*, Value* v) {
                    if (allTheSame == p)
                        allTheSame = v;
                    else if (v != p && v != allTheSame)
                        allTheSame = nullptr;
                });
                if (allTheSame) {
                    p->replaceUsesWith(allTheSame);
                    if (ret == p)
                        ret = allTheSame;
                    it = bb->remove(it);
                    changed = true;
                    continue;
                }
                auto t = p->type;
                p->updateTypeAndEffects();
                if (t != p->type)
                    changed = true;
                it++;
            }
        });
    }

    if (insert.getCurrentBB()) {
        assert(insert.getCurrentBB()->isEmpty() ||
               !insert.getCurrentBB()->last()->exits());
        insert(new Return(ret));
    }

    InsertCast c(insert.code, insert.env, log);
    c();

    finalized = true;
}

} // namespace pir
} // namespace rir
