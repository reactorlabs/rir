#include "rir_2_pir.h"
#include "../../analysis/query.h"
#include "../../analysis/verifier.h"
#include "../../pir/pir_impl.h"
#include "../../transform/insert_cast.h"
#include "../../util/ConvertAssumptions.h"
#include "../../util/arg_match.h"
#include "../../util/builder.h"
#include "../../util/cfg.h"
#include "../../util/visitor.h"
#include "R/Funtab.h"
#include "R/RList.h"
#include "R/Symbols.h"
#include "ir/BC.h"
#include "ir/Compiler.h"
#include "simple_instruction_list.h"
#include "utils/FormalArgs.h"

#include <sstream>
#include <unordered_map>
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
        : seen(seen), entryBB(entryBB), entryPC(entryPC), stack(other.stack){};

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

Checkpoint* Rir2Pir::addCheckpoint(rir::Code* srcCode, Opcode* pos,
                                   const RirStack& stack,
                                   Builder& insert) const {
    // Checkpoints in promises are badly supported (cannot inline promises
    // anymore) and checkpoints in eagerly inlined promises are wrong. So for
    // now we do not emit them in promises!
    assert(!inPromise());
    return insert.emitCheckpoint(srcCode, pos, stack);
}

Value* Rir2Pir::tryCreateArg(rir::Code* promiseCode, Builder& insert,
                             bool eager) const {
    Promise* prom = insert.function->createProm(promiseCode);
    {
        Builder promiseBuilder(insert.function, prom);
        if (!tryCompilePromise(promiseCode, promiseBuilder)) {
            log.warn("Failed to compile a promise for call");
            return nullptr;
        }
    }

    Value* eagerVal = UnboundValue::instance();
    if (eager || Query::pure(prom)) {
        eagerVal = tryTranslatePromise(promiseCode, insert);
        if (!eagerVal) {
            log.warn("Failed to inline a promise");
            return nullptr;
        }
    }

    if (eager) {
        assert(eagerVal != UnboundValue::instance());
        return eagerVal;
    }

    return insert(new MkArg(prom, eagerVal, insert.env));
}

bool Rir2Pir::compileBC(const BC& bc, Opcode* pos, Opcode* nextPos,
                        rir::Code* srcCode, RirStack& stack, Builder& insert,
                        CallTargetFeedback& callTargetFeedback) const {
    Value* env = insert.env;

    unsigned srcIdx = srcCode->getSrcIdxAt(pos, true);

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
            stack.at(i) = insert(new Force(at(i), env));
        }
    };

    switch (bc.bc) {

    case Opcode::push_: {
        auto c = bc.immediateConst();
        if (c == R_UnboundValue)
            push(UnboundValue::instance());
        else if (c == R_MissingArg)
            push(MissingArg::instance());
        else
            push(insert(new LdConst(bc.immediateConst())));
        break;
    }

    case Opcode::ldvar_:
    case Opcode::ldvar_cached_:
    case Opcode::ldvar_for_update_:
    case Opcode::ldvar_for_update_cache_:
        v = insert(new LdVar(bc.immediateConst(), env));
        // Checkpoint might be useful if we end up inlining this force
        if (!inPromise())
            addCheckpoint(srcCode, pos, stack, insert);
        push(insert(new Force(v, env)));
        break;

    case Opcode::starg_:
    case Opcode::starg_cached_:
        v = pop();
        insert(new StArg(bc.immediateConst(), v, env));
        break;

    case Opcode::stvar_:
    case Opcode::stvar_cached_:
        v = pop();
        insert(new StVar(bc.immediateConst(), v, env));
        break;

    case Opcode::ldvar_super_:
        push(insert(new LdVarSuper(bc.immediateConst(), env)));
        break;

    case Opcode::stvar_super_:
        v = pop();
        insert(new StVarSuper(bc.immediateConst(), v, env));
        break;

    case Opcode::asbool_:
        push(insert(new AsTest(pop())));
        break;

    case Opcode::aslogical_:
        push(insert(new AsLogical(pop(), srcIdx)));
        break;

    case Opcode::ceil_: {
        push(insert(new AsInt(pop(), true)));
        break;
    }

    case Opcode::floor_: {
        push(insert(new AsInt(pop(), false)));
        break;
    }

    case Opcode::ldfun_: {
        auto ld = insert(new LdFun(bc.immediateConst(), env));
        if (!inPromise()) {
            auto cp = addCheckpoint(srcCode, pos, stack, insert);
            callTargetFeedback[ld].first = cp;
        }
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

    case Opcode::close_: {
        Value* srcref = pop();
        Value* body = pop();
        Value* formals = pop();
        push(insert(new MkCls(formals, body, srcref, env)));
        break;
    }

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

    case Opcode::record_type_: {
        if (bc.immediate.typeFeedback.numTypes)
            at(0)->typeFeedback.merge(bc.immediate.typeFeedback);
        break;
    }

    case Opcode::record_call_: {
        Value* target = top();
        callTargetFeedback[target].second = bc.immediate.callFeedback;
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
        if (val == UnboundValue::instance() && Query::pure(prom)) {
            val = tryTranslatePromise(promiseCode, insert);
            if (!val) {
                log.warn("Failed to compile a promise");
                return false;
            }
        }
        push(insert(new MkArg(prom, val, env)));
        break;
    }

    case Opcode::named_call_:
    case Opcode::call_: {
        long nargs = bc.immediate.callFixedArgs.nargs;
        auto toPop = nargs + 1;
        std::vector<Value*> args(nargs);
        for (long i = 0; i < nargs; ++i)
            args[nargs - i - 1] = at(i);

        SEXP monomorphic = nullptr;
        auto callee = at(nargs);

        // See if the call feedback suggests a monomorphic target
        // TODO: Deopts in promises are not supported by the promise inliner. So
        // currently it does not pay off to put any deopts in there.
        if (!inPromise() && callTargetFeedback.count(callee)) {
            auto& feedback = callTargetFeedback.at(callee).second;
            // If this call was never executed. Might as well compile an
            // unconditional deopt. We do this by converting the checkpoint at
            // the ldfun into a goto to the deopt branch.
            // (We cannot deopt later, ie. at the current position, because then
            // we would not record the new call target in the baseline and might
            // end up in a deopt-loop)
            if (feedback.taken == 0 && srcCode->funInvocationCount > 1) {
                if (auto cp = callTargetFeedback.at(callee).first) {
                    auto before = cp->bb();
                    auto deoptBB = cp->deoptBranch();
                    auto now = cp->nextBB();
                    assert(now == insert.getCurrentBB());

                    before->next0 = deoptBB;
                    before->next1 = nullptr;
                    before->remove(before->end() - 1);

                    delete now;

                    stack.clear();
                    insert.reenterBB(deoptBB);
                    break;
                }
            }

            if (feedback.taken > 1 && feedback.numTargets == 1) {
                monomorphic = feedback.getTarget(srcCode, 0);
            }
        }

        bool monomorphicClosure =
            monomorphic && isValidClosureSEXP(monomorphic);
        bool monomorphicBuiltin = monomorphic &&
                                  TYPEOF(monomorphic) == BUILTINSXP &&
                                  // TODO implement support for call_builtin_
                                  // with names
                                  bc.bc == Opcode::call_;

        if (monomorphicBuiltin) {
            int arity = getBuiltinArity(monomorphic);
            if (arity != -1 && arity != nargs)
                monomorphicBuiltin = false;
        }
        if (monomorphicClosure) {
            if (DispatchTable::unpack(BODY(monomorphic))
                    ->baseline()
                    ->unoptimizable)
                monomorphicClosure = false;
        }

        auto ldfun = LdFun::Cast(callee);
        if (inPromise()) {
            if (ldfun) {
                ldfun->hint =
                    monomorphic ? monomorphic : symbol::ambiguousCallTarget;
            }
            monomorphicBuiltin = monomorphicClosure = false;
            monomorphic = nullptr;
        }

        Assume* assumption = nullptr;
        // Insert a guard if we want to speculate
        if (monomorphicBuiltin || monomorphicClosure) {
            // We use ldvar instead of ldfun for the guard. The reason is that
            // ldfun can force promises, which is a pain for our optimizer to
            // deal with. If we use a ldvar here, the actual ldfun will be
            // delayed into the deopt branch. Note that ldvar is conservative.
            // If we find a non-function binding with the same name, we will
            // deopt unneccessarily. In the case of `c` this is guaranteed to
            // cause problems, since many variables are called "c". Therefore we
            // keep the ldfun in this case.
            // TODO: Implement this with a dependency on the binding cell
            // instead of an eager check.
            auto cp = callTargetFeedback.at(callee).first;
            if (!cp) {
                cp = addCheckpoint(srcCode, pos, stack, insert);
            }
            auto bb = cp->nextBB();
            auto pos = bb->begin();

            auto expected = new LdConst(monomorphic);
            pos = bb->insert(pos, expected);
            pos++;

            Value* given = callee;
            if (ldfun && ldfun->varName != symbol::c) {
                auto ldvar = new LdVar(ldfun->varName, ldfun->env());
                pos = bb->insert(pos, ldvar);
                pos++;
                given = ldvar;
            }

            auto t = new Identical(given, expected);
            pos = bb->insert(pos, t);
            pos++;

            assumption = new Assume(t, cp);
            bb->insert(pos, assumption);
        }

        if (monomorphicBuiltin) {
            for (size_t i = 0; i < args.size(); ++i) {
                if (auto mk = MkArg::Cast(args[i])) {
                    if (mk->isEager()) {
                        args[i] = mk->eagerArg();
                    } else {
                        assert(at(nargs - 1 - i) == args[i]);
                        args[i] =
                            tryCreateArg(mk->prom()->rirSrc(), insert, true);
                        if (!args[i]) {
                            log.warn("Failed to compile a promise");
                            return false;
                        }
                        // Inlined argument evaluation might have side effects.
                        // Let's have a checkpoint here. This checkpoint needs
                        // to capture the so far evaluated promises.
                        stack.at(nargs - 1 - i) =
                            insert(new MkArg(mk->prom(), args[i], mk->env()));
                        addCheckpoint(srcCode, pos, stack, insert);
                    }
                }
            }
        }

        size_t missingArgs = 0;
        auto matchedArgs(args);
        // Static argument name matching
        // Currently we only match callsites with the correct number of
        // arguments passed. Thus, we set those given assumptions below.
        if (monomorphicClosure) {
            bool correctOrder = bc.bc != Opcode::named_call_;

            if (!correctOrder) {
                correctOrder = ArgumentMatcher::reorder(
                    FORMALS(monomorphic), bc.callExtra().callArgumentNames,
                    matchedArgs);
            }

            size_t needed = RList(FORMALS(monomorphic)).length();

            if (!correctOrder || needed < matchedArgs.size()) {
                monomorphicClosure = false;
                assert(assumption);
                // Kill unnecessary speculation
                assumption->arg<0>().val() = True::instance();
            }

            missingArgs = needed - matchedArgs.size();
        }

        // Emit the actual call
        auto ast = bc.immediate.callFixedArgs.ast;
        auto insertGenericCall = [&]() {
            popn(toPop);
            if (bc.bc == Opcode::named_call_) {
                push(insert(new NamedCall(env, callee, args,
                                          bc.callExtra().callArgumentNames,
                                          bc.immediate.callFixedArgs.ast)));
            } else {
                Value* fs = nullptr;
                if (inPromise())
                    fs = Tombstone::framestate();
                else
                    fs = insert.registerFrameState(srcCode, nextPos, stack);
                push(insert(new Call(env, callee, args, fs,
                                     bc.immediate.callFixedArgs.ast)));
            }
        };

        if (monomorphicClosure) {
            std::string name = "";
            if (ldfun)
                name = CHAR(PRINTNAME(ldfun->varName));

            Assumptions given;
            // Make some optimistic assumptions, they might be reset below...
            given.add(Assumption::NoExplicitlyMissingArgs);
            given.numMissing(missingArgs);
            given.add(Assumption::NotTooManyArguments);
            given.add(Assumption::CorrectOrderOfArguments);

            {
                size_t i = 0;
                for (const auto& arg : matchedArgs) {
                    if (arg == MissingArg::instance()) {
                        given.remove(Assumption::NoExplicitlyMissingArgs);
                        i++;
                    } else {
                        writeArgTypeToAssumptions(given, arg, i++);
                    }
                }
            }

            compiler.compileClosure(
                monomorphic, name, given,
                [&](ClosureVersion* f) {
                    popn(toPop);
                    auto fs =
                        insert.registerFrameState(srcCode, nextPos, stack);
                    push(insert(new StaticCall(insert.env, f->owner(),
                                               matchedArgs, fs, ast)));
                },
                insertGenericCall);
        } else if (monomorphicBuiltin) {
            popn(toPop);
            push(insert(BuiltinCallFactory::New(env, monomorphic, args, ast)));
        } else {
            insertGenericCall();
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
        break;
    }

    case Opcode::seq_: {
        auto step = pop();
        auto stop = pop();
        auto start = pop();
        push(insert(new Seq(start, stop, step)));
        break;
    }

    case Opcode::for_seq_size_:
        push(insert(new ForSeqSize(top())));
        break;

    case Opcode::extract1_1_: {
        if (!inPromise()) {
            forceIfPromised(1); // <- ensure forced captured in framestate
            forceIfPromised(0);
            addCheckpoint(srcCode, pos, stack, insert);
        }
        Value* idx = pop();
        Value* vec = pop();
        push(insert(new Extract1_1D(vec, idx, env, srcIdx)));
        break;
    }

    case Opcode::extract2_1_: {
        if (!inPromise()) {
            forceIfPromised(
                1); // <- ensure forced version are captured in framestate
            forceIfPromised(0);
            addCheckpoint(srcCode, pos, stack, insert);
        }
        Value* idx = pop();
        Value* vec = pop();
        push(insert(new Extract2_1D(vec, idx, env, srcIdx)));
        break;
    }

    case Opcode::extract1_2_: {
        // TODO: checkpoint here is broken. What we should do here is insert a
        // checkpoint between every force, and then deopt between forcing. E.g.
        // if in a[b,c] b turns out to be an object, we need a deopt exit that
        // captures the forced a, forced b and unforced c. So we cannot have
        // deopt like now right in front of the Extract2_2D, but instead we need
        // 3 different deopts after every force.
        // For that we need to fix elide_env_spec to insert the assumes in the
        // right place (ie, after the force and not before the Extract)
        // insert.addCheckpoint(srcCode, pos, stack);
        Value* idx2 = pop();
        Value* idx1 = pop();
        Value* vec = pop();
        push(insert(new Extract1_2D(vec, idx1, idx2, env, srcIdx)));
        break;
    }

    case Opcode::extract2_2_: {
        // TODO: checkpoint here is broken. What we should do here is insert a
        // checkpoint between every force, and then deopt between forcing. E.g.
        // if in a[b,c] b turns out to be an object, we need a deopt exit that
        // captures the forced a, forced b and unforced c. So we cannot have
        // deopt like now right in front of the Extract2_2D, but instead we need
        // 3 different deopts after every force.
        // For that we need to fix elide_env_spec to insert the assumes in the
        // right place (ie, after the force and not before the Extract)
        // insert.addCheckpoint(srcCode, pos, stack);
        Value* idx2 = pop();
        Value* idx1 = pop();
        Value* vec = pop();
        push(insert(new Extract2_2D(vec, idx1, idx2, env, srcIdx)));
        break;
    }

    case Opcode::subassign1_1_: {
        Value* idx = pop();
        Value* vec = pop();
        Value* val = pop();
        push(insert(new Subassign1_1D(val, vec, idx, env, srcIdx)));
        break;
    }

    case Opcode::subassign2_1_: {
        Value* idx = pop();
        Value* vec = pop();
        Value* val = pop();
        push(insert(new Subassign2_1D(val, vec, idx, env, srcIdx)));
        break;
    }

    case Opcode::subassign1_2_: {
        Value* idx2 = pop();
        Value* idx1 = pop();
        Value* vec = pop();
        Value* val = pop();
        push(insert(new Subassign1_2D(val, vec, idx1, idx2, env, srcIdx)));
        break;
    }

    case Opcode::subassign2_2_: {
        Value* idx2 = pop();
        Value* idx1 = pop();
        Value* vec = pop();
        Value* val = pop();
        push(insert(new Subassign2_2D(val, vec, idx1, idx2, env, srcIdx)));
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
        if (!inPromise()) {                                                    \
            forceIfPromised(1);                                                \
            forceIfPromised(0);                                                \
            addCheckpoint(srcCode, pos, stack, insert);                        \
        }                                                                      \
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
        push(insert(new Identical(lhs, rhs)));
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
        UNOP_NOENV(Length, length_);
        UNOP_NOENV(Inc, inc_);
        UNOP_NOENV(Dec, dec_);
#undef UNOP_NOENV

    case Opcode::missing_:
        push(insert(new Missing(Pool::get(bc.immediate.pool), env)));
        break;

    case Opcode::is_:
        push(insert(new Is(bc.immediate.i, pop())));
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

#define V(_, name, Name)                                                       \
    case Opcode::name##_:                                                      \
        insert(new Name());                                                    \
        break;
        SIMPLE_INSTRUCTIONS(V, _)
#undef V

    // Silently ignored
    case Opcode::clear_binding_cache_:
    // TODO implement!
    case Opcode::isfun_:
        break;

    // Currently unused opcodes:
    case Opcode::brobj_:
    case Opcode::alloc_:
    case Opcode::push_code_:
    case Opcode::set_names_:
    case Opcode::names_:

    // Invalid opcodes:
    case Opcode::invalid_:
    case Opcode::num_of:

    // Opcodes handled elsewhere
    case Opcode::brtrue_:
    case Opcode::brfalse_:
    case Opcode::br_:
    case Opcode::ret_:
    case Opcode::return_:
        assert(false);

    // Opcodes that only come from PIR
    case Opcode::deopt_:
    case Opcode::force_:
    case Opcode::mk_stub_env_:
    case Opcode::mk_env_:
    case Opcode::get_env_:
    case Opcode::parent_env_:
    case Opcode::set_env_:
    case Opcode::ldvar_noforce_:
    case Opcode::ldvar_noforce_cached_:
    case Opcode::ldvar_noforce_super_:
    case Opcode::ldarg_:
    case Opcode::ldloc_:
    case Opcode::stloc_:
    case Opcode::movloc_:
    case Opcode::isobj_:
    case Opcode::isstubenv_:
    case Opcode::check_missing_:
    case Opcode::static_call_:
    case Opcode::pop_context_:
    case Opcode::push_context_:
    case Opcode::ldvar_noforce_stubbed_:
    case Opcode::stvar_stubbed_:
    case Opcode::assert_type_:
        log.unsupportedBC("Unsupported BC (are you recompiling?)", bc);
        assert(false && "Recompiling PIR not supported for now.");

    // Unsupported opcodes:
    case Opcode::asast_:
    case Opcode::beginloop_:
    case Opcode::endloop_:
    case Opcode::ldddvar_:
    case Opcode::call_dots_:
        log.unsupportedBC("Unsupported BC", bc);
        return false;
    }

    return true;
} // namespace pir

bool Rir2Pir::tryCompile(rir::Code* srcCode, Builder& insert) {
    if (auto res = tryTranslate(srcCode, insert)) {
        finalize(res, insert);
        return true;
    }
    return false;
}

bool Rir2Pir::tryCompilePromise(rir::Code* prom, Builder& insert) const {
    return PromiseRir2Pir(compiler, srcFunction, log, name)
        .tryCompile(prom, insert);
}

Value* Rir2Pir::tryTranslatePromise(rir::Code* srcCode, Builder& insert) const {
    return PromiseRir2Pir(compiler, srcFunction, log, name)
        .tryTranslate(srcCode, insert);
}

Value* Rir2Pir::tryTranslate(rir::Code* srcCode, Builder& insert) const {
    assert(!finalized);

    CallTargetFeedback callTargetFeedback;
    std::vector<ReturnSite> results;

    std::unordered_map<Opcode*, State> mergepoints;
    for (auto p : findMergepoints(srcCode))
        mergepoints.emplace(p, State());

    std::deque<State> worklist;
    State cur;
    cur.seen = true;

    Opcode* end = srcCode->endCode();
    Opcode* finger = srcCode->code();

    auto popWorklist = [&]() {
        assert(!worklist.empty());
        cur = std::move(worklist.back());
        worklist.pop_back();
        insert.enterBB(cur.entryBB);
        return cur.entryPC;
    };
    auto pushWorklist = [&](BB* bb, Opcode* pos) {
        worklist.push_back(State(cur, false, bb, pos));
    };

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
        }
        const auto pos = finger;
        BC bc = BC::advance(&finger, srcCode);
        const auto nextPos = finger;

        assert(pos != end);
        if (bc.isJmp()) {
            auto trg = bc.jmpTarget(pos);
            if (bc.isUncondJmp()) {
                finger = trg;
                continue;
            }

            // Conditional jump
            switch (bc.bc) {
            case Opcode::brtrue_:
            case Opcode::brfalse_: {
                Value* v = cur.stack.pop();
                insert(new Branch(v));
                break;
            }
            case Opcode::brobj_: {
                Value* v = insert(new IsObject(cur.stack.top()));
                insert(new Branch(v));
                break;
            }
            case Opcode::beginloop_:
                log.warn("Cannot compile Function. Unsupported beginloop bc");
                return nullptr;
            default:
                assert(false);
            }

            auto edgeSplit = [&](Opcode* trg, BB* branch) {
                if (mergepoints.count(trg)) {
                    BB* next = insert.createBB();
                    branch->setNext(next);
                    branch = next;
                }
                return branch;
            };

            BB* branch = edgeSplit(trg, insert.createBB());
            BB* fall = edgeSplit(nextPos, insert.createBB());

            switch (bc.bc) {
            case Opcode::brtrue_:
                insert.setBranch(branch, fall);
                break;
            case Opcode::brfalse_:
            case Opcode::brobj_:
                insert.setBranch(fall, branch);
                break;
            default:
                assert(false);
            }

            pushWorklist(branch, trg);

            insert.enterBB(fall);
            continue;
        }

        if (bc.isExit()) {
            Value* tos = cur.stack.top();
            switch (bc.bc) {
            case Opcode::ret_:
                cur.stack.pop();
                break;
            case Opcode::return_:
                if (inPromise()) {
                    log.warn("Cannot compile Function. Unsupported return bc "
                             "in promise");
                    return nullptr;
                }
                // Return bytecode as top-level statement cannot cause non-local
                // return. Therefore we can treat it as normal local return
                // instruction. We just need to make sure to empty the stack.
                cur.stack.clear();
                break;
            case Opcode::deopt_:
                log.warn("Cannot compile Function. Unsupported deopt bc");
                return nullptr;
            default:
                assert(false);
            }
            assert(cur.stack.empty());
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
            rir::Function* function = dt->baseline();

            std::stringstream inner;
            inner << name;
            // Try to find the name of this inner function by peeking for the
            // stvar
            {
                auto n = pc;
                for (int i = 0; i < 2 && n < end; ++i, n = BC::next(n))
                    ;
                if (n < end) {
                    auto nextbc = BC::decodeShallow(n);
                    if (nextbc.bc == Opcode::stvar_)
                        inner << ">"
                              << CHAR(PRINTNAME(nextbc.immediateConst()));
                }
            }
            inner << "@";
            if (srcCode != srcFunction->body()) {
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

            compiler.compileFunction(
                function, inner.str(), formals, srcRef,
                [&](ClosureVersion* innerF) {
                    cur.stack.push(
                        insert(new MkFunCls(innerF->owner(), dt, insert.env)));

                    // Skip those instructions
                    finger = pc;
                    skip = true;
                },
                []() {
                    // If the closure does not compile, we
                    // can still call the unoptimized
                    // version (which is what happens on
                    // `tryRunCurrentBC` below)
                });
        });

        if (!skip) {
            int size = cur.stack.size();
            if (!compileBC(bc, pos, nextPos, srcCode, cur.stack, insert,
                           callTargetFeedback)) {
                log.failed("Abort r2p due to unsupported bc");
                return nullptr;
            }

            if (!inPromise() && !insert.getCurrentBB()->isEmpty()) {
                auto last = insert.getCurrentBB()->last();

                if (Deopt::Cast(last)) {
                    finger = end;
                    continue;
                }

                if (last->isDeoptBarrier())
                    addCheckpoint(srcCode, nextPos, cur.stack, insert);
            }

            if (cur.stack.size() != size - bc.popCount() + bc.pushCount()) {
                srcCode->print(std::cerr);
                std::cerr << "After interpreting '";
                bc.print(std::cerr);
                std::cerr << "' which is supposed to pop " << bc.popCount()
                          << " and push " << bc.pushCount() << " we got from "
                          << size << " to " << cur.stack.size() << "\n";
                assert(false);
                return nullptr;
            }
        }
    }
    assert(cur.stack.empty());

    if (results.size() == 0) {
        // Cannot compile functions with infinite loop
        log.warn("Aborting, it looks like this function has an infinite loop");
        return nullptr;
    }

    Value* res;
    if (results.size() == 1) {
        res = results.back().second;
        insert.reenterBB(results.back().first);
    } else {
        BB* merge = insert.createBB();
        insert.enterBB(merge);
        Phi* phi = insert(new Phi());
        for (auto r : results) {
            r.first->setNext(merge);
            phi->addInput(r.first, r.second);
        }
        phi->updateTypeAndEffects();
        res = phi;
    }

    results.clear();

    return res;
}

void Rir2Pir::finalize(Value* ret, Builder& insert) {
    assert(!finalized);
    assert(ret);
    assert(insert.getCurrentBB()->isExit() &&
           "Builder needs to be on an exit-block to insert return");

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

    // Return in promise can lead to non-local return, which currently needs env
    // to find the context to return to.
    insert(new Return(ret));

    InsertCast c(insert.code->entry, insert.env);
    c();

    finalized = true;
}

} // namespace pir
} // namespace rir
