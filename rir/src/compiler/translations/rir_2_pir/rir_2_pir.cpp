#include "rir_2_pir.h"
#include "../../analysis/query.h"
#include "../../analysis/verifier.h"
#include "../../pir/pir_impl.h"
#include "../../transform/insert_cast.h"
#include "../../util/builder.h"
#include "../../util/cfg.h"
#include "../../util/visitor.h"
#include "R/Funtab.h"
#include "R/RList.h"
#include "ir/BC.h"
#include "ir/Compiler.h"
#include "utils/FormalArgs.h"

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
        if (in != p) {
            p->addInput(incomBB, in);
        }
    }
    incomBB->setNext(entryBB);
}

std::unordered_set<Opcode*> findMergepoints(rir::Code* srcCode) {
    std::unordered_map<Opcode*, std::vector<Opcode*>> incom;
    // Mark incoming jmps
    for (auto pc = srcCode->code(); pc != srcCode->endCode();) {
        BC bc = BC::decode(pc);
        if (bc.isJmp()) {
            incom[bc.jmpTarget(pc)].push_back(pc);
        }
        pc = BC::next(pc);
    }
    // Mark falltrough to label
    for (auto pc = srcCode->code(); pc != srcCode->endCode();) {
        BC bc = BC::decode(pc);
        if (!bc.isUncondJmp() && !bc.isReturn()) {
            Opcode* next = BC::next(pc);
            if (incom.count(next))
                incom[next].push_back(pc);
        }
        pc = BC::next(pc);
    }

    std::unordered_set<Opcode*> mergepoints;
    // Create mergepoints
    for (auto m : incom)
        if (std::get<1>(m).size() > 1)
            mergepoints.insert(m.first);
    return mergepoints;
}

} // namespace

namespace rir {
namespace pir {

bool Rir2Pir::compileBC(BC bc, Opcode* pos, rir::Code* srcCode, RirStack& stack,
                        Builder& insert,
                        std::unordered_map<Value*, CallFeedback>& callFeedback,
                        std::array<TypeFeedback, 2>& typeFeedback) const {
    Value* env = insert.env;

    unsigned srcIdx = srcCode->getSrcIdxAt(pos, true);

    Value* v;
    Value* x;
    Value* y;

    auto push = [&stack](Value* v) { stack.push(v); };
    auto pop = [&stack]() { return stack.pop(); };
    auto at = [&stack](unsigned i) { return stack.at(i); };
    auto top = [&stack]() { return stack.at(0); };
    auto set = [&stack](unsigned i, Value* v) { stack.at(i) = v; };

    switch (bc.bc) {

    case Opcode::push_:
        push(insert(new LdConst(bc.immediateConst())));
        break;

    case Opcode::ldvar_:
        v = insert(new LdVar(bc.immediateConst(), env));
        push(insert(new Force(v, env)));
        break;

    case Opcode::stvar_:
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
    case Opcode::aslogical_:
        push(insert(new AsLogical(pop(), srcIdx)));
        break;

    case Opcode::ldfun_:
        push(insert(new LdFun(bc.immediateConst(), env)));
        break;

    case Opcode::guard_fun_:
        compiler.getLogger().warningBC(srcFunction, WARNING_GUARD_STRING, bc);
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

    case Opcode::record_binop_: {
        typeFeedback[0] = bc.immediate.binopFeedback[0];
        typeFeedback[1] = bc.immediate.binopFeedback[1];
        break;
    }

    case Opcode::record_call_: {
        Value* target = top();
        callFeedback[target] = bc.immediate.callFeedback;
        break;
    }

    case Opcode::call_implicit_: {
        std::vector<Value*> args;
        for (auto argi : bc.immediateCallArguments) {
            if (argi == DOTS_ARG_IDX) {
                return false;
            } else if (argi == MISSING_ARG_IDX) {
                return false;
            }
            rir::Code* promiseCode = srcFunction->codeAt(argi);
            Promise* prom = insert.function->createProm(promiseCode->src);
            {
                Builder promiseBuilder(insert.function, prom);
                if (!tryCompilePromise(promiseCode, promiseBuilder))
                    return false;
            }
            Value* val = Missing::instance();
            if (Query::pure(prom)) {
                translate(promiseCode, insert,
                          [&](Value* success) { val = success; });
            }
            args.push_back(insert(new MkArg(prom, val, env)));
        }

        Value* callee = top();
        SEXP monomorphic = nullptr;
        if (callFeedback.count(callee)) {
            auto& feedback = callFeedback.at(callee);
            if (feedback.numTargets == 1)
                monomorphic = feedback.targets[0];
        }

        auto ast = bc.immediate.callFixedArgs.ast;
        auto insertGenericCall = [&]() {
            push(insert(new Call(insert.env, pop(), args, ast)));
        };
        if (monomorphic && isValidClosureSEXP(monomorphic)) {
            compiler.compileClosure(
                monomorphic,
                [&](Closure* f) {
                    Value* expected = insert(new LdConst(monomorphic));
                    Value* t = insert(new Identical(callee, expected));

                    insert.conditionalDeopt(t, srcCode, pos, stack, true);
                    pop();
                    push(insert(
                        new StaticCall(insert.env, f, args, monomorphic, ast)));
                },
                insertGenericCall);
        } else {
            insertGenericCall();
        }
        break;
    }

    case Opcode::promise_: {
        unsigned promi = bc.immediate.i;
        rir::Code* promiseCode = srcFunction->codeAt(promi);
        Value* val = pop();
        Promise* prom = insert.function->createProm(promiseCode->src);
        {
            Builder promiseBuilder(insert.function, prom);
            if (tryCompilePromise(promiseCode, promiseBuilder))
                return false;
        }
        push(insert(new MkArg(prom, val, env)));
        break;
    }

    case Opcode::call_: {
        unsigned n = bc.immediate.callFixedArgs.nargs;
        std::vector<Value*> args(n);
        for (size_t i = 0; i < n; ++i)
            args[n - i - 1] = pop();

        auto target = pop();
        push(insert(
            new Call(env, target, args, bc.immediate.callFixedArgs.ast)));
        break;
    }

    case Opcode::static_call_: {
        unsigned n = bc.immediate.staticCallFixedArgs.nargs;
        auto ast = bc.immediate.staticCallFixedArgs.ast;
        SEXP target = rir::Pool::get(bc.immediate.staticCallFixedArgs.target);

        std::vector<Value*> args(n);
        for (size_t i = 0; i < n; ++i)
            args[n - i - 1] = pop();

        if (TYPEOF(target) == BUILTINSXP) {
            // TODO: compile a list of safe builtins
            static int vector = findBuiltin("vector");

            if (getBuiltinNr(target) == vector)
                push(insert(new CallSafeBuiltin(target, args, ast)));
            else
                push(insert(new CallBuiltin(env, target, args, ast)));
        } else {
            assert(TYPEOF(target) == CLOSXP);
            if (!isValidClosureSEXP(target)) {
                Compiler::compileClosure(target);
            }
            bool failed = false;
            compiler.compileClosure(
                target,
                [&](Closure* f) {
                    push(insert(new StaticCall(env, f, args, target, ast)));
                },
                [&]() { failed = true; });
            if (failed)
                return false;
        }
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
        Value* idx = pop();
        Value* vec = pop();
        push(insert(new Extract1_1D(vec, idx, env, srcIdx)));
        break;
    }

    case Opcode::extract2_1_: {
        Value* idx = pop();
        Value* vec = pop();
        push(insert(new Extract2_1D(vec, idx, env, srcIdx)));
        break;
    }

    case Opcode::extract1_2_: {
        Value* idx2 = pop();
        Value* idx1 = pop();
        Value* vec = pop();
        push(insert(new Extract1_2D(vec, idx1, idx2, env, srcIdx)));
        break;
    }

    case Opcode::extract2_2_: {
        Value* idx2 = pop();
        Value* idx1 = pop();
        Value* vec = pop();
        push(insert(new Extract2_2D(vec, idx1, idx2, env, srcIdx)));
        break;
    }

    case Opcode::subassign1_: {
        Value* idx = pop();
        Value* vec = pop();
        Value* val = pop();
        push(insert(new Subassign1_1D(val, vec, idx, env, srcIdx)));
        break;
    }

    case Opcode::subassign2_: {
        Value* idx = pop();
        Value* vec = pop();
        Value* val = pop();
        push(insert(new Subassign2_1D(val, vec, idx, env, srcIdx)));
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

#define BINOP(Name, Op)                                                        \
    case Opcode::Op: {                                                         \
        auto rhs = pop();                                                      \
        auto lhs = pop();                                                      \
        if (typeFeedback[0].numTypes > 0 && typeFeedback[1].numTypes > 0 &&    \
            !typeFeedback[0].observedObject() &&                               \
            !typeFeedback[1].observedObject()) {                               \
            Value* leftIsObj = insert(new IsObject(lhs));                      \
            insert.conditionalDeopt(leftIsObj, srcCode, pos, stack, false);    \
            Value* rightIsObj = insert(new IsObject(rhs));                     \
            insert.conditionalDeopt(rightIsObj, srcCode, pos, stack, false);   \
            push(insert(new Name(lhs, rhs, Env::elided(), srcIdx)));           \
        } else {                                                               \
            push(insert(new Name(lhs, rhs, env, srcIdx)));                     \
        }                                                                      \
        break;                                                                 \
    }

        BINOP(Lt, lt_);
        BINOP(Gt, gt_);
        BINOP(Gte, le_);
        BINOP(Lte, ge_);
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

    case Opcode::identical_: {
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
        UNOP(Length, length_);
#undef UNOP

#define UNOP_NOENV(Name, Op)                                                   \
    case Opcode::Op: {                                                         \
        v = pop();                                                             \
        push(insert(new Name(v)));                                             \
        break;                                                                 \
    }
        UNOP_NOENV(Inc, inc_);
#undef UNOP_NOENV

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
        for (size_t i = 0; i < bc.immediate.i - 1; ++i)
            set(i, at(i + 1));
        set(bc.immediate.i, x);
        break;
    }

    case Opcode::set_shared_:
        push(insert(new SetShared(pop())));
        break;

    case Opcode::int3_:
        insert(new Int3());
        break;

    // TODO implement!
    // (silently ignored)
    case Opcode::invisible_:
    case Opcode::visible_:
    case Opcode::isfun_:
        break;

    // Currently unused opcodes:
    case Opcode::brobj_:
    case Opcode::alloc_:
    case Opcode::push_code_:
    case Opcode::set_names_:
    case Opcode::names_:
    case Opcode::make_unique_:

    // Invalid opcodes:
    case Opcode::label:
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
    case Opcode::make_env_:
    case Opcode::get_env_:
    case Opcode::parent_env_:
    case Opcode::set_env_:
    case Opcode::ldvar_noforce_:
    case Opcode::ldvar_noforce_super_:
    case Opcode::ldarg_:
    case Opcode::ldloc_:
    case Opcode::stloc_:
    case Opcode::movloc_:
    case Opcode::isobj_:
    case Opcode::check_missing_:
        assert(false && "Recompiling PIR not supported for now.");

    // Unsupported opcodes:
    case Opcode::named_call_:
    case Opcode::named_call_implicit_:
    case Opcode::ldlval_:
    case Opcode::asast_:
    case Opcode::missing_:
    case Opcode::beginloop_:
    case Opcode::endcontext_:
    case Opcode::ldddvar_:
        return false;
    }

    return true;
}

void Rir2Pir::translate(rir::Code* srcCode, Builder& insert,
                        MaybeVal<void> success, Maybe<void> fail) const {
    assert(!finalized);

    std::vector<ReturnSite> results;

    std::unordered_map<Opcode*, State> mergepoints;
    for (auto p : findMergepoints(srcCode))
        mergepoints.emplace(p, State());

    std::deque<State> worklist;
    State cur;
    cur.seen = true;

    std::unordered_map<Value*, CallFeedback> callFeedback;

    /* Captures the last type feedback observed. The assumption is that this
       type feedback will be used in subsequent operations. Also that the
       feedback needed concerns, at most, two operands */
    std::array<TypeFeedback, 2> lastTypeFeedback;

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
        BC bc = BC::advance(&finger);
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
                if (compiler.debug.includes(DebugFlag::ShowWarnings))
                    std::cerr << "Cannot compile Function. Unsupported "
                                 "beginloop bc\n";
                fail();
                return;
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

        if (bc.isReturn()) {
            switch (bc.bc) {
            case Opcode::ret_:
                break;
            case Opcode::return_:
                if (compiler.debug.includes(DebugFlag::ShowWarnings))
                    std::cerr
                        << "Cannot compile Function. Unsupported return bc\n";
                fail();
                return;
            default:
                assert(false);
            }
            results.push_back(
                ReturnSite(insert.getCurrentBB(), cur.stack.pop()));
            assert(cur.stack.empty());
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
            BC ldfmls = BC::advance(&pc);
            BC ldcode = BC::advance(&pc);
            BC ldsrc = BC::advance(&pc);
            pc = BC::next(pc); // close

            SEXP fmls = ldfmls.immediateConst();
            SEXP code = ldcode.immediateConst();
            SEXP src = ldsrc.immediateConst();

            FormalArgs formals(fmls);

            DispatchTable* dt = DispatchTable::unpack(code);
            rir::Function* function = dt->first();

            compiler.compileFunction(
                function, formals,
                [&](Closure* innerF) {
                    cur.stack.push(insert(
                        new MkFunCls(innerF, insert.env, fmls, code, src)));

                    // Skip those instructions
                    finger = pc;
                    skip = true;
                },
                []() {
                    // If the closure does not compile, we can still call the
                    // unoptimized version (which is what happens on
                    // `tryRunCurrentBC` below)
                });
        });

        if (!skip) {
            int size = cur.stack.size();
            if (!compileBC(bc, pos, srcCode, cur.stack, insert, callFeedback,
                           lastTypeFeedback)) {
                compiler.getLogger().warningBC(
                    srcFunction, "Abort r2p due to unsupported bc", pos);
                fail();
                return;
            }
            if (cur.stack.size() != size - bc.popCount() + bc.pushCount()) {
                srcCode->print(std::cerr);
                std::cerr << "After interpreting '";
                bc.print(std::cerr);
                std::cerr << "' which is supposed to pop " << bc.popCount()
                          << " and push " << bc.pushCount() << " we got from "
                          << size << " to " << cur.stack.size() << "\n";
                assert(false);
                fail();
                return;
            }
            if (bc.isCall())
                insert.registerSafepoint(srcCode, nextPos, cur.stack);
        }
    }
    assert(cur.stack.empty());

    if (results.size() == 0) {
        // Cannot compile functions with infinite loop
        fail();
        return;
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
        phi->updateType();
        res = phi;
    }

    results.clear();

    success(res);
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
                if (p->updateType()) {
                    changed = true;
                }
                it++;
            }
        });
    }

    insert(new Return(ret));

    InsertCast c(insert.code->entry, insert.env);
    c();

    finalized = true;
}

} // namespace pir
} // namespace rir
