#include "stack_machine.h"
#include "../../analysis/query.h"
#include "../../pir/pir_impl.h"
#include "../../util/builder.h"
#include "R/Funtab.h"
#include "ir/BC.h"
#include "ir/Compiler.h"
#include "rir_2_pir.h"

#include <sstream>

namespace rir {
namespace pir {

void StackMachine::clear() {
    stack.clear();
    entry = nullptr;
    pc = nullptr;
}

size_t StackMachine::stack_size() { return stack.size(); }

Value* StackMachine::pop() {
    assert(stack_size() > 0);
    auto v = stack.back();
    stack.pop_back();
    return v;
}

Value* StackMachine::top() {
    assert(!stack.empty());
    return stack.back();
}
Value* StackMachine::front() {
    assert(!stack.empty());
    return stack.front();
}
void StackMachine::push(Value* v) { stack.push_back(v); }

bool StackMachine::empty() { return stack.empty(); }

Value* StackMachine::at(size_t index) {
    assert(index < stack_size());
    return stack[stack_size() - index - 1];
}

void StackMachine::set(size_t index, Value* value) {
    assert(index < stack_size());
    stack[stack_size() - index - 1] = value;
}

bool StackMachine::tryRunCurrentBC(const Rir2Pir& rir2pir, Builder& insert) {
    assert(pc >= srcCode->code() && pc < srcCode->endCode());

    Value* env = insert.env;

    Value* v;
    Value* x;
    Value* y;
    BC bc = this->getCurrentBC();

    unsigned srcIdx = getSrcIdx();
    auto consumeSrcIdx = [&]() {
        if (srcIdx == 0)
            rir2pir.compiler.getLog().warningBC("trying to use nil src idx:",
                                                bc);
        auto tmp = srcIdx;
        srcIdx = 0;
        return tmp;
    };

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
        push(insert(new AsLogical(pop(), consumeSrcIdx())));
        break;

    case Opcode::ldfun_:
        push(insert(new LdFun(bc.immediateConst(), env)));
        break;

    case Opcode::guard_fun_:
        rir2pir.compiler.getLog().warningBC("Guard ignored", bc);
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
        // TODO
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
                if (!Rir2Pir(rir2pir).tryCompile(promiseCode, promiseBuilder))
                    return false;
            }
            Value* val = Missing::instance();
            if (Query::pure(prom)) {
                rir2pir.translate(promiseCode, insert,
                                  [&](Value* success) { val = success; });
            }
            args.push_back(insert(new MkArg(prom, val, env)));
        }

        Value* callee = pop();
        SEXP monomorphic = nullptr;
        if (callFeedback.count(callee)) {
            auto& feedback = callFeedback.at(callee);
            if (feedback.numTargets == 1)
                monomorphic = feedback.targets[0];
        }

        auto ast = bc.immediate.callFixedArgs.ast;
        auto insertGenericCall = [&]() {
            push(insert(new Call(insert.env, callee, args, ast)));
        };
        if (monomorphic && isValidClosureSEXP(monomorphic)) {
            rir2pir.compiler.compileClosure(
                monomorphic,
                [&](Closure* f) {
                    Value* expected = insert(new LdConst(monomorphic));
                    Value* t = insert(new Identical(callee, expected));
                    insert(new Branch(t));
                    BB* curBB = insert.bb;

                    BB* fallback = insert.createBB();
                    insert.bb = fallback;
                    curBB->next0 = fallback;
                    Value* r1 = insert(new Call(insert.env, callee, args, ast));

                    BB* asExpected = insert.createBB();
                    insert.bb = asExpected;
                    curBB->next1 = asExpected;
                    Value* r2 = insert(
                        new StaticCall(insert.env, f, args, monomorphic, ast));

                    BB* cont = insert.createBB();
                    fallback->next0 = cont;
                    asExpected->next0 = cont;
                    insert.bb = cont;
                    push(insert(new Phi({r1, r2}, {fallback, asExpected})));
                },
                insertGenericCall, false);
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
            if (!Rir2Pir(rir2pir).tryCompile(promiseCode, promiseBuilder))
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
                target = Compiler::compileClosure(target);
                // TODO: we need to keep track of this compiled rir function.
                // For now let's just put it in the constant pool.
                Pool::insert(target);
            }
            bool failed = false;
            rir2pir.compiler.compileClosure(
                target,
                [&](Closure* f) {
                    push(insert(new StaticCall(env, f, args, target, ast)));
                },
                [&]() { failed = true; }, false);
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
        push(insert(new Extract1_1D(vec, idx, env, consumeSrcIdx())));
        break;
    }

    case Opcode::extract2_1_: {
        Value* idx = pop();
        Value* vec = pop();
        push(insert(new Extract2_1D(vec, idx, env, consumeSrcIdx())));
        break;
    }

    case Opcode::extract1_2_: {
        Value* idx2 = pop();
        Value* idx1 = pop();
        Value* vec = pop();
        push(insert(new Extract1_2D(vec, idx1, idx2, env, consumeSrcIdx())));
        break;
    }

    case Opcode::extract2_2_: {
        Value* idx2 = pop();
        Value* idx1 = pop();
        Value* vec = pop();
        push(insert(new Extract2_2D(vec, idx1, idx2, env, consumeSrcIdx())));
        break;
    }

    case Opcode::subassign1_: {
        Value* val = pop();
        Value* idx = pop();
        Value* vec = pop();
        push(insert(new Subassign1_1D(vec, idx, val)));
        break;
    }

    case Opcode::subassign2_: {
        SEXP sym = rir::Pool::get(bc.immediate.pool);
        Value* val = pop();
        Value* idx = pop();
        Value* vec = pop();
        push(insert(new Subassign2_1D(vec, idx, val, sym)));
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
        push(insert(new Name(lhs, rhs, env, consumeSrcIdx())));                \
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
        push(insert(new Name(v, env, consumeSrcIdx())));                       \
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

    case Opcode::int3_:
        insert(new Int3());
        break;

    // TODO implement!
    // (silently ignored)
    case Opcode::set_shared_:
    case Opcode::invisible_:
    case Opcode::visible_:
    case Opcode::isfun_:
    case Opcode::make_unique_:
    case Opcode::brobj_:
        break;

    // Currently unused opcodes:
    case Opcode::alloc_:
    case Opcode::push_code_:
    case Opcode::set_names_:
    case Opcode::names_:
    case Opcode::force_:

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
    case Opcode::make_env_:
    case Opcode::get_env_:
    case Opcode::caller_env_:
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
    case Opcode::guard_env_:
    case Opcode::beginloop_:
    case Opcode::endcontext_:
    case Opcode::ldddvar_:
        return false;
    }

    // TODO: change to assert
    // assert(srcIdx == 0 && "source index is getting lost in translation");
    if (srcIdx)
        rir2pir.compiler.getLog().warningBC("losing src index", bc);

    return true;
}

bool StackMachine::doMerge(Opcode* trg, Builder& builder, StackMachine* other) {
    if (other->entry == nullptr) {
        other->entry = builder.createBB();
        other->pc = trg;
        for (size_t i = 0; i < stack_size(); ++i) {
            auto v = stack.at(i);
            auto p = new Phi;
            other->entry->append(p);
            p->addInput(builder.bb, v);
            other->push(p);
        }

        return true;
    }

    assert(stack_size() == other->stack_size());

    for (size_t i = 0; i < stack_size(); ++i) {
        Phi* p = Phi::Cast(other->stack.at(i));
        assert(p);
        Value* incom = stack.at(i);
        if (incom != p) {
            p->addInput(builder.bb, incom);
        }
    }
    return false;
}

Opcode* StackMachine::getPC() { return pc; }

void StackMachine::setPC(Opcode* opcode) { pc = opcode; }

pir::BB* StackMachine::getEntry() { return entry; }
void StackMachine::setEntry(pir::BB* ent) { entry = ent; }

void StackMachine::advancePC() { pc = BC::next(pc); }

BC StackMachine::getCurrentBC() { return BC::decode(pc); }

unsigned StackMachine::getSrcIdx() { return srcCode->getSrcIdxAt(pc, true); }

} // namespace pir
} // namespace rir
