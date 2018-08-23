#include "builder.h"
#include "../pir/pir_impl.h"

namespace rir {
namespace pir {

BB* Builder::createBB() { return new BB(code, code->nextBBId++); }

void Builder::markDone(BB* bb) {
    if (done.size() <= bb->id)
        done.resize(bb->id + 1);
    done[bb->id] = true;
}

bool Builder::isDone(BB* bb) {
    if (done.size() <= bb->id)
        done.resize(bb->id + 1);
    return done[bb->id];
}

void Builder::setNextBB(BB* next0, BB* next1) {
    assert(bb);
    markDone(bb);
    bb->setNextBranches(next0, next1);
    if (next1)
        assert(!bb->isEmpty() && Branch::Cast(bb->last()));
}

void Builder::enterBB(BB* next) {
    assert(!isDone(next));
    if (bb)
        markDone(bb);
    bb = next;
}

void Builder::createNextBB() {
    auto n = createBB();
    if (bb)
        setNextBB(n);
    bb = n;
}

void Builder::deoptUnless(Value* condition, rir::Code* srcCode, Opcode* pos,
                          const RirStack& stack) {
    add(new Branch(condition));
    auto cont = createBB();
    auto fail = createBB();
    setNextBB(cont, fail);

    enterBB(fail);
    auto sp = add(new Safepoint(env, srcCode, pos, stack));
    add(new Deopt(sp));

    enterBB(cont);
};

Builder::Builder(Closure* fun, Value* closureEnv)
    : function(fun), code(fun), env(nullptr) {
    createNextBB();
    assert(!function->entry);
    function->entry = bb;
    std::vector<Value*> args(fun->argNames.size());
    for (long i = fun->argNames.size() - 1; i >= 0; --i)
        args[i] = this->operator()(new LdArg(i));
    env = add(new MkEnv(closureEnv, fun->argNames, args.data()));
}

Builder::Builder(Closure* fun, Promise* prom)
    : function(fun), code(prom), env(nullptr) {
    createNextBB();
    assert(!prom->entry);
    prom->entry = bb;
    env = add(new LdFunctionEnv());
}
}
}
