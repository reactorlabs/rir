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
    assert(bb->next0 == nullptr);
    assert(bb->next1 == nullptr);
    markDone(bb);
    bb->next0 = next0;
    if (next1)
        bb->next1 = next1;
}

void Builder::enterBB(BB* next) {
    assert(!isDone(next));
    bb = next;
}

void Builder::createNextBB() {
    auto n = createBB();
    if (bb)
        setNextBB(n);
    bb = n;
}

void Builder::ifThenElse(BBCompile ifblock, BBCompile thenblock) {
    assert(bb);
    auto a = createBB();
    auto b = createBB();
    setNextBB(a, b);

    auto j = createBB();

    enterBB(a);
    ifblock();
    setNextBB(j);

    enterBB(b);
    thenblock();
    setNextBB(j);

    enterBB(j);
}

void Builder::deopt(rir::Code* srcCode, Opcode* pos,
                    const std::deque<Value*>& stack) {
    auto sp = operator()(new Safepoint(env, srcCode, pos, stack));
    operator()(new Deopt(sp));
};

Builder::Builder(Closure* fun, Value* closureEnv)
    : function(fun), code(fun), env(nullptr) {
    createNextBB();
    assert(!function->entry);
    function->entry = bb;
    std::vector<Value*> args(fun->argNames.size());
    for (long i = fun->argNames.size() - 1; i >= 0; --i)
        args[i] = this->operator()(new LdArg(i));
    env = this->operator()(new MkEnv(closureEnv, fun->argNames, args.data()));
}

Builder::Builder(Closure* fun, Promise* prom)
    : function(fun), code(prom), env(nullptr) {
    createNextBB();
    assert(!prom->entry);
    prom->entry = bb;
    env = this->operator()(new LdFunctionEnv());
}
}
}
