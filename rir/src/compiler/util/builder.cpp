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

bool Builder::isDone(BB* bb) const {
    if (done.size() <= bb->id)
        return false;
    return done[bb->id];
}

void Builder::setBranch(BB* next0, BB* next1) {
    assert(bb);
    assert(!bb->isEmpty() && bb->last()->branches());
    markDone(bb);
    bb->setBranch(next0, next1);
}

void Builder::setNext(BB* next) {
    assert(bb);
    markDone(bb);
    bb->setNext(next);
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
        setNext(n);
    bb = n;
}

void Builder::add(Instruction* i) {
    switch (i->tag) {
    case Tag::_UNUSED_:
        assert(false && "Invalid instruction");
    case Tag::PirCopy:
    case Tag::CallImplicit:
    case Tag::ScheduledDeopt:
        assert(false && "This instruction is only allowed during lowering");
    default: {}
    }
    bb->append(i);
}

FrameState* Builder::registerFrameState(rir::Code* srcCode, Opcode* pos,
                                        const RirStack& stack) {
    auto sp = new FrameState(env, srcCode, pos, stack);
    add(sp);
    return sp;
};

Checkpoint* Builder::emitCheckpoint(rir::Code* srcCode, Opcode* pos,
                                    const RirStack& stack) {
    auto cp = new Checkpoint();
    add(cp);
    auto cont = createBB();
    auto fail = createBB();
    setBranch(cont, fail);
    enterBB(fail);
    auto sp = registerFrameState(srcCode, pos, stack);
    add(new Deopt(sp));
    markDone(fail);

    enterBB(cont);
    return cp;
};

Builder::Builder(ClosureVersion* version, Value* closureEnv)
    : function(version), code(version), env(nullptr) {
    createNextBB();
    assert(!function->entry);
    function->entry = bb;
    auto closure = version->owner();

    auto& assumptions = version->assumptions();
    std::vector<Value*> args(closure->nargs());
    size_t nargs = closure->nargs() - assumptions.numMissing();
    for (long i = nargs - 1; i >= 0; --i) {
        args[i] = this->operator()(new LdArg(i));
        if (assumptions.isEager(i))
            args[i]->type = PirType::promiseWrappedVal();
        if (assumptions.notObj(i))
            args[i]->type.setNotObject();
    }
    for (size_t i = nargs; i < closure->nargs(); ++i)
        args[i] = MissingArg::instance();

    auto mkenv = new MkEnv(closureEnv, closure->formals().names(), args.data());
    add(mkenv);
    this->env = mkenv;
}

Builder::Builder(ClosureVersion* fun, Promise* prom)
    : function(fun), code(prom), env(nullptr) {
    createNextBB();
    assert(!prom->entry);
    prom->entry = bb;
    auto ldenv = new LdFunctionEnv();
    add(ldenv);
    this->env = ldenv;
}
} // namespace pir
} // namespace rir
