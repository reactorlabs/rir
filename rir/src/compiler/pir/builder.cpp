#include "builder.h"

#include "compiler/pir/pir_impl.h"
#include "runtime/Context.h"
#include "utils/Pool.h"

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
        assert(false && "This instruction is only allowed during lowering");
    default: {}
    }
    bb->append(i);
}

FrameState* Builder::registerFrameState(rir::Code* srcCode, Opcode* pos,
                                        const RirStack& stack, bool inPromise) {
    auto sp = new FrameState(env, srcCode, pos, stack, inPromise);
    add(sp);
    return sp;
}

Checkpoint* Builder::emitCheckpoint(FrameState* fs) {
    auto cp = new Checkpoint();
    add(cp);
    auto cont = createBB();
    auto fail = createBB();
    setBranch(cont, fail);
    enterBB(fail);
    add(new Deopt(fs));
    markDone(fail);

    enterBB(cont);
    return cp;
}

Checkpoint* Builder::emitCheckpoint(rir::Code* srcCode, Opcode* pos,
                                    const RirStack& stack, bool inPromise) {
    auto cp = new Checkpoint();
    add(cp);
    auto cont = createBB();
    auto fail = createBB();
    setBranch(cont, fail);
    enterBB(fail);
    auto sp = registerFrameState(srcCode, pos, stack, inPromise);
    add(new Deopt(sp));
    markDone(fail);

    enterBB(cont);
    return cp;
}

Builder::Builder(Continuation* cnt, Value* closureEnv)
    : function(cnt), code(cnt), env(nullptr) {
    createNextBB();
    assert(!function->entry);
    function->entry = bb;

    // Create another BB to ensure that the entry BB has no predecessors.
    createNextBB();

    std::vector<Value*> args;
    std::vector<SEXP> names;
    std::vector<bool> miss(cnt->continuationContext->envSize(), false);
    auto h = cnt->continuationContext->stackSize();

    if (!cnt->continuationContext->leakedEnv()) {
        auto e = cnt->continuationContext->envBegin();
        size_t i = 0;
        while (e != cnt->continuationContext->envEnd()) {
            auto r = this->operator()(new LdArg(h + i));
            r->type = std::get<PirType>(*e);
            args.push_back(r);
            auto n = std::get<SEXP>(*e);
            names.push_back(n);
            miss[i] = std::get<bool>(*e);
            e++;
            i++;
        }
        auto mkenv = new MkEnv(closureEnv, names, args.data(), miss);

        auto rirCode = cnt->owner()->rirFunction()->body();
        mkenv->updateTypeFeedback().feedbackOrigin.srcCode(rirCode);
        add(mkenv);
        this->env = mkenv;
    } else {
        auto e = new LdFunctionEnv();
        add(e);
        this->env = e;
    }
}

Builder::Builder(ClosureVersion* version, Value* closureEnv)
    : function(version), code(version), env(nullptr) {
    createNextBB();
    assert(!function->entry);
    function->entry = bb;
    auto closure = version->owner();

    // Create another BB to ensure that the entry BB has no predecessors.
    createNextBB();

    auto& context = version->context();
    std::vector<Value*> args(closure->nargs());
    size_t nargs = version->effectiveNArgs();

    auto depromiseArgs = version->owner()->rirFunction()->flags.contains(
        rir::Function::Flag::DepromiseArgs);

    for (long i = nargs - 1; i >= 0; --i) {
        args[i] = this->operator()(new LdArg(i));

        if (closure->formals().names()[i] == R_DotsSymbol)
            args[i]->type = PirType::dotsArg();
        args[i]->type.fromContext(context, i, closure->nargs());

        if (depromiseArgs) {
            args[i]->type = args[i]->type.notPromiseWrapped();
        }
    }
    for (size_t i = nargs; i < closure->nargs(); ++i) {
        args[i] = MissingArg::instance();
    }

    auto mkenv = new MkEnv(closureEnv, closure->formals().names(), args.data());
    auto rirFun = version->owner()->rirFunction();
    if (rirFun->flags.contains(rir::Function::NeedsFullEnv))
        mkenv->neverStub = true;
    mkenv->updateTypeFeedback().feedbackOrigin.srcCode(rirFun->body());
    add(mkenv);
    this->env = mkenv;
}

Builder::Builder(ClosureVersion* fun, Promise* prom)
    : function(fun), code(prom), env(nullptr) {
    createNextBB();
    assert(!prom->entry);
    prom->entry = bb;

    // Create another BB to ensure that the entry BB has no predecessors.
    createNextBB();

    auto ldenv = new LdFunctionEnv();
    add(ldenv);
    this->env = ldenv;
}

} // namespace pir
} // namespace rir
