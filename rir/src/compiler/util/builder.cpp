#include "builder.h"
#include "../pir/pir_impl.h"

namespace rir {
namespace pir {

BB* Builder::createBB() { return new BB(code, code->nextBBId++); }

Builder::Builder(Closure* fun, Value* closureEnv)
    : function(fun), code(fun), env(nullptr), bb(fun->entry) {
    bb = function->entry = createBB();
    std::vector<Value*> args(fun->argNames.size());
    for (long i = fun->argNames.size() - 1; i >= 0; --i)
        args[i] = this->operator()(new LdArg(i));
    env = this->operator()(new MkEnv(closureEnv, fun->argNames, args.data()));
}
Builder::Builder(Closure* fun, Promise* prom)
    : function(fun), code(prom), env(nullptr), bb(prom->entry) {
    bb = prom->entry = createBB();
    env = this->operator()(new LdFunctionEnv());
}
}
}
