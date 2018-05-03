#include "builder.h"
#include "../pir/pir_impl.h"

namespace rir {
namespace pir {

BB* Builder::createBB() { return new BB(code, ++function->maxBBId); }

Builder::Builder(Closure* fun, Value* closureEnv)
    : function(fun), code(fun), env(nullptr), bb(fun->entry) {
    bb = function->entry = createBB();
    std::vector<Value*> args;
    for (size_t i = 0; i < fun->argNames.size(); ++i)
        args.push_back(this->operator()(new LdArg(i)));
    env = this->operator()(new MkEnv(closureEnv, fun->argNames, args.data()));
}
Builder::Builder(Closure* fun, Promise* prom)
    : function(fun), code(prom), env(nullptr), bb(prom->entry) {
    bb = prom->entry = createBB();
    env = this->operator()(new LdFunctionEnv());
}
}
}
