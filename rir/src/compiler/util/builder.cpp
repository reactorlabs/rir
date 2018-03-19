#include "builder.h"
#include "../pir/pir_impl.h"

namespace rir {
namespace pir {

BB* Builder::createBB() { return new BB(function, ++function->maxBBId); }

Builder::Builder(Function* fun, Env* enclos)
    : function(fun), code(fun), env(nullptr), bb(fun->entry) {
    std::vector<Value*> args;
    for (size_t i = 0; i < fun->argNames.size(); ++i)
        args.push_back(this->operator()(new LdArg(i)));
    env = this->operator()(new MkEnv(enclos, fun->argNames, args.data()));
}
Builder::Builder(Function* fun, Promise* prom)
    : function(fun), code(prom), env(nullptr), bb(prom->entry) {
    env = this->operator()(new LdFunctionEnv());
}
}
}
