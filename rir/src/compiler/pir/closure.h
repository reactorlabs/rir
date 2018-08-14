#ifndef COMPILER_FUNCTION_H
#define COMPILER_FUNCTION_H

#include "../../runtime/Function.h"
#include "R/r.h"
#include "code.h"
#include "pir.h"
#include <functional>

namespace rir {
namespace pir {

/*
 * Closure
 *
 * A function does not have an environment per se, but just a number of named
 * arguments. If an environment is necessary, `MkEnv` can bind arguments
 * (referred to by `LdArg`).
 *
 */
class Closure : public Code {
  private:
    friend class Module;
    Closure(std::initializer_list<SEXP> a, Env* env, rir::Function* function)
        : env(env), function(function), argNames(a) {}
    Closure(const std::vector<SEXP>& a, Env* env, rir::Function* function)
        : env(env), function(function), argNames(a) {}

    Env* env;
    rir::Function* function;

  public:
    Env* closureEnv() { return env; }
    rir::Function* rirVersion() { return function; }

    std::vector<SEXP> argNames;
    std::vector<Promise*> defaultArgs;

    std::vector<Promise*> promises;

    void print(std::ostream& out);
    void print();

    Promise* createProm(unsigned srcPoolIdx);

    friend std::ostream& operator<<(std::ostream& out, const Closure& e) {
        out << "Func(" << (void*)&e << ")";
        return out;
    }

    Closure* clone();

    ~Closure();

    typedef std::function<void(Promise*)> PromiseIterator;

    void eachDefaultArg(PromiseIterator it) const {
        for (auto p : defaultArgs)
            if (p)
                it(p);
    }

    void eachPromise(PromiseIterator it) const {
        for (auto p : promises)
            if (p)
                it(p);
    }

};

} // namespace pir
} // namespace rir

#endif
