#ifndef COMPILER_FUNCTION_H
#define COMPILER_FUNCTION_H

#include "R/r.h"
#include "code.h"
#include "pir.h"

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
    Closure(std::initializer_list<SEXP> a, Env* env) : env(env), argNames(a) {}
    Closure(const std::vector<SEXP>& a, Env* env) : env(env), argNames(a) {}

    Env* env;

  public:
    Env* closureEnv() { return env; }

    std::vector<SEXP> argNames;
    std::vector<Promise*> defaultArgs;

    std::vector<Promise*> promises;

    void print(std::ostream& out);

    Promise* createProm();

    size_t maxBBId = 0;

    friend std::ostream& operator<<(std::ostream& out, const Closure& e) {
        out << "Func(" << (void*)&e << ")";
        return out;
    }

    Closure* clone();

    ~Closure();
};
} // namespace pir
} // namespace rir

#endif
