#ifndef COMPILER_FUNCTION_H
#define COMPILER_FUNCTION_H

#include "../../runtime/Function.h"
#include "code.h"
#include "pir.h"
#include <functional>
#include <sstream>
#include <unordered_map>

namespace rir {
namespace pir {

struct ProfiledValues {
    std::unordered_map<Value*, ObservedCallees> callTargets;
    std::unordered_map<Value*, ObservedValues> types;

    bool hasTypesFor(Value* value) {
        return types.count(value) && types.at(value).numTypes;
    }
};

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
    static std::string uniqueName(const Closure* c, const std::string& name) {
        std::stringstream id;
        id << name << "[" << c << "]";
        return id.str();
    }

    Closure(const std::string& name, std::initializer_list<SEXP> a, Env* env,
            rir::Function* function)
        : env(env), function(function), name(uniqueName(this, name)),
          argNames(a) {}
    Closure(const std::string& name, const std::vector<SEXP>& a, Env* env,
            rir::Function* function)
        : env(env), function(function), name(uniqueName(this, name)),
          argNames(a) {}

    Env* env;
    rir::Function* function;

  public:
    const std::string name;

    Env* closureEnv() const { return env; }
    rir::Function* rirVersion() { return function; }

    std::vector<SEXP> argNames;
    std::vector<Promise*> defaultArgs;
    std::vector<Promise*> promises;
    ProfiledValues runtimeFeedback;

    void print(std::ostream& out, bool tty) const;

    Promise* createProm(unsigned srcPoolIdx);

    friend std::ostream& operator<<(std::ostream& out, const Closure& e) {
        out << e.name;
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
