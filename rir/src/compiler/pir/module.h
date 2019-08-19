#ifndef PIR_MODULE_H
#define PIR_MODULE_H

#include "R/r.h"
#include <functional>
#include <iostream>
#include <map>
#include <unordered_map>
#include <vector>

#include "../../runtime/Function.h"
#include "closure_signature.h"
#include "optimization_context.h"
#include "pir.h"

namespace rir {
namespace pir {

class Module {
    std::unordered_map<SEXP, Env*> environments;

  public:
    Env* getEnv(SEXP);

    void print(std::ostream& out = std::cout, bool tty = false);

    Closure* getOrDeclareRirFunction(const std::string& name, rir::Function* f,
                                     SEXP formals, SEXP src,
                                     const ClosureSignature& signature);
    Closure* getOrDeclareRirClosure(const std::string& name, SEXP closure,
                                    rir::Function* f,
                                    const ClosureSignature& signature);

    typedef std::function<void(pir::Closure*)> PirClosureIterator;
    typedef std::function<void(pir::ClosureVersion*)> PirClosureVersionIterator;
    void eachPirClosure(PirClosureIterator it);
    void eachPirClosureVersion(PirClosureVersionIterator it);

    ~Module();
  private:
    typedef std::pair<Function*, Env*> Idx;
    std::map<Idx, Closure*> closures;
};

}
}

#endif
