#ifndef PIR_MODULE_H
#define PIR_MODULE_H

#include "R/r.h"
#include "runtime/TypeFeedback.h"
#include <functional>
#include <iostream>
#include <map>
#include <unordered_map>
#include <vector>

#include "pir.h"
#include "runtime/Function.h"

namespace rir {
namespace pir {

class DeoptReasonWrapper;

class Module {
    std::unordered_map<SEXP, Env*> environments;

  public:
    Env* getEnv(SEXP);

    void print(std::ostream& out = std::cout, bool tty = false);

    Closure* getOrDeclareRirFunction(const std::string& name, rir::Function* f,
                                     SEXP formals, SEXP src,
                                     Context userContext);
    Closure* getOrDeclareRirClosure(const std::string& name, SEXP closure,
                                    rir::Function* f, Context userContext);

    typedef std::function<void(pir::Closure*)> PirClosureIterator;
    typedef std::function<void(pir::ClosureVersion*)> PirClosureVersionIterator;
    void eachPirClosure(PirClosureIterator it);
    void eachPirClosureVersion(PirClosureVersionIterator it);

    DeoptReasonWrapper* deoptReasonValue(const DeoptReason&);
    Value* c(SEXP s);
    Const* c(int s);
    Const* c(double s);

    ~Module();
  private:
    typedef std::pair<Function*, Env*> Idx;
    std::map<Idx, Closure*> closures;

    Const* c(BC::PoolIdx, PirType t);
    std::unordered_map<DeoptReason, DeoptReasonWrapper*> deoptReasons;
    std::unordered_map<BC::PoolIdx, Const*> constants;
};

}
}

#endif
