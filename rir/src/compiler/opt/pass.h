#ifndef PIR_PASS_H
#define PIR_PASS_H

#include "../pir/module.h"
#include <string>

namespace rir {
namespace pir {

class Compiler;
class PassLog;

class Pass {
  public:
    explicit Pass(const std::string& name) : name(name) {}

    virtual bool runOnPromises() const { return false; }
    virtual bool isSlow() const { return false; }

    bool apply(Compiler& cmp, ClosureVersion* function, PassLog& log,
               size_t iteration) const;
    virtual bool apply(Compiler& cmp, ClosureVersion*, Code*, PassLog&,
                       size_t iteration) const = 0;

    std::string getName() const { return this->name; }
    bool changedAnything() const { return changedAnything_; }
    virtual ~Pass() {}
    virtual bool isPhaseMarker() const { return false; }
    virtual unsigned cost() const { return 1; }

  protected:
    std::string name;
    mutable bool changedAnything_ = false;
};

} // namespace pir
} // namespace rir

#endif
