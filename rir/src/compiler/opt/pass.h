#ifndef PIR_PASS_H
#define PIR_PASS_H

#include "../pir/module.h"
#include "compiler/log/stream_logger.h"
#include <string>

namespace rir {
namespace pir {

class Compiler;

class Pass {
  public:
    explicit Pass(const std::string& name) : name(name) {}

    virtual bool runOnPromises() const { return false; }

    bool apply(Compiler& cmp, ClosureVersion* function, LogStream& log) const;
    virtual bool apply(Compiler&, ClosureVersion*, Code*, LogStream&) const = 0;

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
