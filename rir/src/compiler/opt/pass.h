#ifndef PIR_PASS_H
#define PIR_PASS_H

#include "../debugging/stream_logger.h"
#include "../pir/module.h"
#include <string>

namespace rir {
namespace pir {

class RirCompiler;
class Pass {
  public:
    explicit Pass(const std::string& name) : name(name) {}

    virtual bool runOnPromises() const { return false; }

    bool apply(RirCompiler& cmp, ClosureVersion* function,
               LogStream& log) const;
    virtual bool apply(RirCompiler&, ClosureVersion*, Code*,
                       LogStream&) const = 0;

    std::string getName() const { return this->name; }
    virtual ~Pass() {}
    virtual bool isPhaseMarker() const { return false; }
    virtual unsigned cost() const { return 1; }

  protected:
    std::string name;
};

} // namespace pir
} // namespace rir

#endif
