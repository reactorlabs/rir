#pragma once

#include "runtime/TypeFeedback.h"
#include "value.h"

namespace rir {
namespace pir {

class DeoptReasonWrapper : public Value {
  private:
    explicit DeoptReasonWrapper(const DeoptReason&);
    virtual ~DeoptReasonWrapper(){};

  public:
    const DeoptReason reason;
    static DeoptReasonWrapper* unknown();
    void printRef(std::ostream& out) const override final;

    friend class Module;
};

} // namespace pir
} // namespace rir
