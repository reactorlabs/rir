#ifndef COMPILER_PROMISE_H
#define COMPILER_PROMISE_H

#include "code.h"

namespace rir {
namespace pir {

class LdFunctionEnv;

class Promise : public Code {
  public:
    const unsigned id;
    ClosureVersion* owner;

    unsigned srcPoolIdx() const;
    rir::Code* rirSrc() const override final { return rirSrc_; }

    LdFunctionEnv* env() const;

    bool trivial() const;

    void printName(std::ostream& out) const override;

    bool isPromise() const override final { return true; }

  private:
    rir::Code* rirSrc_;
    const unsigned srcPoolIdx_;
    friend class ClosureVersion;
    Promise(ClosureVersion* owner, unsigned id, rir::Code* rirSrc);
};

} // namespace pir
} // namespace rir

#endif
