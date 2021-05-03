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

    LdFunctionEnv* env() const;

    bool trivial() const;

    SEXP expression() const override final { return expression_; }

    void printName(std::ostream& out) const override final;

  private:
    SEXP expression_;
    friend class ClosureVersion;
    Promise(ClosureVersion* owner, unsigned id, SEXP expression);
};

} // namespace pir
} // namespace rir

#endif
