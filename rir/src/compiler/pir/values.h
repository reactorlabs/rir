#pragma once

#include "ir/BC_inc.h"
#include "runtime/TypeFeedback.h"
#include "tag.h"
#include "value.h"

namespace rir {
namespace pir {

template <typename Base, Tag TAG>
class ValueImpl : public Value {
  public:
    explicit ValueImpl(PirType type) : Value(type, TAG) {}
    virtual ~ValueImpl(){};

    static const Base* Cast(const Value* i) {
        if (i->tag == TAG)
            return static_cast<const Base*>(i);
        return nullptr;
    }

    static Base* Cast(Value* i) {
        if (i->tag == TAG)
            return static_cast<Base*>(i);
        return nullptr;
    }
};

class DeoptReasonWrapper
    : public ValueImpl<DeoptReasonWrapper, Tag::DeoptReason> {
  private:
    explicit DeoptReasonWrapper(const DeoptReason&);

  public:
    const DeoptReason reason;
    static DeoptReasonWrapper* unknown();
    void printRef(std::ostream& out) const override final;

    friend class Module;
};

class Const : public ValueImpl<Const, Tag::Constant> {
  private:
    explicit Const(BC::PoolIdx idx, PirType type);
    BC::PoolIdx idx;

  public:
    void printRef(std::ostream& out) const override final;

    SEXP operator()() const;
    SEXP c() const;

    friend class Module;
};

} // namespace pir
} // namespace rir
