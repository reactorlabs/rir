#pragma once

#include "bc/BC_inc.h"
#include "runtime/TypeFeedback.h"
#include "tag.h"
#include "value.h"

namespace rir {
namespace pir {

class Instruction;

template <typename Base, Tag TAG>
class ValueImpl : public Value {
  public:
    explicit ValueImpl(PirType type) : Value(type, TAG) {}
    virtual ~ValueImpl() {}

    static const Base* Cast(const Instruction* i) {
        assert(false && "Non-sensical down-cast from instruction to value");
        return nullptr;
    }

    static Base* Cast(Instruction* i) {
        assert(false && "Non-sensical down-cast from instruction to value");
        return nullptr;
    }

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

    SEXP c() const;
    SEXP operator()() const { return c(); }
    SEXP asRValue() const override final { return c(); }

    friend class Module;
};

class Index : public ValueImpl<Index, Tag::Record> {
  private:
    explicit Index(unsigned idx);
    unsigned idx;
};

} // namespace pir
} // namespace rir
