#ifndef COMPILER_SINGLETON_VALUES_H
#define COMPILER_SINGLETON_VALUES_H

#include "instruction_list.h"
#include "tag.h"
#include "value.h"

#include <functional>
#include <iostream>

namespace rir {
namespace pir {

template <typename T>
class SingletonValue : public Value {
  protected:
    SingletonValue(PirType t, Tag tag) : Value(t, tag) {}

  public:
    SingletonValue(const SingletonValue&) = delete;
    SingletonValue& operator=(const SingletonValue&) = delete;

    static T* instance() {
        static T i;
        return &i;
    }

    SEXP asRValue() const override = 0;
};

class Nil : public SingletonValue<Nil> {
  public:
    void printRef(std::ostream& out) const override final { out << "nil"; }
    SEXP asRValue() const override final { return R_NilValue; }

  private:
    friend class SingletonValue;
    Nil() : SingletonValue(RType::nil, Tag::Nil) {}
};

class MissingArg : public SingletonValue<MissingArg> {
  public:
    void printRef(std::ostream& out) const override final {
        out << "missingArg";
    }
    SEXP asRValue() const override final { return R_MissingArg; }

  private:
    friend class SingletonValue;
    MissingArg() : SingletonValue(RType::missing, Tag::MissingArg) {}
};

class UnboundValue : public SingletonValue<UnboundValue> {
  public:
    void printRef(std::ostream& out) const override final {
        out << "unboundValue";
    }

    SEXP asRValue() const override final { return R_UnboundValue; }

  private:
    friend class SingletonValue;
    UnboundValue()
        : SingletonValue(PirType(RType::unbound).nonLazy(), Tag::UnboundValue) {
    }
};

class True : public SingletonValue<True> {
  public:
    void printRef(std::ostream& out) const override final { out << "true"; }

    SEXP asRValue() const override final { return R_TrueValue; }

  private:
    friend class SingletonValue;
    True() : SingletonValue(NativeType::test, Tag::True) {}
};

class False : public SingletonValue<False> {
  public:
    void printRef(std::ostream& out) const override final { out << "false"; }

    SEXP asRValue() const override final { return R_FalseValue; }

  private:
    friend class SingletonValue;
    False() : SingletonValue(NativeType::test, Tag::False) {}
};

class Tombstone : public Value {
  public:
    void printRef(std::ostream& out) const override final {
        out << "~";
        if (this == closure())
            out << "cls";
        else if (this == framestate())
            out << "fs";
        else
            assert(false);
    }
    static Tombstone* closure() {
        static Tombstone cls(PirType(RType::closure).nonLazy());
        return &cls;
    }
    static Tombstone* framestate() {
        static Tombstone fs(NativeType::frameState);
        return &fs;
    }
    SEXP asRValue() const override final {
        assert(false && "This value is dead");
    }

  private:
    explicit Tombstone(PirType t) : Value(t, Tag::Tombstone) {}
};
} // namespace pir
} // namespace rir

#endif
