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
    UnboundValue() : SingletonValue(RType::unbound, Tag::UnboundValue) {}
};

class True : public SingletonValue<True> {
  public:
    void printRef(std::ostream& out) const override final { out << "true"; }

    SEXP asRValue() const override final { return R_TrueValue; }

  private:
    friend class SingletonValue;
    True() : SingletonValue(PirType::test(), Tag::True) {}
};

class False : public SingletonValue<False> {
  public:
    void printRef(std::ostream& out) const override final { out << "false"; }

    SEXP asRValue() const override final { return R_FalseValue; }

  private:
    friend class SingletonValue;
    False() : SingletonValue(PirType::test(), Tag::False) {}
};

class OpaqueTrue : public SingletonValue<OpaqueTrue> {
  public:
    void printRef(std::ostream& out) const override final {
        out << "opaqueTrue";
    }

    SEXP asRValue() const override final { return nullptr; }

  private:
    friend class SingletonValue;
    OpaqueTrue() : SingletonValue(PirType::test(), Tag::OpaqueTrue) {}
};

class NaLogical : public SingletonValue<NaLogical> {
  public:
    void printRef(std::ostream& out) const override final { out << "na-lgl"; }

    SEXP asRValue() const override final { return R_LogicalNAValue; }

  private:
    friend class SingletonValue;
    NaLogical()
        : SingletonValue(PirType::simpleScalarLogical(), Tag::NaLogical) {}
};

class Tombstone : public Value {
  public:
    void printRef(std::ostream& out) const override final {
        out << "~";
        if (this == closure())
            out << "cls";
        else if (this == framestate())
            out << "fs";
        else if (this == unreachable())
            out << "unreachable";
        else
            assert(false);
    }
    static Tombstone* closure() {
        static Tombstone cls(RType::closure);
        return &cls;
    }
    static Tombstone* framestate() {
        static Tombstone fs(NativeType::frameState);
        return &fs;
    }
    static Tombstone* unreachable() {
        static Tombstone dead(RType::nil);
        return &dead;
    }
    SEXP asRValue() const override final {
        assert(false && "This value is dead");
        return nullptr;
    }

  private:
    explicit Tombstone(PirType t) : Value(t, Tag::Tombstone) {}
};
} // namespace pir
} // namespace rir

#endif
