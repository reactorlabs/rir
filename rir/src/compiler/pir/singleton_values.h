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
};

class Nil : public SingletonValue<Nil> {
  public:
    void printRef(std::ostream& out) { out << "nil"; }

  private:
    friend class SingletonValue;
    Nil() : SingletonValue(RType::nil, Tag::Nil) {}
};

class Missing : public SingletonValue<Missing> {
  public:
    void printRef(std::ostream& out) { out << "missing"; }

  private:
    friend class SingletonValue;
    Missing() : SingletonValue(PirType::missing(), Tag::Missing) {}
};

class Tombstone : public Value {
  public:
    void printRef(std::ostream& out) {
        out << "~";
        if (this == closure())
            out << "cls";
        else if (this == framestate())
            out << "fs";
        else
            assert(false);
    }
    static Tombstone* closure() {
        static Tombstone cls(RType::closure);
        return &cls;
    }
    static Tombstone* framestate() {
        static Tombstone cls(NativeType::frameState);
        return &cls;
    }

  private:
    Tombstone(PirType t) : Value(t, Tag::Tombstone) {}
};
}
}

#endif
