#ifndef COMPILER_VALUE_H
#define COMPILER_VALUE_H

#include "type.h"

#include <functional>
#include <iostream>

namespace rir {
namespace pir {

enum class Tag : uint8_t;

class BB;
class Code;

/*
 * A typed PIR value.
 *
 * Has a tag from either value_list.h or instruction_list.h
 *
 */
class Value {
  public:
    PirType type;
    PirType typeFeedback = PirType::optimistic();
    Tag tag;
    Value(PirType type, Tag tag) : type(type), tag(tag) {}
    virtual void printRef(std::ostream& out) const = 0;
    void printRef() const { printRef(std::cerr); }
    virtual bool isInstruction() { return false; }
    virtual const Value* cFollowCasts() const { return this; }
    virtual const Value* cFollowCastsAndForce() const { return this; }
    Value* followCasts() {
        return const_cast<Value*>(
            const_cast<const Value*>(this)->cFollowCasts());
    }
    Value* followCastsAndForce() {
        return const_cast<Value*>(
            const_cast<const Value*>(this)->cFollowCastsAndForce());
    }
    virtual bool validIn(Code* code) const { return true; }
    virtual SEXP asRValue() const {
        assert(false && "Not a singleton");
        return nullptr;
    }

    bool producesRirResult() const {
        return type != PirType::voyd() && type != NativeType::context;
    }
};

} // namespace pir
} // namespace rir

#endif
