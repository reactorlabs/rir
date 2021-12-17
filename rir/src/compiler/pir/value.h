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
class Instruction;

/*
 * A typed PIR value.
 *
 * Has a tag from either value_list.h or instruction_list.h
 *
 */
class Value {
  public:
    Tag tag;
    PirType type;

    Value(PirType type, Tag tag) : tag(tag), type(type) {}
    virtual void printRef(std::ostream& out) const = 0;
    void printRef() const { printRef(std::cerr); }
    virtual const Value* cFollowCasts() const { return this; }
    virtual const Value* cFollowCastsAndForce() const { return this; }
    virtual const Value* cFollowDownCastsAndForce() const { return this; }
    Value* followCasts() {
        return const_cast<Value*>(
            const_cast<const Value*>(this)->cFollowCasts());
    }
    Value* followCastsAndForce() {
        return const_cast<Value*>(
            const_cast<const Value*>(this)->cFollowCastsAndForce());
    }
    Value* followDownCastsAndForce() {
        return const_cast<Value*>(
            const_cast<const Value*>(this)->cFollowDownCastsAndForce());
    }
    virtual bool validIn(Code* code) const { return true; }
    virtual SEXP asRValue() const {
        return nullptr;
    }

    static constexpr int MAX_REFCOUNT = 2;

    virtual int minReferenceCount() const {
        return type.maybeReferenceCounted() ? 0 : MAX_REFCOUNT;
    }

    void callArgTypeToContext(Context&, unsigned arg) const;

    void checkReplace(Value* replace) const;

    virtual void replaceUsesIn(
        Value* val, BB* target,
        const std::function<void(Instruction*, size_t)>& postAction =
            [](Instruction*, size_t) {},
        const std::function<bool(Instruction*)>& replaceOnly =
            [](Instruction*) { return true; });
};
static_assert(sizeof(Value) <= 16, "");

} // namespace pir
} // namespace rir

#endif
