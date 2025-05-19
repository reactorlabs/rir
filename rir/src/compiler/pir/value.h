#ifndef COMPILER_VALUE_H
#define COMPILER_VALUE_H

#include "observed_types.h"
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
    const PirType& type;

  protected:
    PirType type_;

  public:
    virtual void setType(const PirType& newType, OT::Origin origin,
                         OT::Opt opt = OT::None) {
        type_ = newType;
    }

    Value(const Value& other)
        : tag(other.tag), type(type_), type_(other.type_) {}

    Value(Value&& other)
        : tag(std::move(other.tag)), type(type_),
          type_(std::move(other.type_)) {}

    Value& operator=(const Value& other) {
        if (this == &other) {
            return *this;
        }

        this->tag = other.tag;
        this->type_ = other.type_;
        return *this;
    }

    Value& operator=(Value&& other) {
        if (this == &other) {
            return *this;
        }

        this->tag = std::move(other.tag);
        this->type_ = std::move(other.type_);
        return *this;
    }

    Value(PirType type, Tag tag) : tag(tag), type(type_), type_(type) {}
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
    virtual SEXP asRValue() const { return nullptr; }

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
// static_assert(sizeof(Value) <= 16, "");

} // namespace pir
} // namespace rir

#endif
