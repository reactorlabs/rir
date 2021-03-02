#ifndef COMPILER_CODE_H
#define COMPILER_CODE_H

#include "pir.h"

#include <cstddef>
#include <iostream>

namespace rir {
struct Code;

namespace pir {

enum class CodeTag : uint8_t {
    ClosureVersion,
    Promise,

    Invalid
};

/*
 * A piece of code, starting at the BB entry.
 *
 * Currently: either a Promise or a ClosureVersion.
 *
 */
class Code {
  public:
    CodeTag tag;
    BB* entry = nullptr;

    size_t nextBBId = 0;

    explicit Code(CodeTag tag = CodeTag::Invalid) : tag(tag) {}
    void printCode(std::ostream&, bool tty, bool omitDeoptBranches) const;
    void printGraphCode(std::ostream&, bool omitDeoptBranches) const;
    void printBBGraphCode(std::ostream&, bool omitDeoptBranches) const;
    virtual ~Code();

    virtual size_t size() const;

    virtual rir::Code* rirSrc() const = 0;
};

template <CodeTag CTAG, class Base>
class CodeImpl : public Code {
  public:
    CodeImpl() : Code(CTAG) {}
    static const Base* Cast(const Code* c) {
        if (c->tag == CTAG)
            return static_cast<const Base*>(c);
        return nullptr;
    }
    static Base* Cast(Code* c) {
        if (c->tag == CTAG)
            return static_cast<Base*>(c);
        return nullptr;
    }
};

} // namespace pir
} // namespace rir

#endif
