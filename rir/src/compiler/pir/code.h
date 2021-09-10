#ifndef COMPILER_CODE_H
#define COMPILER_CODE_H

#include "pir.h"

#include <cstddef>
#include <iostream>

namespace rir {
struct Code;

namespace pir {

/*
 * A piece of code, starting at the BB entry.
 *
 * Currently: either a Promise or a ClosureVersion.
 *
 */
class Code {
  public:
    BB* entry = nullptr;

    size_t nextBBId = 0;

    Code() {}
    void printCode(std::ostream&, bool tty, bool omitDeoptBranches) const;
    void printGraphCode(std::ostream&, bool omitDeoptBranches) const;
    void printBBGraphCode(std::ostream&, bool omitDeoptBranches) const;
    virtual ~Code();

    size_t numInstrs() const;

    virtual rir::Code* rirSrc() const = 0;
    virtual void printName(std::ostream&) const = 0;

    friend std::ostream& operator<<(std::ostream& out, const Code& e) {
        e.printName(out);
        return out;
    }
};

} // namespace pir
} // namespace rir

#endif
