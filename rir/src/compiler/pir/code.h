#ifndef COMPILER_CODE_H
#define COMPILER_CODE_H

#include "R/r_incl.h"
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

    virtual void printName(std::ostream& out) const = 0;
    virtual SEXP expression() const = 0;
    size_t numInstrs() const;
};

} // namespace pir
} // namespace rir

#endif
