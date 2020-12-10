#ifndef COMPILER_CODE_H
#define COMPILER_CODE_H

#include "pir.h"

namespace rir {
namespace pir {

/*
 * A piece of code, starting at the BB entry.
 *
 * Currently: either a Promise or a function.
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

    virtual size_t size() const;

    virtual rir::Code* rirSrc() const = 0;
};

}
}

#endif
