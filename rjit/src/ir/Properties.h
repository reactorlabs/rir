#ifndef IR_PROPERTIES_H
#define IR_PROPERTIES_H

#include "llvm.h"

namespace rjit {
namespace ir {

class Pattern;

class Property {
public:
    virtual llvm::Instruction * first() const = 0;
    virtual llvm::Instruction * last() const = 0;

    ir::Pattern * pattern();
};


/** Pattern's result is scalar iff its inputs are scalar
 */
class BinaryOperator: public Property {
public:
    virtual llvm::Value * lhs() = 0;
    virtual llvm::Value * rhs() = 0;
};


} // namespace ir
} // namespace rjit

#endif // IR_PROPERTIES_H
