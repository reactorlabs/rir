#ifndef IR_H
#define IR_H

#include "llvm.h"

#include "RIntlns.h"

#include "Builder.h"

namespace rjit {
namespace ir {

enum class Type {
#include "irTypes.h"
    ret,
    br,
    cbr,
    cmp,
};

/** Generic class for all IR objects.

  They all must point to an existing llvm value.
 */
class Instruction {
public:

    Type match(llvm::BasicBlock::iterator & i) {
    }


protected:
    Instruction(llvm::Instruction * ins):
        ins_(ins) {
    }

    template<typename T>
    T * ins() {
        return llvm::cast<T>(ins_);
    }

private:
    llvm::Instruction * ins_;

};

/** Base class for all intrinsics.

 */
class Intrinsic : public Instruction {
public:
    /** LLVM metadata kind for the ir type associated with the CallInsts for faster matching.
     */
    static char const * const MD_NAME;

    /** Returns the IR type of the intrinsic call for faster matching.
     */
    Type getIRType() {
        llvm::APInt const & ap = llvm::cast<llvm::ConstantInt>(llvm::cast<llvm::ValueAsMetadata>(ins()->getMetadata(MD_NAME))->getValue())->getUniqueInteger();
        assert(ap.isIntN(32) and "Expected 32bit integer");
        return static_cast<Type>(ap.getSExtValue());
    }

    /** Returns the CallInst associated with the intrinsic.
     */
    llvm::CallInst * ins() {
        return Instruction::ins<llvm::CallInst>();
    }

protected:

    Intrinsic(llvm::CallInst * ins):
        Instruction(ins) {
    }

    /** Sets the ir kind for the CallInst.

      It is assumed that this method will be called by the respective intrinsics when they are being created.
     */
    static void setIRType(llvm::CallInst * ins, Type t) {
        std::vector<llvm::Metadata *> v = { llvm::ValueAsMetadata::get(llvm::ConstantInt::get(ins->getContext(), llvm::APInt(32, static_cast<int>(t)))) };
        llvm::MDNode * m = llvm::MDNode::get(ins->getContext(), v);
        ins->setMetadata(MD_NAME, m);
    }

    llvm::Value * getValue(unsigned argIndex) {
        return ins()->getArgOperand(argIndex);
    }

    SEXP getValueSEXP(unsigned argIndex) {
        return nullptr;

    }

    int getValueInt(unsigned argIndex) {
        return Builder::integer(ins()->getArgOperand(argIndex));
    }


};

} // namespace ir

} // namespace rjit


#endif // IR_H

