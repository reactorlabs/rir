#ifndef BUILDER_H
#define BUILDER_H

#include "RIntlns.h"
#include "llvm.h"

#include "Types.h"
#include "StackMap.h"

namespace rjit {
namespace ir {

/** Helper class that aids with building and modifying LLVM IR for functions.

  The interface provided is intended for the intrinsic wrappers.

  */
class Builder {
public:




    /** Returns the llvm::Function corresponding to the intrinsic of given name. If such intrinsic is not present in the module yet, it is declared using the given type.

      NOTE that this function assumes that the intrinsic does not use varargs.
     */
    llvm::Function * intrinsic(char const * name, llvm::FunctionType * type) {
        llvm::Function * result = m_->getFunction(name);
        // if the intrinsic has not been declared, declare it
        if (result == nullptr)
            result = llvm::Function::Create(type, llvm::GlobalValue::ExternalLinkage, name, m_);
        return result;
    }

    /** Builder can typecast to the current module.
     */
    operator llvm::Module * () {
        return m_;
    }

    /** Builder can typecast to the current function.
     */
    operator llvm::Function * () {
        return c_->f;
    }

    /** Builder can typecast to current basic block.
     */
    operator llvm::BasicBlock * () {
        return c_->b;
    }

    /** Returns a llvm::Value created from a constant pool value.

      Note that we do not need to add the constant to the constant pool because the first constant pool element is the whole AST and the understanding is that any constants used are just subtrees of the original AST. In the rare cases when this is not true (new promises being created), they must be inserted to the constant pool explicitly.
     */
    static llvm::Value * constantPoolSexp(SEXP value) {
        return llvm::ConstantExpr::getCast(
            llvm::Instruction::IntToPtr,
            llvm::ConstantInt::get(llvm::getGlobalContext(), llvm::APInt(64, (std::uint64_t)value)),
            t::SEXP);
    }

    /** Given a llvm::Value *, returns the SEXP from constant pool it points to.
     */
    static SEXP * constantPoolSexp(llvm::Value * value) {
        // TODO this is not working yet
        return nullptr;
    }

    /** Converts integer constant to a llvm::Value.
     */
    static llvm::ConstantInt * integer(int value) {
        return llvm::ConstantInt::get(llvm::getGlobalContext(), llvm::APInt(32, value));
    }

    /** Given an llvm::Value * that is a constant integer, returns the constant integer associated with it.
     */
    static int integer(llvm::Value * value) {
        llvm::APInt const & ap = llvm::cast<llvm::ConstantInt>(value)->getUniqueInteger();
        assert(ap.isIntN(32) and "Expected 32bit integer");
        return ap.getSExtValue();
    }

    /** Given a call instruction, sets its attributes wrt stack map statepoints.
     */
    llvm::CallInst * insertCall(llvm::CallInst * f) {
        if (c_->functionId != (uint64_t)-1) {
            assert(c_->functionId > 1);
            assert(c_->functionId < StackMap::nextStackmapId);

            llvm::AttributeSet PAL;
            {
                llvm::SmallVector<llvm::AttributeSet, 4> Attrs;
                llvm::AttributeSet PAS;
                {
                    llvm::AttrBuilder B;
                    B.addAttribute("statepoint-id", std::to_string(c_->functionId));
                    PAS = llvm::AttributeSet::get(m_->getContext(), ~0U, B);
                }
                Attrs.push_back(PAS);
                PAL = llvm::AttributeSet::get(m_->getContext(), Attrs);
            }
            f->setAttributes(PAL);
        }
        return f;
    }





private:
    class Context {
    public:


        /** Adds the given object into the constant pool served by the objects field.
         */
        void addObject(SEXP object) {
            PROTECT(object);
            objects.push_back(object);
        }

        bool isReturnJumpNeeded;
        bool isResultVisible;

        llvm::Function * f;
        llvm::BasicBlock * b;

        llvm::BasicBlock * breakTarget;
        llvm::BasicBlock * nextTarget;

        llvm::Value * rho;

        unsigned functionId;

        std::vector<SEXP> objects;
    };

    /** The module into which we are currently building.
     */
    llvm::Module * m_;

    Context * c_;

};

} // namespace ir
} // namespace rjit

#endif // BUILDER_H

