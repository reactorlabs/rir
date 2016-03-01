#include "Ir.h"
#include "ir/Pass.h"

using namespace llvm;

namespace rjit {
namespace ir {

char const* const Pattern::MD_NAME = "r_ir_type";

llvm::Instruction* const ir::Pattern::Sentinel::singleton =
    (new ir::Nop())->ins_;

llvm::Value* Predicate::constantPool(ir::Pass& p) { return p.constantPool; }

bool GetVectorElement::FromConstantPool::match(ir::Pass& p,
                                               GetVectorElement* vge) {
    if (vge->vector() == constantPool(p) and
        llvm::isa<llvm::ConstantInt>(vge->index())) {
        index_ = Builder::integer(vge->index());
        llvm::Function* f = vge->first()->getParent()->getParent();
        JITModule* m = static_cast<JITModule*>(f->getParent());
        value_ = VECTOR_ELT(m->constPool(f), index_);
        return true;
    } else {
        return false;
    }
}

} // namespace ir

} // namespace rjit
