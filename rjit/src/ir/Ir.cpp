#include "Ir.h"

using namespace llvm;

namespace rjit {
namespace ir {

char const* const Pattern::MD_NAME = "r_ir_type";

llvm::Instruction * const ir::Pattern::Sentinel::singleton = (new ir::Nop())->ins_;

} // namespace ir

} // namespace rjit
