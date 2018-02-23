#include "closure_wrapper.h"
#include "pir_impl.h"

namespace rir {
namespace pir {
ClosureWrapper::ClosureWrapper(Function* fun, Value* env)
    : Value(RType::closure, Tag::ClosureWrapper), fun(fun), env(env) {}
void ClosureWrapper::printRef(std::ostream& out) {
    out << "close(" << *fun << ", ";
    env->printRef(out);
    out << ")";
}
}
}
