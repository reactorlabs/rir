#ifndef PIR_CLOSURE_WRAPPER
#define PIR_CLOSURE_WRAPPER

#include "tag.h"
#include "value.h"

namespace rir {
namespace pir {

class Function;

// Closure wrapper allows one to create a closure inside of a MkEnv
// instruction. So for example:
//   e = MkEnv(...)
//   c = MkCls(f, e)
//       StVar("f", c, e)
// is equivalent to:
//   e = MkEnv(..., "f"= ClosureWrapper(f))
class ClosureWrapper : public Value {
  public:
    Function* fun;
    Value* env;
    ClosureWrapper(Function* fun, Value* env);
    void printRef(std::ostream& out);
    static ClosureWrapper* Cast(Value* v) {
        if (v->tag == Tag::ClosureWrapper)
            return static_cast<ClosureWrapper*>(v);
        return nullptr;
    }
};
}
}

#endif
