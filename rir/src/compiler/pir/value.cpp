#include "value.h"
#include "pir_impl.h"

namespace rir {
namespace pir {

void Value::printRef(std::ostream& out) {
    switch (tag) {
#define V(Name)                                                                \
    case Tag::Name:                                                            \
        static_cast<Name*>(this)->printRef(out);                               \
        break;
        COMPILER_INSTRUCTIONS(V);
#undef V
#define V(Name)                                                                \
    case Tag::Name:                                                            \
        static_cast<Name*>(this)->printRef(out);                               \
        break;
        COMPILER_VALUES(V);
#undef V
    case Tag::Unused:
        assert(false);
    };
}

bool Value::isInstruction() {
    switch (tag) {
#define V(Name) case Tag::Name:
        COMPILER_INSTRUCTIONS(V);
        return true;
#undef V
#define V(Name) case Tag::Name:
        COMPILER_VALUES(V);
        return false;
#undef V
    case Tag::Unused:
        assert(false);
    };
    assert(false);
    return false;
}

Value* Value::replaceRefs(Value* from, Value* to) {
    if (tag == Tag::ClosureWrapper) {
        auto c = ClosureWrapper::Cast(this);
        assert(c);
        if (c->env == from)
            return new ClosureWrapper(c->fun, to);
    }
    return this;
}
}
}
