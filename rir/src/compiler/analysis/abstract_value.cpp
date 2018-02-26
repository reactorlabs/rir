#include "../pir/pir_impl.h"
#include "query.h"
#include "scope.h"

namespace rir {
namespace pir {

AbstractValue::AbstractValue() : type(PirType::bottom()) {}
AbstractValue::AbstractValue(Value* v, Instruction* o) : type(v->type) {
    vals.insert(ValOrig(v, o));
}

bool AbstractValue::merge(const AbstractValue& other) {
    assert(other.type != PirType::bottom());

    if (unknown)
        return false;
    if (type == PirType::bottom()) {
        *this = other;
        return true;
    }

    bool changed = false;
    if (!std::includes(vals.begin(), vals.end(), other.vals.begin(),
                       other.vals.end())) {
        vals.insert(other.vals.begin(), other.vals.end());
        changed = true;
    }

    return changed;
}

void AbstractValue::print(std::ostream& out) {
    if (unknown) {
        out << "??";
        return;
    }
    out << "(";
    for (auto it = vals.begin(); it != vals.end();) {
        auto vo = *it;
        vo.first->printRef(out);
        out << "@";
        vo.second->printRef(out);
        it++;
        if (it != vals.end())
            out << "|";
    }
    out << ") : " << type;
}
}
}
