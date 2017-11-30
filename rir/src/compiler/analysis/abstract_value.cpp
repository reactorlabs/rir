#include "../pir/pir_impl.h"
#include "query.h"
#include "scope.h"

namespace rir {
namespace pir {

AbstractValue::AbstractValue() : type(PirType::bottom()) {}
AbstractValue::AbstractValue(Value* v, Instruction* o) : type(v->type) {
    vals.insert(v);
    origin.insert(o);
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
    if (!std::includes(origin.begin(), origin.end(), other.origin.begin(),
                       other.origin.end())) {
        origin.insert(other.origin.begin(), other.origin.end());
        changed = true;
    }

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
        (*it)->printRef(out);
        it++;
        if (it != vals.end())
            out << "|";
    }
    out << ") : " << type;
    out << ", @(";
    for (auto it = origin.begin(); it != origin.end();) {
        (*it)->printRef(out);
        it++;
        if (it != origin.end())
            out << "|";
    }
    out << ")";
}
}
}
