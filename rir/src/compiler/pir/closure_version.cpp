#include "closure_version.h"
#include "../transform/bb.h"
#include "../util/visitor.h"
#include "closure.h"
#include "pir_impl.h"

#include <iostream>

namespace rir {
namespace pir {

void ClosureVersion::print(std::ostream& out, bool tty) const {
    out << *this << "\n";
    printCode(out, tty);
    for (auto p : promises) {
        if (p)
            p->print(out, tty);
    }
}

Promise* ClosureVersion::createProm(unsigned srcPoolIdx) {
    Promise* p = new Promise(this, promises.size(), srcPoolIdx);
    promises.push_back(p);
    return p;
}

ClosureVersion::~ClosureVersion() {
    for (auto p : promises)
        delete p;
}

ClosureVersion* ClosureVersion::clone(const Assumptions& newAssumptions) {
    auto ctx = optimizationContext;
    ctx.assumptions = ctx.assumptions | newAssumptions;
    auto c = closure->declareVersion(ctx);
    c->properties = properties;
    c->entry = BBTransform::clone(entry, c, c);
    return c;
}

size_t ClosureVersion::size() const {
    size_t s = 0;
    eachPromise([&s](Promise* p) { s += p->size(); });
    return s + Code::size();
}

size_t ClosureVersion::nargs() const { return closure->nargs(); }

const std::string& ClosureVersion::name() const {
    static std::string theName = "";
    if (theName.empty()) {
        auto id = std::stringstream();
        id << closure->name << " [" << this << "]";
        theName = id.str();
    }
    return theName;
}

} // namespace pir
} // namespace rir
