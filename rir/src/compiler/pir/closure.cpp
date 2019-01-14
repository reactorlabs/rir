#include "closure.h"
#include "../transform/bb.h"
#include "../util/visitor.h"
#include "pir_impl.h"

#include <iostream>

namespace rir {
namespace pir {

void Closure::print(std::ostream& out, bool tty) const {
    out << *this << "\n";
    printCode(out, tty);
    for (auto p : promises) {
        if (p)
            p->print(out, tty);
    }
}

Promise* Closure::createProm(unsigned srcPoolIdx) {
    Promise* p = new Promise(this, promises.size(), srcPoolIdx);
    promises.push_back(p);
    return p;
}

Closure::~Closure() {
    for (auto p : promises)
        delete p;
}

Closure* Closure::clone(const Assumptions& newAssumptions) {
    Closure* c = new Closure(name, argNames, env, function,
                             assumptions | newAssumptions, properties);
    c->entry = BBTransform::clone(entry, c, c);

    return c;
}

size_t Closure::size() const {
    size_t s = 0;
    eachPromise([&s](Promise* p) { s += p->size(); });
    return s + Code::size();
}

} // namespace pir
} // namespace rir
