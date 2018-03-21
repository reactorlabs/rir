#include "function.h"
#include "pir_impl.h"

#include <iostream>

namespace rir {
namespace pir {

void Function::print(std::ostream& out) {
    out << "Function " << this << "\n";
    Code::print(out);
    for (auto p : promises) {
        if (p)
            p->print(out);
    }
}

Promise* Function::createProm() {
    Promise* p = new Promise(this, promises.size());
    promises.push_back(p);
    return p;
}

Function::~Function() {
    for (auto p : promises)
        if (p)
            delete p;
    for (auto p : defaultArgs)
        delete p;
}
}
}
