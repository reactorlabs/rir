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

size_t Closure::promiseId(Code* c) const {
    for (size_t i = 0; i < promises.size(); ++i)
        if (promises[i] == c)
            return i;
    assert(false);
    return -1;
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

Closure* Closure::clone() {
    Closure* c = new Closure(name, argNames, env, function);

    // clone code
    c->entry = BBTransform::clone(entry, c);

    // clone promises
    std::unordered_map<Promise*, Promise*> promMap;
    for (auto p : promises) {
        if (!p)
            continue;
        Promise* clonedP = new Promise(c, c->promises.size(), p->srcPoolIdx);
        c->promises.push_back(clonedP);
        clonedP->entry = BBTransform::clone(p->entry, clonedP);
        promMap[p] = clonedP;
    }

    // fix promise references in body code and promise code
    Visitor::run(c->entry, [&](Instruction* i) {
        if (auto a = MkArg::Cast(i))
            a->updatePromise(promMap.at(a->prom()));
    });
    for (auto p : c->promises)
        Visitor::run(p->entry, [&](Instruction* i) {
            if (auto a = MkArg::Cast(i))
                a->updatePromise(promMap.at(a->prom()));
        });

    return c;
}

size_t Closure::size() const {
    size_t s = 0;
    eachPromise([&s](Promise* p) { s += p->size(); });
    return s + Code::size();
}

} // namespace pir
} // namespace rir
