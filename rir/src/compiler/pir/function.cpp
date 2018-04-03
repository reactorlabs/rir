#include "function.h"
#include "../transform/bb.h"
#include "../util/visitor.h"
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

Function* Function::clone() {
    Function* c = new Function(argNames);

    // clone code
    c->entry = BBTransform::clone(entry, c);

    // clone promises
    std::unordered_map<Promise*, Promise*> promMap;
    for (auto p : promises) {
        if (!p)
            continue;
        Promise* clonedP = new Promise(c, c->promises.size());
        c->promises.push_back(clonedP);
        clonedP->entry = BBTransform::clone(p->entry, clonedP);
        promMap[p] = clonedP;
    }

    // fix promise references in body code and promise code
    Visitor::run(c->entry, [&](Instruction* i) {
        auto a = MkArg::Cast(i);
        if (a)
            a->prom = promMap[a->prom];
    });
    for (auto p : c->promises)
        Visitor::run(p->entry, [&](Instruction* i) {
            auto a = MkArg::Cast(i);
            if (a)
                a->prom = promMap[a->prom];
        });

    return c;
}
}
}
