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
    for (auto p : promises_) {
        if (p)
            p->print(out, tty);
    }
}

Promise* ClosureVersion::createProm(unsigned srcPoolIdx) {
    Promise* p = new Promise(this, promises_.size(), srcPoolIdx);
    promises_.push_back(p);
    return p;
}

ClosureVersion::~ClosureVersion() {
    for (auto p : promises_)
        delete p;
}

ClosureVersion* ClosureVersion::clone(const Assumptions& newAssumptions) {
    auto ctx = optimizationContext_;
    ctx.assumptions = ctx.assumptions | newAssumptions;
    auto c = owner_->declareVersion(ctx);
    c->properties = properties;
    c->entry = BBTransform::clone(entry, c, c);
    return c;
}

void ClosureVersion::erasePromise(unsigned id) {
    auto p = promises_.at(id);
    assert(p && "Promise already deleted");

    // If we delete a corrupt promise it get's hard to debug...
    assert(p->owner == this);
    assert(promise(p->id) == p);

    delete promises_[id];
    promises_[id] = nullptr;
}

size_t ClosureVersion::size() const {
    size_t s = 0;
    eachPromise([&s](Promise* p) { s += p->size(); });
    return s + Code::size();
}

size_t ClosureVersion::nargs() const { return owner_->nargs(); }

ClosureVersion::ClosureVersion(Closure* closure,
                               const OptimizationContext& optimizationContext,
                               const Properties& properties)
    : owner_(closure), optimizationContext_(optimizationContext),
      properties(properties) {
    auto id = std::stringstream();
    id << closure->name() << "[" << this << "]";
    name_ = id.str();
    id.str("");
    id << this;
    nameSuffix_ = id.str();
}

} // namespace pir
} // namespace rir
