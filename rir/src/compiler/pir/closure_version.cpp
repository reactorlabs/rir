#include "closure_version.h"
#include "closure.h"
#include "compiler/util/bb_transform.h"
#include "compiler/util/visitor.h"
#include "pir_impl.h"

#include <iostream>

namespace rir {
namespace pir {

void ClosureVersion::print(std::ostream& out, bool tty) const {
    print(DebugStyle::Standard, out, tty, false);
}

void ClosureVersion::print(DebugStyle style, std::ostream& out, bool tty,
                           bool omitDeoptBranches) const {
    switch (style) {
    case DebugStyle::Standard:
        printStandard(out, tty, omitDeoptBranches);
        break;
    case DebugStyle::GraphViz:
        printGraph(out, omitDeoptBranches);
        break;
    case DebugStyle::GraphVizBB:
        printBBGraph(out, omitDeoptBranches);
        break;
    default:
        assert(false);
    }
}

void ClosureVersion::printStandard(std::ostream& out, bool tty,
                                   bool omitDeoptBranches) const {
    out << *this << "\n";
    printCode(out, tty, omitDeoptBranches);
    for (auto p : promises_) {
        if (p) {
            out << "Prom " << p->id << ":\n";
            p->printCode(out, tty, omitDeoptBranches);
        }
    }
}

void ClosureVersion::printGraph(std::ostream& out,
                                bool omitDeoptBranches) const {
    out << "digraph {\n";
    out << "label=\"" << *this << "\";\n";
    printGraphCode(out, omitDeoptBranches);
    for (auto p : promises_) {
        if (p) {
            out << "subgraph p" << p->id << "{\n";
            out << "label = \"Promise " << p->id << "\";\n";
            p->printGraphCode(out, omitDeoptBranches);
            out << "}\n";
        }
    }
    out << "}\n";
}

void ClosureVersion::printBBGraph(std::ostream& out,
                                  bool omitDeoptBranches) const {
    out << "digraph {\n";
    out << "label=\"" << *this << "\";\n";
    printBBGraphCode(out, omitDeoptBranches);
    for (auto p : promises_) {
        if (p) {
            out << "subgraph {\n";
            out << "label=\"Promise " << p->id << "\";\n";
            p->printBBGraphCode(out, omitDeoptBranches);
            out << "}\n";
        }
    }
    out << "}\n";
}

Promise* ClosureVersion::createProm(SEXP expression) {
    Promise* p = new Promise(this, promises_.size(), expression);
    promises_.push_back(p);
    return p;
}

ClosureVersion::~ClosureVersion() {
    for (auto p : promises_) {
        if (p)
            delete p;
    }
}

ClosureVersion* ClosureVersion::clone(const Context& newAssumptions) {
    auto ctx = optimizationContext_ | newAssumptions;
    auto c = owner_->declareVersion(ctx, false, optFunction);
    c->properties = properties;
    c->entry = BBTransform::clone(entry, c, c);
    return c;
}

void ClosureVersion::erasePromise(unsigned id) {
    assert(promises_.at(id) && "Promise already deleted");

    // If we delete a corrupt promise it get's hard to debug...
    assert(promises_.at(id)->owner == this);
    assert(promise(promises_.at(id)->id) == promises_.at(id));

    delete promises_[id];
    promises_[id] = nullptr;
}

size_t ClosureVersion::numNonDeoptInstrs() const {
    size_t s = 0;
    VisitorNoDeoptBranch::run(entry, [&](BB* bb) { s += bb->size(); });
    return s;
}

size_t ClosureVersion::nargs() const { return owner_->nargs(); }
size_t ClosureVersion::effectiveNArgs() const {
    return owner_->nargs() - optimizationContext_.numMissing();
}

SEXP ClosureVersion::expression() const { return owner()->expression(); }

ClosureVersion::ClosureVersion(Closure* closure, rir::Function* optFunction,
                               bool root, const Context& optimizationContext,
                               const Properties& properties)
    : root(root), optFunction(optFunction), owner_(closure),
      optimizationContext_(optimizationContext), properties(properties) {
    auto id = std::stringstream();
    id << closure->name() << "[" << this << "]";
    name_ = id.str();
    id.str("");
    id << this;
    nameSuffix_ = id.str();
}

void ClosureVersion::printName(std::ostream& out) const { out << name(); }

std::ostream& operator<<(std::ostream& out, const ClosureVersion::Property& p) {
    switch (p) {
    case ClosureVersion::Property::IsEager:
        out << "Eager";
        break;
    case ClosureVersion::Property::NoReflection:
        out << "!Reflection";
        break;
    }
    return out;
}

std::ostream& operator<<(std::ostream& out,
                         const ClosureVersion::Properties& props) {
    for (auto p = props.begin(); p != props.end(); ++p) {
        out << *p;
        if ((p + 1) != props.end())
            out << ", ";
    }
    if (props.argumentForceOrder.size() > 0) {
        if (!props.empty())
            out << ", ";
        out << "ForceOrd: ";
        for (auto o = props.argumentForceOrder.begin();
             o != props.argumentForceOrder.end(); ++o) {
            out << *o;
            if ((o + 1) != props.argumentForceOrder.end())
                out << " ";
        }
    }
    return out;
}

} // namespace pir
} // namespace rir
