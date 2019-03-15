#include "code.h"
#include "../util/visitor.h"
#include "pir_impl.h"

#include <stack>

namespace rir {
namespace pir {

void Code::printCode(std::ostream& out, bool tty) const {
    BreadthFirstVisitor::run(entry, [&](BB* bb) { bb->print(out, tty); });
}

void Code::printGraphCode(std::ostream& out, bool tty) const {
    out << "digraph {\n";
    BreadthFirstVisitor::run(entry, [&](BB* bb) { bb->printGraph(out, tty); });
    out << "}\n";
}

void Code::printBBGraphCode(std::ostream& out) const {
    out << "digraph {\n";
    BreadthFirstVisitor::run(entry, [&](BB* bb) { bb->printBBGraph(out); });
    out << "}\n";
}

Code::~Code() {
    std::stack<BB*> toDel;
    Visitor::run(entry, [&toDel](BB* bb) { toDel.push(bb); });
    while (!toDel.empty()) {
        auto d = toDel.top();
        toDel.pop();
        assert(d->owner == this);
        delete d;
    }
}

size_t Code::size() const {
    size_t s = 0;
    Visitor::run(entry, [&](BB* bb) { s += bb->size(); });
    return s;
}

} // namespace pir
} // namespace rir
