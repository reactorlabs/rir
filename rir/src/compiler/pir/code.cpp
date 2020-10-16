#include "code.h"
#include "../util/visitor.h"
#include "pir_impl.h"

#include <stack>

namespace rir {
namespace pir {

void Code::printCode(std::ostream& out, bool tty,
                     bool omitDeoptBranches) const {
    BreadthFirstVisitor::run(entry, [&](BB* bb) {
        if (omitDeoptBranches &&
            (bb->isDeopt() || (bb->isJmp() && bb->next()->isDeopt())))
            return;
        bb->print(out, tty);
    });
}

void Code::printGraphCode(std::ostream& out, bool omitDeoptBranches) const {
    BreadthFirstVisitor::run(entry, [&](BB* bb) {
        if (omitDeoptBranches &&
            (bb->isDeopt() || (bb->isJmp() && bb->next()->isDeopt())))
            return;
        bb->printGraph(out, omitDeoptBranches);
    });
}

void Code::printBBGraphCode(std::ostream& out, bool omitDeoptBranches) const {
    BreadthFirstVisitor::run(entry, [&](BB* bb) {
        if (omitDeoptBranches &&
            (bb->isDeopt() || (bb->isJmp() && bb->next()->isDeopt())))
            return;
        bb->printBBGraph(out, omitDeoptBranches);
    });
}

Code::~Code() {
    std::stack<BB*> toDel;
    if (entry)
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
