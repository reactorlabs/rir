#include "code.h"
#include "../util/visitor.h"
#include "pir_impl.h"

#include <stack>

namespace rir {
namespace pir {

void Code::printCode(std::ostream& out, bool tty) const {
    BreadthFirstVisitor::run(entry, [&](BB* bb) { bb->print(out, tty); });
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

} // namespace pir
} // namespace rir
