#include "code.h"
#include "../util/visitor.h"
#include "pir_impl.h"

#include <stack>

namespace rir {
namespace pir {

void Code::print(std::ostream& out) const {
    BreadthFirstVisitor::run(entry, [&out](BB* bb) { bb->print(out); });
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
