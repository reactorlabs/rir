#include "code.h"
#include "../util/visitor.h"
#include "pir_impl.h"

#include <stack>

namespace rir {
namespace pir {

Code::Code() {}

void Code::print(std::ostream& out) {
    BreadthFirstVisitor::run(entry, [&out](BB* bb) { bb->print(out); });
}

Code::~Code() {
    std::stack<BB*> toDel;
    Visitor::run(entry, [&toDel](BB* bb) { toDel.push(bb); });
    while (!toDel.empty()) {
        assert(toDel.top()->owner == this);
        delete toDel.top();
        toDel.pop();
    }
}
}
}
