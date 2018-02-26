#include "query.h"
#include "../pir/pir_impl.h"
#include "../util/visitor.h"

namespace rir {
namespace pir {

bool Query::pure(Code* c) {
    return Visitor::check(c->entry, [](BB* bb) {
        for (auto i : *bb)
            if (i->mightIO() || i->changesEnv())
                return false;
        return true;
    });
}

}
}
