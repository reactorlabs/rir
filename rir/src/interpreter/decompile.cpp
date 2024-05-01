#include "interpreter/instance.h"
#include "runtime/DispatchTable.h"
#include "runtime/Promise.h"

namespace rir {

SEXP rirDecompile(SEXP s) {
    if (auto p = Promise::check(s)) {
        return src_pool_at(p->code()->src);
    }
    if (auto c = Code::check(s)) {
        return src_pool_at(c->src);
    }
    if (auto f = Function::check(s)) {
        return src_pool_at(f->body()->src);
    }
    if (auto t = DispatchTable::check(s)) {
        // Default is the source of the first function in the dispatch table
        auto f = t->baseline();
        return src_pool_at(f->body()->src);
    }
    return s;
}

} // namespace rir
