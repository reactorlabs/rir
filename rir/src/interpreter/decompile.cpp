#include "R/Funtab.h"
#include "R/RList.h"
#include "R/Symbols.h"
#include "compiler/compiler.h"
#include "compiler/parameter.h"
#include "interp_incl.h"
#include "runtime/Deoptimization.h"
#include "runtime/DispatchTable.h"
#include "runtime/LazyArglist.h"
#include "runtime/LazyEnvironment.h"
#include "utils/Pool.h"

#include <R/r.h>
#include <assert.h>

namespace rir {

SEXP rirDecompile(SEXP s) {
    if (auto c = Code::check(s)) {
        return src_pool_at(c->src);
    }
    if (auto f = Function::check(s)) {
        return src_pool_at(f->body()->src);
    }
    if (auto t = DispatchTable::check(s)) {
        // Default is the source of the first function in the dispatch table
        Function* f = t->baseline();
        return src_pool_at(f->body()->src);
    }
    return s;
}

}; // namespace rir
