#include "LazyArglist.h"

namespace rir {

LazyArglist* LazyArglist::deserialize(SEXP refTable, R_inpstream_t inp) {
    (void)refTable;
    (void)inp;
    assert(false && "TODO LazyArglist::deserialize");
}

void LazyArglist::serialize(SEXP refTable, R_outpstream_t out) const {
    (void)this;
    (void)refTable;
    (void)out;
    assert(false && "TODO LazyArglist::serialize");
}

} // namespace rir