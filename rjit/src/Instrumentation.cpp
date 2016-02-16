#include "Instrumentation.h"

#include "RIntlns.h"
#include "TypeInfo.h"

#include <iostream>
#include <cassert>

namespace rjit {

TypeFeedback::TypeFeedback(SEXP store) : store(store) {
    assert(TYPEOF(store) == INTSXP);
}

void TypeFeedback::record(SEXP value, int idx) {
    assert(idx < XLENGTH(store));

    TypeInfo old_info(INTEGER(store)[idx]);
    TypeInfo info(INTEGER(store)[idx]);

    info.mergeAll(value);

    if (old_info != info) {
        INTEGER(store)[idx] = info;
    }
}
}

extern "C" void recordType(SEXP value, SEXP store, int idx) {
    rjit::TypeFeedback record(store);
    record.record(value, idx);
}
