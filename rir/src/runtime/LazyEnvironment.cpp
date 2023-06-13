#include "LazyEnvironment.h"
#include "R/Protect.h"
#include "R/Serialize.h"
#include "utils/Pool.h"

namespace rir {

size_t LazyEnvironment::getArgIdx(SEXP n) const {
    size_t i = 0;
    while (i < nargs) {
        auto name = Pool::get(names[i]);
        if (TYPEOF(name) == LISTSXP)
            name = CAR(name);
        if (name == n)
            break;
        i++;
    }
    return i;
}

SEXP LazyEnvironment::getArg(SEXP n) const {
    auto i = getArgIdx(n);
    if (i == nargs)
        return R_UnboundValue;
    return getArg(i);
}

bool LazyEnvironment::isMissing(SEXP n) const {
    auto i = getArgIdx(n);
    if (i == nargs)
        return false;
    return isMissing(i);
}

bool LazyEnvironment::isMissing(size_t i) const {
    assert(i < nargs);
    return missing[i] || getArg(i) == R_MissingArg;
}

LazyEnvironment* LazyEnvironment::deserialize(SEXP refTable, R_inpstream_t inp) {
    Protect p;
    int size = InInteger(inp);
    int nargs = InInteger(inp);
    auto missing = new char[nargs];
    auto names = new Immediate[nargs];
    for (int i = 0; i < nargs; i++) {
        missing[i] = InChar(inp);
    }
    for (int i = 0; i < nargs; i++) {
        names[i] = Pool::readItem(refTable, inp);
    }
    SEXP materialized = p.nullable(ReadNullableItem(refTable, inp));
    SEXP parent = p.nullable(ReadNullableItem(refTable, inp));
    SEXP store = p(Rf_allocVector(EXTERNALSXP, size));
    auto le = new (DATAPTR(store)) LazyEnvironment(parent, nargs, names);
    le->materialized(materialized);
    for (int i = 0; i < nargs; i++) {
        le->missing[i] = missing[i];
        le->setArg(i, ReadNullableItem(refTable, inp), false);
    }
    delete[] missing;
    // names won't get deleted because its now owned by LazyEnvironment,
    // but does LazyEnvironment free when destroyed?
    return le;
}

void LazyEnvironment::serialize(SEXP refTable, R_outpstream_t out) const {
    OutInteger(out, (int)size());
    OutInteger(out, (int)nargs);
    for (int i = 0; i < (int)nargs; i++) {
        OutChar(out, missing[i]);
    }
    for (int i = 0; i < (int)nargs; i++) {
        Pool::writeItem(names[i], refTable, out);
    }
    WriteNullableItem(materialized(), refTable, out);
    // TODO: Why are getParent() and getArg(i) null after deopt in pir_regression_check_code.R?
    WriteNullableItem(getParent(), refTable, out);
    for (int i = 0; i < (int)nargs; i++) {
        WriteNullableItem(getArg((size_t)i), refTable, out);
    }
}

size_t LazyEnvironment::size() const {
    return sizeof(LazyEnvironment) + sizeof(char) * nargs +
           sizeof(SEXP) * (nargs + ArgOffset);
}


} // namespace rir
