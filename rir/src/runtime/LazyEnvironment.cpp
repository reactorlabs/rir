#include "LazyEnvironment.h"
#include "R/Protect.h"
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

LazyEnvironment* LazyEnvironment::deserialize(AbstractDeserializer& deserializer) {
    Protect p;
    auto size = deserializer.readBytesOf<R_xlen_t>();
    SEXP store = p(Rf_allocVector(EXTERNALSXP, size));
    deserializer.addRef(store);

    auto nargs = deserializer.readBytesOf<int>();
    auto missing = new char[nargs];
    auto names = new Immediate[nargs];
    for (int i = 0; i < nargs; i++) {
        missing[i] = deserializer.readBytesOf<char>();
    }
    for (int i = 0; i < nargs; i++) {
        names[i] = deserializer.readConst();
    }
    SEXP materialized = p.nullable(deserializer.readNullable());
    SEXP parent = p.nullable(deserializer.readNullable());
    auto le = new (DATAPTR(store)) LazyEnvironment(parent, nargs, names);
    le->materialized(materialized);
    for (int i = 0; i < nargs; i++) {
        le->missing[i] = missing[i];
        le->setArg(i, deserializer.readNullable(), false);
    }
    delete[] missing;
    // names won't get deleted because its now owned by LazyEnvironment,
    // but does LazyEnvironment free when destroyed?
    return le;
}

void LazyEnvironment::serialize(AbstractSerializer& serializer) const {
    serializer.writeBytesOf((R_xlen_t)size());
    serializer.writeBytesOf((int)nargs);
    for (int i = 0; i < (int)nargs; i++) {
        serializer.writeBytesOf(missing[i]);
    }
    for (int i = 0; i < (int)nargs; i++) {
        serializer.writeConst(names[i]);
    }
    serializer.writeNullable(materialized());
    // TODO: Why are getParent() and getArg(i) null after deopt in pir_regression_check_code.R?
    serializer.writeNullable(getParent());
    for (int i = 0; i < (int)nargs; i++) {
        serializer.writeNullable(getArg((size_t)i));
    }
}

void LazyEnvironment::hash(HasherOld& hasher) const {
    hasher.hashBytesOf(nargs);
    for (int i = 0; i < (int)nargs; i++) {
        hasher.hashBytesOf(missing[i]);
    }
    for (int i = 0; i < (int)nargs; i++) {
        hasher.hashConstant(names[i]);
    }
    hasher.hashNullable(materialized());
    // TODO: Why are getParent() and getArg(i) null after deopt in pir_regression_check_code.R?
    hasher.hashNullable(getParent());
    for (int i = 0; i < (int)nargs; i++) {
        hasher.hashNullable(getArg((size_t)i));
    }
}

void LazyEnvironment::addConnected(ConnectedCollectorOld& collector) const {
    for (int i = 0; i < (int)nargs; i++) {
        collector.addConstant(names[i]);
    }
    collector.addNullable(materialized(), false);
    // TODO: Why are getParent() and getArg(i) null after deopt in pir_regression_check_code.R?
    collector.addNullable(getParent(), false);
    for (int i = 0; i < (int)nargs; i++) {
        collector.addNullable(getArg((size_t)i), false);
    }
}

size_t LazyEnvironment::size() const {
    return sizeof(LazyEnvironment) + sizeof(char) * nargs +
           sizeof(SEXP) * (nargs + ArgOffset);
}


} // namespace rir
