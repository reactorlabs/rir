//
// Created by Jakob Hain on 10/23/23.
//

#include "ProxyEnv.h"
#include "R/Printing.h"
#include "R/r.h"
#include "serializeHash/globals.h"

namespace rir {

ProxyEnv::ProxyEnv(unsigned depth, unsigned depthToGlobal, SEXP global)
    : RirRuntimeObject(0, 0), depth(depth),
      depthToGlobal(depthToGlobal), global(global) {
    assert(depth < depthToGlobal && TYPEOF(global) == ENVSXP &&
           isGlobalEnv(global));
}

SEXP ProxyEnv::create(SEXP env) {
    if (globalsSet.count(env)) {
        return env;
    }
    auto global = ENCLOS(env);
    unsigned depthToGlobal = 1;
    // Every env has a global eventually (R_EmptyEnv is global)
    while (!globalsSet.count(global)) {
        depthToGlobal++;
        global = ENCLOS(global);
    }

    auto store = Rf_allocVector(EXTERNALSXP, sizeof(ProxyEnv));
    new (DATAPTR(store)) ProxyEnv(0, depthToGlobal, global);
    return store;
}

SEXP ProxyEnv::parent() const {
    if (depth + 1 == depthToGlobal) {
        return global;
    } else {
        auto store = Rf_allocVector(EXTERNALSXP, sizeof(ProxyEnv));
        new (DATAPTR(store)) ProxyEnv(depth + 1, depthToGlobal, global);
        return store;
    }
}

SEXP ProxyEnv::materialize(SEXP env) const {
    assert(TYPEOF(env) == ENVSXP);
    for (unsigned i = 0; i < depth; i++) {
        env = ENCLOS(env);
    }
    return env;
}

void ProxyEnv::print(std::ostream& out) const {
    out << "^" << depth << ", ^" << depthToGlobal << " is "
        << Print::dumpSexp(global) << "";
}

ProxyEnv* ProxyEnv::deserialize(AbstractDeserializer& deserializer) {
    auto depth = deserializer.readBytesOf<unsigned>();
    auto depthToGlobal = deserializer.readBytesOf<unsigned>();
    auto global = deserializer.read();
    auto store = Rf_allocVector(EXTERNALSXP, sizeof(ProxyEnv));
    new (DATAPTR(store)) ProxyEnv(depth, depthToGlobal, global);
    return unpack(store);
}

void ProxyEnv::serialize(AbstractSerializer& serializer) const {
    serializer.writeBytesOf(depth);
    serializer.writeBytesOf(depthToGlobal);
    serializer.write(global);
}

void ProxyEnv::hash(HasherOld& hasher) const {
    hasher.hashBytesOf(depth);
    hasher.hashBytesOf(depthToGlobal);
    hasher.hash(global);
}

void ProxyEnv::addConnected(ConnectedCollectorOld& collector) const {
    collector.add(global);
}

bool isGlobalEnv(SEXP env) {
    assert(TYPEOF(env) == ENVSXP && "only call this on environments");
    return globalsSet.count(env) || R_IsPackageEnv(env) || R_IsNamespaceEnv(env);
}

} // namespace rir