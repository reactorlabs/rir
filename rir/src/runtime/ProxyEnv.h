//
// Created by Jakob Hain on 10/23/23.
//

#pragma once

#include "R/r_incl.h"
#include "RirRuntimeObject.h"
#include "serializeHash/hash/getConnectedOld.h"
#include "serializeHash/hash/hashRootOld.h"
#include "serializeHash/serializeUni.h"
#include <iostream>

namespace rir {

#define PROXY_ENV_MAGIC 0xeeee1702

/// Proxy for an ENVSEXP that exists in the compiler client, so we don't have to
///   send the entire environment (it may be very large and have other compiled
///   closures). The main reason we use "proxy" instead of "stub" is because
///   "stub env" already refers to something else

class ProxyEnv :
    public RirRuntimeObject<ProxyEnv, PROXY_ENV_MAGIC> {
    /// 0 if this is the env of the closure being compiled, 1 if its the parent,
    /// 2 if ancestor, etc.
    unsigned depth;
    /// Depth to get to nearest global (typically `R_GlobalEnv`)
    unsigned depthToGlobal;
    /// Nearest global (typically `R_GlobalEnv`)
    SEXP global;

    ProxyEnv(unsigned depth, unsigned depthToGlobal, SEXP global);
  public:
    /// Create an ENVSXP stubbing the given closure environment
    static SEXP create(SEXP env);
    /// The proxy's parent environment
    SEXP parent() const;

    /// Convert back into a regular env, given the closure environment it was
    /// originally created from (the closure environment can't be stored inside
    /// this because we want to send it to the compiler server without sending
    /// the closure, so we need it again)
    SEXP materialize(SEXP env) const;

    void print(std::ostream& out) const;
    static ProxyEnv* deserialize(AbstractDeserializer& deserializer);
    void serialize(AbstractSerializer& serializer) const;
    void hash(HasherOld& hasher) const;
    void addConnected(ConnectedCollectorOld& collector) const;
};

/// Is the environment toplevel. Asserts argument is ENVSXP
///
/// TODO: Move this somewhere else?
bool isGlobalEnv(SEXP env);

} // namespace rir
