#pragma once

#include "R/Symbols.h"

namespace rir {

#define LIST_OF_PIR_CHECKS(V)                                                  \
    V(IsPirCompilable)                                                         \
    V(NoLoad)                                                                  \
    V(NoStore)                                                                 \
    V(NoStSuper)                                                               \
    V(NoEnvForAdd)                                                             \
    V(NoEnvSpec)                                                               \
    V(NoEnv)                                                                   \
    V(NoExternalCalls)                                                         \
    V(Returns42L)

struct PirCheck {
    enum class Type : unsigned {
#define V(Check) Check,
        LIST_OF_PIR_CHECKS(V)
#undef V
            Invalid
    };

    Type type;

    static Type parseType(const char* str);
    explicit PirCheck(Type type) : type(type) { assert(type != Type::Invalid); }
    bool run(SEXP f);
};

} // namespace rir
