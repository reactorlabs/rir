#pragma once

#include "R/Symbols.h"
#include "common.h"
#include <list>

namespace rir {

#define LIST_OF_PIR_CHECKS(V)                                                  \
    V(IsPirCompilable)                                                         \
    V(NoLoad)                                                                  \
    V(NoStore)                                                                 \
    V(NoStSuper)                                                               \
    V(NoEnvForAdd)                                                             \
    V(NoEnvSpec)                                                               \
    V(NoEnv)                                                                   \
    V(NoPromise)                                                               \
    V(NoExternalCalls)                                                         \
    V(Returns42L)                                                              \
    V(NoAsInt)                                                                 \
    V(NoEq)                                                                    \
    V(OneEq)                                                                   \
    V(OneNot)

struct PirCheck {
    enum class Type : unsigned {
#define V(Check) Check,
        LIST_OF_PIR_CHECKS(V)
#undef V
            Invalid
    };

    std::list<Type> types;

    static Type parseType(const char* str);
    explicit PirCheck(std::list<Type>& types) : types(types) {
#ifdef ENABLE_SLOWASSERT
        for (Type type : types)
            assert(type != Type::Invalid);
#endif
    }
    bool run(SEXP f);
};

} // namespace rir
