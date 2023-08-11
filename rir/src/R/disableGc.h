//
// Created by Jakob Hain on 7/22/23.
//

#pragma once

#include "R/r_incl.h"
#include <functional>

template<typename F> static ALWAYS_INLINE void disableGc(F f) {
    auto gcEnabled = R_GCEnabled;
    R_GCEnabled = 0;
    f();
    R_GCEnabled = gcEnabled;
}

template<typename F> static ALWAYS_INLINE SEXP disableGc2(F f) {
    auto gcEnabled = R_GCEnabled;
    R_GCEnabled = 0;
    auto res = f();
    R_GCEnabled = gcEnabled;
    return res;
}

template<typename F> static ALWAYS_INLINE rir::UUID disableGc3(F f) {
    auto gcEnabled = R_GCEnabled;
    R_GCEnabled = 0;
    auto res = f();
    R_GCEnabled = gcEnabled;
    return res;
}
