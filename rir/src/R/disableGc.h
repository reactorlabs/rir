//
// Created by Jakob Hain on 7/22/23.
//

#pragma once

#include "R/r_incl.h"
#include <functional>

static inline void disableGc(const std::function<void()>&& f) {
    auto gcEnabled = R_GCEnabled;
    R_GCEnabled = 0;
    f();
    R_GCEnabled = gcEnabled;
}


template<typename T> static inline T disableGc(const std::function<T()>&& f) {
    auto gcEnabled = R_GCEnabled;
    R_GCEnabled = 0;
    auto res = f();
    R_GCEnabled = gcEnabled;
    return res;
}
