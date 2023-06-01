//
// Created by Jakob Hain on 6/1/23.
//

#pragma once

#include "common.h"

/// Keep the exact byte pattern but change the type
static uint32_t reinterpret_int32(int32_t x) {
    // This is how to do it in the C++ standard
    uint32_t y;
    memcpy(&y, &x, sizeof(uint32_t));
    return y;
}

/// Keep the exact byte pattern but change the type
static int32_t reinterpret_uint32(uint32_t x) {
    // This is how to do it in the C++ standard
    int32_t y;
    memcpy(&y, &x, sizeof(int32_t));
    return y;
}
