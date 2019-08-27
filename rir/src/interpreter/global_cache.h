#pragma once

#include <R/r.h>

// Starts at 1 so cacheVersion = 0 is always invalid
static size_t globalCacheVersion = 1;

__attribute__((unused)) static void invalidateGlobalCache() {
    globalCacheVersion++;
}