#ifndef RIR_COMMON_H
#define RIR_COMMON_H

#include <cassert>
#include <cstdint>

// TODO force inlining for clang & gcc
#define RIR_INLINE __attribute__((always_inline)) inline

extern void printCBacktrace();
extern void printRBacktrace();
extern void printBacktrace();

#ifdef ENABLE_SLOWASSERT
#define SLOWASSERT(what) assert(what)
#else
#define SLOWASSERT(what)                                                       \
    {}
#endif

// from boost
#include <functional>
template <class T>
inline size_t hash_combine(size_t seed, const T& v) {
    std::hash<T> hasher;
    return hasher(v) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
}

#endif
