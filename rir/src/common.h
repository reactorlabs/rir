#ifndef RIR_COMMON_H
#define RIR_COMMON_H

#include <cassert>
#include <cstdint>

extern void printCBacktrace();
extern void printRBacktrace();
extern void printBacktrace();

#ifdef ENABLE_SLOWASSERT
#define SLOWASSERT(what) assert(what)
#else
#define SLOWASSERT(what)                                                       \
    {}
#endif

#define REXPORT extern "C"

// from boost
#include <functional>
template <class T>
inline std::size_t hash_combine(std::size_t seed, const T& v) {
    std::hash<T> hasher;
    return hasher(v) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
}

struct pairhash {
  public:
    template <typename T, typename U>
    std::size_t operator()(const std::pair<T, U>& x) const {
        return hash_combine(hash_combine(0, x.first), x.second);
    }
};

#endif
