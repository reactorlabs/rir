#pragma once

namespace rir {

// low-quality but fast PRNG
class Random {

    unsigned long x = 123456789, y = 362436069, z = 521288629;

  public:
    unsigned long operator()() { // period 2^96-1
        unsigned long t;
        x ^= x << 16;
        x ^= x >> 5;
        x ^= x << 1;

        t = x;
        x = y;
        y = z;
        z = t ^ x ^ y;

        return z;
    }

    static Random& singleton() {
        static Random r;
        return r;
    }
};

} // namespace rir
