#ifndef RIR_UTILS_ERRORS_H
#define RIR_UTILS_ERRORS_H

#include "R/r.h"

namespace rir {

struct Errors {
    enum class Signature : uint32_t {
        NoArgs,
        Str,
        Int,
        IntIntInt,
        StrIntStr,
    };
    static size_t signatureNargs(Signature s) {
        switch (s) {
        case Signature::NoArgs:
            return 0;
        case Signature::Str:
        case Signature::Int:
            return 1;
        case Signature::IntIntInt:
        case Signature::StrIntStr:
            return 3;
        }
        assert(false);
    }
    static const char* signature2char(Signature s) {
        switch (s) {
        case Signature::NoArgs:
            return "NoArgs";
        case Signature::Str:
            return "Str";
        case Signature::Int:
            return "Int";
        case Signature::IntIntInt:
            return "IntIntInt";
        case Signature::StrIntStr:
            return "StrIntStr";
        }
        assert(false);
    }
    static void makeCall(const char* msg, Signature signature);
};

} // namespace rir

#endif // RIR_UTILS_ERRORS_H
