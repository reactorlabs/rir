#ifndef RIR_UTILS_SNIPPETS_H
#define RIR_UTILS_SNIPPETS_H

#include "R/r.h"

namespace rir {

struct Snippets {
    enum class Snippet : uint32_t {
        TYPEOF,
        Rf_allocVector,
        Rf_coerceVector,
        Rf_type2char,
        Rf_PrintValue,
        REALINTinc,
        VapplyDimAndNames,
        VapplyOffsetInc,
        VapplySubassign,
    };
    static size_t nargs(Snippet s) {
        switch (s) {
        case Snippet::TYPEOF:
        case Snippet::Rf_type2char:
        case Snippet::Rf_PrintValue:
            return 1;
        case Snippet::Rf_allocVector:
        case Snippet::Rf_coerceVector:
        case Snippet::REALINTinc:
        case Snippet::VapplyOffsetInc:
            return 2;
        case Snippet::VapplySubassign:
            return 6;
        case Snippet::VapplyDimAndNames:
            return 9;
        }
        assert(false);
    }
    static const char* str(Snippet s) {
        switch (s) {
        case Snippet::TYPEOF:
            return "TYPEOF";
        case Snippet::Rf_allocVector:
            return "Rf_allocVector";
        case Snippet::Rf_coerceVector:
            return "Rf_coerceVector";
        case Snippet::Rf_type2char:
            return "Rf_type2char";
        case Snippet::Rf_PrintValue:
            return "Rf_PrintValue";
        case Snippet::REALINTinc:
            return "REALINTinc";
        case Snippet::VapplyDimAndNames:
            return "VapplyDimAndNames";
        case Snippet::VapplyOffsetInc:
            return "VapplyOffsetInc";
        case Snippet::VapplySubassign:
            return "VapplySubassign";
        }
        assert(false);
    }
    static SEXP execute(Snippet s);

    static uint32_t toImmediate(Snippet s) { return static_cast<uint32_t>(s); }
    static size_t nargs(uint32_t i) { return nargs(static_cast<Snippet>(i)); }
    static const char* str(uint32_t i) { return str(static_cast<Snippet>(i)); }
    static SEXP execute(uint32_t i) { return execute(static_cast<Snippet>(i)); }
};

} // namespace rir

#endif // RIR_UTILS_SNIPPETS_H
