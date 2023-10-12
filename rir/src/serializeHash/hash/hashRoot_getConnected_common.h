// TODO: Merge more in hashRoot.cpp and getConnected.cpp if it's not noticeably
//  slower or too complicated.
#pragma once

#include <vector>
#include "R/r.h"

__attribute__((unused)) static bool hasTag(SEXP sexp) {
    switch (TYPEOF(sexp)) {
    case LISTSXP:
    case LANGSXP:
    case PROMSXP:
    case DOTSXP:
        return TAG(sexp) != R_NilValue;
    case CLOSXP:
        return true;
    // External pointers have tags but they are handled differently.
    // Some other SEXPs have tags in bytecodes, also handled differently.
    default:
        return false;
    }
}