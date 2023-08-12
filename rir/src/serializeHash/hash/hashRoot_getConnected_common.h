// TODO: Merge more in hashRoot.cpp and getConnected.cpp if it's not noticeably
//  slower or too complicated.
#pragma once

#include <vector>
#include "R/r.h"

// Globals aren't considered connected and references to them don't have
// recursive connected references
static std::vector<SEXP> globals{
    R_GlobalEnv,    R_BaseEnv,        R_BaseNamespace, R_TrueValue,
    R_NilValue,     R_FalseValue,     R_UnboundValue,  R_MissingArg,
    R_RestartToken, R_LogicalNAValue, R_EmptyEnv,      R_DimSymbol,
    R_DotsSymbol,   R_NamesSymbol,    NA_STRING};

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