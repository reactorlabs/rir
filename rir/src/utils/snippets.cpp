#include "snippets.h"
#include "interpreter/instance.h"

namespace rir {

SEXP Snippets::execute(Snippet s) {
    switch (s) {
    case Snippet::TYPEOF: {
        auto res = Rf_allocVector(INTSXP, 1);
        auto type = TYPEOF(ostack_top(ctx));
        INTEGER(res)[0] = type;
        return res;
    }
    case Snippet::Rf_allocVector: {
        auto type = ostack_at(ctx, 1);
        assert(IS_SIMPLE_SCALAR(type, INTSXP));
        auto len = ostack_at(ctx, 0);
        assert(IS_SIMPLE_SCALAR(len, INTSXP));
        // std::cout << "Snippet::Rf_allocVector(" << INTEGER(type)[0] << ", "
        //           << INTEGER(len)[0] << ")\n";
        return Rf_allocVector(INTEGER(type)[0], INTEGER(len)[0]);
    }
    case Snippet::Rf_coerceVector: {
        auto val = ostack_at(ctx, 1);
        auto type = ostack_at(ctx, 0);
        assert(IS_SIMPLE_SCALAR(type, INTSXP));
        return Rf_coerceVector(val, INTEGER(type)[0]);
    }
    case Snippet::Rf_PrintValue: {
        auto val = ostack_top(ctx);
        // std::cout << "Snippet::Rf_PrintValue(" << val << ")\n===\n";
        if (TYPEOF(val) == CHARSXP) {
            std::cout << "===== " << CHAR(val) << " =====\n";
        } else {
            Rf_PrintValue(val);
        }
        return R_NilValue;
    }
    case Snippet::Rf_type2char: {
        auto type = ostack_at(ctx, 0);
        assert(IS_SIMPLE_SCALAR(type, INTSXP));
        auto cstr = Rf_type2char(INTEGER(type)[0]);
        return Rf_mkChar(cstr);
    }
    case Snippet::REALINTinc: {
        auto ind = ostack_at(ctx, 1);
        assert(IS_SIMPLE_SCALAR(ind, REALSXP) || IS_SIMPLE_SCALAR(ind, INTSXP));
        auto i = ostack_at(ctx, 0);
        assert(IS_SIMPLE_SCALAR(i, INTSXP));
        if (TYPEOF(ind) == REALSXP)
            REAL(ind)[0] = (double)(INTEGER(i)[0] + 1);
        else
            INTEGER(ind)[0] = (int)(INTEGER(i)[0] + 1);
        return R_NilValue;
    }
    case Snippet::VapplyDimAndNames: {
        // [useNames, n, commonLen, dim_v, array_value, ans, names,
        // rowNames, rnk_v]
        int useNames = (ostack_at(ctx, 8) == R_TrueValue ? 1 : 0);
        R_xlen_t n = INTEGER(ostack_at(ctx, 7))[0];
        int commonLen = INTEGER(ostack_at(ctx, 6))[0];
        SEXP dim_v = ostack_at(ctx, 5);
        Rboolean array_value =
            (ostack_at(ctx, 4) == R_TrueValue ? TRUE : FALSE);
        SEXP ans = ostack_at(ctx, 3);
        SEXP names = ostack_at(ctx, 2);
        SEXP rowNames = ostack_at(ctx, 1);
        int rnk_v = INTEGER(ostack_at(ctx, 0))[0];

        if (commonLen != 1) {
            SEXP dim;
            rnk_v = array_value ? LENGTH(dim_v) : 1;
            PROTECT(dim = allocVector(INTSXP, rnk_v + 1));
            if (array_value)
                for (int j = 0; j < rnk_v; j++)
                    INTEGER(dim)[j] = INTEGER(dim_v)[j];
            else
                INTEGER(dim)[0] = commonLen;
            INTEGER(dim)[rnk_v] = (int)n; // checked above
            setAttrib(ans, R_DimSymbol, dim);
            UNPROTECT(1);
        }

        if (useNames) {
            if (commonLen == 1) {
                if (!isNull(names))
                    setAttrib(ans, R_NamesSymbol, names);
            } else {
                if (!isNull(names) || !isNull(rowNames)) {
                    SEXP dimnames;
                    PROTECT(dimnames = allocVector(VECSXP, rnk_v + 1));
                    if (array_value && !isNull(rowNames)) {
                        if (TYPEOF(rowNames) != VECSXP ||
                            LENGTH(rowNames) != rnk_v)
#ifndef _
#define _(x) x
#endif
                            // should never happen ..
                            Rf_error(_("dimnames(<value>) is neither NULL nor "
                                       "list of length %d"),
                                     rnk_v);
                        for (int j = 0; j < rnk_v; j++)
                            SET_VECTOR_ELT(dimnames, j,
                                           VECTOR_ELT(rowNames, j));
                    } else
                        SET_VECTOR_ELT(dimnames, 0, rowNames);

                    SET_VECTOR_ELT(dimnames, rnk_v, names);
                    setAttrib(ans, R_DimNamesSymbol, dimnames);
                    UNPROTECT(1);
                }
            }
        }

        return ans;
    }
    case Snippet::VapplyOffsetInc: {
        // [common_len_offset, commonLen]
        auto common_len_offset = ostack_at(ctx, 1);
        assert(IS_SIMPLE_SCALAR(common_len_offset, INTSXP));
        auto commonLen = ostack_at(ctx, 0);
        assert(IS_SIMPLE_SCALAR(commonLen, INTSXP));

        if (MAYBE_REFERENCED(common_len_offset)) {
            int i = INTEGER(common_len_offset)[0];
            SEXP n = Rf_allocVector(INTSXP, 1);
            INTEGER(n)[0] = i + INTEGER(commonLen)[0];
            return n;
        }
        INTEGER(common_len_offset)[0] += INTEGER(commonLen)[0];
        return common_len_offset;
    }
    case Snippet::VapplySubassign: {
        // [val, ans, commonLen, commonType, i, common_len_offset]
        SEXP val = ostack_at(ctx, 5);
        SEXP ans = ostack_at(ctx, 4);
        int commonLen = INTEGER(ostack_at(ctx, 3))[0];
        int commonType = INTEGER(ostack_at(ctx, 2))[0];
        R_xlen_t i = INTEGER(ostack_at(ctx, 1))[0];
        int common_len_offset = INTEGER(ostack_at(ctx, 0))[0];

        if (commonLen == 1) { // common case
            switch (commonType) {
            case CPLXSXP:
                COMPLEX(ans)[i] = COMPLEX(val)[0];
                break;
            case REALSXP:
                REAL(ans)[i] = REAL(val)[0];
                break;
            case INTSXP:
                INTEGER(ans)[i] = INTEGER(val)[0];
                break;
            case LGLSXP:
                LOGICAL(ans)[i] = LOGICAL(val)[0];
                break;
            case RAWSXP:
                RAW(ans)[i] = RAW(val)[0];
                break;
            case STRSXP:
                SET_STRING_ELT(ans, i, STRING_ELT(val, 0));
                break;
            case VECSXP:
                SET_VECTOR_ELT(ans, i, VECTOR_ELT(val, 0));
                break;
            }
        } else { // commonLen > 1 (typically, or == 0) :
            switch (commonType) {
            case REALSXP:
                memcpy(REAL(ans) + common_len_offset, REAL(val),
                       commonLen * sizeof(double));
                break;
            case INTSXP:
                memcpy(INTEGER(ans) + common_len_offset, INTEGER(val),
                       commonLen * sizeof(int));
                break;
            case LGLSXP:
                memcpy(LOGICAL(ans) + common_len_offset, LOGICAL(val),
                       commonLen * sizeof(int));
                break;
            case RAWSXP:
                memcpy(RAW(ans) + common_len_offset, RAW(val),
                       commonLen * sizeof(Rbyte));
                break;
            case CPLXSXP:
                memcpy(COMPLEX(ans) + common_len_offset, COMPLEX(val),
                       commonLen * sizeof(Rcomplex));
                break;
            case STRSXP:
                for (int j = 0; j < commonLen; j++)
                    SET_STRING_ELT(ans, common_len_offset + j,
                                   STRING_ELT(val, j));
                break;
            case VECSXP:
                for (int j = 0; j < commonLen; j++)
                    SET_VECTOR_ELT(ans, common_len_offset + j,
                                   VECTOR_ELT(val, j));
                break;
            }
        }

        return ans;
    }
    }
    assert(false);
    return R_NilValue;
}

} // namespace rir
