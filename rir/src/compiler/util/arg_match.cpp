#include "arg_match.h"
#include "../pir/pir_impl.h"
#include "R/Protect.h"
#include "utils/Pool.h"

namespace rir {
namespace pir {

#define ARGUSED(x) LEVELS(x)
#define SET_ARGUSED(x, v) SETLEVELS(x, v)
#define streql(s, t) (!strcmp((s), (t)))

// cppcheck-suppress constParameter
bool ArgumentMatcher::reorder(Builder& insert, SEXP formals,
                              const std::vector<BC::PoolIdx>& actualNames,
                              std::vector<Value*>& givenArgs,
                              std::vector<BC::ArgIdx>& argOrderOrig) {
    // Build up the list of supplied args. We use the names from actualNames
    // and integers to represent the index into givenArgs.
    SEXP supplied = R_NilValue;
    for (int pos = givenArgs.size() - 1; pos >= 0; pos--) {
        PROTECT(supplied);
        SEXP idx = Rf_allocVector(INTSXP, 1);
        INTEGER(idx)[0] = pos;
        supplied = CONS_NR(idx, supplied);
        if (actualNames.size() > (unsigned)pos)
            SET_TAG(supplied, Pool::get(actualNames[pos]));
        UNPROTECT(1);
    }

    // The following code is mostly a copy of Rf_machArgs from main/match.c,
    // where errors have been replaced by 'return false'. Also our Protect
    // wrapper is used and the unused argument list is not built.
    Rboolean seendots;
    size_t arg_i = 0;
    SEXP f, a, b, dots, actuals;

    actuals = R_NilValue;
    for (f = formals; f != R_NilValue; f = CDR(f), arg_i++) {
        /* CONS_NR is used since argument lists created here are only
           used internally and so should not increment reference
           counts */
        actuals = CONS_NR(R_MissingArg, actuals);
        SET_MISSING(actuals, 1);
    }
    /* We use fargused instead of ARGUSED/SET_ARGUSED on elements of
       formals to avoid modification of the formals SEXPs.  A gc can
       cause matchArgs to be called from finalizer code, resulting in
       another matchArgs call with the same formals.  In R-2.10.x, this
       corrupted the ARGUSED data of the formals and resulted in an
       incorrect "formal argument 'foo' matched by multiple actual
       arguments" error.
     */
    int fargused[arg_i ? arg_i : 1]; // avoid undefined behaviour
    memset(fargused, 0, sizeof(fargused));

    for (b = supplied; b != R_NilValue; b = CDR(b))
        SET_ARGUSED(b, 0);

    {
        Protect p(actuals);
        unsigned i;

        /* First pass: exact matches by tag */
        /* Grab matched arguments and check */
        /* for multiple exact matches. */

        f = formals;
        a = actuals;
        arg_i = 0;
        while (f != R_NilValue) {
            SEXP ftag = TAG(f);
            const char* ftag_name = CHAR(PRINTNAME(ftag));
            if (ftag != R_DotsSymbol && ftag != R_NilValue) {
                for (b = supplied, i = 1; b != R_NilValue; b = CDR(b), i++) {
                    SEXP btag = TAG(b);
                    if (btag != R_NilValue) {
                        const char* btag_name = CHAR(PRINTNAME(btag));
                        if (streql(ftag_name, btag_name)) {
                            if (fargused[arg_i] == 2) {
                                return false;
                            }
                            if (ARGUSED(b) == 2) {
                                return false;
                            }
                            SETCAR(a, CAR(b));
                            if (CAR(b) != R_MissingArg)
                                SET_MISSING(a, 0);
                            SET_ARGUSED(b, 2);
                            fargused[arg_i] = 2;
                        }
                    }
                }
            }
            f = CDR(f);
            a = CDR(a);
            arg_i++;
        }

        /* Second pass: partial matches based on tags */
        /* An exact match is required after first ... */
        /* The location of the first ... is saved in "dots" */

        dots = R_NilValue;
        seendots = FALSE;
        f = formals;
        a = actuals;
        arg_i = 0;
        while (f != R_NilValue) {
            if (fargused[arg_i] == 0) {
                if (TAG(f) == R_DotsSymbol && !seendots) {
                    /* Record where ... value goes */
                    dots = a;
                    seendots = TRUE;
                } else {
                    for (b = supplied, i = 1; b != R_NilValue;
                         b = CDR(b), i++) {
                        if (ARGUSED(b) != 2 && TAG(b) != R_NilValue &&
                            pmatch(TAG(f), TAG(b), seendots)) {
                            return false;
                        }
                    }
                }
            }
            f = CDR(f);
            a = CDR(a);
            arg_i++;
        }

        /* Third pass: matches based on order */
        /* All args specified in tag=value form */
        /* have now been matched.  If we find ... */
        /* we gobble up all the remaining args. */
        /* Otherwise we bind untagged values in */
        /* order to any unmatched formals. */

        f = formals;
        a = actuals;
        b = supplied;
        seendots = FALSE;

        while (f != R_NilValue && b != R_NilValue && !seendots) {
            if (TAG(f) == R_DotsSymbol) {
                /* Skip ... matching until all tags done */
                seendots = TRUE;
                f = CDR(f);
                a = CDR(a);
            } else if (CAR(a) != R_MissingArg) {
                /* Already matched by tag */
                /* skip to next formal */
                f = CDR(f);
                a = CDR(a);
            } else if (ARGUSED(b) || TAG(b) != R_NilValue) {
                /* This value used or tagged , skip to next value */
                /* The second test above is needed because we */
                /* shouldn't consider tagged values for positional */
                /* matches. */
                /* The formal being considered remains the same */
                b = CDR(b);
            } else {
                /* We have a positional match */
                SETCAR(a, CAR(b));
                if (CAR(b) != R_MissingArg)
                    SET_MISSING(a, 0);
                SET_ARGUSED(b, 1);
                b = CDR(b);
                f = CDR(f);
                a = CDR(a);
            }
        }

        if (dots != R_NilValue) {
            /* Gobble up all unused actuals */
            SET_MISSING(dots, 0);
            i = 0;
            for (a = supplied; a != R_NilValue; a = CDR(a))
                if (!ARGUSED(a))
                    i++;

            if (i) {
                a = allocList(i);
                SET_TYPEOF(a, DOTSXP);
                f = a;
                for (b = supplied; b != R_NilValue; b = CDR(b))
                    if (!ARGUSED(b)) {
                        SETCAR(f, CAR(b));
                        SET_TAG(f, TAG(b));
                        f = CDR(f);
                    }
                SETCAR(dots, a);
            }
        } else {
            /* Check that all arguments are used */
            for (b = supplied; b != R_NilValue; b = CDR(b))
                if (!ARGUSED(b))
                    return false;
        }
    }

    // End of copy/paste snippet. Collecting results.

    RList result(actuals);
    for (auto r : result) {
        auto expected =
            TYPEOF(r) == DOTSXP || r == R_MissingArg || TYPEOF(r) == INTSXP;
        assert(expected && "Static argument matching bug, this "
                           "actual value was not put there by us");
        if (!expected)
            return false;
    }

    // passing ... is impossible to statically match, we first need to figure
    // out what's in the ... list.
    for (auto r : result) {
        if (TYPEOF(r) == INTSXP) {
            int idx = INTEGER(r)[0];
            if (ExpandDots::Cast(givenArgs[idx]))
                return false;
        }
        if (TYPEOF(r) == DOTSXP) {
            auto dal = RList(r);
            for (auto da = dal.begin(); da != dal.end(); ++da) {
                int idx = INTEGER(*da)[0];
                if (ExpandDots::Cast(givenArgs[idx]))
                    return false;
            }
        }
    }

    std::vector<Value*> copy(givenArgs);
    givenArgs.resize(result.length());
    size_t pos = 0;
    size_t reorderPos = 0;
    const static bool DEBUG = false;
    if (DEBUG)
        std::cout << "MATCHED : ";
    f = formals;

    // This loop updates all positions in argOrderOrig, no need to clear&resize
    for (auto r : result) {
        if (DEBUG)
            std::cout << CHAR(PRINTNAME(TAG(f))) << "=";
        if (TYPEOF(r) == INTSXP) {
            int idx = INTEGER(r)[0];
            argOrderOrig[idx] = BC::encodeArgOrder(
                reorderPos++, actualNames.size() > (unsigned)idx &&
                                  Pool::get(actualNames[idx]) != R_NilValue);
            givenArgs[pos++] = copy[idx];
            if (DEBUG) {
                copy[idx]->printRef(std::cout);
                if (actualNames.size() > (unsigned)idx &&
                    Pool::get(actualNames[idx]) != R_NilValue)
                    std::cout << "("
                              << CHAR(PRINTNAME(Pool::get(actualNames[idx])))
                              << ")";
            }
        } else if (r == R_MissingArg) {
            reorderPos++;
            givenArgs[pos++] = MissingArg::instance();
            if (DEBUG)
                std::cout << "miss";
        } else if (TYPEOF(r) == DOTSXP) {
            // We pass individual arguments, but the callee receives them as
            // `...` list. Therefore we gobble all arguments up into a dotslist
            // and pass them as a single arg.
            auto dal = RList(r);
            auto conv = new DotsList;
            if (DEBUG)
                std::cout << "(";
            for (auto da = dal.begin(); da != dal.end(); ++da) {
                assert(TYPEOF(*da) == INTSXP);
                int idx = INTEGER(*da)[0];
                conv->addInput(da.tag(), copy[idx]);
                argOrderOrig[idx] =
                    BC::encodeArgOrder(reorderPos++, da.tag() != R_NilValue);
                if (DEBUG) {
                    if (da.tag() != R_NilValue)
                        std::cout << CHAR(PRINTNAME(da.tag())) << "=";
                    copy[idx]->printRef(std::cout);
                    if (da + 1 != dal.end())
                        std::cout << ", ";
                }
            }
            if (DEBUG)
                std::cout << ")";
            givenArgs[pos++] = conv;
            insert(conv);
        } else {
            assert(false);
        }
        f = CDR(f);
        if (DEBUG && f != R_NilValue)
            std::cout << ", ";
    }
    if (DEBUG) {
        std::cout << " { ";
        for (auto arg : argOrderOrig)
            std::cout << BC::decodeArgOrder(arg)
                      << (BC::isArgOrderNamed(arg) ? "(N) " : " ");
        std::cout << "}\n";
    }

    while (!givenArgs.empty() && givenArgs.back() == MissingArg::instance())
        givenArgs.pop_back();

    return true;
}

} // namespace pir
} // namespace rir
