#ifndef LAZY_ENVIRONMENT_H
#define LAZY_ENVIRONMENT_H

#include "../ir/BC_inc.h"
#include "../runtime/RirRuntimeObject.h"
#include "instance.h"
#include "interp_incl.h"

#include <cassert>
#include <cstdint>
#include <functional>

namespace rir {

#define LAZY_ENVIRONMENT_MAGIC 0xe4210e47

/**
 * EnvironmentStub holds the information needed to create an
 * environment lazily.
 */
struct LazyEnvironment
    : public RirRuntimeObject<LazyEnvironment, LAZY_ENVIRONMENT_MAGIC> {
    LazyEnvironment() = delete;
    LazyEnvironment(const LazyEnvironment&) = delete;
    LazyEnvironment& operator=(const LazyEnvironment&) = delete;

    LazyEnvironment(SEXP parent, size_t nargs, Immediate* names)
        : RirRuntimeObject(sizeof(LazyEnvironment), nargs + 2), nargs(nargs),
          names(names) {}

    SEXP materialized() { return getEntry(nargs + 1); }
    void materialized(SEXP m) { setEntry(nargs + 1, m); }

    size_t nargs;
    Immediate* names;

    SEXP getArg(size_t i) { return getEntry(i); }
    void setArg(size_t i, SEXP val) { setEntry(i, val); }

    SEXP getParent() { return getEntry(nargs); }

    static LazyEnvironment* BasicNew(SEXP parent, size_t nargs,
                                     Immediate* names) {
        SEXP wrapper = Rf_allocVector(
            EXTERNALSXP, sizeof(LazyEnvironment) + sizeof(SEXP) * (nargs + 2));
        return new (DATAPTR(wrapper))
            LazyEnvironment(parent, nargs, (Immediate*)names);
    }

    static LazyEnvironment* New(SEXP parent, size_t nargs, Immediate* names) {
        auto le = BasicNew(parent, nargs, names);
        for (long i = nargs - 1; i >= 0; --i) {
            le->setArg(i, ostack_pop(ctx));
        }
        le->setEntry(nargs, parent);
        return le;
    }
};
} // namespace rir

#endif
