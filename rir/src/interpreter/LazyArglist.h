#ifndef RIR_ARGS_LAZY_H
#define RIR_ARGS_LAZY_H

#include "runtime/RirRuntimeObject.h"

#include "interp_incl.h"

#include <cassert>
#include <cstdint>
#include <functional>

namespace rir {

static constexpr size_t LAZY_ARGS_MAGIC = 0x1a27a000;

/**
 * ArgsLazyCreation holds the information needed to recreate the
 * arguments list needed by gnu-r contexts whenever a function
 * is called, lazily. In RCNTXT the field that hold the list is
 * promargs.
 */

struct LazyArglist : public RirRuntimeObject<LazyArglist, LAZY_ARGS_MAGIC> {
  public:
    LazyArglist() = delete;
    LazyArglist(const LazyArglist&) = delete;
    LazyArglist& operator=(const LazyArglist&) = delete;

  private:
    LazyArglist(size_t length, const R_bcstack_t* args, SEXP ast, bool onStack)
        : RirRuntimeObject(sizeof(LazyArglist), onStack ? 0 : length),
          length(length), ast(ast) {
#ifdef ENABLE_SLOWASSERT
        for (size_t i = 0; i < length; ++i) {
            assert(args[i].tag == 0);
            assert(args[i].u.sxpval);
        }
#endif
        if (onStack) {
            stackArgs = args;
        } else {
            for (size_t i = 0; i < length; ++i) {
                assert(args[i].tag == 0);
                setEntry(i, args[i].u.sxpval);
            }
            stackArgs = nullptr;
        }
    };

    friend struct LazyArglistOnHeap;
    friend struct LazyArglistOnStack;

  public:
    // TODO: remove once we can correctly reorder!
    bool wrong = false;

  private:
    const uint32_t length;
    uint32_t actualNargs = 0;
    const R_bcstack_t* stackArgs;
    // Needed to recover the names
    SEXP ast;

  public:
    size_t nargs() {
        if (length == 0)
            return 0;

        // Cache
        if (actualNargs != 0)
            return actualNargs;

        for (size_t i = 0; i < length; ++i) {
            SEXP arg;
            if (stackArgs) {
                assert(stackArgs[i].tag == 0);
                arg = stackArgs[i].u.sxpval;
            } else {
                arg = getEntry(i);
            }
            if (TYPEOF(arg) == DOTSXP) {
                while (arg != R_NilValue) {
                    actualNargs++;
                    arg = CDR(arg);
                }
                continue;
            }
            actualNargs++;
        }
        return actualNargs;
    }

    SEXP createArgsLists(InterpreterInstance* ctx) {
        SLOWASSERT(!wrong);
        return createLegacyArgsListFromStackValues(
            length, stackArgs, this, nullptr, ast, false, true, ctx);
    }

    using RirRuntimeObject::getEntry;
};

struct LazyArglistOnStack {
  public:
    // This needs to come first and provides a SEXPREC header to not confuse
    // the R garbage collector.
    LazyArglistOnStack() = delete;
    LazyArglistOnStack(const LazyArglistOnStack&) = delete;
    LazyArglistOnStack& operator=(const LazyArglistOnStack&) = delete;

    LazyArglistOnStack(size_t length, const R_bcstack_t* args, SEXP ast)
        : content(length, args, ast, true) {
        fakeSEXP.attrib = R_NilValue;
        fakeSEXP.gengc_next_node = R_NilValue;
        fakeSEXP.gengc_prev_node = R_NilValue;
        fakeSEXP.sxpinfo.gcgen = 1;
        fakeSEXP.sxpinfo.mark = 1;
        fakeSEXP.sxpinfo.named = 2;
        fakeSEXP.sxpinfo.type = EXTERNALSXP;
    }

    SEXP asSexp() { return (SEXP)this; }

  private:
    VECTOR_SEXPREC fakeSEXP;

  public:
    LazyArglist content;
};

struct LazyArglistOnHeap {
  public:
    static SEXP New(size_t length, const R_bcstack_t* args, SEXP ast) {
        SEXP wrapper = Rf_allocVector(EXTERNALSXP, sizeof(LazyArglist) +
                                                       sizeof(SEXP) * (length));
        auto la = new (DATAPTR(wrapper)) LazyArglist(length, args, ast, false);
        return la->container();
    }
};

} // namespace rir

#endif
