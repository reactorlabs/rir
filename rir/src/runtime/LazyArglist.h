#ifndef RIR_ARGS_LAZY_H
#define RIR_ARGS_LAZY_H

#include "runtime/ArglistOrder.h"
#include "runtime/RirRuntimeObject.h"

#include "interpreter/interp_incl.h"

#include <cassert>
#include <cstdint>
#include <functional>

namespace rir {

#pragma pack(push)
#pragma pack(1)

static constexpr size_t LAZY_ARGS_MAGIC = 0x1a27a000;

/**
 * LazyArglist holds the information needed to recreate the
 * arguments list needed by gnu-r contexts whenever a function
 * is called, lazily. In RCNTXT the field that hold the list is
 * promargs.
 */

struct LazyArglist : public RirRuntimeObject<LazyArglist, LAZY_ARGS_MAGIC> {
  public:
    LazyArglist() = delete;
    LazyArglist(const LazyArglist&) = delete;
    LazyArglist& operator=(const LazyArglist&) = delete;

    size_t nargs() {
        if (length == 0)
            return 0;

        // Cache
        if (actualNargs != 0)
            return actualNargs;

        for (size_t i = 0; i < length; ++i) {
            SEXP arg = getArg(i);
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

    SEXP getArg(size_t i) {
        if (stackArgs) {
            assert(stackArgs[i].tag == 0);
            return stackArgs[i].u.sxpval;
        } else {
            return heapArgs[i];
        }
    }

    SEXP createArglist(InterpreterInstance* ctx) {
        return createLegacyArglist(
            callId, length, stackArgs, stackArgs ? nullptr : heapArgs, nullptr,
            ast, reordering ? ArglistOrder::unpack(reordering) : nullptr, false,
            true, ctx);
    }

  private:
    // cppcheck-suppress uninitMemberVarPrivate
    LazyArglist(ArglistOrder::CallId id, SEXP arglistOrder, size_t length,
                const R_bcstack_t* args, SEXP ast, bool onStack)
        // GC tracked pointers are the reordering and length args
        : RirRuntimeObject(sizeof(LazyArglist) - sizeof(SEXP),
                           onStack ? 0 : (1 + length)),
          callId(id), length(length), ast(ast), reordering(arglistOrder) {
#ifdef ENABLE_SLOWASSERT
        for (size_t i = 0; i < length; ++i) {
            assert(args[i].tag == 0);
            assert(args[i].u.sxpval);
        }
#endif
        if (onStack) {
            stackArgs = args;
        } else {
            stackArgs = nullptr;
            for (size_t i = 0; i < length; ++i) {
                assert(args[i].tag == 0);
                setEntry(i + 1, args[i].u.sxpval);
            }
        }
    }

    friend struct LazyArglistOnHeap;
    friend struct LazyArglistOnStack;

    const ArglistOrder::CallId callId;
    const uint32_t length;
    uint32_t actualNargs = 0;
    const R_bcstack_t* stackArgs;
    // Needed to recover the names
    SEXP ast;
    // Needed to reorder arguments. Has to be the last field to be seen by the
    // GC!
    SEXP reordering;
    SEXP heapArgs[];
};

#pragma pack(pop)

struct LazyArglistOnStack {
  public:
    // This needs to come first and provides a SEXPREC header to not confuse
    // the R garbage collector.
    LazyArglistOnStack() = delete;
    LazyArglistOnStack(const LazyArglistOnStack&) = delete;
    LazyArglistOnStack& operator=(const LazyArglistOnStack&) = delete;

    LazyArglistOnStack(ArglistOrder::CallId id, SEXP arglistOrder,
                       size_t length, const R_bcstack_t* args, SEXP ast)
        : content(id, arglistOrder, length, args, ast, true) {
        fakeSEXP.attrib = R_NilValue;
        fakeSEXP.gengc_next_node = R_NilValue;
        fakeSEXP.gengc_prev_node = R_NilValue;
        fakeSEXP.sxpinfo.gcgen = 1;
        fakeSEXP.sxpinfo.mark = 1;
        fakeSEXP.sxpinfo.named = 2;
        fakeSEXP.sxpinfo.type = EXTERNALSXP;
        PROTECT(arglistOrder);
    }

    ~LazyArglistOnStack() { UNPROTECT(1); }

    SEXP asSexp() { return (SEXP)this; }

  private:
    VECTOR_SEXPREC fakeSEXP;

  public:
    LazyArglist content;
};

struct LazyArglistOnHeap {
  public:
    static SEXP New(ArglistOrder::CallId id, SEXP arglistOrder, size_t length,
                    const R_bcstack_t* args, SEXP ast) {
        SEXP wrapper = Rf_allocVector(
            EXTERNALSXP, sizeof(LazyArglist) + sizeof(SEXP) * (1 + length));
        auto la = new (DATAPTR(wrapper))
            LazyArglist(id, arglistOrder, length, args, ast, false);
        return la->container();
    }
};

} // namespace rir

#endif
