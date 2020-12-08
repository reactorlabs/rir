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
          length(length), onStack(onStack), args(args), ast(ast) {
#ifdef ENABLE_SLOWASSERT
        for (size_t i = 0; i < length; ++i) {
            assert(args[i].tag == 0);
            assert(args[i].u.sxpval);
        }
#endif
        if (!onStack) {
            for (size_t i = 0; i < length; ++i) {
                assert(args[i].tag == 0);
                setEntry(i, args[i].u.sxpval);
            }
            this->args = nullptr;
        }
    };

    friend struct LazyArglistOnHeap;
    friend struct LazyArglistOnStack;

  public:
    const size_t length;

    // TODO: remove once we can correctly reorder!
    bool wrong = false;

  private:
    bool onStack;
    const R_bcstack_t* args;
    SEXP ast;

  public:
    SEXP createArgsLists(InterpreterInstance* ctx) {
        SLOWASSERT(!wrong);
        return createLegacyArgsListFromStackValues(
            length, onStack ? args : nullptr, onStack ? nullptr : this, nullptr,
            ast, false, ctx);
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
