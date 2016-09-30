#include "BC.h"

#include "utils/Pool.h"
#include <iostream>
#include <iomanip>

#include "R/RList.h"
#include "CodeStream.h"
#include "R/r.h"
#include "R/Funtab.h"

namespace rir {

bool BC::operator==(const BC& other) const {
    if (bc != other.bc)
        return false;

    switch (bc) {
    case BC_t::push_:
    case BC_t::ldfun_:
    case BC_t::ldddvar_:
    case BC_t::ldarg_:
    case BC_t::ldvar_:
    case BC_t::isspecial_:
    case BC_t::stvar_:
    case BC_t::missing_:
    case BC_t::subassign2_:
        return immediate.pool == other.immediate.pool;

    case BC_t::call_:
        return immediate.call_args.args == other.immediate.call_args.args &&
               immediate.call_args.names == other.immediate.call_args.names &&
               immediate.call_args.call == other.immediate.call_args.call;

    case BC_t::dispatch_:
        return immediate.dispatch_args.args ==
                   other.immediate.dispatch_args.args &&
               immediate.dispatch_args.names ==
                   other.immediate.dispatch_args.names &&
               immediate.dispatch_args.selector ==
                   other.immediate.dispatch_args.selector &&
               immediate.dispatch_args.call ==
                   other.immediate.dispatch_args.call;

    case BC_t::dispatch_stack_:
        return immediate.dispatch_stack_args.nargs ==
                   other.immediate.dispatch_stack_args.nargs &&
               immediate.dispatch_stack_args.names ==
                   other.immediate.dispatch_stack_args.names &&
               immediate.dispatch_stack_args.selector ==
                   other.immediate.dispatch_stack_args.selector &&
               immediate.dispatch_stack_args.call ==
                   other.immediate.dispatch_stack_args.call;

    case BC_t::call_stack_:
        return immediate.call_stack_args.nargs ==
                   other.immediate.call_stack_args.nargs &&
               immediate.call_stack_args.names ==
                   other.immediate.call_stack_args.names &&
               immediate.call_stack_args.call ==
                   other.immediate.call_stack_args.call;

    case BC_t::promise_:
    case BC_t::push_code_:
        return immediate.fun == other.immediate.fun;

    case BC_t::br_:
    case BC_t::brtrue_:
    case BC_t::beginloop_:
    case BC_t::brobj_:
    case BC_t::brfalse_:
        return immediate.offset == other.immediate.offset;

    case BC_t::pick_:
    case BC_t::pull_:
    case BC_t::is_:
    case BC_t::put_:
    case BC_t::alloc_:
        return immediate.i == other.immediate.i;

    case BC_t::subset2_:
    case BC_t::extract2_:
    case BC_t::subset1_:
    case BC_t::extract1_:
    case BC_t::ret_:
    case BC_t::length_:
    case BC_t::names_:
    case BC_t::set_names_:
    case BC_t::force_:
    case BC_t::pop_:
    case BC_t::close_:
    case BC_t::asast_:
    case BC_t::asbool_:
    case BC_t::dup_:
    case BC_t::dup2_:
    case BC_t::test_bounds_:
    case BC_t::swap_:
    case BC_t::int3_:
    case BC_t::uniq_:
    case BC_t::aslogical_:
    case BC_t::lgl_and_:
    case BC_t::lgl_or_:
    case BC_t::inc_:
    case BC_t::add_:
    case BC_t::mul_:
    case BC_t::sub_:
    case BC_t::lt_:
    case BC_t::seq_:
    case BC_t::return_:
    case BC_t::isfun_:
    case BC_t::invisible_:
    case BC_t::visible_:
    case BC_t::endcontext_:
    case BC_t::subassign_:
        return true;

    case BC_t::invalid_:
    case BC_t::num_of:
    case BC_t::label:
        break;
    }

    assert(false);
    return false;
}

void BC::write(CodeStream& cs) const {
    cs.insert(bc);
    switch (bc) {
    case BC_t::push_:
    case BC_t::ldarg_:
    case BC_t::ldfun_:
    case BC_t::ldddvar_:
    case BC_t::ldvar_:
    case BC_t::isspecial_:
    case BC_t::stvar_:
    case BC_t::missing_:
    case BC_t::subassign2_:
        cs.insert(immediate.pool);
        return;

    case BC_t::call_:
        cs.insert(immediate.call_args);
        break;

    case BC_t::dispatch_:
        cs.insert(immediate.dispatch_args);
        break;

    case BC_t::dispatch_stack_:
        cs.insert(immediate.dispatch_stack_args);
        break;

    case BC_t::call_stack_:
        cs.insert(immediate.call_stack_args);
        break;

    case BC_t::promise_:
    case BC_t::push_code_:
        cs.insert(immediate.fun);
        return;

    case BC_t::br_:
    case BC_t::brtrue_:
    case BC_t::beginloop_:
    case BC_t::brobj_:
    case BC_t::brfalse_:
        cs.patchpoint(immediate.offset);
        return;

    case BC_t::pick_:
    case BC_t::pull_:
    case BC_t::is_:
    case BC_t::put_:
    case BC_t::alloc_:
        cs.insert(immediate.i);
        return;

    case BC_t::subset2_:
    case BC_t::extract2_:
    case BC_t::subset1_:
    case BC_t::extract1_:
    case BC_t::ret_:
    case BC_t::length_:
    case BC_t::names_:
    case BC_t::set_names_:
    case BC_t::force_:
    case BC_t::pop_:
    case BC_t::close_:
    case BC_t::asast_:
    case BC_t::asbool_:
    case BC_t::dup_:
    case BC_t::dup2_:
    case BC_t::test_bounds_:
    case BC_t::swap_:
    case BC_t::int3_:
    case BC_t::uniq_:
    case BC_t::aslogical_:
    case BC_t::lgl_and_:
    case BC_t::lgl_or_:
    case BC_t::inc_:
    case BC_t::add_:
    case BC_t::mul_:
    case BC_t::sub_:
    case BC_t::lt_:
    case BC_t::seq_:
    case BC_t::return_:
    case BC_t::isfun_:
    case BC_t::invisible_:
    case BC_t::visible_:
    case BC_t::endcontext_:
    case BC_t::subassign_:
        return;

    case BC_t::invalid_:
    case BC_t::num_of:
    case BC_t::label:
        assert(false);
        return;
    }
}

SEXP BC::immediateConst() { return Pool::get(immediate.pool); }

fun_idx_t* BC::immediateCallArgs() {
    switch (bc) {
    case BC_t::call_: {
        SEXP c = Pool::get(immediate.call_args.args);
        assert(TYPEOF(c) == INTSXP);
        return (fun_idx_t*)INTEGER(c);
    }
    case BC_t::dispatch_: {
        SEXP c = Pool::get(immediate.dispatch_args.args);
        assert(TYPEOF(c) == INTSXP);
        return (fun_idx_t*)INTEGER(c);
    }
    default:
        assert(false);
    }
    return nullptr;
}
num_args_t BC::immediateCallNargs() {
    switch (bc) {
    case BC_t::call_stack_:
        return immediate.call_stack_args.nargs;
    case BC_t::dispatch_stack_:
        return immediate.dispatch_stack_args.nargs;
    case BC_t::call_: {
        size_t nargs =
            Rf_length(Pool::get(immediate.call_args.args)) / sizeof(fun_idx_t);
        assert(nargs < MAX_NUM_ARGS);
        return (num_args_t)nargs;
    }
    case BC_t::dispatch_: {
        size_t nargs = Rf_length(Pool::get(immediate.dispatch_args.args)) /
                       sizeof(fun_idx_t);
        assert(nargs < MAX_NUM_ARGS);
        return (num_args_t)nargs;
    }
    case BC_t::promise_: {
        return 1;
    }
    default:
        assert(false);
    }
    assert(false);
    return 0;
}
SEXP BC::immediateCallNames() {
    pool_idx_t names = 0;
    switch (bc) {
    case BC_t::call_stack_:
        names = immediate.call_stack_args.names;
        break;
    case BC_t::call_:
        names = immediate.call_args.names;
        break;
    case BC_t::dispatch_:
        names = immediate.dispatch_args.names;
        break;
    case BC_t::dispatch_stack_:
        names = immediate.dispatch_stack_args.names;
        break;
    default:
        assert(false);
    }
    return names ? Pool::get(names) : nullptr;
}

void BC::print() {
    if (bc != BC_t::label) {
        Rprintf("   ");
        Rprintf("%s ", name(bc));
    }

    switch (bc) {
    case BC_t::invalid_:
    case BC_t::num_of:
        assert(false);
        break;
    case BC_t::dispatch_: {
        SEXP selector = Pool::get(immediate.dispatch_args.selector);
        Rprintf(" `%s` ", CHAR(PRINTNAME(selector)));

        fun_idx_t* args = immediateCallArgs();
        num_args_t nargs = immediateCallNargs();
        Rprintf("[");
        for (unsigned i = 0; i < nargs; ++i) {
            if (args[i] == MISSING_ARG_IDX)
                Rprintf(" _");
            else if (args[i] == DOTS_ARG_IDX)
                Rprintf(" ...");
            else
                Rprintf(" %x", args[i]);
        }
        Rprintf("] ");
        SEXP names = immediateCallNames();
        if (names != R_NilValue) {
            Rprintf("[");
            for (auto n : RVector(names)) {
                Rprintf(" %s", (n == R_NilValue ? "_" : CHAR(PRINTNAME(n))));
            }
            Rprintf("]");
        }
        break;
    }
    case BC_t::call_: {
        fun_idx_t* args = immediateCallArgs();
        num_args_t nargs = immediateCallNargs();
        Rprintf("[");
        for (unsigned i = 0; i < nargs; ++i) {
            if (args[i] == MISSING_ARG_IDX)
                Rprintf(" _");
            else if (args[i] == DOTS_ARG_IDX)
                Rprintf(" ...");
            else
                Rprintf(" %x", args[i]);
        }
        Rprintf("] ");
        SEXP names = immediateCallNames();
        if (names != R_NilValue) {
            Rprintf("[");
            for (auto n : RVector(names)) {
                Rprintf(" %s", (n == R_NilValue ? "_" : CHAR(PRINTNAME(n))));
            }
            Rprintf("]");
        }
        break;
    }
    case BC_t::call_stack_: {
        num_args_t nargs = immediateCallNargs();
        Rprintf(" %d ", nargs);
        SEXP names = immediateCallNames();
        if (names != R_NilValue) {
            Rprintf("[");
            for (auto n : RVector(names)) {
                Rprintf(" %s", (n == R_NilValue ? "_" : CHAR(PRINTNAME(n))));
            }
            Rprintf("]");
        }
        break;
    }
    case BC_t::dispatch_stack_: {
        SEXP selector = Pool::get(immediate.dispatch_stack_args.selector);
        Rprintf(" `%s` ", CHAR(PRINTNAME(selector)));
        num_args_t nargs = immediateCallNargs();
        Rprintf(" %d ", nargs);
        SEXP names = immediateCallNames();
        if (names != R_NilValue) {
            Rprintf("[");
            for (auto n : RVector(names)) {
                Rprintf(" %s", (n == R_NilValue ? "_" : CHAR(PRINTNAME(n))));
            }
            Rprintf("]");
        }
        break;
    }
    case BC_t::push_:
        Rprintf(" %u # ", immediate.pool);
        Rf_PrintValue(immediateConst());
        return;
    case BC_t::isspecial_:
    case BC_t::ldarg_:
    case BC_t::ldfun_:
    case BC_t::ldvar_:
    case BC_t::ldddvar_:
    case BC_t::stvar_:
    case BC_t::missing_:
        Rprintf(" %u # %s", immediate.pool, CHAR(PRINTNAME((immediateConst()))));
        break;
    case BC_t::pick_:
    case BC_t::pull_:
    case BC_t::put_:
        Rprintf(" %i", immediate.i);
        break;
    case BC_t::is_:
    case BC_t::alloc_:
        Rprintf(" %s", type2char(immediate.i));
        break;
    case BC_t::force_:
    case BC_t::pop_:
    case BC_t::seq_:
    case BC_t::ret_:
    case BC_t::swap_:
    case BC_t::int3_:
    case BC_t::uniq_:
    case BC_t::dup_:
    case BC_t::inc_:
    case BC_t::dup2_:
    case BC_t::test_bounds_:
    case BC_t::asast_:
    case BC_t::asbool_:
    case BC_t::add_:
    case BC_t::mul_:
    case BC_t::sub_:
    case BC_t::lt_:
    case BC_t::return_:
    case BC_t::isfun_:
    case BC_t::invisible_:
    case BC_t::visible_:
    case BC_t::subset2_:
    case BC_t::extract2_:
    case BC_t::subset1_:
    case BC_t::extract1_:
    case BC_t::close_:
    case BC_t::length_:
    case BC_t::names_:
    case BC_t::set_names_:
    case BC_t::endcontext_:
    case BC_t::aslogical_:
    case BC_t::lgl_or_:
    case BC_t::lgl_and_:
    case BC_t::subassign_:
    case BC_t::subassign2_:
        break;
    case BC_t::promise_:
    case BC_t::push_code_:
        Rprintf(" %x", immediate.fun);
        break;
    case BC_t::beginloop_:
    case BC_t::brtrue_:
    case BC_t::brobj_:
    case BC_t::brfalse_:
    case BC_t::br_:
        Rprintf(" %d", immediate.offset);
        break;
    case BC_t::label:
        Rprintf("%d:", immediate.offset);
        break;
    }
    Rprintf("\n");
}

BC BC::dispatch_stack(SEXP selector, uint32_t nargs, std::vector<SEXP> names,
                      SEXP call) {
    assert(nargs == names.size() || names.empty());
    assert(nargs <= MAX_ARG_IDX);
    assert(TYPEOF(selector) == SYMSXP);

    Protect p;
    bool hasNames = false;
    if (!names.empty())
        for (auto n : names) {
            if (n != R_NilValue) {
                hasNames = true;
                break;
            }
        }

    SEXP n;
    if (hasNames) {
        n = Rf_allocVector(VECSXP, names.size());
        p(n);
        for (size_t i = 0; i < names.size(); ++i) {
            SET_VECTOR_ELT(n, i, names[i]);
        }
        dispatch_stack_args_t args_ = {
            nargs, Pool::insert(n), Pool::insert(selector), Pool::insert(call)};
        immediate_t i;
        i.dispatch_stack_args = args_;
        return BC(BC_t::dispatch_stack_, i);
    }

    dispatch_stack_args_t args_ = {nargs, Pool::insert(R_NilValue),
                                   Pool::insert(selector), Pool::insert(call)};
    immediate_t i;
    i.dispatch_stack_args = args_;
    return BC(BC_t::dispatch_stack_, i);
}

BC BC::dispatch(SEXP selector, std::vector<fun_idx_t> args,
                std::vector<SEXP> names, SEXP call) {
    assert(args.size() == names.size() || names.empty());
    assert(args.size() <= MAX_ARG_IDX);
    assert(TYPEOF(selector) == SYMSXP);

    Protect p;
    SEXP a = Rf_allocVector(INTSXP, sizeof(fun_idx_t) * args.size());
    p(a);

    fun_idx_t* argsArray = (fun_idx_t*)INTEGER(a);
    for (size_t i = 0; i < args.size(); ++i) {
        argsArray[i] = args[i];
    }

    bool hasNames = false;
    if (!names.empty())
        for (auto n : names) {
            if (n != R_NilValue) {
                hasNames = true;
                break;
            }
        }

    SEXP n;
    if (hasNames) {
        n = Rf_allocVector(VECSXP, names.size());
        p(n);
        for (size_t i = 0; i < args.size(); ++i) {
            SET_VECTOR_ELT(n, i, names[i]);
        }
        dispatch_args_t args_ = {Pool::insert(a), Pool::insert(n),
                                 Pool::insert(selector), Pool::insert(call)};
        immediate_t i;
        i.dispatch_args = args_;
        return BC(BC_t::dispatch_, i);
    }

    dispatch_args_t args_ = {Pool::insert(a), Pool::insert(R_NilValue),
                             Pool::insert(selector), Pool::insert(call)};
    immediate_t i;
    i.dispatch_args = args_;
    return BC(BC_t::dispatch_, i);
}

BC BC::call(std::vector<fun_idx_t> args, std::vector<SEXP> names, SEXP call) {
    assert(args.size() == names.size() || names.empty());
    assert(args.size() <= MAX_ARG_IDX);

    Protect p;
    SEXP a = Rf_allocVector(INTSXP, sizeof(fun_idx_t) * args.size());
    p(a);

    fun_idx_t* argsArray = (fun_idx_t*)INTEGER(a);
    for (size_t i = 0; i < args.size(); ++i) {
        argsArray[i] = args[i];
    }

    bool hasNames = false;
    if (!names.empty())
        for (auto n : names) {
            if (n != R_NilValue) {
                hasNames = true;
                break;
            }
        }

    SEXP n;
    if (hasNames) {
        n = Rf_allocVector(VECSXP, names.size());
        p(n);
        for (size_t i = 0; i < args.size(); ++i) {
            SET_VECTOR_ELT(n, i, names[i]);
        }
        call_args_t args_ = {Pool::insert(a), Pool::insert(n),
                             Pool::insert(call)};
        immediate_t i;
        i.call_args = args_;
        return BC(BC_t::call_, i);
    }

    call_args_t args_ = {Pool::insert(a), Pool::insert(R_NilValue),
                         Pool::insert(call)};
    immediate_t i;
    i.call_args = args_;
    return BC(BC_t::call_, i);
}

BC BC::call_stack(unsigned nargs, std::vector<SEXP> names, SEXP call) {
    assert(nargs == names.size() || names.empty());

    bool hasNames = false;
    if (!names.empty())
        for (auto n : names) {
            if (n != R_NilValue) {
                hasNames = true;
                break;
            }
        }

    SEXP n;
    if (hasNames) {
        Protect p;

        n = Rf_allocVector(VECSXP, names.size());
        p(n);
        for (size_t i = 0; i < nargs; ++i) {
            SET_VECTOR_ELT(n, i, names[i]);
        }
        call_stack_args_t args_ = {nargs, Pool::insert(n), Pool::insert(call)};
        immediate_t i;
        i.call_stack_args = args_;
        return BC(BC_t::call_stack_, i);
    }

    call_stack_args_t args_ = {nargs, Pool::insert(R_NilValue),
                               Pool::insert(call)};
    immediate_t i;
    i.call_stack_args = args_;
    return BC(BC_t::call_stack_, i);
}
}
