#include "BC.h"

#include "utils/Pool.h"
#include <iostream>
#include <iomanip>

#include "R/RList.h"
#include "CodeStream.h"
#include "R/r.h"
#include "R/Funtab.h"

#include "interpreter/deoptimizer.h"

namespace rir {

bool BC::operator==(const BC& other) const {
    if (bc != other.bc)
        return false;

    switch (bc) {
    case Opcode::push_:
    case Opcode::ldfun_:
    case Opcode::ldddvar_:
    case Opcode::ldarg_:
    case Opcode::ldvar_:
    case Opcode::ldvar2_:
    case Opcode::ldlval_:
    case Opcode::stvar_:
    case Opcode::stvar2_:
    case Opcode::missing_:
    case Opcode::subassign2_:
        return immediate.pool == other.immediate.pool;

    case Opcode::dispatch_:
    case Opcode::call_:
    case Opcode::call_stack_:
    case Opcode::static_call_stack_:
    case Opcode::dispatch_stack_:
        return immediate.call_args.call_id == other.immediate.call_args.call_id;

    case Opcode::guard_env_:
        return immediate.guard_id == other.immediate.guard_id;

    case Opcode::guard_fun_:
        return immediate.guard_fun_args.name ==
                   other.immediate.guard_fun_args.name &&
               immediate.guard_fun_args.expected ==
                   other.immediate.guard_fun_args.expected;

    case Opcode::promise_:
    case Opcode::push_code_:
        return immediate.fun == other.immediate.fun;

    case Opcode::br_:
    case Opcode::brtrue_:
    case Opcode::beginloop_:
    case Opcode::brobj_:
    case Opcode::brfalse_:
    case Opcode::label:
        return immediate.offset == other.immediate.offset;

    case Opcode::pick_:
    case Opcode::pull_:
    case Opcode::is_:
    case Opcode::put_:
    case Opcode::alloc_:
        return immediate.i == other.immediate.i;

    case Opcode::nop_:
    case Opcode::subset2_:
    case Opcode::extract2_:
    case Opcode::subset1_:
    case Opcode::extract1_:
    case Opcode::ret_:
    case Opcode::length_:
    case Opcode::names_:
    case Opcode::set_names_:
    case Opcode::force_:
    case Opcode::pop_:
    case Opcode::close_:
    case Opcode::asast_:
    case Opcode::asbool_:
    case Opcode::dup_:
    case Opcode::dup2_:
    case Opcode::test_bounds_:
    case Opcode::swap_:
    case Opcode::int3_:
    case Opcode::make_unique_:
    case Opcode::set_shared_:
    case Opcode::aslogical_:
    case Opcode::lgl_and_:
    case Opcode::lgl_or_:
    case Opcode::inc_:
    case Opcode::add_:
    case Opcode::mul_:
    case Opcode::div_:
    case Opcode::idiv_:
    case Opcode::mod_:
    case Opcode::pow_:
    case Opcode::sub_:
    case Opcode::uplus_:
    case Opcode::uminus_:
    case Opcode::not_:
    case Opcode::lt_:
    case Opcode::gt_:
    case Opcode::le_:
    case Opcode::ge_:
    case Opcode::eq_:
    case Opcode::ne_:
    case Opcode::seq_:
    case Opcode::colon_:
    case Opcode::return_:
    case Opcode::isfun_:
    case Opcode::invisible_:
    case Opcode::visible_:
    case Opcode::endcontext_:
    case Opcode::subassign_:
        return true;

    case Opcode::invalid_:
    case Opcode::num_of:
        break;
    }

    assert(false);
    return false;
}

void BC::write(CodeStream& cs) const {
    cs.insert(bc);
    switch (bc) {
    case Opcode::push_:
    case Opcode::ldarg_:
    case Opcode::ldfun_:
    case Opcode::ldddvar_:
    case Opcode::ldvar_:
    case Opcode::ldvar2_:
    case Opcode::ldlval_:
    case Opcode::stvar_:
    case Opcode::stvar2_:
    case Opcode::missing_:
    case Opcode::subassign2_:
        cs.insert(immediate.pool);
        return;

    case Opcode::guard_env_:
        cs.insert(immediate.guard_id);
        return;

    case Opcode::guard_fun_:
        cs.insert(immediate.guard_fun_args);
        return;

    // They have to be inserted by CodeStream::insertCall
    case Opcode::call_:
    case Opcode::dispatch_:
    case Opcode::call_stack_:
    case Opcode::static_call_stack_:
    case Opcode::dispatch_stack_:
        assert(false);
        break;

    case Opcode::promise_:
    case Opcode::push_code_:
        cs.insert(immediate.fun);
        return;

    case Opcode::br_:
    case Opcode::brtrue_:
    case Opcode::beginloop_:
    case Opcode::brobj_:
    case Opcode::brfalse_:
        cs.patchpoint(immediate.offset);
        return;

    case Opcode::pick_:
    case Opcode::pull_:
    case Opcode::is_:
    case Opcode::put_:
    case Opcode::alloc_:
        cs.insert(immediate.i);
        return;

    case Opcode::nop_:
    case Opcode::subset2_:
    case Opcode::extract2_:
    case Opcode::subset1_:
    case Opcode::extract1_:
    case Opcode::ret_:
    case Opcode::length_:
    case Opcode::names_:
    case Opcode::set_names_:
    case Opcode::force_:
    case Opcode::pop_:
    case Opcode::close_:
    case Opcode::asast_:
    case Opcode::asbool_:
    case Opcode::dup_:
    case Opcode::dup2_:
    case Opcode::test_bounds_:
    case Opcode::swap_:
    case Opcode::int3_:
    case Opcode::make_unique_:
    case Opcode::set_shared_:
    case Opcode::aslogical_:
    case Opcode::lgl_and_:
    case Opcode::lgl_or_:
    case Opcode::inc_:
    case Opcode::add_:
    case Opcode::mul_:
    case Opcode::div_:
    case Opcode::idiv_:
    case Opcode::mod_:
    case Opcode::pow_:
    case Opcode::sub_:
    case Opcode::uplus_:
    case Opcode::uminus_:
    case Opcode::not_:
    case Opcode::lt_:
    case Opcode::gt_:
    case Opcode::le_:
    case Opcode::ge_:
    case Opcode::eq_:
    case Opcode::ne_:
    case Opcode::seq_:
    case Opcode::colon_:
    case Opcode::return_:
    case Opcode::isfun_:
    case Opcode::invisible_:
    case Opcode::visible_:
    case Opcode::endcontext_:
    case Opcode::subassign_:
        return;

    case Opcode::invalid_:
    case Opcode::num_of:
    case Opcode::label:
        assert(false);
        return;
    }
}

SEXP BC::immediateConst() { return Pool::get(immediate.pool); }

void BC::printArgs(CallSite cs) {
    Rprintf("[");
    for (unsigned i = 0; i < cs.nargs(); ++i) {
        auto arg = cs.arg(i);
        if (arg == MISSING_ARG_IDX)
            Rprintf(" _");
        else if (arg == DOTS_ARG_IDX)
            Rprintf(" ...");
        else
            Rprintf(" %x", arg);
    }
    Rprintf("] ");
}

void BC::printNames(CallSite cs) {
    if (cs.hasNames()) {
        Rprintf("[");
        for (unsigned i = 0; i < cs.nargs(); ++i) {
            SEXP n = cs.name(i);
            Rprintf(
                " %s",
                (n == nullptr || n == R_NilValue ? "_" : CHAR(PRINTNAME(n))));
        }
        Rprintf("]");
    }
}

void BC::printProfile(CallSite cs) {
    if (cs.hasProfile()) {
        CallSiteProfile* prof = cs.profile();
        Rprintf("           Prof : [");
        if (prof->takenOverflow)
            Rprintf("*, <");
        else
            Rprintf("%u, <", prof->taken);
        if (prof->targetsOverflow)
            Rprintf("*>, ");
        else
            Rprintf("%u> ", prof->numTargets);
        for (int i = 0; i < prof->numTargets; ++i)
            Rprintf("%p(%s) ", prof->targets[i],
                    type2char(TYPEOF(prof->targets[i])));
        Rprintf("]\n");
    }
}

CallSite BC::callSite(Code* code) {
    return CallSite(*this, CallSite_get(code, immediate.call_args.call_id));
}

void BC::print() { print(CallSite()); }

void BC::print(CallSite cs) {
    if (bc != Opcode::label) {
        Rprintf("   ");
        Rprintf("%s ", name(bc));
    }

    switch (bc) {
    case Opcode::invalid_:
    case Opcode::num_of:
        assert(false);
        break;
    case Opcode::dispatch_: {
        if (cs.isValid()) {
            SEXP selector = cs.selector();
            Rprintf(" `%s` ", CHAR(PRINTNAME(selector)));
            Rprintf("\n        # ");
            Rf_PrintValue(cs.call());
            printProfile(cs);
        }
        break;
    }
    case Opcode::call_: {
        if (cs.isValid()) {
            printArgs(cs);
            printNames(cs);
            Rprintf("\n        -> ");
            Rf_PrintValue(cs.call());
            printProfile(cs);
        }
        break;
    }
    case Opcode::call_stack_: {
        NumArgsT nargs = immediate.call_args.nargs;
        Rprintf(" %d ", nargs);
        if (cs.isValid()) {
            printNames(cs);
            Rprintf("\n        -> ");
            Rf_PrintValue(cs.call());
            printProfile(cs);
        }
        break;
    }
    case Opcode::static_call_stack_: {
        NumArgsT nargs = immediate.call_args.nargs;
        Rprintf(" %d ", nargs);
        if (cs.isValid()) {
            Rprintf(" %p ", cs.target());
            Rprintf("\n        -> ");
            Rf_PrintValue(cs.call());
            printProfile(cs);
        }
        break;
    }
    case Opcode::dispatch_stack_: {
        if (cs.isValid()) {
            Rprintf(" `%s` ", CHAR(PRINTNAME(cs.selector())));
            Rprintf(" %d ", cs.nargs());
            printNames(cs);
            Rprintf("\n        -> ");
            Rf_PrintValue(cs.call());
            printProfile(cs);
        }
        break;
    }
    case Opcode::push_:
        Rprintf(" %u # ", immediate.pool);
        Rf_PrintValue(immediateConst());
        return;
    case Opcode::ldarg_:
    case Opcode::ldfun_:
    case Opcode::ldvar_:
    case Opcode::ldvar2_:
    case Opcode::ldlval_:
    case Opcode::ldddvar_:
    case Opcode::stvar_:
    case Opcode::stvar2_:
    case Opcode::missing_:
        Rprintf(" %u # %s", immediate.pool, CHAR(PRINTNAME((immediateConst()))));
        break;
    case Opcode::guard_fun_: {
        SEXP name = Pool::get(immediate.guard_fun_args.name);
        Rprintf(" %s == %p", CHAR(PRINTNAME(name)),
                Pool::get(immediate.guard_fun_args.expected));
        break;
    }
    case Opcode::pick_:
    case Opcode::pull_:
    case Opcode::put_:
        Rprintf(" %i", immediate.i);
        break;
    case Opcode::is_:
    case Opcode::alloc_:
        Rprintf(" %s", type2char(immediate.i));
        break;
    case Opcode::guard_env_:
        Deoptimizer_print(immediate.guard_id);
        Rprintf("\n");
        break;
    case Opcode::nop_:
    case Opcode::force_:
    case Opcode::pop_:
    case Opcode::seq_:
    case Opcode::colon_:
    case Opcode::ret_:
    case Opcode::swap_:
    case Opcode::int3_:
    case Opcode::make_unique_:
    case Opcode::set_shared_:
    case Opcode::dup_:
    case Opcode::inc_:
    case Opcode::dup2_:
    case Opcode::test_bounds_:
    case Opcode::asast_:
    case Opcode::asbool_:
    case Opcode::add_:
    case Opcode::mul_:
    case Opcode::div_:
    case Opcode::idiv_:
    case Opcode::mod_:
    case Opcode::pow_:
    case Opcode::sub_:
    case Opcode::uplus_:
    case Opcode::uminus_:
    case Opcode::not_:
    case Opcode::lt_:
    case Opcode::gt_:
    case Opcode::le_:
    case Opcode::ge_:
    case Opcode::eq_:
    case Opcode::ne_:
    case Opcode::return_:
    case Opcode::isfun_:
    case Opcode::invisible_:
    case Opcode::visible_:
    case Opcode::subset2_:
    case Opcode::extract2_:
    case Opcode::subset1_:
    case Opcode::extract1_:
    case Opcode::close_:
    case Opcode::length_:
    case Opcode::names_:
    case Opcode::set_names_:
    case Opcode::endcontext_:
    case Opcode::aslogical_:
    case Opcode::lgl_or_:
    case Opcode::lgl_and_:
    case Opcode::subassign_:
    case Opcode::subassign2_:
        break;
    case Opcode::promise_:
    case Opcode::push_code_:
        Rprintf(" %x", immediate.fun);
        break;
    case Opcode::beginloop_:
    case Opcode::brtrue_:
    case Opcode::brobj_:
    case Opcode::brfalse_:
    case Opcode::br_:
        Rprintf(" %d", immediate.offset);
        break;
    case Opcode::label:
        Rprintf("%d:", immediate.offset);
        break;
    }
    Rprintf("\n");
}

CallSite::CallSite(BC bc, CallSiteStruct* cs) : bc(bc), cs(cs) {
    assert(bc.isCallsite());
}

SEXP CallSite::selector() { return Pool::get(*CallSite_selector(cs)); }

SEXP CallSite::target() { return Pool::get(*CallSite_target(cs)); }

SEXP CallSite::call() { return Pool::get(cs->call); }

SEXP CallSite::name(PoolIdxT i) { return Pool::get(CallSite_names(cs)[i]); }
}
