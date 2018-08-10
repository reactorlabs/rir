#include "BC.h"

#include "utils/Pool.h"
#include <iomanip>
#include <iostream>

#include "CodeStream.h"
#include "R/Funtab.h"
#include "R/Printing.h"
#include "R/RList.h"
#include "R/r.h"

#include "interpreter/deoptimizer.h"

namespace rir {

void BC::write(CodeStream& cs) const {
    cs.insert(bc);
    switch (bc) {
    case Opcode::push_:
    case Opcode::ldfun_:
    case Opcode::ldddvar_:
    case Opcode::ldvar_:
    case Opcode::ldvar_noforce_:
    case Opcode::ldvar_super_:
    case Opcode::ldvar_noforce_super_:
    case Opcode::ldlval_:
    case Opcode::stvar_:
    case Opcode::stvar_super_:
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

    case Opcode::call_implicit_:
        cs.insert(immediate.callFixedArgs);
        for (FunIdx arg : immediateCallArguments)
            cs.insert(arg);
        break;

    case Opcode::named_call_implicit_:
        cs.insert(immediate.callFixedArgs);
        for (FunIdx arg : immediateCallArguments)
            cs.insert(arg);
        for (PoolIdx name : callArgumentNames)
            cs.insert(name);
        break;

    case Opcode::call_:
        cs.insert(immediate.callFixedArgs);
        break;

    case Opcode::named_call_:
        cs.insert(immediate.callFixedArgs);
        for (PoolIdx name : callArgumentNames)
            cs.insert(name);
        break;

    case Opcode::static_call_:
        cs.insert(immediate.staticCallFixedArgs);
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

    case Opcode::ldarg_:
        cs.insert(immediate.arg_idx);
        return;

    case Opcode::ldloc_:
    case Opcode::stloc_:
        cs.insert(immediate.loc);
        return;

    case Opcode::movloc_:
        cs.insert(immediate.loc_cpy);
        return;

    case Opcode::record_call_:
        cs.insert(immediate.callFeedback);
        return;

    case Opcode::nop_:
    case Opcode::make_env_:
    case Opcode::get_env_:
    case Opcode::caller_env_:
    case Opcode::set_env_:
    case Opcode::extract1_1_:
    case Opcode::extract1_2_:
    case Opcode::extract2_1_:
    case Opcode::extract2_2_:
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
    case Opcode::for_seq_size_:
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
    case Opcode::identical_:
    case Opcode::ne_:
    case Opcode::seq_:
    case Opcode::colon_:
    case Opcode::return_:
    case Opcode::isfun_:
    case Opcode::invisible_:
    case Opcode::visible_:
    case Opcode::endcontext_:
    case Opcode::subassign1_:
    case Opcode::isobj_:
    case Opcode::check_missing_:
        return;

    case Opcode::invalid_:
    case Opcode::num_of:
    case Opcode::label:
        assert(false);
        return;
    }
}

SEXP BC::immediateConst() const { return Pool::get(immediate.pool); }

void BC::printImmediateArgs() const {
    Rprintf("[");
    for (auto arg : immediateCallArguments) {
        if (arg == MISSING_ARG_IDX)
            Rprintf(" _");
        else if (arg == DOTS_ARG_IDX)
            Rprintf(" ...");
        else
            Rprintf(" %x", arg);
    }
    Rprintf(" ] ");
}

void BC::printNames() const {
    Rprintf("[");
    for (auto name : callArgumentNames) {
        SEXP n = Pool::get(name);
        Rprintf(" %s",
                (n == nullptr || n == R_NilValue ? "_" : CHAR(PRINTNAME(n))));
    }
    Rprintf(" ]");
}

void BC::print() const {
    if (bc != Opcode::label) {
        Rprintf("   ");
        Rprintf("%s ", name(bc));
    }

    switch (bc) {
    case Opcode::invalid_:
    case Opcode::num_of:
        assert(false);
        break;
    case Opcode::call_implicit_: {
        printImmediateArgs();
        break;
    }
    case Opcode::named_call_implicit_: {
        printImmediateArgs();
        printNames();
        break;
    }
    case Opcode::call_: {
        auto args = immediate.callFixedArgs;
        BC::NumArgs nargs = args.nargs;
        Rprintf(" %d ", nargs);
        break;
    }

    case Opcode::named_call_: {
        auto args = immediate.callFixedArgs;
        BC::NumArgs nargs = args.nargs;
        Rprintf(" %d ", nargs);
        printNames();
        break;
    }
    case Opcode::static_call_: {
        auto args = immediate.staticCallFixedArgs;
        BC::NumArgs nargs = args.nargs;
        auto target = Pool::get(args.target);
        Rprintf(" %d : %s", nargs, dumpSexp(target).c_str());
        break;
    }
    case Opcode::push_:
        if (immediateConst() == R_UnboundValue)
            Rprintf(" -\n");
        else
            Rprintf("%s\n", dumpSexp(immediateConst()).c_str());
        return;
    case Opcode::ldfun_:
    case Opcode::ldvar_:
    case Opcode::ldvar_noforce_:
    case Opcode::ldvar_super_:
    case Opcode::ldvar_noforce_super_:
    case Opcode::ldlval_:
    case Opcode::ldddvar_:
    case Opcode::stvar_:
    case Opcode::stvar_super_:
    case Opcode::missing_:
        Rprintf(" %s", CHAR(PRINTNAME((immediateConst()))));
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
    case Opcode::ldarg_:
        Rprintf(" %u", immediate.arg_idx);
        break;
    case Opcode::ldloc_:
    case Opcode::stloc_:
        Rprintf(" @%i", immediate.loc);
        break;
    case Opcode::movloc_:
        Rprintf(" @%i -> @%i", immediate.loc_cpy.source,
                immediate.loc_cpy.target);
        break;
    case Opcode::is_:
    case Opcode::alloc_:
        Rprintf(" %s", type2char(immediate.i));
        break;
    case Opcode::guard_env_:
        Deoptimizer_print(immediate.guard_id);
        Rprintf("\n");
        break;

    case Opcode::record_call_: {
        CallFeedback prof = immediate.callFeedback;
        Rprintf(" [ ");
        if (prof.taken == CallFeedback::CounterOverflow)
            Rprintf("*, <");
        else
            Rprintf("%u, <", prof.taken);
        if (prof.numTargets == CallFeedback::MaxTargets)
            Rprintf("*>, ");
        else
            Rprintf("%u> ", prof.numTargets);
        for (int i = 0; i < prof.numTargets; ++i)
            Rprintf("%p(%s) ", prof.targets[i],
                    type2char(TYPEOF(prof.targets[i])));
        Rprintf("]");
        break;
    }

    case Opcode::nop_:
    case Opcode::make_env_:
    case Opcode::get_env_:
    case Opcode::caller_env_:
    case Opcode::set_env_:
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
    case Opcode::for_seq_size_:
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
    case Opcode::identical_:
    case Opcode::ne_:
    case Opcode::return_:
    case Opcode::isfun_:
    case Opcode::invisible_:
    case Opcode::visible_:
    case Opcode::extract1_1_:
    case Opcode::extract1_2_:
    case Opcode::extract2_1_:
    case Opcode::extract2_2_:
    case Opcode::close_:
    case Opcode::length_:
    case Opcode::names_:
    case Opcode::set_names_:
    case Opcode::endcontext_:
    case Opcode::aslogical_:
    case Opcode::lgl_or_:
    case Opcode::lgl_and_:
    case Opcode::subassign1_:
    case Opcode::subassign2_:
    case Opcode::isobj_:
    case Opcode::check_missing_:
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
}
