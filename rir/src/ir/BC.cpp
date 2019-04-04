#include "BC.h"

#include "utils/Pool.h"
#include <iomanip>
#include <iostream>

#include "CodeStream.h"
#include "R/Funtab.h"
#include "R/Printing.h"
#include "R/RList.h"
#include "R/r.h"

namespace rir {

void BC::write(CodeStream& cs) const {
    cs.insert(bc);
    switch (bc) {
#define V(NESTED, name, name_) case Opcode::name_##_:
BC_NOARGS(V, _)
#undef V
        return;

    case Opcode::record_call_:
        // Call feedback targets are stored in the code extra pool. We don't
        // have access to them here, so we can't write a call feedback with
        // preseeded values.
        assert(immediate.callFeedback.numTargets == 0 &&
               "cannot write call feedback targets");
        cs.insert(immediate.callFeedback);
        return;

    case Opcode::record_binop_:
        cs.insert(immediate.binopFeedback[0]);
        cs.insert(immediate.binopFeedback[1]);
        return;

    case Opcode::push_:
    case Opcode::deopt_:
    case Opcode::ldfun_:
    case Opcode::ldddvar_:
    case Opcode::ldvar_:
    case Opcode::ldvar_noforce_:
    case Opcode::ldvar_super_:
    case Opcode::ldvar_noforce_super_:
    case Opcode::starg_:
    case Opcode::stvar_:
    case Opcode::stvar_super_:
    case Opcode::missing_:
        cs.insert(immediate.pool);
        return;

    case Opcode::guard_fun_:
        cs.insert(immediate.guard_fun_args);
        return;

    case Opcode::call_implicit_:
        cs.insert(immediate.callFixedArgs);
        for (FunIdx arg : callExtra().immediateCallArguments)
            cs.insert(arg);
        break;

    case Opcode::named_call_implicit_:
        cs.insert(immediate.callFixedArgs);
        for (FunIdx arg : callExtra().immediateCallArguments)
            cs.insert(arg);
        for (PoolIdx name : callExtra().callArgumentNames)
            cs.insert(name);
        break;

    case Opcode::call_:
        cs.insert(immediate.callFixedArgs);
        break;

    case Opcode::named_call_:
        cs.insert(immediate.callFixedArgs);
        for (PoolIdx name : callExtra().callArgumentNames)
            cs.insert(name);
        break;

    case Opcode::static_call_:
        cs.insert(immediate.staticCallFixedArgs);
        break;

    case Opcode::call_builtin_:
        cs.insert(immediate.callBuiltinFixedArgs);
        break;

    case Opcode::promise_:
    case Opcode::push_code_:
        cs.insert(immediate.fun);
        return;

    case Opcode::mk_stub_env_:
    case Opcode::mk_env_:
        cs.insert(immediate.mkEnvFixedArgs);
        for (PoolIdx name : mkEnvExtra().names)
            cs.insert(name);
        break;

    case Opcode::br_:
    case Opcode::brtrue_:
    case Opcode::beginloop_:
    case Opcode::push_context_:
    case Opcode::brobj_:
    case Opcode::brfalse_:
        cs.patchpoint(immediate.offset);
        return;

    case Opcode::popn_:
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

    case Opcode::invalid_:
    case Opcode::num_of:
        assert(false);
        return;
    }
}

SEXP BC::immediateConst() const { return Pool::get(immediate.pool); }

void BC::printImmediateArgs(std::ostream& out) const {
    out << "[";
    for (auto arg : callExtra().immediateCallArguments) {
        if (arg == MISSING_ARG_IDX)
            out << " _";
        else if (arg == DOTS_ARG_IDX)
            out << " ...";
        else
            out << " " << std::hex << arg << std::dec;
    }
    out << " ]";
}

void BC::printNames(std::ostream& out,
                    const std::vector<PoolIdx>& names) const {
    out << "[";
    for (auto name : names) {
        SEXP n = Pool::get(name);
        out << " "
            << (n == nullptr || n == R_NilValue ? "_" : CHAR(PRINTNAME(n)));
    }
    out << " ]";
}

void BC::printOpcode(std::ostream& out) const { out << name(bc) << "  "; }

void BC::print(std::ostream& out) const {
    out << "   ";
    if (bc != Opcode::record_call_ && bc != Opcode::record_binop_)
        printOpcode(out);

    switch (bc) {
    case Opcode::invalid_:
    case Opcode::num_of:
        assert(false);
        break;
    case Opcode::call_implicit_: {
        printImmediateArgs(out);
        break;
    }
    case Opcode::named_call_implicit_: {
        printImmediateArgs(out);
        out << " ";
        printNames(out, callExtra().callArgumentNames);
        break;
    }
    case Opcode::call_: {
        auto args = immediate.callFixedArgs;
        BC::NumArgs nargs = args.nargs;
        out << nargs;
        break;
    }

    case Opcode::named_call_: {
        auto args = immediate.callFixedArgs;
        BC::NumArgs nargs = args.nargs;
        out << nargs << " ";
        printNames(out, callExtra().callArgumentNames);
        break;
    }
    case Opcode::call_builtin_: {
        auto args = immediate.callBuiltinFixedArgs;
        BC::NumArgs nargs = args.nargs;
        auto target = Pool::get(args.builtin);
        out << nargs << " : " << dumpSexp(target).c_str();
        break;
    }
    case Opcode::static_call_: {
        auto args = immediate.staticCallFixedArgs;
        BC::NumArgs nargs = args.nargs;
        auto target = Pool::get(args.targetClosure);
        auto targetV = Pool::get(args.versionHint);
        out << nargs << " : ";
        if (targetV != R_NilValue)
            out << "(" << Function::unpack(targetV) << ") ";
        out << dumpSexp(target).c_str();
        break;
    }
    case Opcode::mk_stub_env_:
    case Opcode::mk_env_: {
        auto args = immediate.mkEnvFixedArgs;
        BC::NumArgs nargs = args.nargs;
        out << nargs << ", c" << args.context << "  ";
        printNames(out, mkEnvExtra().names);
        break;
    }
    case Opcode::deopt_: {
        DeoptMetadata* m = (DeoptMetadata*)DATAPTR(immediateConst());
        m->print(out);
        break;
    }
    case Opcode::push_:
        out << dumpSexp(immediateConst()).c_str();
        break;
    case Opcode::ldfun_:
    case Opcode::ldvar_:
    case Opcode::ldvar_noforce_:
    case Opcode::ldvar_super_:
    case Opcode::ldvar_noforce_super_:
    case Opcode::ldddvar_:
    case Opcode::starg_:
    case Opcode::stvar_:
    case Opcode::stvar_super_:
    case Opcode::missing_:
        out << CHAR(PRINTNAME(immediateConst()));
        break;
    case Opcode::guard_fun_: {
        SEXP name = Pool::get(immediate.guard_fun_args.name);
        out << CHAR(PRINTNAME(name))
            << " == " << Pool::get(immediate.guard_fun_args.expected);
        break;
    }
    case Opcode::popn_:
    case Opcode::pick_:
    case Opcode::pull_:
    case Opcode::put_:
        out << immediate.i;
        break;
    case Opcode::ldarg_:
        out << immediate.arg_idx;
        break;
    case Opcode::ldloc_:
    case Opcode::stloc_:
        out << "@" << immediate.loc;
        break;
    case Opcode::movloc_:
        out << "@" << immediate.loc_cpy.source << " -> @"
            << immediate.loc_cpy.target;
        break;
    case Opcode::is_:
    case Opcode::alloc_:
        out << type2char(immediate.i);
        break;
    case Opcode::record_call_: {
        ObservedCallees prof = immediate.callFeedback;
        out << "[ ";
        if (prof.taken == ObservedCallees::CounterOverflow)
            out << "*, <";
        else
            out << prof.taken << ", <";
        if (prof.numTargets == ObservedCallees::MaxTargets)
            out << "*>, ";
        else
            out << prof.numTargets << ">" << (prof.numTargets ? ", " : " ");
        for (int i = 0; i < prof.numTargets; ++i)
            out << callFeedbackExtra().targets[i] << "("
                << type2char(TYPEOF(callFeedbackExtra().targets[i])) << ") ";
        out << "]";
        break;
    }

    case Opcode::record_binop_: {
        auto prof = immediate.binopFeedback;
        out << "[ ";
        for (size_t j = 0; j < 2; ++j) {
            if (prof[j].numTypes) {
                for (size_t i = 0; i < prof[j].numTypes; ++i) {
                    auto t = prof[j].seen[i];
                    out << Rf_type2char(t.sexptype) << "("
                        << (t.object ? "o" : "") << (t.attribs ? "a" : "")
                        << (t.scalar ? "s" : "") << ")";
                    if (i != (unsigned)prof[j].numTypes - 1)
                        out << ", ";
                }
            } else {
                out << "<?>";
            }
            if (j == 0)
                out << " x ";
        }
        out << " ]";
        break;
    }

#define V(NESTED, name, name_) case Opcode::name_##_:
BC_NOARGS(V, _)
#undef V
        break;
    case Opcode::promise_:
    case Opcode::push_code_:
        out << std::hex << immediate.fun << std::dec;
        break;
    case Opcode::beginloop_:
    case Opcode::push_context_:
    case Opcode::brtrue_:
    case Opcode::brobj_:
    case Opcode::brfalse_:
    case Opcode::br_:
        out << immediate.offset;
        break;
    }
    out << "\n";
}

} // namespace rir
