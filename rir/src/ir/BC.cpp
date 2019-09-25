#include "BC.h"

#include "utils/Pool.h"
#include <iomanip>
#include <iostream>

#include "CodeStream.h"
#include "R/Funtab.h"
#include "R/Printing.h"
#include "R/RList.h"
#include "R/Serialize.h"
#include "R/r.h"

namespace rir {

void BC::write(CodeStream& cs) const {
    cs.insert(bc);
    switch (bc) {
#define V(NESTED, name, name_) case Opcode::name_##_:
        BC_NOARGS(V, _)
#undef V
        return;

    case Opcode::clear_binding_cache_:
        cs.insert(immediate.cacheIdx);
        return;

    case Opcode::record_call_:
        // Call feedback targets are stored in the code extra pool. We don't
        // have access to them here, so we can't write a call feedback with
        // preseeded values.
        assert(immediate.callFeedback.numTargets == 0 &&
               "cannot write call feedback targets");
        cs.insert(immediate.callFeedback);
        return;

    case Opcode::record_type_:
        cs.insert(immediate.typeFeedback);
        break;

    case Opcode::push_:
    case Opcode::deopt_:
    case Opcode::ldfun_:
    case Opcode::ldddvar_:
    case Opcode::ldvar_:
    case Opcode::ldvar_for_update_:
    case Opcode::ldvar_noforce_:
    case Opcode::ldvar_super_:
    case Opcode::ldvar_noforce_super_:
    case Opcode::stvar_:
    case Opcode::starg_:
    case Opcode::stvar_super_:
    case Opcode::missing_:
        cs.insert(immediate.pool);
        return;

    case Opcode::ldvar_cached_:
    case Opcode::ldvar_noforce_cached_:
    case Opcode::ldvar_for_update_cache_:
    case Opcode::stvar_cached_:
    case Opcode::starg_cached_:
        cs.insert(immediate.poolAndCache);
        return;

    case Opcode::guard_fun_:
        cs.insert(immediate.guard_fun_args);
        return;

    case Opcode::record_deopt_:
        cs.insert(immediate.deoptReason);
        return;

    case Opcode::call_:
        cs.insert(immediate.callFixedArgs);
        break;

    case Opcode::named_call_:
    case Opcode::call_dots_:
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

    case Opcode::mk_promise_:
    case Opcode::mk_eager_promise_:
    case Opcode::push_code_:
        cs.insert(immediate.fun);
        return;

    case Opcode::mk_stub_env_:
    case Opcode::mk_env_:
        cs.insert(immediate.mkEnvFixedArgs);
        for (PoolIdx name : mkEnvExtra().names)
            cs.insert(name);
        break;

    case Opcode::mk_dotlist_:
        cs.insert(immediate.mkDotlistFixedArgs);
        for (PoolIdx name : mkEnvExtra().names)
            cs.insert(name);
        break;

    case Opcode::pop_context_:
        cs.insert(immediate.offset);
        return;

    case Opcode::br_:
    case Opcode::brtrue_:
    case Opcode::beginloop_:
    case Opcode::push_context_:
    case Opcode::brfalse_:
        cs.patchpoint(immediate.offset);
        return;

    case Opcode::popn_:
    case Opcode::pick_:
    case Opcode::pull_:
    case Opcode::is_:
    case Opcode::istype_:
    case Opcode::put_:
    case Opcode::alloc_:
    case Opcode::stvar_stubbed_:
    case Opcode::starg_stubbed_:
    case Opcode::ldvar_noforce_stubbed_:
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

    case Opcode::assert_type_:
        cs.insert(immediate.assertTypeArgs);
        return;

    case Opcode::invalid_:
    case Opcode::num_of:
        assert(false);
        return;
    }
}

SEXP BC::immediateConst() const {
    if (is(Opcode::ldvar_cached_) || is(Opcode::stvar_cached_))
        return Pool::get(immediate.poolAndCache.poolIndex);
    else
        return Pool::get(immediate.pool);
}

// #define DEBUG_SERIAL
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-align"

void BC::deserialize(SEXP refTable, R_inpstream_t inp, Opcode* code,
                     size_t codeSize, Code* container) {
    while (codeSize > 0) {
        *code = (Opcode)InChar(inp);
        unsigned size = BC::fixedSize(*code);
        ImmediateArguments& i = *(ImmediateArguments*)(code + 1);
        switch (*code) {
#define V(NESTED, name, name_) case Opcode::name_##_:
            BC_NOARGS(V, _)
#undef V
            assert(*code != Opcode::nop_);
            break;
        case Opcode::push_:
        case Opcode::ldfun_:
        case Opcode::ldddvar_:
        case Opcode::ldvar_:
        case Opcode::ldvar_for_update_:
        case Opcode::ldvar_noforce_:
        case Opcode::ldvar_super_:
        case Opcode::ldvar_noforce_super_:
        case Opcode::stvar_:
        case Opcode::stvar_super_:
        case Opcode::missing_:
            i.pool = Pool::insert(ReadItem(refTable, inp));
            break;
        case Opcode::ldvar_cached_:
        case Opcode::ldvar_noforce_cached_:
        case Opcode::ldvar_for_update_cache_:
        case Opcode::stvar_cached_:
            i.poolAndCache.poolIndex = Pool::insert(ReadItem(refTable, inp));
            i.poolAndCache.cacheIndex = InInteger(inp);
            break;
        case Opcode::guard_fun_:
            i.guard_fun_args.name = Pool::insert(ReadItem(refTable, inp));
            i.guard_fun_args.expected = Pool::insert(ReadItem(refTable, inp));
            i.guard_fun_args.id = InInteger(inp);
            break;
        case Opcode::mk_stub_env_:
        case Opcode::mk_env_: {
            InBytes(inp, code + 1, sizeof(MkEnvFixedArgs));
            BC::PoolIdx* names =
                (BC::PoolIdx*)(code + 1 + sizeof(MkEnvFixedArgs));
            for (unsigned j = 0; j < i.mkEnvFixedArgs.nargs; j++)
                names[j] = Pool::insert(ReadItem(refTable, inp));
            break;
        }
        case Opcode::mk_dotlist_: {
            InBytes(inp, code + 1, sizeof(MkDotlistFixedArgs));
            BC::PoolIdx* names =
                (BC::PoolIdx*)(code + 1 + sizeof(MkDotlistFixedArgs));
            for (unsigned j = 0; j < i.mkDotlistFixedArgs.nargs; j++)
                names[j] = Pool::insert(ReadItem(refTable, inp));
            break;
        }
        case Opcode::call_:
        case Opcode::named_call_:
        case Opcode::call_dots_: {
            i.callFixedArgs.nargs = InInteger(inp);
            i.callFixedArgs.ast = Pool::insert(ReadItem(refTable, inp));
            InBytes(inp, &i.callFixedArgs.given, sizeof(Assumptions));
            Opcode* c = code + 1 + sizeof(CallFixedArgs);
            // Read implicit promise argument offsets
            // Read named arguments
            if (*code == Opcode::named_call_ || *code == Opcode::call_dots_) {
                PoolIdx* names = (PoolIdx*)c;
                for (size_t j = 0; j < i.callFixedArgs.nargs; j++)
                    names[j] = Pool::insert(ReadItem(refTable, inp));
            }
            break;
        }
        case Opcode::call_builtin_:
            i.callBuiltinFixedArgs.nargs = InInteger(inp);
            i.callBuiltinFixedArgs.ast = Pool::insert(ReadItem(refTable, inp));
            i.callBuiltinFixedArgs.builtin =
                Pool::insert(ReadItem(refTable, inp));
            break;
        case Opcode::static_call_:
            i.callFixedArgs.nargs = InInteger(inp);
            i.callFixedArgs.ast = Pool::insert(ReadItem(refTable, inp));
            InBytes(inp, &i.callFixedArgs.given, sizeof(Assumptions));
            i.staticCallFixedArgs.targetClosure =
                Pool::insert(ReadItem(refTable, inp));
            i.staticCallFixedArgs.versionHint =
                Pool::insert(ReadItem(refTable, inp));
            break;
        case Opcode::deopt_: {
            SEXP meta = DeoptMetadata::deserialize(code, refTable, inp);
            i.pool = Pool::insert(meta);
            break;
        }
        case Opcode::assert_type_:
            i.assertTypeArgs.typeData1 = InInteger(inp);
            i.assertTypeArgs.typeData2 = InInteger(inp);
            if (InChar(inp))
                i.assertTypeArgs.instr = Pool::insert(ReadItem(refTable, inp));
            else
                i.assertTypeArgs.instr = -1;
            break;
        case Opcode::record_deopt_:
        case Opcode::record_call_:
        case Opcode::record_type_:
        case Opcode::mk_promise_:
        case Opcode::mk_eager_promise_:
        case Opcode::push_code_:
        case Opcode::br_:
        case Opcode::brtrue_:
        case Opcode::beginloop_:
        case Opcode::push_context_:
        case Opcode::brfalse_:
        case Opcode::popn_:
        case Opcode::pick_:
        case Opcode::pull_:
        case Opcode::is_:
        case Opcode::istype_:
        case Opcode::put_:
        case Opcode::alloc_:
        case Opcode::ldarg_:
        case Opcode::starg_:
        case Opcode::starg_cached_:
        case Opcode::ldloc_:
        case Opcode::stloc_:
        case Opcode::movloc_:
        case Opcode::ldvar_noforce_stubbed_:
        case Opcode::stvar_stubbed_:
        case Opcode::starg_stubbed_:
        case Opcode::clear_binding_cache_:
            assert((size - 1) % 4 == 0);
            InBytes(inp, code + 1, size - 1);
            break;
        case Opcode::invalid_:
        case Opcode::num_of:
        default:
            assert(false);
            break;
        }
        size = BC::size(code);
#ifdef DEBUG_SERIAL
        if (*code == Opcode::deopt_) {
            BC aBc = BC::decode(code, container);
            std::cout << "deserialized: ";
            aBc.print(std::cout);
        }
#endif
        assert(codeSize >= size);
        code += size;
        codeSize -= size;
    }
}

void BC::serialize(SEXP refTable, R_outpstream_t out, const Opcode* code,
                   size_t codeSize, const Code* container) {
    while (codeSize > 0) {
        const BC bc = BC::decode((Opcode*)code, container);
        OutChar(out, (int)*code);
        unsigned size = BC::fixedSize(*code);
        ImmediateArguments i = bc.immediate;
        switch (*code) {
#define V(NESTED, name, name_) case Opcode::name_##_:
            BC_NOARGS(V, _)
#undef V
            assert(*code != Opcode::nop_);
            break;
        case Opcode::push_:
        case Opcode::ldfun_:
        case Opcode::ldddvar_:
        case Opcode::ldvar_:
        case Opcode::ldvar_for_update_:
        case Opcode::ldvar_noforce_:
        case Opcode::ldvar_super_:
        case Opcode::ldvar_noforce_super_:
        case Opcode::stvar_:
        case Opcode::stvar_super_:
        case Opcode::missing_:
            WriteItem(Pool::get(i.pool), refTable, out);
            break;
        case Opcode::ldvar_cached_:
        case Opcode::ldvar_noforce_cached_:
        case Opcode::ldvar_for_update_cache_:
        case Opcode::stvar_cached_:
            WriteItem(Pool::get(i.poolAndCache.poolIndex), refTable, out);
            OutInteger(out, i.poolAndCache.cacheIndex);
            break;
        case Opcode::guard_fun_:
            WriteItem(Pool::get(i.guard_fun_args.name), refTable, out);
            WriteItem(Pool::get(i.guard_fun_args.expected), refTable, out);
            OutInteger(out, i.guard_fun_args.id);
            break;
        case Opcode::mk_stub_env_:
        case Opcode::mk_env_:
            OutBytes(out, code + 1, sizeof(MkEnvFixedArgs));
            for (unsigned j = 0; j < i.mkEnvFixedArgs.nargs; j++)
                WriteItem(Pool::get(bc.mkEnvExtra().names[j]), refTable, out);
            break;
        case Opcode::mk_dotlist_:
            OutBytes(out, code + 1, sizeof(MkDotlistFixedArgs));
            for (unsigned j = 0; j < i.mkDotlistFixedArgs.nargs; j++)
                WriteItem(Pool::get(bc.mkEnvExtra().names[j]), refTable, out);
            break;
        case Opcode::call_:
        case Opcode::call_dots_:
        case Opcode::named_call_:
            OutInteger(out, i.callFixedArgs.nargs);
            WriteItem(Pool::get(i.callFixedArgs.ast), refTable, out);
            OutBytes(out, &i.callFixedArgs.given, sizeof(Assumptions));
            // Write named arguments
            if (*code == Opcode::named_call_ || *code == Opcode::call_dots_) {
                for (size_t j = 0; j < i.callFixedArgs.nargs; j++)
                    WriteItem(Pool::get(bc.callExtra().callArgumentNames[j]),
                              refTable, out);
            }
            break;
        case Opcode::call_builtin_:
            OutInteger(out, i.callBuiltinFixedArgs.nargs);
            WriteItem(Pool::get(i.callBuiltinFixedArgs.ast), refTable, out);
            WriteItem(Pool::get(i.callBuiltinFixedArgs.builtin), refTable, out);
            break;
        case Opcode::static_call_:
            OutInteger(out, i.staticCallFixedArgs.nargs);
            WriteItem(Pool::get(i.staticCallFixedArgs.ast), refTable, out);
            OutBytes(out, &i.staticCallFixedArgs.given, sizeof(Assumptions));
            WriteItem(Pool::get(i.staticCallFixedArgs.targetClosure), refTable,
                      out);
            WriteItem(Pool::get(i.staticCallFixedArgs.versionHint), refTable,
                      out);
            break;
        case Opcode::deopt_: {
            DeoptMetadata* meta = (DeoptMetadata*)DATAPTR(Pool::get(i.pool));
            meta->serialize(code, refTable, out);
            break;
        }
        case Opcode::assert_type_:
            OutInteger(out, i.assertTypeArgs.typeData1);
            OutInteger(out, i.assertTypeArgs.typeData2);
            if ((int)i.assertTypeArgs.instr == -1)
                OutChar(out, false);
            else {
                OutChar(out, true);
                WriteItem(Pool::get(i.assertTypeArgs.instr), refTable, out);
            }
            break;
        case Opcode::record_deopt_:
        case Opcode::record_call_:
        case Opcode::record_type_:
        case Opcode::mk_promise_:
        case Opcode::mk_eager_promise_:
        case Opcode::push_code_:
        case Opcode::br_:
        case Opcode::brtrue_:
        case Opcode::beginloop_:
        case Opcode::push_context_:
        case Opcode::brfalse_:
        case Opcode::popn_:
        case Opcode::pick_:
        case Opcode::pull_:
        case Opcode::is_:
        case Opcode::istype_:
        case Opcode::put_:
        case Opcode::alloc_:
        case Opcode::ldarg_:
        case Opcode::starg_:
        case Opcode::starg_cached_:
        case Opcode::ldloc_:
        case Opcode::stloc_:
        case Opcode::movloc_:
        case Opcode::ldvar_noforce_stubbed_:
        case Opcode::stvar_stubbed_:
        case Opcode::starg_stubbed_:
        case Opcode::clear_binding_cache_:
            assert((size - 1) % 4 == 0);
            if (size != 0)
                OutBytes(out, code + 1, size - 1);
            break;
        case Opcode::invalid_:
        case Opcode::num_of:
        default:
            assert(false);
            break;
        }
        size = bc.size();
#ifdef DEBUG_SERIAL
        if (bc.bc == Opcode::deopt_) {
            std::cout << "serialized: ";
            bc.print(std::cout);
        }
#endif
        assert(codeSize >= size);
        code += size;
        codeSize -= size;
    }
}

#pragma GCC diagnostic pop

void BC::printImmediateArgs(std::ostream& out) const {
    out << "[";
    for (auto arg : callExtra().immediateCallArguments) {
        out << " " << std::hex << arg << std::dec;
    }
    out << " ]";
}

void BC::printNames(std::ostream& out,
                    const std::vector<PoolIdx>& names) const {
    out << "[";
    for (auto name : names) {
        SEXP n = Pool::get(name);
        out << " ";
        if (TYPEOF(n) == LISTSXP) {
            out << "(miss)";
            n = CAR(n);
        }
        out << (n == nullptr || n == R_NilValue ? "_" : CHAR(PRINTNAME(n)));
    }
    out << " ]";
}

void BC::printOpcode(std::ostream& out) const { out << name(bc) << "  "; }

void BC::print(std::ostream& out) const {
    out << "   ";
    if (bc != Opcode::record_call_ && bc != Opcode::record_type_)
        printOpcode(out);

    auto printTypeFeedback = [&](const ObservedValues& prof) {
        if (prof.numTypes) {
            for (size_t i = 0; i < prof.numTypes; ++i) {
                auto t = prof.seen[i];
                out << Rf_type2char(t.sexptype) << "(" << (t.object ? "o" : "")
                    << (t.attribs ? "a" : "") << (t.scalar ? "s" : "") << ")";
                if (i != (unsigned)prof.numTypes - 1)
                    out << ", ";
            }
            if (prof.stateBeforeLastForce !=
                ObservedValues::StateBeforeLastForce::unknown) {
                out << " | "
                    << ((prof.stateBeforeLastForce ==
                         ObservedValues::StateBeforeLastForce::value)
                            ? "value"
                            : (prof.stateBeforeLastForce ==
                               ObservedValues::StateBeforeLastForce::
                                   evaluatedPromise)
                                  ? "evaluatedPromise"
                                  : "promise");
            }
        } else {
            out << "<?>";
        }
    };

    switch (bc) {
    case Opcode::invalid_:
    case Opcode::num_of:
        assert(false);
        break;
    case Opcode::call_: {
        auto args = immediate.callFixedArgs;
        BC::NumArgs nargs = args.nargs;
        out << nargs;
        break;
    }
    case Opcode::call_dots_: {
        auto args = immediate.callFixedArgs;
        BC::NumArgs nargs = args.nargs;
        out << nargs << " ";
        printNames(out, callExtra().callArgumentNames);
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
    case Opcode::mk_dotlist_: {
        auto args = immediate.mkDotlistFixedArgs;
        BC::NumArgs nargs = args.nargs;
        out << nargs << ", ";
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
    case Opcode::ldvar_for_update_:
    case Opcode::ldvar_noforce_:
    case Opcode::ldvar_super_:
    case Opcode::ldvar_noforce_super_:
    case Opcode::ldddvar_:
    case Opcode::stvar_:
    case Opcode::starg_:
    case Opcode::stvar_super_:
    case Opcode::missing_:
        out << CHAR(PRINTNAME(immediateConst()));
        break;
    case Opcode::ldvar_cached_:
    case Opcode::ldvar_noforce_cached_:
    case Opcode::ldvar_for_update_cache_:
    case Opcode::stvar_cached_:
    case Opcode::starg_cached_:
        out << CHAR(PRINTNAME(immediateConst())) << "{"
            << immediate.poolAndCache.cacheIndex << "}";
        break;
    case Opcode::guard_fun_: {
        SEXP name = Pool::get(immediate.guard_fun_args.name);
        out << CHAR(PRINTNAME(name))
            << " == " << Pool::get(immediate.guard_fun_args.expected);
        break;
    }
    case Opcode::record_deopt_: {
        out << immediate.deoptReason.reason << " @ "
            << immediate.deoptReason.srcCode << "+"
            << immediate.deoptReason.originOffset;
        break;
    }
    case Opcode::popn_:
    case Opcode::pick_:
    case Opcode::pull_:
    case Opcode::put_:
    case Opcode::stvar_stubbed_:
    case Opcode::starg_stubbed_:
    case Opcode::ldvar_noforce_stubbed_:
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
    case Opcode::istype_:
    case Opcode::alloc_:
        switch (immediate.i) {
        case static_cast<Immediate>(TypeChecks::RealNonObject):
            out << "RealNonObject";
            break;
        case static_cast<Immediate>(TypeChecks::RealSimpleScalar):
            out << "RealSimpleScalar";
            break;
        case static_cast<Immediate>(TypeChecks::IntegerNonObject):
            out << "IntegerNotObject";
            break;
        case static_cast<Immediate>(TypeChecks::IntegerSimpleScalar):
            out << "IntegerSimpleScalar";
            break;
        case static_cast<Immediate>(TypeChecks::RealNonObjectWrapped):
            out << "RealNonObjectWrapped";
            break;
        case static_cast<Immediate>(TypeChecks::RealSimpleScalarWrapped):
            out << "RealSimpleScalarWrapped";
            break;
        case static_cast<Immediate>(TypeChecks::IntegerNonObjectWrapped):
            out << "IntegerNotObjectWrapped";
            break;
        case static_cast<Immediate>(TypeChecks::IntegerSimpleScalarWrapped):
            out << "IntegerSimpleScalarWrapped";
            break;
        case static_cast<Immediate>(TypeChecks::IsObject):
            out << "IsObject";
            break;
        case static_cast<Immediate>(TypeChecks::IsObjectWrapped):
            out << "IsObjectWrapped";
            break;
        default:
            out << type2char(immediate.i);
            break;
        }
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

    case Opcode::record_type_: {
        out << "[ ";
        printTypeFeedback(immediate.typeFeedback);
        out << " ]";
        break;
    }

#define V(NESTED, name, name_) case Opcode::name_##_:
        BC_NOARGS(V, _)
#undef V
        break;
    case Opcode::mk_promise_:
    case Opcode::mk_eager_promise_:
    case Opcode::push_code_:
        out << std::hex << immediate.fun << std::dec;
        break;
    case Opcode::beginloop_:
    case Opcode::pop_context_:
    case Opcode::push_context_:
    case Opcode::brtrue_:
    case Opcode::brfalse_:
    case Opcode::br_:
        out << immediate.offset;
        break;
    case Opcode::clear_binding_cache_:
        out << immediate.cacheIdx.start << " " << immediate.cacheIdx.size;
        break;
    case Opcode::assert_type_:
        out << immediate.assertTypeArgs.pirType();
        break;
    }
    out << "\n";
}

} // namespace rir
