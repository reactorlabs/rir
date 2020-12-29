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
    case Opcode::isnonobj_:
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

    case Opcode::record_test_:
        cs.insert(immediate.testFeedback);
        break;

    case Opcode::record_type_:
        cs.insert(immediate.typeFeedback);
        break;

    case Opcode::push_:
    case Opcode::ldfun_:
    case Opcode::ldddvar_:
    case Opcode::ldvar_:
    case Opcode::ldvar_for_update_:
    case Opcode::ldvar_super_:
    case Opcode::stvar_:
    case Opcode::starg_:
    case Opcode::stvar_super_:
    case Opcode::missing_:
        cs.insert(immediate.pool);
        return;

    case Opcode::ldvar_cached_:
    case Opcode::ldvar_for_update_cache_:
    case Opcode::stvar_cached_:
    case Opcode::starg_cached_:
        cs.insert(immediate.poolAndCache);
        return;

    case Opcode::guard_fun_:
        cs.insert(immediate.guard_fun_args);
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

    case Opcode::call_builtin_:
        cs.insert(immediate.callBuiltinFixedArgs);
        break;

    case Opcode::mk_promise_:
    case Opcode::mk_eager_promise_:
    case Opcode::push_code_:
        cs.insert(immediate.fun);
        return;

    case Opcode::br_:
    case Opcode::brtrue_:
    case Opcode::beginloop_:
    case Opcode::brfalse_:
        cs.patchpoint(immediate.offset);
        break;

    case Opcode::popn_:
    case Opcode::pick_:
    case Opcode::pull_:
    case Opcode::is_:
    case Opcode::put_:
        cs.insert(immediate.i);
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
        case Opcode::isnonobj_:
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
        case Opcode::ldvar_super_:
        case Opcode::stvar_:
        case Opcode::stvar_super_:
        case Opcode::missing_:
            i.pool = Pool::insert(ReadItem(refTable, inp));
            break;
        case Opcode::ldvar_cached_:
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
        case Opcode::call_:
        case Opcode::named_call_:
        case Opcode::call_dots_: {
            i.callFixedArgs.nargs = InInteger(inp);
            i.callFixedArgs.ast = Pool::insert(ReadItem(refTable, inp));
            InBytes(inp, &i.callFixedArgs.given, sizeof(Context));
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
        case Opcode::record_call_:
        case Opcode::record_type_:
        case Opcode::record_test_:
        case Opcode::mk_promise_:
        case Opcode::mk_eager_promise_:
        case Opcode::push_code_:
        case Opcode::br_:
        case Opcode::brtrue_:
        case Opcode::beginloop_:
        case Opcode::brfalse_:
        case Opcode::popn_:
        case Opcode::pick_:
        case Opcode::pull_:
        case Opcode::is_:
        case Opcode::put_:
        case Opcode::starg_:
        case Opcode::starg_cached_:
        case Opcode::clear_binding_cache_:
            assert((size - 1) % 4 == 0);
            InBytes(inp, code + 1, size - 1);
            break;
        case Opcode::invalid_:
        case Opcode::num_of:
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
        case Opcode::isnonobj_:
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
        case Opcode::ldvar_super_:
        case Opcode::stvar_:
        case Opcode::stvar_super_:
        case Opcode::missing_:
            WriteItem(Pool::get(i.pool), refTable, out);
            break;
        case Opcode::ldvar_cached_:
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
        case Opcode::call_:
        case Opcode::call_dots_:
        case Opcode::named_call_:
            OutInteger(out, i.callFixedArgs.nargs);
            WriteItem(Pool::get(i.callFixedArgs.ast), refTable, out);
            OutBytes(out, &i.callFixedArgs.given, sizeof(Context));
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
        case Opcode::record_call_:
        case Opcode::record_type_:
        case Opcode::record_test_:
        case Opcode::mk_promise_:
        case Opcode::mk_eager_promise_:
        case Opcode::push_code_:
        case Opcode::br_:
        case Opcode::brtrue_:
        case Opcode::beginloop_:
        case Opcode::brfalse_:
        case Opcode::popn_:
        case Opcode::pick_:
        case Opcode::pull_:
        case Opcode::is_:
        case Opcode::put_:
        case Opcode::starg_:
        case Opcode::starg_cached_:
        case Opcode::clear_binding_cache_:
            assert((size - 1) % 4 == 0);
            if (size != 0)
                OutBytes(out, code + 1, size - 1);
            break;
        case Opcode::invalid_:
        case Opcode::num_of:
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
    if (bc != Opcode::record_call_ && bc != Opcode::record_type_ &&
        bc != Opcode::record_test_)
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
    case Opcode::push_:
        out << dumpSexp(immediateConst());
        break;
    case Opcode::ldfun_:
    case Opcode::ldvar_:
    case Opcode::ldvar_for_update_:
    case Opcode::ldvar_super_:
    case Opcode::ldddvar_:
    case Opcode::stvar_:
    case Opcode::starg_:
    case Opcode::stvar_super_:
    case Opcode::missing_:
        out << CHAR(PRINTNAME(immediateConst()));
        break;
    case Opcode::ldvar_cached_:
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
    case Opcode::popn_:
    case Opcode::pick_:
    case Opcode::pull_:
    case Opcode::put_:
        out << immediate.i;
        break;
    case Opcode::is_:
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

    case Opcode::record_test_: {
        out << "[ ";
        switch (immediate.testFeedback.seen) {
        case ObservedTest::None:
            out << "_";
            break;
        case ObservedTest::OnlyTrue:
            out << "T";
            break;
        case ObservedTest::OnlyFalse:
            out << "F";
            break;
        case ObservedTest::Both:
            out << "?";
            break;
        }
        out << " ]";
        break;
    }

    case Opcode::record_type_: {
        out << "[ ";
        printTypeFeedback(immediate.typeFeedback);
        out << " ]";
        break;
    }

    case Opcode::isnonobj_:
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
    case Opcode::brtrue_:
    case Opcode::brfalse_:
    case Opcode::br_:
        out << immediate.offset;
        break;
    case Opcode::clear_binding_cache_:
        out << immediate.cacheIdx.start << " " << immediate.cacheIdx.size;
        break;
    }
    out << "\n";
}

} // namespace rir
