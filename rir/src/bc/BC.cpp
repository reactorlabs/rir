#include "BC.h"
#include "R/Printing.h"
#include "R/Serialize.h"
#include "R/r.h"
#include "bc/CodeStream.h"
#include "serializeHash/serialize/serialize.h"
#include "utils/Pool.h"

#include <iomanip>

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
    case Opcode::ldvar_noforce_:
    case Opcode::ldvar_for_update_:
    case Opcode::ldvar_super_:
    case Opcode::stvar_:
    case Opcode::stvar_super_:
    case Opcode::missing_:
        cs.insert(immediate.pool);
        return;

    case Opcode::ldvar_cached_:
    case Opcode::ldvar_for_update_cache_:
    case Opcode::stvar_cached_:
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
#define V(NESTED, name, name_) case Opcode::name_##_:
            BC_NOARGS(V, _)
#undef V
            assert(*code != Opcode::nop_);
            break;
        case Opcode::push_:
        case Opcode::ldfun_:
        case Opcode::ldddvar_:
        case Opcode::ldvar_:
        case Opcode::ldvar_noforce_:
        case Opcode::ldvar_for_update_:
        case Opcode::ldvar_super_:
        case Opcode::stvar_:
        case Opcode::stvar_super_:
        case Opcode::missing_:
            i.pool = Pool::readItem(refTable, inp);
            break;
        case Opcode::ldvar_cached_:
        case Opcode::ldvar_for_update_cache_:
        case Opcode::stvar_cached_:
            i.poolAndCache.poolIndex = Pool::readItem(refTable, inp);
            i.poolAndCache.cacheIndex = InInteger(inp);
            break;
        case Opcode::guard_fun_:
            i.guard_fun_args.name = Pool::readItem(refTable, inp);
            i.guard_fun_args.expected = Pool::readItem(refTable, inp);
            i.guard_fun_args.id = InInteger(inp);
            break;
        case Opcode::call_:
        case Opcode::named_call_:
        case Opcode::call_dots_: {
            i.callFixedArgs.nargs = InInteger(inp);
            i.callFixedArgs.ast = Pool::readItem(refTable, inp);
            InBytes(inp, &i.callFixedArgs.given, sizeof(Context));
            Opcode* c = code + 1 + sizeof(CallFixedArgs);
            // Read implicit promise argument offsets
            // Read named arguments
            if (*code == Opcode::named_call_ || *code == Opcode::call_dots_) {
                PoolIdx* names = (PoolIdx*)c;
                for (size_t j = 0; j < i.callFixedArgs.nargs; j++)
                    names[j] = Pool::readItem(refTable, inp);
            }
            break;
        }
        case Opcode::call_builtin_:
            i.callBuiltinFixedArgs.nargs = InInteger(inp);
            i.callBuiltinFixedArgs.ast = Pool::readItem(refTable, inp);
            i.callBuiltinFixedArgs.builtin =
                Pool::readItem(refTable, inp);
            break;
        case Opcode::record_call_:
        case Opcode::record_type_:
        case Opcode::record_test_:
        case Opcode::mk_promise_:
        case Opcode::mk_eager_promise_:
        case Opcode::br_:
        case Opcode::brtrue_:
        case Opcode::beginloop_:
        case Opcode::brfalse_:
        case Opcode::popn_:
        case Opcode::pick_:
        case Opcode::pull_:
        case Opcode::is_:
        case Opcode::put_:
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
#define V(NESTED, name, name_) case Opcode::name_##_:
            BC_NOARGS(V, _)
#undef V
            assert(*code != Opcode::nop_);
            break;
        case Opcode::push_:
        case Opcode::ldfun_:
        case Opcode::ldddvar_:
        case Opcode::ldvar_:
        case Opcode::ldvar_noforce_:
        case Opcode::ldvar_for_update_:
        case Opcode::ldvar_super_:
        case Opcode::stvar_:
        case Opcode::stvar_super_:
        case Opcode::missing_:
            Pool::writeItem(i.pool, refTable, out);
            break;
        case Opcode::ldvar_cached_:
        case Opcode::ldvar_for_update_cache_:
        case Opcode::stvar_cached_:
            Pool::writeItem(i.poolAndCache.poolIndex, refTable, out);
            OutInteger(out, i.poolAndCache.cacheIndex);
            break;
        case Opcode::guard_fun_:
            Pool::writeItem(i.guard_fun_args.name, refTable, out);
            Pool::writeItem(i.guard_fun_args.expected, refTable, out);
            OutInteger(out, i.guard_fun_args.id);
            break;
        case Opcode::call_:
        case Opcode::call_dots_:
        case Opcode::named_call_:
            OutInteger(out, i.callFixedArgs.nargs);
            Pool::writeItem(i.callFixedArgs.ast, refTable, out);
            OutBytes(out, &i.callFixedArgs.given, sizeof(Context));
            // Write named arguments
            if (*code == Opcode::named_call_ || *code == Opcode::call_dots_) {
                for (size_t j = 0; j < i.callFixedArgs.nargs; j++)
                    Pool::writeItem(bc.callExtra().callArgumentNames[j],
                                   refTable, out);
            }
            break;
        case Opcode::call_builtin_:
            OutInteger(out, i.callBuiltinFixedArgs.nargs);
            Pool::writeItem(i.callBuiltinFixedArgs.ast, refTable, out);
            Pool::writeItem(i.callBuiltinFixedArgs.builtin, refTable, out);
            break;
        case Opcode::record_call_:
        case Opcode::record_type_:
        case Opcode::record_test_:
        case Opcode::mk_promise_:
        case Opcode::mk_eager_promise_:
        case Opcode::br_:
        case Opcode::brtrue_:
        case Opcode::beginloop_:
        case Opcode::brfalse_:
        case Opcode::popn_:
        case Opcode::pick_:
        case Opcode::pull_:
        case Opcode::is_:
        case Opcode::put_:
        case Opcode::clear_binding_cache_:
            assert((size - 1) % 4 == 0);
            if (size != 0)
                OutBytes(out, code + 1, (int)size - 1);
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

void BC::hash(Hasher& hasher, const Opcode* code, size_t codeSize,
              const Code* container) {
    while (codeSize > 0) {
        const BC bc = BC::decode((Opcode*)code, container);
        hasher.hashBytesOf(*code);
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
        case Opcode::ldvar_noforce_:
        case Opcode::ldvar_for_update_:
        case Opcode::ldvar_super_:
        case Opcode::stvar_:
        case Opcode::stvar_super_:
        case Opcode::missing_:
            hasher.hashConstant(i.pool);
            break;
        case Opcode::ldvar_cached_:
        case Opcode::ldvar_for_update_cache_:
        case Opcode::stvar_cached_:
            hasher.hashConstant(i.poolAndCache.poolIndex);
            hasher.hashBytesOf(i.poolAndCache.cacheIndex);
            break;
        case Opcode::guard_fun_:
            hasher.hashConstant(i.guard_fun_args.name);
            hasher.hashConstant(i.guard_fun_args.expected);
            hasher.hashBytesOf(i.guard_fun_args.id);
            break;
        case Opcode::call_:
        case Opcode::call_dots_:
        case Opcode::named_call_:
            hasher.hashBytesOf(i.callFixedArgs.nargs);
            hasher.hashConstant(i.callFixedArgs.ast);
            hasher.hashBytesOf(i.callFixedArgs.given);
            // Hash named arguments
            if (*code == Opcode::named_call_ || *code == Opcode::call_dots_) {
                for (size_t j = 0; j < i.callFixedArgs.nargs; j++) {
                    hasher.hashConstant(bc.callExtra().callArgumentNames[j]);
                }
            }
            break;
        case Opcode::call_builtin_:
            hasher.hashBytesOf(i.callBuiltinFixedArgs.nargs);
            hasher.hashConstant(i.callBuiltinFixedArgs.ast);
            hasher.hashConstant(i.callBuiltinFixedArgs.builtin);
            break;
        case Opcode::record_call_:
        case Opcode::record_type_:
        case Opcode::record_test_:
            assert((size - 1) % 4 == 0);
            // Don't hash because these are recording instructions
            break;
        case Opcode::mk_promise_:
        case Opcode::mk_eager_promise_:
        case Opcode::br_:
        case Opcode::brtrue_:
        case Opcode::beginloop_:
        case Opcode::brfalse_:
        case Opcode::popn_:
        case Opcode::pick_:
        case Opcode::pull_:
        case Opcode::is_:
        case Opcode::put_:
        case Opcode::clear_binding_cache_:
            assert((size - 1) % 4 == 0);
            if (size != 0) {
                hasher.hashBytes(code + 1, (int)size - 1);
            }
            break;
        case Opcode::invalid_:
        case Opcode::num_of:
            assert(false);
            break;
        }
        size = bc.size();
#ifdef DEBUG_SERIAL
        if (bc.bc == Opcode::deopt_) {
            std::cout << "hashed: ";
            bc.print(std::cout);
        }
#endif
        assert(codeSize >= size);
        code += size;
        codeSize -= size;
    }
}

void BC::addConnected(ConnectedCollector& collector, const Opcode* code,
                      size_t codeSize, const Code* container) {
    while (codeSize > 0) {
        const BC bc = BC::decode((Opcode*)code, container);
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
        case Opcode::ldvar_noforce_:
        case Opcode::ldvar_for_update_:
        case Opcode::ldvar_super_:
        case Opcode::stvar_:
        case Opcode::stvar_super_:
        case Opcode::missing_:
            collector.addConstant(i.pool);
            break;
        case Opcode::ldvar_cached_:
        case Opcode::ldvar_for_update_cache_:
        case Opcode::stvar_cached_:
            collector.addConstant(i.poolAndCache.poolIndex);
            break;
        case Opcode::guard_fun_:
            collector.addConstant(i.guard_fun_args.name);
            collector.addConstant(i.guard_fun_args.expected);
            break;
        case Opcode::call_:
        case Opcode::call_dots_:
        case Opcode::named_call_:
            collector.addConstant(i.callFixedArgs.ast);
            // Add named arguments
            if (*code == Opcode::named_call_ || *code == Opcode::call_dots_) {
                for (size_t j = 0; j < i.callFixedArgs.nargs; j++) {
                    collector.addConstant(bc.callExtra().callArgumentNames[j]);
                }
            }
            break;
        case Opcode::call_builtin_:
            collector.addConstant(i.callBuiltinFixedArgs.ast);
            collector.addConstant(i.callBuiltinFixedArgs.builtin);
            break;
        case Opcode::record_call_:
        case Opcode::record_type_:
        case Opcode::record_test_:
        case Opcode::mk_promise_:
        case Opcode::mk_eager_promise_:
        case Opcode::br_:
        case Opcode::brtrue_:
        case Opcode::beginloop_:
        case Opcode::brfalse_:
        case Opcode::popn_:
        case Opcode::pick_:
        case Opcode::pull_:
        case Opcode::is_:
        case Opcode::put_:
        case Opcode::clear_binding_cache_:
            break;
        case Opcode::invalid_:
        case Opcode::num_of:
            assert(false);
            break;
        }
        size = bc.size();
#ifdef DEBUG_SERIAL
        if (bc.bc == Opcode::deopt_) {
            std::cout << "added connected in: ";
            bc.print(std::cout);
        }
#endif
        assert(codeSize >= size);
        code += size;
        codeSize -= size;
    }
}

void BC::addToPrettyGraph(const PrettyGraphInnerPrinter& p,
                          std::vector<bool>& addedExtraPoolEntries,
                          const rir::Opcode* code, size_t codeSize,
                          const rir::Code* container) {
    auto addEntry = [&](SEXP sexp, const char* type, PrettyGraphContentPrinter description){
        bool isInPool = false;
        for (unsigned i = 0; i < container->extraPoolSize; i++) {
            if (sexp == container->getExtraPoolEntry(i)) {
                addedExtraPoolEntries[i] = true;
                isInPool = true;
            }
        }
        if (TYPEOF(sexp) == EXTERNALSXP) {
            p.addEdgeTo(sexp, false, type, description,
                        !isInPool);
        }
    };
    auto addConstant = [&](PoolIdx idx, const char* type, PrettyGraphContentPrinter description = [](std::ostream& s){}){
        addEntry(Pool::get(idx), type, description);
    };
    auto addExtraPoolEntry = [&](PoolIdx idx, bool isChild, const char* type, PrettyGraphContentPrinter description = [](std::ostream& s){}){
        auto sexp = container->getExtraPoolEntry(idx);
        addedExtraPoolEntries[idx] = true;
        p.addEdgeTo(sexp, isChild, type, description);
    };

    while (codeSize > 0) {
        const BC bc = BC::decode((Opcode*)code, container);
        unsigned size = BC::fixedSize(*code);
        ImmediateArguments i = bc.immediate;
        switch (*code) {
#define V(NESTED, name, name_) case Opcode::name_##_:
            BC_NOARGS(V, _)
#undef V
            assert(*code != Opcode::nop_);
            break;
#define CONSTANT_CASE(op, accessor, type) case Opcode::op##_:                  \
            addConstant(i.accessor, type);  \
            break;
        CONSTANT_CASE(push, pool, "push")
        CONSTANT_CASE(ldfun, pool, "unexpected-name")
        CONSTANT_CASE(ldddvar, pool, "unexpected-name")
        CONSTANT_CASE(ldvar, pool, "unexpected-name")
        CONSTANT_CASE(ldvar_noforce, pool, "unexpected-name")
        CONSTANT_CASE(ldvar_for_update, pool, "unexpected-name")
        CONSTANT_CASE(ldvar_super, pool, "unexpected-name")
        CONSTANT_CASE(stvar, pool, "unexpected-name")
        CONSTANT_CASE(stvar_super, pool, "unexpected-name")
        CONSTANT_CASE(missing, pool, "unexpected-name")
        CONSTANT_CASE(ldvar_cached, poolAndCache.poolIndex, "unexpected-name")
        CONSTANT_CASE(ldvar_for_update_cache, poolAndCache.poolIndex, "unexpected-name")
        CONSTANT_CASE(stvar_cached, poolAndCache.poolIndex, "unexpected-name")
        case Opcode::guard_fun_:
            addConstant(i.guard_fun_args.name, "unexpected-name", [&](std::ostream& s){
                s << "guard_fun";
            });
            addConstant(i.guard_fun_args.expected, "guard");
            break;
        case Opcode::call_:
        case Opcode::call_dots_:
        case Opcode::named_call_: {
            auto callType =
                *code == Opcode::call_ ? "call" :
                *code == Opcode::call_dots_ ? "call_dots" :
                "named_call";
            addConstant(i.callFixedArgs.ast, "unexpected-ast", [&](std::ostream& s){
                s << callType << " ast";
            });
            // Add named arguments
            if (*code == Opcode::named_call_ || *code == Opcode::call_dots_) {
                for (size_t j = 0; j < i.callFixedArgs.nargs; j++) {
                    addConstant(bc.callExtra().callArgumentNames[j], "unexpected-name", [&](std::ostream& s){
                        s << callType << " argument";
                    });
                }
            }
            break;
        }
        case Opcode::call_builtin_:
            addConstant(i.callBuiltinFixedArgs.ast, "unexpected-ast");
            addConstant(i.callBuiltinFixedArgs.builtin, "unexpected-builtin");
            break;
        case Opcode::record_call_:
            for (auto j = 0; j < i.callFeedback.numTargets; j++) {
                addExtraPoolEntry(i.callFeedback.targets[j], false, "target", [&](std::ostream& s){
                    s << "record_call " << j;
                });
            }
            break;
        case Opcode::record_type_:
        case Opcode::record_test_:
            break;
        case Opcode::mk_promise_:
        case Opcode::mk_eager_promise_:
            addExtraPoolEntry(i.fun, true, "promise");
            break;
        case Opcode::br_:
        case Opcode::brtrue_:
        case Opcode::beginloop_:
        case Opcode::brfalse_:
        case Opcode::popn_:
        case Opcode::pick_:
        case Opcode::pull_:
        case Opcode::is_:
        case Opcode::put_:
        case Opcode::clear_binding_cache_:
            break;
        case Opcode::invalid_:
        case Opcode::num_of:
            // TODO: mark extra pool entry and add edge for any other bytecodes
            //   which reference extra pool entries
            assert(false);
            break;
        }
        size = bc.size();
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
        out << nargs << " : " << Print::dumpSexp(target);
        break;
    }
    case Opcode::push_:
        out << Print::dumpSexp(immediateConst());
        break;
    case Opcode::ldfun_:
    case Opcode::ldvar_:
    case Opcode::ldvar_noforce_:
    case Opcode::ldvar_for_update_:
    case Opcode::ldvar_super_:
    case Opcode::ldddvar_:
    case Opcode::stvar_:
    case Opcode::stvar_super_:
    case Opcode::missing_:
        out << CHAR(PRINTNAME(immediateConst()));
        break;
    case Opcode::ldvar_cached_:
    case Opcode::ldvar_for_update_cache_:
    case Opcode::stvar_cached_:
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
        out << (BC::RirTypecheck)immediate.i;
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
            out << prof.numTargets << ">, ";

        out << (prof.invalid ? "invalid" : "valid");
        out << (prof.numTargets ? ", " : " ");

        for (int i = 0; i < prof.numTargets; ++i)
            out << callFeedbackExtra().targets[i] << "("
                << Rf_type2char(TYPEOF(callFeedbackExtra().targets[i])) << ") ";
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
        immediate.typeFeedback.print(out);
        out << " ]";
        break;
    }

#define V(NESTED, name, name_) case Opcode::name_##_:
        BC_NOARGS(V, _)
#undef V
        break;
    case Opcode::mk_promise_:
    case Opcode::mk_eager_promise_:
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

std::ostream& operator<<(std::ostream& out, BC::RirTypecheck t) {
    switch (t) {
    case BC::RirTypecheck::isFactor:
        out << "Factor";
        break;
    case BC::RirTypecheck::isNonObject:
        out << "NonObject";
        break;
    case BC::RirTypecheck::isVector:
        out << "Vector";
        break;
    case BC::RirTypecheck::isNILSXP:
    case BC::RirTypecheck::isLGLSXP:
    case BC::RirTypecheck::isREALSXP:
    case BC::RirTypecheck::isSTRSXP:
    case BC::RirTypecheck::isINTSXP:
    case BC::RirTypecheck::isCPLXSXP:
    case BC::RirTypecheck::isRAWSXP:
    case BC::RirTypecheck::isEXPRSXP:
    case BC::RirTypecheck::isVECSXP:
    case BC::RirTypecheck::isLISTSXP:
        out << Rf_type2char((int)t);
        break;
    }
    return out;
}

constexpr std::array<BC::RirTypecheck, 8> BC::isVectorTypes;

} // namespace rir
