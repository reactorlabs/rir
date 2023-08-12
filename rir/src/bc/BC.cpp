#include "BC.h"
#include "R/Printing.h"
#include "R/Serialize.h"
#include "R/r.h"
#include "bc/CodeStream.h"
#include "runtime/log/printRirObject.h"
#include "serializeHash/hash/UUIDPool.h"
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

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-align"

void BC::deserializeR(SEXP refTable, R_inpstream_t inp, Opcode* code,
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
        case Opcode::mk_promise_:
        case Opcode::mk_eager_promise_:
            i.fun = InInteger(inp);
            break;
        case Opcode::record_call_:
        case Opcode::record_type_:
        case Opcode::record_test_:
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
            if (size > 1) {
                InBytes(inp, code + 1, (int)size - 1);
            }
            break;
        case Opcode::invalid_:
        case Opcode::num_of:
            assert(false);
            break;
        }
        size = BC::size(code);
        assert(codeSize >= size);
        code += size;
        codeSize -= size;
    }
}

void BC::serializeR(std::vector<bool>& extraPoolChildren, SEXP refTable,
                    R_outpstream_t out, const Opcode* code, size_t codeSize,
                    const Code* container) {
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
        case Opcode::mk_promise_:
        case Opcode::mk_eager_promise_:
            OutInteger(out, i.fun);
            extraPoolChildren[i.fun] = true;
            break;
        case Opcode::record_call_:
        case Opcode::record_type_:
        case Opcode::record_test_:
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
            if (size > 1)
                OutBytes(out, code + 1, (int)size - 1);
            break;
        case Opcode::invalid_:
        case Opcode::num_of:
            assert(false);
            break;
        }
        size = bc.size();
        assert(codeSize >= size);
        code += size;
        codeSize -= size;
    }
}

void BC::deserialize(AbstractDeserializer& deserializer,
                     std::vector<SerialFlags>& extraPoolFlags, Opcode* code,
                     size_t codeSize, Code* container) {
    while (codeSize > 0) {
        *code = deserializer.readBytesOf<Opcode>(SerialFlags::CodeMisc);
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
            i.pool = deserializer.readConst(SerialFlags::CodeMisc);
            break;
        case Opcode::ldvar_cached_:
        case Opcode::ldvar_for_update_cache_:
        case Opcode::stvar_cached_:
            i.poolAndCache.poolIndex = deserializer.readConst(SerialFlags::CodeMisc);
            i.poolAndCache.cacheIndex = deserializer.readBytesOf<CacheIdx>(SerialFlags::CodeMisc);
            break;
        case Opcode::guard_fun_:
            i.guard_fun_args.name = deserializer.readConst(SerialFlags::CodeMisc);
            i.guard_fun_args.expected = deserializer.readConst(SerialFlags::CodeMisc);
            i.guard_fun_args.id = deserializer.readBytesOf<Immediate>(SerialFlags::CodeMisc);
            break;
        case Opcode::call_:
        case Opcode::named_call_:
        case Opcode::call_dots_: {
            i.callFixedArgs.nargs = deserializer.readBytesOf<NumArgs>(SerialFlags::CodeMisc);
            i.callFixedArgs.ast = deserializer.readConst(SerialFlags::CodeMisc);
            i.callFixedArgs.given = deserializer.readBytesOf<Context>(SerialFlags::CodeMisc);
            Opcode* c = code + 1 + sizeof(CallFixedArgs);
            // Read implicit promise argument offsets
            // Read named arguments
            if (*code == Opcode::named_call_ || *code == Opcode::call_dots_) {
                auto names = (PoolIdx*)c;
                for (size_t j = 0; j < i.callFixedArgs.nargs; j++) {
                    names[j] = deserializer.readConst(SerialFlags::CodeMisc);
                }
            }
            break;
        }
        case Opcode::call_builtin_:
            i.callBuiltinFixedArgs.nargs = deserializer.readBytesOf<NumArgs>(SerialFlags::CodeMisc);
            i.callBuiltinFixedArgs.ast = deserializer.readConst(SerialFlags::CodeMisc);
            i.callBuiltinFixedArgs.builtin = deserializer.readConst(SerialFlags::CodeMisc);
            break;
        case Opcode::mk_promise_:
        case Opcode::mk_eager_promise_:
            i.fun = deserializer.readBytesOf<FunIdx>(SerialFlags::CodeMisc);
            extraPoolFlags[i.fun] = SerialFlags::CodePromise;
            break;
        case Opcode::record_call_:
            i.callFeedback.numTargets = deserializer.readBytesOf<uint32_t>(SerialFlags::CodeFeedback);
            i.callFeedback.taken = deserializer.readBytesOf<uint32_t>(SerialFlags::CodeFeedback);
            i.callFeedback.invalid = deserializer.readBytesOf<uint32_t>(SerialFlags::CodeFeedback);
            for (size_t j = 0; j < i.callFeedback.numTargets; j++) {
                auto targetIdx = deserializer.readBytesOf<unsigned>(SerialFlags::CodeFeedback);
                extraPoolFlags[targetIdx] = SerialFlags::CodeFeedback;
                i.callFeedback.targets[j] = targetIdx;
            }
            break;
        case Opcode::record_type_:
            i.typeFeedback = deserializer.readBytesOf<ObservedValues>(SerialFlags::CodeFeedback);
            break;
        case Opcode::record_test_:
            i.testFeedback = deserializer.readBytesOf<ObservedTest>(SerialFlags::CodeFeedback);
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
            assert((size - 1) % 4 == 0);
            if (size > 1) {
                deserializer.readBytes((void*)(code + 1), size - 1,
                                       SerialFlags::CodeMisc);
            }
            break;
        case Opcode::invalid_:
        case Opcode::num_of:
            assert(false);
            break;
        }
        size = BC::size(code);
        assert(codeSize >= size);
        code += size;
        codeSize -= size;
    }
}

void BC::serialize(AbstractSerializer& serializer,
                   std::vector<SerialFlags>& extraPoolFlags,
                   const Opcode* code, size_t codeSize,
                   const Code* container) {
    while (codeSize > 0) {
        const auto bc = BC::decode((Opcode*)code, container);
        serializer.writeBytesOf<Opcode>(*code, SerialFlags::CodeMisc);
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
            serializer.writeConst(i.pool, SerialFlags::CodeMisc);
            break;
        case Opcode::ldvar_cached_:
        case Opcode::ldvar_for_update_cache_:
        case Opcode::stvar_cached_:
            serializer.writeConst(i.poolAndCache.poolIndex, SerialFlags::CodeMisc);
            serializer.writeBytesOf<CacheIdx>(i.poolAndCache.cacheIndex, SerialFlags::CodeMisc);
            break;
        case Opcode::guard_fun_:
            serializer.writeConst(i.guard_fun_args.name, SerialFlags::CodeMisc);
            serializer.writeConst(i.guard_fun_args.expected, SerialFlags::CodeMisc);
            serializer.writeBytesOf<Immediate>(i.guard_fun_args.id, SerialFlags::CodeMisc);
            break;
        case Opcode::call_:
        case Opcode::call_dots_:
        case Opcode::named_call_:
            serializer.writeBytesOf<NumArgs>(i.callFixedArgs.nargs, SerialFlags::CodeMisc);
            serializer.writeConst(i.callFixedArgs.ast, SerialFlags::CodeMisc);
            serializer.writeBytesOf<Context>(i.callFixedArgs.given, SerialFlags::CodeMisc);
            // Write named arguments
            if (*code == Opcode::named_call_ || *code == Opcode::call_dots_) {
                for (size_t j = 0; j < i.callFixedArgs.nargs; j++) {
                    serializer.writeConst(bc.callExtra().callArgumentNames[j], SerialFlags::CodeMisc);
                }
            }
            break;
        case Opcode::call_builtin_:
            serializer.writeBytesOf<NumArgs>(i.callBuiltinFixedArgs.nargs, SerialFlags::CodeMisc);
            serializer.writeConst(i.callBuiltinFixedArgs.ast, SerialFlags::CodeMisc);
            serializer.writeConst(i.callBuiltinFixedArgs.builtin, SerialFlags::CodeMisc);
            break;
        case Opcode::mk_promise_:
        case Opcode::mk_eager_promise_:
            serializer.writeBytesOf<FunIdx>(i.fun, SerialFlags::CodeMisc);
            extraPoolFlags[i.fun] = SerialFlags::CodePromise;
            break;
        case Opcode::record_call_:
            serializer.writeBytesOf(i.callFeedback.numTargets, SerialFlags::CodeFeedback);
            serializer.writeBytesOf(i.callFeedback.taken, SerialFlags::CodeFeedback);
            serializer.writeBytesOf(i.callFeedback.invalid, SerialFlags::CodeFeedback);
            for (size_t j = 0; j < i.callFeedback.numTargets; j++) {
                auto targetIdx = i.callFeedback.targets[j];
                serializer.writeBytesOf(targetIdx, SerialFlags::CodeFeedback);
                extraPoolFlags[targetIdx] = SerialFlags::CodeFeedback;
            }
            break;
        case Opcode::record_type_:
            serializer.writeBytesOf(i.typeFeedback, SerialFlags::CodeFeedback);
            break;
        case Opcode::record_test_:
            serializer.writeBytesOf(i.testFeedback, SerialFlags::CodeFeedback);
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
            assert((size - 1) % 4 == 0);
            if (size > 1) {
                serializer.writeBytes((void*)(code + 1), size - 1, SerialFlags::CodeMisc);
            }
            break;
        case Opcode::invalid_:
        case Opcode::num_of:
            assert(false);
            break;
        }
        size = bc.size();
        assert(codeSize >= size);
        code += size;
        codeSize -= size;
    }
}

void BC::deserializeSrc(ByteBuffer& buffer, Opcode* code, size_t codeSize,
                        Code* container) {
    size_t poolIdx = 0;
    while (codeSize > 0) {
        *code = (Opcode)buffer.getChar();
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
            i.pool = Pool::insert(rir::deserialize(buffer, false));
            break;
        case Opcode::ldvar_cached_:
        case Opcode::ldvar_for_update_cache_:
        case Opcode::stvar_cached_:
            i.poolAndCache.poolIndex = Pool::insert(rir::deserialize(buffer, false));
            i.poolAndCache.cacheIndex = buffer.getInt();
            break;
        case Opcode::guard_fun_:
            i.guard_fun_args.name = Pool::insert(rir::deserialize(buffer, false));
            i.guard_fun_args.expected = Pool::insert(rir::deserialize(buffer, false));
            i.guard_fun_args.id = buffer.getInt();
            break;
        case Opcode::call_:
        case Opcode::named_call_:
        case Opcode::call_dots_: {
            i.callFixedArgs.nargs = buffer.getInt();
            i.callFixedArgs.ast = Pool::insert(rir::deserialize(buffer, false));
            buffer.getBytes((uint8_t*)&i.callFixedArgs.given, sizeof(Context));
            Opcode* c = code + 1 + sizeof(CallFixedArgs);
            // Read implicit promise argument offsets
            // Read named arguments
            if (*code == Opcode::named_call_ || *code == Opcode::call_dots_) {
                auto names = (PoolIdx*)c;
                for (size_t j = 0; j < i.callFixedArgs.nargs; j++) {
                    names[j] = Pool::insert(rir::deserialize(buffer, false));
                }
            }
            break;
        }
        case Opcode::call_builtin_:
            i.callBuiltinFixedArgs.nargs = buffer.getInt();
            i.callBuiltinFixedArgs.ast = Pool::insert(rir::deserialize(buffer, false));
            i.callBuiltinFixedArgs.builtin = Pool::insert(rir::deserialize(buffer, false));
            break;
        case Opcode::mk_promise_:
        case Opcode::mk_eager_promise_:
            i.fun = poolIdx++;
            break;
        case Opcode::record_call_:
        case Opcode::record_type_:
        case Opcode::record_test_:
            // This is recording information
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
            assert((size - 1) % 4 == 0);
            if (size > 1) {
                buffer.getBytes((uint8_t*)(code + 1), size - 1);
            }
            break;
        case Opcode::invalid_:
        case Opcode::num_of:
            assert(false);
            break;
        }
        size = BC::size(code);
        assert(codeSize >= size);
        code += size;
        codeSize -= size;
    }
}

void BC::serializeSrc(ByteBuffer& buffer,
                      std::vector<ExtraPoolEntryRefInSrc>& entries,
                      const Opcode* code, size_t codeSize,
                      const Code* container) {
    while (codeSize > 0) {
        const BC bc = BC::decode((Opcode*)code, container);
        buffer.putChar((char)*code);
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
            rir::serialize(Pool::get(i.pool), buffer, false);
            break;
        case Opcode::ldvar_cached_:
        case Opcode::ldvar_for_update_cache_:
        case Opcode::stvar_cached_:
            rir::serialize(Pool::get(i.poolAndCache.poolIndex), buffer, false);
            buffer.putInt(i.poolAndCache.cacheIndex);
            break;
        case Opcode::guard_fun_:
            rir::serialize(Pool::get(i.guard_fun_args.name), buffer, false);
            rir::serialize(Pool::get(i.guard_fun_args.expected), buffer, false);
            buffer.putInt(i.guard_fun_args.id);
            break;
        case Opcode::call_:
        case Opcode::call_dots_:
        case Opcode::named_call_:
            buffer.putInt(i.callFixedArgs.nargs);
            rir::serialize(Pool::get(i.callFixedArgs.ast), buffer, false);
            buffer.putBytes((uint8_t*)&i.callFixedArgs.given, sizeof(Context));
            // Write named arguments
            if (*code == Opcode::named_call_ || *code == Opcode::call_dots_) {
                for (size_t j = 0; j < i.callFixedArgs.nargs; j++) {
                    rir::serialize(Pool::get(bc.callExtra().callArgumentNames[j]), buffer, false);
                }
            }
            break;
        case Opcode::call_builtin_:
            buffer.putInt(i.callBuiltinFixedArgs.nargs);
            rir::serialize(Pool::get(i.callBuiltinFixedArgs.ast), buffer, false);
            rir::serialize(Pool::get(i.callBuiltinFixedArgs.builtin), buffer, false);
            break;
        case Opcode::mk_promise_:
        case Opcode::mk_eager_promise_:
            entries.push_back({i.fun, ExtraPoolEntryRefInSrc::Promise});
            break;
        case Opcode::record_call_:
        case Opcode::record_type_:
        case Opcode::record_test_:
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
            if (size > 1) {
                buffer.putBytes((uint8_t*)(code + 1), (int)size - 1);
            }
            break;
        case Opcode::invalid_:
        case Opcode::num_of:
            assert(false);
            break;
        }
        size = bc.size();
        assert(codeSize >= size);
        code += size;
        codeSize -= size;
    }
}

void BC::deserializeFeedback(ByteBuffer& buffer, Opcode* code,
                             size_t codeSize, Code* container) {
    while (codeSize > 0) {
        unsigned size = BC::fixedSize(*code);
        ImmediateArguments& i = *(ImmediateArguments*)(code + 1);
        switch (*code) {
        // Feedback codes
        case Opcode::record_call_:
            i.callFeedback.numTargets = buffer.getInt();
            i.callFeedback.taken = buffer.getInt();
            i.callFeedback.invalid = buffer.getInt();
            for (size_t j = 0; j < i.callFeedback.numTargets; j++) {
                UUID targetUuid;
                buffer.getBytes((uint8_t*)&targetUuid, sizeof(UUID));
                auto target = UUIDPool::get(targetUuid);
                // TODO: Try to retrieve from client if not found?
                if (target) {
                    std::cerr << "Found target: " << targetUuid << " -> " << target << "\n";
                } else {
                    std::cerr << "Target not found: " << targetUuid << "\n";
                }
                i.callFeedback.targets[j] = target ? Pool::insert(target) : 0;
            }
            break;
        case Opcode::record_type_:
            buffer.getBytes((uint8_t*)&i.typeFeedback, sizeof(ObservedValues));
            break;
        case Opcode::record_test_:
            buffer.getBytes((uint8_t*)&i.testFeedback, sizeof(ObservedTest));
            break;
        // Everything else (not feedback, skipped)
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
        case Opcode::ldvar_cached_:
        case Opcode::ldvar_for_update_cache_:
        case Opcode::stvar_cached_:
        case Opcode::guard_fun_:
        case Opcode::call_:
        case Opcode::named_call_:
        case Opcode::call_dots_:
        case Opcode::call_builtin_:
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
#define V(NESTED, name, name_) case Opcode::name_##_:
            BC_NOARGS(V, _)
#undef V
            assert(*code != Opcode::nop_);
            break;
        case Opcode::invalid_:
        case Opcode::num_of:
            assert(false);
            break;
        }
        size = BC::size(code);
        assert(codeSize >= size);
        code += size;
        codeSize -= size;
    }
}

void BC::serializeFeedback(ByteBuffer& buffer, const Opcode* code,
                           size_t codeSize, const Code* container) {
    while (codeSize > 0) {
        const BC bc = BC::decode((Opcode*)code, container);
        unsigned size = BC::fixedSize(*code);
        ImmediateArguments i = bc.immediate;
        switch (*code) {
        // Feedback codes
        case Opcode::record_call_:
            buffer.putInt(i.callFeedback.numTargets);
            buffer.putInt(i.callFeedback.taken);
            buffer.putInt(i.callFeedback.invalid);
            for (size_t j = 0; j < i.callFeedback.numTargets; j++) {
                auto target = Pool::get(i.callFeedback.targets[j]);
                auto targetUuid = UUIDPool::getHash(target);
                if (!targetUuid) {
                    targetUuid = hashRoot(target);
                }
                buffer.putBytes((uint8_t*)&targetUuid, sizeof(UUID));
            }
            break;
        case Opcode::record_type_:
            buffer.putBytes((uint8_t*)&i.typeFeedback, sizeof(ObservedValues));
            break;
        case Opcode::record_test_:
            buffer.putBytes((uint8_t*)&i.testFeedback, sizeof(ObservedTest));
            break;
        // Everything else (not feedback, skipped)
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
        case Opcode::ldvar_cached_:
        case Opcode::ldvar_for_update_cache_:
        case Opcode::stvar_cached_:
        case Opcode::guard_fun_:
        case Opcode::call_:
        case Opcode::call_dots_:
        case Opcode::named_call_:
        case Opcode::call_builtin_:
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
#define V(NESTED, name, name_) case Opcode::name_##_:
            BC_NOARGS(V, _)
#undef V
            assert(*code != Opcode::nop_);
            break;
        case Opcode::invalid_:
        case Opcode::num_of:
            assert(false);
            break;
        }
        size = bc.size();
        assert(codeSize >= size);
        code += size;
        codeSize -= size;
    }
}

void BC::hash(Hasher& hasher, std::vector<bool>& extraPoolIgnored,
              const Opcode* code, size_t codeSize, const Code* container) {
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
            // Don't hash because this is a recording instruction,
            // but we also want to skip hashing recorded extra pool entries
            for (size_t j = 0; j < i.callFeedback.numTargets; j++) {
                extraPoolIgnored[i.callFeedback.targets[j]] = true;
            }
            break;
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
            if (size > 1) {
                hasher.hashBytes(code + 1, (int)size - 1);
            }
            break;
        case Opcode::invalid_:
        case Opcode::num_of:
            assert(false);
            break;
        }
        size = bc.size();
        assert(codeSize >= size);
        code += size;
        codeSize -= size;
    }
}

void BC::addConnected(std::vector<bool>& extraPoolChildren,
                      ConnectedCollector& collector, const Opcode* code,
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
            break;
        case Opcode::mk_promise_:
        case Opcode::mk_eager_promise_:
            extraPoolChildren[i.fun] = true;
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
            assert(false);
            break;
        }
        size = bc.size();
        assert(codeSize >= size);
        code += size;
        codeSize -= size;
    }
}

void BC::addToPrettyGraph(const PrettyGraphInnerPrinter& p,
                          std::vector<bool>& addedExtraPoolEntries,
                          const Opcode* code, size_t codeSize,
                          const Code* container) {
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

/// Compare bytecodes and print differences.
void BC::debugCompare(const Opcode* code1, const Opcode* code2,
                      size_t codeSize1, size_t codeSize2,
                      const Code* container1, const Code* container2,
                      const char* prefix, std::stringstream& differences) {
    auto loggedDifferences = false;
    auto initialCodeSize1 = codeSize1;
    while (codeSize1 > 0 && codeSize2 > 0) {
        auto pc1 = (Opcode*)code1;
        auto pc2 = (Opcode*)code2;
        auto opcode1 = *pc1;
        auto opcode2 = *pc2;
        const BC bc1 = BC::decode(pc1, container1);
        const BC bc2 = BC::decode(pc2, container2);
        auto size1 = BC::fixedSize(opcode1);
        auto size2 = BC::fixedSize(opcode2);
        if (opcode1 != opcode2 || size1 != size2 ||
            (memcmp(pc1, pc2, size1) != 0 &&
             // For non-trivial SEXPs like environments, calls will push
             // different values
             opcode1 != Opcode::push_ &&
             // Calls will have different closures
             opcode1 != Opcode::record_call_)) {
            // Even if the bytecode data is different, it could just be different pool
            // entries for equivalent SEXPs. So we check by printing the bytecode (not
            // perfect, there's a slim chance of true negative, but good enough)
            std::string associated1;
            std::string associated2;
            if (opcode1 == opcode2) {
                std::stringstream associated1Stream;
                bc1.printAssociatedData(associated1Stream, true);
                std::stringstream associated2Stream;
                bc2.printAssociatedData(associated2Stream, true);
                associated1 = associated1Stream.str();
                associated2 = associated2Stream.str();
            }
            if (opcode1 != opcode2 || associated1 != associated2) {
                if (!loggedDifferences) {
                    differences << prefix << " bytecode differs, first at "
                                << initialCodeSize1 - codeSize1 << "\n"
                                << prefix << " bytecode:";
                    loggedDifferences = true;
                }
                differences << " ";
                if (opcode1 == opcode2) {
                    differences << name(opcode1) << "(" << associated1 << ")|(" << associated2 << ")";
                } else {
                    differences << name(opcode1) << "|" << name(opcode2);
                }
                loggedDifferences = true;
            }
        }
        size1 = bc1.size();
        size2 = bc2.size();
        code1 += size1;
        code2 += size2;
        codeSize1 -= size1;
        codeSize2 -= size2;
    }
    if (loggedDifferences) {
        differences << "\n";
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

    printAssociatedData(out);
    out << "\n";
}

void BC::printAssociatedData(std::ostream& out, bool printDetailed) const {
    auto printSexp = [&](SEXP s) {
        if (printDetailed) {
            printRirObject(s, out);
        } else {
            out << Print::dumpSexp(s);
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
        out << nargs << " : ";
        printSexp(target);
        break;
    }
    case Opcode::push_:
        printSexp(immediateConst());
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
