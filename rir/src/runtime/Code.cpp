#include "Code.h"
#include "Function.h"
#include "R/Printing.h"
#include "R/Serialize.h"
#include "bc/BC.h"
#include "compiler/native/pir_jit_llvm.h"
#include "compiler/parameter.h"
#include "runtime/log/printPrettyGraph.h"
#include "serializeHash/hash/UUIDPool.h"
#include "serializeHash/hash/hashAst.h"
#include "serializeHash/serialize/serialize.h"
#include "serializeHash/serialize/serializeR.h"
#include "utils/HTMLBuilder/escapeHtml.h"
#include "utils/Pool.h"
#include "utils/measuring.h"

#include <llvm/ExecutionEngine/JITSymbol.h>

#include <iomanip>
#include <sstream>

namespace rir {

static const unsigned PRETTY_GRAPH_CODE_NAME_MAX_LENGTH = 25;

// cppcheck-suppress uninitMemberVar; symbol=data
Code::Code(Kind kind, FunctionSEXP fun, SEXP src, unsigned srcIdx, unsigned cs,
           unsigned sourceLength, size_t localsCnt, size_t bindingsCnt)
    : RirRuntimeObject(
          // GC area starts just after the header
          (intptr_t)&locals_ - (intptr_t)this,
          NumLocals),
      kind(kind), nativeCode_(nullptr), src(srcIdx), trivialExpr(nullptr),
      stackLength(0), localsCount(localsCnt), bindingCacheSize(bindingsCnt),
      codeSize(cs), srcLength(sourceLength), extraPoolSize(0) {
    setEntry(0, R_NilValue);
    if (src && TYPEOF(src) == SYMSXP)
        trivialExpr = src;
    assert(!fun || rir::Function::check(fun));
    if (fun)
        setEntry(3, fun);
}

Code* Code::New(Kind kind, Immediate ast, size_t codeSize, size_t sources,
                size_t locals, size_t bindingCache) {
    unsigned totalSize = Code::size(codeSize, sources);
    SEXP store = Rf_allocVector(EXTERNALSXP, totalSize);
    void* payload = DATAPTR(store);
    return new (payload) Code(kind, nullptr, src_pool_at(ast), ast, codeSize,
                              sources, locals, bindingCache);
}

Code* Code::NewBytecode(Immediate ast, size_t codeSize, size_t sources,
                        size_t locals, size_t bindingCache) {
    return New(Kind::Bytecode, ast, codeSize, sources, locals, bindingCache);
}

Code* Code::NewNative(Immediate ast) {
    return New(Kind::Native, ast, 0, 0, 0, 0);
}

void Code::setLazyCodeModuleFinalizer() {
    makeFinalizer(Code::finalizeLazyCodeModuleFromContainer, false);
}

void Code::finalizeLazyCodeModuleFromContainer(SEXP sexp) {
    Code::unpack(sexp)->finalizeLazyCodeModule();
}

void Code::finalizeLazyCodeModule() {
    assert(lazyCodeModule);
    // Causes this to free the shared reference
    lazyCodeModule = nullptr;
}

void Code::lazyCode(const std::string& handle, const SerialModuleRef& module) {
    assert(!handle.empty());
    assert(handle.size() < MAX_CODE_HANDLE_LENGTH);
    assert(kind == Kind::Native);
    assert(lazyCodeHandle[0] == '\0' && !lazyCodeModule);
    strncpy(lazyCodeHandle, handle.c_str(), MAX_CODE_HANDLE_LENGTH - 1);
    lazyCodeModule = module;
    UUIDPool::reintern(container());
    if (module) {
        setLazyCodeModuleFinalizer();
    }
}

void Code::function(Function* fun) { setEntry(3, fun->container()); }

rir::Function* Code::function() const {
    auto f = getEntry(3);
    if (!f && kind == Kind::Deserializing) {
        assert(false && "can't access function of code while it's being deserialized");
    }
    assert(f);
    return rir::Function::unpack(f);
}

unsigned Code::getSrcIdxAt(const Opcode* pc, bool allowMissing) const {
    if (srcLength == 0) {
        assert(allowMissing);
        return 0;
    }

    SrclistEntry* sl = srclist();
    Opcode* start = code();
    auto pcOffset = pc - start;

    if (srcLength == 1) {
        auto sidx = sl[0].pcOffset == pcOffset ? sl[0].srcIdx : 0;
        SLOWASSERT(allowMissing || sidx);
        return sidx;
    }

    // Binary search through src list
    int lower = 0;
    int upper = srcLength - 1;
    int finger = upper / 2;
    unsigned sidx = 0;

    while (lower <= upper) {
        if (sl[finger].pcOffset == pcOffset) {
            sidx = sl[finger].srcIdx;
            break;
        }
        if (sl[finger].pcOffset < pcOffset)
            lower = finger + 1;
        else
            upper = finger - 1;
        finger = lower + (upper - lower) / 2;
    }
    SLOWASSERT(sidx == 0 || sl[finger].pcOffset == pcOffset);
    SLOWASSERT(allowMissing || sidx);

    return sidx;
}

Code* Code::deserializeR(SEXP outer, SEXP refTable, R_inpstream_t inp) {
    Protect p;
    auto size = InInteger(inp);
    SEXP store = p(Rf_allocVector(EXTERNALSXP, size));
    AddReadRef(refTable, store);
    useRetrieveHashIfSet(inp, store);
    Code* code = new (DATAPTR(store)) Code;

    // Header
    code->src = src_pool_read_item(refTable, inp);
    bool hasTr = InInteger(inp);
    if (hasTr)
        code->trivialExpr = UUIDPool::readItem(refTable, inp);
    code->stackLength = InInteger(inp);
    *const_cast<unsigned*>(&code->localsCount) = InInteger(inp);
    *const_cast<unsigned*>(&code->bindingCacheSize) = InInteger(inp);
    code->codeSize = InInteger(inp);
    code->srcLength = InInteger(inp);
    code->extraPoolSize = InInteger(inp);
    auto hasArgReorder = InInteger(inp);
    SEXP argReorder = nullptr;
    if (hasArgReorder) {
        argReorder = p(UUIDPool::readItem(refTable, inp));
    }
    if (!outer) {
        outer = p(UUIDPool::readItem(refTable, inp));
    }
    assert(Function::check(outer));

    // Bytecode
    BC::deserializeR(refTable, inp, code->code(), code->codeSize, code);

    // Extra pool
    SEXP extraPool = p(Rf_allocVector(VECSXP, code->extraPoolSize));
    for (unsigned i = 0; i < code->extraPoolSize; ++i) {
        SET_VECTOR_ELT(extraPool, i, UUIDPool::readItem(refTable, inp));
    }

    // Srclist
    for (unsigned i = 0; i < code->srcLength; i++) {
        code->srclist()[i].pcOffset = InInteger(inp);
        // TODO: Intern
        code->srclist()[i].srcIdx = src_pool_read_item(refTable, inp);
    }
    code->info = {// GC area starts just after the header
                  (uint32_t)((intptr_t)&code->locals_ - (intptr_t)code),
                  NumLocals, CODE_MAGIC};
    code->setEntry(0, extraPool);
    code->setEntry(3, outer);
    if (hasArgReorder) {
        code->setEntry(2, argReorder);
    }

    // Native code
    code->kind = (Kind)InInteger(inp);
    if (code->kind == Kind::Native) {
        auto lazyCodeHandleLen = InInteger(inp);
        InBytes(inp, code->lazyCodeHandle, lazyCodeHandleLen);
        code->lazyCodeHandle[lazyCodeHandleLen] = '\0';
        if (InBool(inp)) {
            code->lazyCodeModule = pir::PirJitLLVM::deserializeModuleR(inp, code);
            code->setLazyCodeModuleFinalizer();
        }
    }
    // Native code is always null here because it's lazy
    code->nativeCode_ = nullptr;

    return code;
}

void Code::serializeR(bool includeOuter, SEXP refTable, R_outpstream_t out) const {
    HashAdd(container(), refTable);
    OutInteger(out, (int)size());

    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "Code.cpp: serializeR source", container(), [&]{
        src_pool_write_item(src, refTable, out);
        OutInteger(out, trivialExpr != nullptr);
        if (trivialExpr)
            UUIDPool::writeItem(trivialExpr, false, refTable, out);
    });
    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "Code.cpp: serializeR numbers", container(), [&]{
        OutInteger(out, (int)stackLength);
        OutInteger(out, (int)localsCount);
        OutInteger(out, (int)bindingCacheSize);
        OutInteger(out, (int)codeSize);
        OutInteger(out, (int)srcLength);
        OutInteger(out, (int)extraPoolSize);
    });

    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "Code.cpp: serializeR call argument reordering metadata", container(), [&]{
        OutInteger(out, getEntry(2) != nullptr);
        if (getEntry(2))
            UUIDPool::writeItem(getEntry(2), false, refTable, out);
    });
    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "Code.cpp: serializeR outer function", container(), [&]{
        if (includeOuter) {
            UUIDPool::writeItem(function()->container(), false, refTable, out);
        }
    });

    std::vector<bool> extraPoolChildren;
    extraPoolChildren.resize(extraPoolSize);
    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "Code.cpp: serializeR bytecode", container(), [&]{
        // One might think we can skip serializing entries which are just
        // recorded calls, but it breaks semantics and causes a test failure
        BC::serializeR(extraPoolChildren, refTable, out, code(), codeSize, this);
    });

    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "Code.cpp: serializeR extra pool", container(), [&]{
        for (unsigned i = 0; i < extraPoolSize; ++i) {
            UUIDPool::writeItem(getExtraPoolEntry(i), extraPoolChildren[i], refTable, out);
        }
    });

    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "Code.cpp: serializeR srclist", container(), [&]{
        for (unsigned i = 0; i < srcLength; i++) {
            OutInteger(out, (int)srclist()[i].pcOffset);
            src_pool_write_item(srclist()[i].srcIdx, refTable, out);
        }
    });

    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "Code.cpp: serializeR native", container(), [&]{
        OutInteger(out, (int)kind);
        assert((kind != Kind::Native || lazyCodeHandle[0] != '\0') &&
               "Code in bad pending state");
        if (kind == Kind::Native && lazyCodeHandle[0] != '\0') {
            assert(lazyCodeHandle[0] != '\0');
            auto lazyCodeHandleLen = (int)strlen(lazyCodeHandle);
            OutInteger(out, lazyCodeHandleLen);
            OutBytes(out, (const char*)lazyCodeHandle, lazyCodeHandleLen);
            OutBool(out, lazyCodeModule != nullptr);
            if (lazyCodeModule) {
                lazyCodeModule->serializeR(out);
            }
        }
    });
}

Code* Code::deserialize(SEXP outer, AbstractDeserializer& deserializer) {
    Protect p;
    auto size = deserializer.readBytesOf<R_xlen_t>(SerialFlags::CodeMisc);
    SEXP store = p(Rf_allocVector(EXTERNALSXP, size));
    deserializer.addRef(store);
    Code* code = new (DATAPTR(store)) Code;

    // Header
    code->src = deserializer.readSrc(SerialFlags::CodeAst);
    code->trivialExpr = deserializer.readNullable(SerialFlags::CodeAst);
    code->stackLength = deserializer.readBytesOf<unsigned>(SerialFlags::CodeMisc);
    *const_cast<unsigned*>(&code->localsCount) = deserializer.readBytesOf<unsigned>(SerialFlags::CodeMisc);
    *const_cast<unsigned*>(&code->bindingCacheSize) = deserializer.readBytesOf<unsigned>(SerialFlags::CodeMisc);
    code->codeSize = deserializer.readBytesOf<unsigned>(SerialFlags::CodeMisc);
    code->srcLength = deserializer.readBytesOf<unsigned>(SerialFlags::CodeMisc);
    code->extraPoolSize = deserializer.readBytesOf<unsigned>(SerialFlags::CodeMisc);
    auto argReorder = deserializer.readNullable(SerialFlags::CodeArglistOrder);
    if (!outer) {
        outer = p(deserializer.read(SerialFlags::CodeOuterFun));
    }
    // Can't check magic because it may not be assigned yet
    assert(TYPEOF(outer) == EXTERNALSXP);

    // Bytecode
    std::vector<SerialFlags> extraPoolFlags(code->extraPoolSize, SerialFlags::CodePoolUnknown);
    BC::deserialize(deserializer, extraPoolFlags, code->code(), code->codeSize, code);

    // Extra pool
    SEXP extraPool = p(Rf_allocVector(VECSXP, code->extraPoolSize));
    for (unsigned i = 0; i < code->extraPoolSize; ++i) {
        SET_VECTOR_ELT(extraPool, i, deserializer.read(extraPoolFlags[i]));
    }

    // Srclist
    for (unsigned i = 0; i < code->srcLength; i++) {
        code->srclist()[i].pcOffset = deserializer.readBytesOf<uint32_t>(SerialFlags::CodeMisc);
        code->srclist()[i].srcIdx = deserializer.readSrc(SerialFlags::CodeAst);
    }
    code->info = {// GC area starts just after the header
                  (uint32_t)((intptr_t)&code->locals_ - (intptr_t)code),
                  NumLocals, CODE_MAGIC};
    code->setEntry(0, extraPool);
    code->setEntry(3, outer);
    if (argReorder) {
        code->setEntry(2, argReorder);
    }

    // Native code
    code->kind = deserializer.readBytesOf<Kind>(SerialFlags::CodeNative);
    if (code->kind == Kind::Native) {
        auto lazyCodeHandleLen = deserializer.readBytesOf<unsigned>(SerialFlags::CodeNative);
        deserializer.readBytes(code->lazyCodeHandle, lazyCodeHandleLen, SerialFlags::CodeNative);
        code->lazyCodeHandle[lazyCodeHandleLen] = '\0';
        if (deserializer.readBytesOf<bool>(SerialFlags::CodeNative)) {
            code->lazyCodeModule = pir::PirJitLLVM::deserializeModule(deserializer, code);
            code->setLazyCodeModuleFinalizer();
        }
    }
    // Native code is always null here because it's lazy
    code->nativeCode_ = nullptr;

    return code;
}

void Code::serialize(bool includeOuter, AbstractSerializer& serializer) const {
    serializer.writeBytesOf((R_xlen_t)size(), SerialFlags::CodeMisc);

    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "Code.cpp: serialize source", container(), [&]{
        serializer.writeSrc(src, SerialFlags::CodeAst);
        serializer.writeNullable(trivialExpr, SerialFlags::CodeAst);
    });
    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "Code.cpp: serialize numbers", container(), [&]{
        serializer.writeBytesOf((unsigned)stackLength, SerialFlags::CodeMisc);
        serializer.writeBytesOf((unsigned)localsCount, SerialFlags::CodeMisc);
        serializer.writeBytesOf((unsigned)bindingCacheSize, SerialFlags::CodeMisc);
        serializer.writeBytesOf((unsigned)codeSize, SerialFlags::CodeMisc);
        serializer.writeBytesOf((unsigned)srcLength, SerialFlags::CodeMisc);
        serializer.writeBytesOf((unsigned)extraPoolSize, SerialFlags::CodeMisc);
    });

    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "Code.cpp: serialize call argument reordering metadata", container(), [&]{
        serializer.writeNullable(getEntry(2), SerialFlags::CodeArglistOrder);
    });
    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "Code.cpp: serialize outer function", container(), [&]{
        if (includeOuter) {
            serializer.write(getEntry(3), SerialFlags::CodeOuterFun);
        }
    });

    std::vector<SerialFlags> extraPoolFlags(extraPoolSize, SerialFlags::CodePoolUnknown);
    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "Code.cpp: serialize bytecode", container(), [&]{
        // One might think we can skip serializing entries which are just
        // recorded calls, but it breaks semantics and causes a test failure
        BC::serialize(serializer, extraPoolFlags, code(), codeSize, this);
    });

    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "Code.cpp: serialize extra pool", container(), [&]{
        for (unsigned i = 0; i < extraPoolSize; ++i) {
            serializer.write(getExtraPoolEntry(i), extraPoolFlags[i]);
        }
    });

    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "Code.cpp: serialize srclist", container(), [&]{
        for (unsigned i = 0; i < srcLength; i++) {
            serializer.writeBytesOf(srclist()[i].pcOffset, SerialFlags::CodeMisc);
            serializer.writeSrc(srclist()[i].srcIdx, SerialFlags::CodeAst);
        }
    });

    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "Code.cpp: serialize native", container(), [&]{
        serializer.writeBytesOf(kind, SerialFlags::CodeNative);
        assert((kind != Kind::Native || lazyCodeHandle[0] != '\0') &&
               "Code in bad pending state");
        if (kind == Kind::Native && lazyCodeHandle[0] != '\0') {
            assert(lazyCodeHandle[0] != '\0');
            auto lazyCodeHandleLen = (unsigned)strlen(lazyCodeHandle);
            serializer.writeBytesOf(lazyCodeHandleLen, SerialFlags::CodeNative);
            serializer.writeBytes(lazyCodeHandle, lazyCodeHandleLen, SerialFlags::CodeNative);
            serializer.writeBytesOf(lazyCodeModule != nullptr, SerialFlags::CodeNative);
            if (lazyCodeModule) {
                lazyCodeModule->serialize(serializer);
            }
        }
    });
}

Code* Code::deserializeSrc(SEXP outer, ByteBuffer& buffer) {
    Protect p;
    R_xlen_t size = buffer.getInt();
    SEXP store = p(Rf_allocVector(EXTERNALSXP, size));
    Code* code = new (DATAPTR(store)) Code;

    // Header
    code->src = src_pool_add(p(rir::deserialize(buffer, false)));
    if (buffer.getBool()) {
        code->trivialExpr = p(rir::deserialize(buffer, false));
    }
    code->stackLength = buffer.getInt();
    *const_cast<unsigned*>(&code->localsCount) = buffer.getInt();
    *const_cast<unsigned*>(&code->bindingCacheSize) = buffer.getInt();
    code->codeSize = buffer.getInt();
    code->srcLength = buffer.getInt();
    if (buffer.getBool()) {
        code->arglistOrder(ArglistOrder::unpack(p(rir::deserialize(buffer, false))));
    }
    code->setEntry(3, outer);

    // Bytecode
    BC::deserializeSrc(buffer, code->code(), code->codeSize, code);

    // Extra pool
    code->extraPoolSize = buffer.getInt();
    SEXP extraPool = p(Rf_allocVector(VECSXP, code->extraPoolSize));
    for (unsigned i = 0; i < code->extraPoolSize; ++i) {
        SEXP entrySexp;
        switch ((ExtraPoolEntryRefInSrc::Type)buffer.getInt()) {
        case ExtraPoolEntryRefInSrc::Promise:
            entrySexp = p(Code::deserializeSrc(outer, buffer)->container());
            break;
        case ExtraPoolEntryRefInSrc::ArbitrarySexp:
            entrySexp = p(rir::deserialize(buffer, false));
            break;
        default:
            assert(false && "corrupt deserialization data (corrupt extra pool ref type)");
        }
        SET_VECTOR_ELT(extraPool, i, entrySexp);
    }
    code->setEntry(0, extraPool);

    // Srclist
    for (unsigned i = 0; i < code->srcLength; i++) {
        code->srclist()[i].pcOffset = buffer.getInt();
        // TODO: Intern
        code->srclist()[i].srcIdx = src_pool_add(p(rir::deserialize(buffer, false)));
    }
    code->info = {// GC area starts just after the header
                  (uint32_t)((intptr_t)&code->locals_ - (intptr_t)code),
                  NumLocals, CODE_MAGIC};

    // Src codes are always bytecode
    code->kind = Kind::Bytecode;
    code->nativeCode_ = nullptr;

    return code;
}

void Code::serializeSrc(ByteBuffer& buffer) const {
    // Header
    rir::serialize(src_pool_at(src), buffer, false);
    buffer.putBool(trivialExpr);
    if (trivialExpr) {
        rir::serialize(trivialExpr, buffer, false);
    }
    buffer.putInt(stackLength);
    buffer.putInt(localsCount);
    buffer.putInt(bindingCacheSize);
    buffer.putInt(codeSize);
    buffer.putInt(srcLength);
    buffer.putBool(arglistOrder());
    if (arglistOrder()) {
        rir::serialize(arglistOrder()->container(), buffer, false);
    }

    // Bytecode
    std::vector<ExtraPoolEntryRefInSrc> extraPoolEntries;
    BC::serializeSrc(buffer, extraPoolEntries, code(), codeSize, this);

    // Extra pool
    buffer.putInt(extraPoolEntries.size());
    for (auto& entry : extraPoolEntries) {
        auto entrySexp = getExtraPoolEntry(entry.idx);
        buffer.putInt((unsigned)entry.type);
        switch (entry.type) {
        case ExtraPoolEntryRefInSrc::Promise:
            Code::unpack(entrySexp)->serializeSrc(buffer);
            break;
        case ExtraPoolEntryRefInSrc::ArbitrarySexp:
            rir::serialize(entrySexp, buffer, false);
            break;
        default:
            assert(false);
        }
    }

    // Srclist
    for (unsigned i = 0; i < srcLength; i++) {
        buffer.putInt(srclist()[i].pcOffset);
        rir::serialize(src_pool_at(srclist()[i].srcIdx), buffer, false);
    }
}

void Code::deserializeFeedback(ByteBuffer& buffer) {
    BC::deserializeFeedback(buffer, code(), codeSize, this);
}

void Code::serializeFeedback(ByteBuffer& buffer) const {
    BC::serializeFeedback(buffer, code(), codeSize, this);
}

void Code::hash(Hasher& hasher) const {
    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "Code.cpp: hash source", container(), [&]{
        hasher.hashSrc(src);
    });
    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "Code.cpp: hash numbers", container(), [&]{
        hasher.hashBytesOf<unsigned>(stackLength);
        hasher.hashBytesOf<unsigned>(localsCount);
        hasher.hashBytesOf<unsigned>(bindingCacheSize);
        hasher.hashBytesOf<unsigned>(codeSize);
        hasher.hashBytesOf<unsigned>(srcLength);
        hasher.hashBytesOf<unsigned>(extraPoolSize);
    });
    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "Code.cpp: hash call argument reordering metadata", container(), [&]{
        hasher.hashNullable(getEntry(2));
    });
    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "Code.cpp: hash outer function", container(), [&]{
        hasher.hash(function()->container());
    });

    std::vector<bool> extraPoolIgnored;
    extraPoolIgnored.resize(extraPoolSize);
    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "Code.cpp: hash bytecode", container(), [&]{
        BC::hash(hasher, extraPoolIgnored, code(), codeSize, this);
    });

    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "Code.cpp: hash extra pool", container(), [&]{
        hasher.hashBytesOf(extraPoolSize);
        for (unsigned i = 0; i < extraPoolSize; ++i) {
            if (!extraPoolIgnored[i]) {
                hasher.hash(getExtraPoolEntry(i));
            }
        }
    });

    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "Code.cpp: hash srclist", container(), [&]{
        for (unsigned i = 0; i < srcLength; i++) {
            hasher.hashBytesOf<unsigned>(srclist()[i].pcOffset);
            hasher.hashSrc(srclist()[i].srcIdx);
        }
    });

    // Don't hash native code
}

void Code::addConnected(ConnectedCollector& collector) const {
    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "Code.cpp: add connected in source", container(), [&]{
        collector.addSrc(src);
    });
    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "Code.cpp: add connected in call argument reordering metadata", container(), [&]{
        collector.addNullable(getEntry(2), false);
    });
    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "Code.cpp: add connected in outer function", container(), [&]{
        collector.add(function()->container(), false);
    });

    std::vector<bool> extraPoolChildren;
    extraPoolChildren.resize(extraPoolSize);
    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "Code.cpp: add connected in bytecode", container(), [&]{
        BC::addConnected(extraPoolChildren, collector, code(), codeSize, this);
    });

    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "Code.cpp: add connected in extra pool", container(), [&]{
        for (unsigned i = 0; i < extraPoolSize; ++i) {
            collector.add(getExtraPoolEntry(i), extraPoolChildren[i]);
        }
    });

    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "Code.cpp: add connected in srclist", container(), [&]{
        for (unsigned i = 0; i < srcLength; i++) {
            collector.addSrc(srclist()[i].srcIdx);
        }
    });

    // No connected in SEXPs native code
}

void Code::disassemble(std::ostream& out, const std::string& prefix) const {
    if (auto map = pirTypeFeedback()) {
        map->forEachSlot(
            [&](size_t i, const PirTypeFeedback::MDEntry& mdEntry) {
                auto feedback = mdEntry.feedback;
                out << " - slot #" << i << ": " << mdEntry.offset << " : [";
                feedback.print(out);
                out << "] (" << mdEntry.sampleCount << " records - "
                    << (mdEntry.readyForReopt ? "ready" : "not ready")
                    << ") prev: " << mdEntry.previousType << "\n";
            });
    }

    switch (kind) {
    case Kind::Bytecode: {
        Opcode* pc = code();
        size_t label = 0;
        std::map<Opcode*, size_t> targets;
        targets[pc] = label++;
        while (pc < endCode()) {
            if (BC::decodeShallow(pc).isJmp()) {
                auto t = BC::jmpTarget(pc);
                if (!targets.count(t))
                    targets[t] = label++;
            }
            pc = BC::next(pc);
        }
        // sort labels ascending
        label = 0;
        for (auto& t : targets)
            t.second = label++;

        auto formatLabel = [&](size_t label) { out << label; };

        pc = code();
        std::vector<BC::FunIdx> promises;

        while (pc < endCode()) {

            if (targets.count(pc)) {
                formatLabel(targets[pc]);
                out << ":\n";
            }

            BC bc = BC::decode(pc, this);
            bc.addMyPromArgsTo(promises);

            const size_t OFFSET_WIDTH = 7;
            out << std::right << std::setw(OFFSET_WIDTH)
                << ((uintptr_t)pc - (uintptr_t)code()) << std::left;

            unsigned s = getSrcIdxAt(pc, true);
            if (s != 0)
                out << "   ; " << Print::dumpSexp(src_pool_at(s)) << "\n"
                    << std::setw(OFFSET_WIDTH) << "";

            // Print call ast
            switch (bc.bc) {
            case Opcode::call_:
            case Opcode::named_call_:
                out << "   ; "
                    << Print::dumpSexp(
                           Pool::get(bc.immediate.callFixedArgs.ast))
                    << "\n"
                    << std::setw(OFFSET_WIDTH) << "";
                break;
            default: {
            }
            }

            if (bc.isJmp()) {
                out << "   ";
                bc.printOpcode(out);
                formatLabel(targets[BC::jmpTarget(pc)]);
                out << "\n";
            } else {
                bc.print(out);
            }

            pc = BC::next(pc);
        }

        for (auto i : promises) {
            auto c = getPromise(i);
            out << "\n[Prom (index " << prefix << i << ")]\n";
            std::stringstream ss;
            ss << prefix << i << ".";
            c->disassemble(out, ss.str());
        }
        break;
    }
    case Kind::Native: {
        if (nativeCode_) {
            out << "nativeCode " << nativeCode_ << ", module:";
            if (lazyCodeModule) {
                out << "\n" << lazyCodeModule;
            } else {
                out << " (elided)";
            }
            out << "\n";
        } else {
            out << "nativeCode (compilation pending)\n";
        }
        break;
    }
    default:
        assert(false);
    }

    if (auto a = arglistOrder()) {
        out << "arglistOrder:\n";
        for (size_t i = 0; i < a->nCalls; i++) {
            out << "  id " << i << ": ";
            for (size_t j = 0; j < a->originalArglistLength(i); j++) {
                out << ArglistOrder::decodeArg(a->index(i, j))
                    << (ArglistOrder::isArgNamed(a->index(i, j)) ? "n " : " ");
            }
            out << "\n";
        }
    }
}

void Code::print(std::ostream& out, bool isDetailed) const {
    out << "Code object\n";
    out << std::left << std::setw(20) << "   Source: " << src
        << " (index into src pool)\n";
    out << std::left << std::setw(20) << "   Magic: " << std::hex
        << info.magic << std::dec << " (hex)\n";
    out << std::left << std::setw(20) << "   Stack (o): " << stackLength
        << "\n";
    out << std::left << std::setw(20) << "   Code size: " << codeSize
        << "[B]\n";
    if (isDetailed) {
        out << std::left << std::setw(20) << "   Size: " << size()
            << "[B]\n";
    }

    if (info.magic != CODE_MAGIC) {
        out << "Wrong magic number -- corrupted IR bytecode";
        Rf_error("Wrong magic number -- corrupted IR bytecode");
    }

    out << "\n";
    disassemble(out);

    if (isDetailed) {
        out << "extra pool = \n"
            << Print::dumpSexp(getEntry(0), SIZE_MAX) << "\n";
        out << "src = \n"
            << Print::dumpSexp(src_pool_at(src), SIZE_MAX)
            << ", hash = " << hashAst(src_pool_at(src)) << "\n";
        for (unsigned i = 0; i < srcLength; i++) {
            out << "src[" << i << "] @ " << srclist()[i].pcOffset
                << " = \n";
            out << Print::dumpSexp(src_pool_at(i), SIZE_MAX)
                << ", hash = " << hashAst(src_pool_at(i)) << "\n";
        }
    }
}


static bool isEqualOrInExtraPool(const Code* outer, const Code* inner) { // NOLINT(*-no-recursion)
    if (outer == inner) {
        return true;
    }
    for (unsigned i = 0; i < outer->extraPoolSize; ++i) {
        auto codeEntry = Code::check(outer->getExtraPoolEntry(i));
        if (codeEntry && isEqualOrInExtraPool(codeEntry, inner)) {
            return true;
        }
    }
    return false;
}

static bool isInFunction(const Function* outer, const Code* inner) {
    if (isEqualOrInExtraPool(outer->body(), inner)) {
        return true;
    }
    for (unsigned i = 0; i < outer->nargs(); ++i) {
        auto arg = outer->defaultArg(i);
        if (arg && isEqualOrInExtraPool(arg, inner)) {
            return true;
        }
    }
    return false;
}

void Code::printPrettyGraphContent(const PrettyGraphInnerPrinter& print) const {
    auto srcPrint = Print::dumpSexp(src_pool_at(src), SIZE_MAX);
    print.addName([&](std::ostream& s) {
        if (srcPrint.length() < PRETTY_GRAPH_CODE_NAME_MAX_LENGTH) {
            s << srcPrint;
        } else {
            s << srcPrint.substr(0, PRETTY_GRAPH_CODE_NAME_MAX_LENGTH)
              << "...";
        }
    });
    print.addBody([&](std::ostream& s) {
        if (srcPrint.length() >= PRETTY_GRAPH_CODE_NAME_MAX_LENGTH) {
            s << "<pre>" << escapeHtml(srcPrint) << "</pre>\n";
        }
        std::stringstream str;
        disassemble(str);
        s << "<pre>" << escapeHtml(str.str()) << "</pre>";
    });
    auto addSourceEdge = [&](SEXP sexp, const char* type, size_t index = SIZE_MAX){
        if (sexp && sexp != R_NilValue && TYPEOF(sexp) != SYMSXP &&
            TYPEOF(sexp) != LANGSXP && TYPEOF(sexp) != INTSXP &&
            TYPEOF(sexp) != LGLSXP && TYPEOF(sexp) != REALSXP &&
            TYPEOF(sexp) != CPLXSXP && TYPEOF(sexp) != CHARSXP &&
            TYPEOF(sexp) != STRSXP) {
            print.addEdgeTo(sexp, false, "unexpected-ast", [&](std::ostream& s){
                s << type;
                if (index != SIZE_MAX) {
                    s << " " << index;
                }
                s << " isn't a source type!";
            });
        }
    };
    addSourceEdge(src_pool_at(src), "source");
    addSourceEdge(trivialExpr, "trivial-expr");
    for (unsigned i = 0; i < srcLength; i++) {
        addSourceEdge(src_pool_at(i), "src-pool entry", i);
    }
    if (arglistOrderContainer()) {
        print.addEdgeTo(arglistOrderContainer(), true, "arglist-order", [&](std::ostream& s) {
            s << "arglist order";
        });
    }
    if (!isInFunction(function(), this)) {
        print.addEdgeTo(function()->container(), false, "unexpected", [&](std::ostream& s) {
            s << "function, its not this code's parent!";
        });
    }
    std::vector<bool> addedExtraPoolEntries;
    addedExtraPoolEntries.resize(extraPoolSize);
    BC::addToPrettyGraph(print, addedExtraPoolEntries, code(), codeSize, this);
    for (unsigned i = 0; i < extraPoolSize; i++) {
        if (!addedExtraPoolEntries[i]) {
            print.addEdgeTo(getExtraPoolEntry(i), false, "unknown-extra-pool", [&](std::ostream& s) {
                s << "pool " << i;
            });
        }
    }
}

static void compareAsts(SEXP ast1, SEXP ast2,
                        const char* prefix, const char* srcPrefix,
                        std::stringstream& differences) {
    // Asts can be compared via printing
    auto print1 = ast1 ? Print::dumpSexp(ast1, SIZE_MAX) : "(null)";
    auto print2 = ast1 ? Print::dumpSexp(ast2, SIZE_MAX) : "(null)";
    if (print1 != print2) {
        differences << prefix << " " << srcPrefix << " asts differ:\n";
        differences << prefix << "  " << srcPrefix << "1: " << print1 << "\n";
        differences << prefix << "  " << srcPrefix << "2: " << print2 << "\n";
    }
}

static void compareSrcs(unsigned src1, unsigned src2,
                        const char* prefix, const char* srcPrefix,
                        std::stringstream& differences) {
    compareAsts(src_pool_at(src1), src_pool_at(src2), prefix,
                srcPrefix, differences);
}

void Code::debugCompare(const Code* c1, const Code* c2, const char* prefix,
                        std::stringstream& differences) {
    compareSrcs(c1->src, c2->src, prefix, "src", differences);
    compareAsts(c1->trivialExpr, c2->trivialExpr, prefix, "trivialExpr", differences);
    if (c1->srcLength != c2->srcLength) {
        differences << prefix << " srcLengths differ: " << c1->srcLength
                    << " vs " << c2->srcLength << "\n";
    }
    if (c1->codeSize != c2->codeSize) {
        differences << prefix << " codeSizes differ: " << c1->codeSize << " vs "
                    << c2->codeSize << "\n";
    }
    if (c1->stackLength != c2->stackLength) {
        differences << prefix << " stackLengths differ: " << c1->stackLength
                    << " vs " << c2->stackLength << "\n";
    }
    if (c1->extraPoolSize != c2->extraPoolSize) {
        differences << prefix << " extraPoolSizes differ: " << c1->extraPoolSize
                    << " vs " << c2->extraPoolSize << "\n";
    }
    if (c1->bindingCacheSize != c2->bindingCacheSize) {
        differences << prefix << " bindingCacheSizes differ: "
                    << c1->bindingCacheSize << " vs " << c2->bindingCacheSize
                    << "\n";
    }
    for (unsigned i = 0; i < std::min(c1->srcLength, c2->srcLength); i++) {
        auto src1 = c1->srclist()[i];
        auto src2 = c2->srclist()[i];
        if (src1.pcOffset != src2.pcOffset) {
            differences << prefix << " src " << i << " pcOffsets differ: "
                        << src1.pcOffset << " vs " << src2.pcOffset << "\n";
        }
        char srcPrefix[100];
        sprintf(srcPrefix, "src %d", i);
        compareSrcs(src1.srcIdx, src2.srcIdx, prefix,
                    srcPrefix, differences);
    }
    BC::debugCompare(c1->code(), c2->code(), c1->codeSize, c2->codeSize, c1, c2,
                     prefix, differences);
}

unsigned Code::addExtraPoolEntry(SEXP v) {
    SEXP cur = getEntry(0);
    unsigned curLen = cur == R_NilValue ? 0 : (unsigned)LENGTH(cur);
    if (curLen == extraPoolSize) {
        unsigned newCapacity = curLen ? curLen * 2 : 2;
        PROTECT(v);
        SEXP newPool = PROTECT(Rf_allocVector(VECSXP, newCapacity));
        for (unsigned i = 0; i < curLen; ++i) {
            SET_VECTOR_ELT(newPool, i, VECTOR_ELT(cur, i));
        }
        setEntry(0, newPool);
        UNPROTECT(2);
        cur = newPool;
    }
    SET_VECTOR_ELT(cur, extraPoolSize, v);
    return extraPoolSize++;
}

llvm::ExitOnError ExitOnErr;

NativeCode Code::lazyCompile() {
    assert(kind == Kind::Native);
    assert(*lazyCodeHandle != '\0');
    auto symbol = ExitOnErr(pir::PirJitLLVM::JIT->lookup(lazyCodeHandle));
    nativeCode_ = (NativeCode)symbol.getAddress();
    return nativeCode_;
}

} // namespace rir
