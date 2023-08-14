#include "Function.h"
#include "R/Protect.h"
#include "R/Serialize.h"
#include "compiler/compiler.h"
#include "runtime/log/printPrettyGraph.h"
#include "serializeHash/hash/UUIDPool.h"
#include "serializeHash/serialize/serializeR.h"

namespace rir {

void Function::setFlag(rir::Function::Flag f) {
    // UUIDPool::reintern(container());
    flags_.set(f);
}

void Function::resetFlag(rir::Function::Flag f) {
    // UUIDPool::reintern(container());
    flags_.reset(f);
}

Function* Function::deserializeR(SEXP refTable, R_inpstream_t inp) {
    Protect p;
    R_xlen_t functionSize = InInteger(inp);
    const FunctionSignature sig = FunctionSignature::deserialize(refTable, inp);
    Context as;
    InBytes(inp, &as, sizeof(Context));
    SEXP store = p(Rf_allocVector(EXTERNALSXP, functionSize));
    AddReadRef(refTable, store);
    useRetrieveHashIfSet(inp, store);
    // Set size to 0 in constructor so we can call with null body, and have an
    // assertion which checks for null body if we call without size == 0 (any
    // time when we're not deserializing)
    auto fun = new (DATAPTR(store)) Function(0, nullptr, {}, sig, as);
    fun->size = functionSize;
    fun->numArgs_ = InInteger(inp);
    fun->info.gc_area_length += fun->numArgs_;
    SEXP body = p(UUIDPool::readItem(refTable, inp));
    fun->body(body);
    for (unsigned i = 0; i < fun->numArgs_; i++) {
        if ((bool)InInteger(inp)) {
            SEXP arg = p(UUIDPool::readItem(refTable, inp));
            assert(Code::check(arg));
            fun->setEntry(Function::NUM_PTRS + i, arg);
        } else {
            fun->setEntry(Function::NUM_PTRS + i, nullptr);
        }
    }
    fun->flags_ = EnumSet<Flag>(InU64(inp));
    fun->invocationCount_ = InUInt(inp);
    fun->deoptCount_ = InUInt(inp);
    fun->deadCallReached_ = InUInt(inp);
    fun->invoked = InU64(inp);
    fun->execTime = InU64(inp);
    return fun;
}

void Function::serializeR(SEXP refTable, R_outpstream_t out) const {
    HashAdd(container(), refTable);
    OutInteger(out, (int)size);
    signature().serialize(refTable, out);
    OutBytes(out, &context_, sizeof(Context));
    OutInteger(out, (int)numArgs_);
    assert(getEntry(0) && "tried to serialize function without a body. "
                          "Is the function corrupted or being constructed?");
    UUIDPool::writeItem(getEntry(0), false, refTable, out);

    for (unsigned i = 0; i < numArgs_; i++) {
        CodeSEXP arg = defaultArg_[i];
        OutInteger(out, (int)(arg != nullptr));
        if (arg) {
            assert(Code::check(arg));
            // arg->serialize(false, refTable, out);
            UUIDPool::writeItem(arg, false, refTable, out);
        }
    }
    OutU64(out, flags_.to_i());
    OutUInt(out, invocationCount_);
    OutUInt(out, deoptCount_);
    OutUInt(out, deadCallReached_);
    OutU64(out, invoked);
    OutU64(out, execTime);
}

Function* Function::deserialize(AbstractDeserializer& deserializer,
                                Function* fun) {
    Protect p;
    auto funSize = deserializer.readBytesOf<R_xlen_t>(SerialFlags::FunMiscBytes);
    auto sig = FunctionSignature::deserialize(deserializer);
    auto ctx = deserializer.readBytesOf<Context>(SerialFlags::FunMiscBytes);
    auto flags = deserializer.readBytesOf<EnumSet<Flag>>(SerialFlags::FunMiscBytes);
    auto invocationCount_ = deserializer.readBytesOf<unsigned>(SerialFlags::FunStats);
    auto deoptCount_ = deserializer.readBytesOf<unsigned>(SerialFlags::FunStats);
    auto deadCallReached_ = deserializer.readBytesOf<unsigned>(SerialFlags::FunStats);
    auto invoked = deserializer.readBytesOf<unsigned long>(SerialFlags::FunStats);
    auto execTime = deserializer.readBytesOf<unsigned long>(SerialFlags::FunStats);
    SEXP store = fun ? fun->container() : p(Rf_allocVector(EXTERNALSXP, funSize));
    deserializer.addRef(store);

    // This assertion could be statically checked
    assert(deserializer.willRead(SerialFlags::FunBody) &&
           deserializer.willRead(SerialFlags::FunDefaultArg) &&
           "must deserialize function body and default args when we deserialize"
           "function");
    TODO: Handle refs when we deserialize with existing
    auto body = Code::deserialize(deserializer,
                                  fun ? fun->body() : nullptr);
    if (!fun) {
        p(body->container());
    }
    std::vector<SEXP> defaultArgs;
    if (!fun) {
        defaultArgs.resize(sig.numArguments);
    }
    for (unsigned i = 0; i < sig.numArguments; i++) {
        if (deserializer.readBytesOf<bool>(SerialFlags::FunDefaultArg)) {
            auto defaultArg = Code::deserialize(deserializer, fun ? fun->defaultArg(i) : nullptr);
            if (!fun) {
                defaultArgs[i] = p(defaultArg->container());
            }
        }
    }

    if (!fun) {
        fun = new (DATAPTR(store)) Function(funSize, body->container(), defaultArgs, sig, ctx);
    } else if (deserializer.willRead(SerialFlags::FunMiscBytes)) {
        // Assignment is implicitly deleted because of constant, but the
        // constant value doesn't apply here (this entire else-if is actually
        // never used as of now, because we only have existing fun when we are
        // deserializing feedback)
        memcpy(&fun->signature_, &sig, sizeof(FunctionSignature));
        fun->context_ = ctx;
    }
    if (deserializer.willRead(SerialFlags::FunMiscBytes)) {
        fun->flags_ = flags;
    }
    if (deserializer.willRead(SerialFlags::FunStats)) {
        fun->invocationCount_ = invocationCount_;
        fun->deoptCount_ = deoptCount_;
        fun->deadCallReached_ = deadCallReached_;
        fun->invoked = invoked;
        fun->execTime = execTime;
    }
    return fun;
}

void Function::serialize(AbstractSerializer& serializer) const {
    serializer.writeBytesOf((R_xlen_t)size, SerialFlags::FunMiscBytes);
    signature().serialize(serializer);
    serializer.writeBytesOf(context_, SerialFlags::FunMiscBytes);
    serializer.writeBytesOf(flags_, SerialFlags::FunMiscBytes);
    serializer.writeBytesOf(invocationCount_, SerialFlags::FunStats);
    serializer.writeBytesOf(deoptCount_, SerialFlags::FunStats);
    serializer.writeBytesOf(deadCallReached_, SerialFlags::FunStats);
    serializer.writeBytesOf(invoked, SerialFlags::FunStats);
    serializer.writeBytesOf(execTime, SerialFlags::FunStats);
    serializer.write(body()->container(), SerialFlags::FunBody);
    for (unsigned i = 0; i < numArgs_; i++) {
        serializer.writeBytesOf(defaultArg_[i] != nullptr, SerialFlags::FunMiscBytes);
        if (defaultArg_[i]) {
            serializer.write(defaultArg_[i], SerialFlags::FunBody);
        }
    }
}

Function* Function::deserializeSrc(ByteBuffer& buffer) {
    Protect p;
    R_xlen_t funSize = buffer.getInt();
    auto sig = FunctionSignature::deserialize(buffer);
    Context ctx;
    buffer.getBytes((uint8_t*)&ctx, sizeof(Context));
    SEXP store = p(Rf_allocVector(EXTERNALSXP, funSize));
    auto flags = EnumSet<Flag>(buffer.getLong());
    auto body = p(Code::deserializeSrc(store, buffer)->container());
    std::vector<SEXP> defaultArgs;
    defaultArgs.resize(sig.numArguments);
    for (unsigned i = 0; i < sig.numArguments; i++) {
        if (buffer.getBool()) {
            defaultArgs[i] = p(Code::deserializeSrc(store, buffer)->container());
        }
    }

    auto fun = new (DATAPTR(store))
        Function(funSize, body, defaultArgs, sig, ctx);
    fun->flags_ = flags;
    return fun;
}

void Function::serializeSrc(ByteBuffer& buffer) const {
    buffer.putInt(size);
    signature().serialize(buffer);
    buffer.putBytes((uint8_t*)&context_, sizeof(Context));
    buffer.putLong(flags_.to_i());
    body()->serializeSrc(buffer);
    for (unsigned i = 0; i < numArgs_; i++) {
        buffer.putBool(defaultArg_[i] != nullptr);
        if (defaultArg_[i]) {
            Code::unpack(defaultArg_[i])->serializeSrc(buffer);
        }
    }
}

void Function::deserializeFeedback(ByteBuffer& buffer) {
    body()->deserializeFeedback(buffer);
    for (unsigned i = 0; i < numArgs_; i++) {
        if (defaultArg_[i]) {
            Code::unpack(defaultArg_[i])->deserializeFeedback(buffer);
        }
    }
}

void Function::serializeFeedback(ByteBuffer& buffer) const {
    body()->serializeFeedback(buffer);
    for (unsigned i = 0; i < numArgs_; i++) {
        if (defaultArg_[i]) {
            Code::unpack(defaultArg_[i])->serializeFeedback(buffer);
        }
    }
}

void Function::hash(Hasher& hasher) const {
    hasher.hashBytesOf(signature());
    hasher.hashBytesOf(context_);
    hasher.hashBytesOf(numArgs_);
    // TODO: why are body and args not set sometimes when we hash
    //  deserialized value to check hash consistency? It probably has
    //  something to do with cyclic references in serialization, but why?
    //  (This is one of the reasons we use SEXP instead of unpacking Code
    //  for body and default args, also because we are going to serialize
    //  the SEXP anyways to properly handle cyclic references)
    hasher.hash(getEntry(0));

    for (unsigned i = 0; i < numArgs_; i++) {
        CodeSEXP arg = defaultArg_[i];
        hasher.hashNullable(arg);
    }

    // Don't hash flags because they change
}

void Function::addConnected(ConnectedCollector& collector) const {
    collector.add(getEntry(0), false);

    for (unsigned i = 0; i < numArgs_; i++) {
        CodeSEXP arg = defaultArg_[i];
        collector.addNullable(arg, false);
    }
}

void Function::disassemble(std::ostream& out) const {
    print(out);
}

void Function::print(std::ostream& out, bool isDetailed) const {
    if (isDetailed) {
        out << "[size]" << size << "\n[numArgs] " << numArgs_ << "\n";
    }
    out << "[signature] ";
    signature().print(out);
    if (!context_.empty())
        out << "| context: [" << context_ << "]";
    out << "\n";
    out << "[flags]    ";
#define V(F)                                                                   \
if (flags_.includes(F))                                                    \
    out << #F << " ";
    RIR_FUNCTION_FLAGS(V)
#undef V
    out << "\n";
    out << "[stats]    ";
    out << "invoked: " << invocationCount()
        << ", time: " << ((double)invocationTime() / 1e6)
        << "ms, deopt: " << deoptCount();
    out << "\n";
    if (isDetailed) {
        body()->print(out, isDetailed);
        for (unsigned i = 0; i < numArgs_; i++) {
            CodeSEXP arg = defaultArg_[i];
            if (arg) {
                out << "[default arg " << i << "]\n";
                Code::unpack(arg)->print(out, isDetailed);
            }
        }
    } else {
        body()->disassemble(out);
    }
}

void Function::printPrettyGraphContent(const PrettyGraphInnerPrinter& print) const {
    print.addName([&](std::ostream& s) {
        auto ast = src_pool_at(body()->src);
        auto headAst = TYPEOF(ast) == LANGSXP ? CAR(ast) : R_NilValue;
        if (TYPEOF(headAst) == SYMSXP) {
            s << CHAR(PRINTNAME(headAst));
        } else {
            s << "<anon size=" << size << " numArgs=" << numArgs_ << ">";
        }
    });
    print.addBody([&](std::ostream& s) {
        s << "<p class=\"function-signature\">(";
        signature().print(s);
        s << ")</p>";
        if (!context_.empty()) {
            s << "<p class=\"function-context\">[" << context_
              << "]</p>";
        }
        if (!flags_.empty()) {
            s << "<p class=\"function-flags\">{";
        }
#define V(F)                                                                   \
        if (flags_.includes(F))                                                \
            s << #F << " ";
            RIR_FUNCTION_FLAGS(V)
#undef V
        if (!flags_.empty()) {
            s << "}</p>";
        }
        s << "<p class=\"function-stats\">"
          << "invoked: " << invocationCount()
          << ", time: " << ((double)invocationTime() / 1e6)
          << "ms, deopt: " << deoptCount()
          << "</p>";
    });
    print.addEdgeTo(body()->container(), true, "body");
    for (unsigned i = 0; i < numArgs_; i++) {
        CodeSEXP arg = defaultArg_[i];
        if (arg) {
            print.addEdgeTo(arg, true, "default-arg", [&](std::ostream& s) {
                s << "arg " << i << " default";
            });
        }
    }
}

void Function::debugCompare(const Function* f1, const Function* f2,
                            std::stringstream& differences) {
    FunctionSignature::debugCompare(f1->signature(), f2->signature(), differences);
    if (f1->context() != f2->context()) {
        differences << "context: " << f1->context() << " != " << f2->context()
                    << "\n";
    }
    if (f1->flags() != f2->flags()) {
        differences << "flags: ";
#define V(F)                                                                   \
        if (f1->flags_.includes(F))                                                \
            differences << #F << " ";
        RIR_FUNCTION_FLAGS(V)
#undef V
        differences << " != ";
#define V(F)                                                                   \
        if (f2->flags_.includes(F))                                                \
            differences << #F << " ";
        RIR_FUNCTION_FLAGS(V)
#undef V
        differences << "\n";
    }
    if (f1->size != f2->size) {
        differences << "size: " << f1->size << " != " << f2->size << "\n";
    }
    if (f1->numArgs_ != f2->numArgs_) {
        differences << "numArgs: " << f1->numArgs_ << " != " << f2->numArgs_
                    << "(note: signature also has numArgs)\n";
    }
    if (f1->invocationCount() != f2->invocationCount()) {
        differences << "invocationCount: " << f1->invocationCount() << " != "
                    << f2->invocationCount() << "\n";
    }
    if (f1->invocationTime() != f2->invocationTime()) {
        differences << "invocationTime: " << f1->invocationTime() << " != "
                    << f2->invocationTime() << "\n";
    }
    if (f1->deoptCount() != f2->deoptCount()) {
        differences << "deoptCount: " << f1->deoptCount() << " != "
                    << f2->deoptCount() << "\n";
    }
    Code::debugCompare(f1->body(), f2->body(), "body", differences);
    for (unsigned i = 0; i < std::min(f1->numArgs_, f2->numArgs_); i++) {
        auto arg1 = f1->defaultArg_[i];
        auto arg2 = f2->defaultArg_[i];
        auto hasArg1 = (arg1 != nullptr);
        auto hasArg2 = (arg2 != nullptr);
        if (hasArg1 != hasArg2) {
            differences << "defaultArg[" << i << "] != nullptr: " << hasArg1
                        << " != " << hasArg2 << "\n";
        }
        if (hasArg1 && hasArg2) {
            char prefix[100];
            sprintf(prefix, "defaultArg[%d]", i);
            Code::debugCompare(Code::unpack(arg1), Code::unpack(arg2),
                               prefix, differences);
        }
    }
}

static int GLOBAL_SPECIALIZATION_LEVEL =
    getenv("PIR_GLOBAL_SPECIALIZATION_LEVEL")
        ? atoi(getenv("PIR_GLOBAL_SPECIALIZATION_LEVEL"))
        : 100;
void Function::clearDisabledAssumptions(Context& given) const {
    if (flags_.contains(Function::DisableArgumentTypeSpecialization))
        given.clearTypeFlags();
    if (flags_.contains(Function::DisableNumArgumentsSpezialization))
        given.clearNargs();
    if (flags_.contains(Function::DisableAllSpecialization))
        given.clearExcept(pir::Compiler::minimalContext);

    if (GLOBAL_SPECIALIZATION_LEVEL < 100)
        given.setSpecializationLevel(GLOBAL_SPECIALIZATION_LEVEL);
}

} // namespace rir
