#include "Function.h"
#include "R/Protect.h"
#include "R/Serialize.h"
#include "compiler/compiler.h"
#include "hash/UUIDPool.h"
#include "interpreter/serialize.h"

namespace rir {

Function* Function::deserialize(SEXP refTable, R_inpstream_t inp) {
    Protect p;
    size_t functionSize = InInteger(inp);
    const FunctionSignature sig = FunctionSignature::deserialize(refTable, inp);
    const Context as = Context::deserialize(refTable, inp);
    SEXP store = p(Rf_allocVector(EXTERNALSXP, functionSize));
    AddReadRef(refTable, store);
    useRetrieveHashIfSet(inp, store);
    auto fun = new (DATAPTR(store)) Function(functionSize, nullptr, {}, sig, as);
    fun->numArgs_ = InInteger(inp);
    fun->info.gc_area_length += fun->numArgs_;
    SEXP body = p(UUIDPool::readItem(refTable, inp));
    fun->body(body);
    for (unsigned i = 0; i < fun->numArgs_; i++) {
        if ((bool)InInteger(inp)) {
            SEXP arg = p(UUIDPool::readItem(refTable, inp));
            fun->setEntry(Function::NUM_PTRS + i, arg);
        } else
            fun->setEntry(Function::NUM_PTRS + i, nullptr);
    }
    fun->flags = EnumSet<Flag>(InInteger(inp));
    return fun;
}

void Function::serialize(SEXP refTable, R_outpstream_t out) const {
    // Some stuff is mutable or not part of the structural identity, so we don't
    // want to hash it. However, we still need to serialize recursive items. To
    // do this, we temporarily replace out with a void stream.
    // TODO!: Working on this...
    // R_outpstream_st nullOut = nullOutputStream();
    auto noHashOut = out;

    HashAdd(container(), refTable);
    OutInteger(out, size);
    signature().serialize(refTable, out);
    context_.serialize(refTable, out);
    OutInteger(out, numArgs_);
    // TODO: why are body and args not set sometimes when we hash deserialized
    //     value to check hash consistency? It probably has something to do with
    //     cyclic references in serialization, but why?
    //     (This is one of the reasons we use SEXP instead of unpacking Code for
    //      body and default args, also because we are going to serialize the
    //      SEXP anyways to properly handle cyclic references)
    UUIDPool::writeItem(getEntry(0), refTable, out);
    for (unsigned i = 0; i < numArgs_; i++) {
        CodeSEXP arg = defaultArg_[i];
        OutInteger(out, (int)(arg != nullptr));
        if (arg) {
            // arg->serialize(false, refTable, out);
            UUIDPool::writeItem(arg, refTable, out);
        }
    }
    OutInteger(noHashOut, (int)flags.to_i());
}

void Function::disassemble(std::ostream& out) const {
    print(out);
}

void Function::print(std::ostream& out, bool hashInfo) const {
    if (hashInfo) {
        out << "[size]" << size << "\n[numArgs] " << numArgs_ << "\n";
    }
    out << "[signature] ";
    signature().print(out);
    if (!context_.empty())
        out << "| context: [" << context_ << "]";
    out << "\n";
    out << "[flags]    ";
#define V(F)                                                                   \
    if (flags.includes(F))                                                     \
        out << #F << " ";
    RIR_FUNCTION_FLAGS(V)
#undef V
    out << "\n";
    out << "[stats]    ";
    out << "invoked: " << invocationCount()
        << ", time: " << ((double)invocationTime() / 1e6)
        << "ms, deopt: " << deoptCount();
    out << "\n";
    if (hashInfo) {
        body()->print(out, true);
        for (unsigned i = 0; i < numArgs_; i++) {
            CodeSEXP arg = defaultArg_[i];
            if (arg) {
                out << "[default arg " << i << "]\n";
                Code::unpack(arg)->print(out, true);
            }
        }
    } else {
        body()->disassemble(out);
    }
}

static int GLOBAL_SPECIALIZATION_LEVEL =
    getenv("PIR_GLOBAL_SPECIALIZATION_LEVEL")
        ? atoi(getenv("PIR_GLOBAL_SPECIALIZATION_LEVEL"))
        : 100;
void Function::clearDisabledAssumptions(Context& given) const {
    if (flags.contains(Function::DisableArgumentTypeSpecialization))
        given.clearTypeFlags();
    if (flags.contains(Function::DisableNumArgumentsSpezialization))
        given.clearNargs();
    if (flags.contains(Function::DisableAllSpecialization))
        given.clearExcept(pir::Compiler::minimalContext);

    if (GLOBAL_SPECIALIZATION_LEVEL < 100)
        given.setSpecializationLevel(GLOBAL_SPECIALIZATION_LEVEL);
}

} // namespace rir
