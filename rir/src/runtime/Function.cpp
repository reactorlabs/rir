#include "Function.h"
#include "R/Protect.h"
#include "R/Serialize.h"
#include "Rinternals.h"
#include "compiler/compiler.h"
#include "utils/UUIDPool.h"
#include "runtime/TypeFeedback.h"

namespace rir {

Function* Function::deserialize(SEXP refTable, R_inpstream_t inp) {
    Protect p;
    size_t functionSize = InInteger(inp);
    const FunctionSignature sig = FunctionSignature::deserialize(refTable, inp);
    const Context as = Context::deserialize(refTable, inp);
    SEXP store = p(Rf_allocVector(EXTERNALSXP, functionSize));
    void* payload = DATAPTR(store);
    Function* fun =
        new (payload) Function(functionSize, nullptr, {}, sig, as, nullptr);
    AddReadRef(refTable, store);
    fun->numArgs_ = InInteger(inp);
    fun->info.gc_area_length += fun->numArgs_;
    // What this loop does is that it sets the function owned (yet not
    // deserialized) SEXPs to something reasonable so it will not confuse the GC
    // which might run while they are deserialized.
    // TODO: wouldn't it be better to change the serialization order?
    for (unsigned i = 0; i < fun->numArgs_ + NUM_PTRS; i++) {
        fun->setEntry(i, R_NilValue);
    }
    auto feedback = p(UUIDPool::readItem(refTable, inp));
    fun->typeFeedback(TypeFeedback::unpack(feedback));
    auto body = p(UUIDPool::readItem(refTable, inp));
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
    OutInteger(out, size);
    signature().serialize(refTable, out);
    context_.serialize(refTable, out);
    HashAdd(container(), refTable);
    OutInteger(out, numArgs_);
    UUIDPool::writeItem(typeFeedback()->container(), refTable, out);
    // TODO: why are body and args not set sometimes when we hash deserialized
    //     value to check hash consistency? It probably has something to do with
    //     cyclic references in serialization, but why?
    //     (This is one of the reasons we use SEXP instead of unpacking Code for
    //      body and default args, also because we are going to serialize the
    //      SEXP anyways to properly handle cyclic references)
    UUIDPool::writeItem(refTable, getEntry(0), out);
    for (unsigned i = 0; i < numArgs_; i++) {
        CodeSEXP arg = defaultArg_[i];
        OutInteger(out, (int)(arg != nullptr));
        if (arg) {
            // arg->serialize(false, refTable, out);
            UUIDPool::writeItem(refTable, arg, out);
        }
    }
    OutInteger(out, flags.to_i());
}

void Function::disassemble(std::ostream& out) {
    out << "[sigature] ";
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
    body()->disassemble(out);
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
