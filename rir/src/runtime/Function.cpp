#include "Function.h"
#include "R/Serialize.h"
#include "Rinternals.h"
#include "compiler/compiler.h"
#include "interpreter/call_context.h"
#include "runtime/TypeFeedback.h"

namespace rir {

Function* Function::deserialize(SEXP refTable, R_inpstream_t inp) {
    size_t functionSize = InInteger(inp);
    const FunctionSignature sig = FunctionSignature::deserialize(refTable, inp);
    const Context as = Context::deserialize(refTable, inp);
    SEXP store = Rf_allocVector(EXTERNALSXP, functionSize);
    void* payload = DATAPTR(store);
    Function* fun =
        new (payload) Function(functionSize, nullptr, {}, sig, as, nullptr);
    fun->numArgs_ = InInteger(inp);
    fun->info.gc_area_length += fun->numArgs_;
    // What this loop does is that it sets the function owned (yet not
    // deserialized) SEXPs to something reasonable so it will not confuse the GC
    // which might run while they are deserialized.
    for (unsigned i = 0; i < fun->numArgs_ + NUM_PTRS; i++) {
        fun->setEntry(i, R_NilValue);
    }
    PROTECT(store);
    AddReadRef(refTable, store);
    TypeFeedback* feedback = TypeFeedback::deserialize(refTable, inp);
    PROTECT(feedback->container());
    fun->typeFeedback(feedback);
    SEXP body = Code::deserialize(refTable, inp)->container();
    fun->body(body);
    PROTECT(body);
    int protectCount = 3;
    for (unsigned i = 0; i < fun->numArgs_; i++) {
        if ((bool)InInteger(inp)) {
            SEXP arg = Code::deserialize(refTable, inp)->container();
            PROTECT(arg);
            protectCount++;
            fun->setEntry(Function::NUM_PTRS + i, arg);
        } else
            fun->setEntry(Function::NUM_PTRS + i, nullptr);
    }
    fun->flags = EnumSet<Flag>(InInteger(inp));
    UNPROTECT(protectCount);
    return fun;
}

void Function::serialize(SEXP refTable, R_outpstream_t out) const {
    OutInteger(out, size);
    signature().serialize(refTable, out);
    context_.serialize(refTable, out);
    OutInteger(out, numArgs_);
    HashAdd(container(), refTable);
    typeFeedback()->serialize(refTable, out);
    body()->serialize(refTable, out);
    for (unsigned i = 0; i < numArgs_; i++) {
        Code* arg = defaultArg(i);
        OutInteger(out, (int)(arg != nullptr));
        if (arg)
            defaultArg(i)->serialize(refTable, out);
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
    out << "invoked: " << invocationCount() << ", deopt: " << deoptCount();
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

Function* Function::baseline() {
    if (!dispatchTable()) {
        assert(signature().optimization ==
               FunctionSignature::OptimizationLevel::Baseline);
        return this;
    }
    return dispatchTable()->baseline();
}

const Function* Function::baseline() const {
    if (!dispatchTable()) {
        assert(signature().optimization ==
               FunctionSignature::OptimizationLevel::Baseline);
        return this;
    }
    return dispatchTable()->baseline();
}

TypeFeedback* Function::typeFeedback() const {
    return TypeFeedback::unpack(getEntry(TYPE_FEEDBACK_IDX));
}

TypeFeedback* Function::typeFeedback(const Context& ctx) {
    if (dispatchTable() && ctx != context())
        return dispatchTable()->getOrCreateTypeFeedback(ctx);
    return typeFeedback();
}

size_t Function::recordingCount(const Context& ctx) {
    if (ctx == context())
        typeFeedback()->recordingCount();
    auto dp = dispatchTable();
    if (!dp)
        return 0;
    auto entry = dp->dispatchTypeFeedback(ctx);
    if (entry.first != ctx || !entry.second)
        return 0;
    return entry.second->recordingCount();
}

} // namespace rir
