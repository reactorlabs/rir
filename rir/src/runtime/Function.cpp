#include "Function.h"
#include "R/Serialize.h"
#include "compiler/compiler.h"

namespace rir {

Function* Function::deserialize(SEXP refTable, R_inpstream_t inp) {
    size_t functionSize = InInteger(inp);
    const FunctionSignature sig = FunctionSignature::deserialize(refTable, inp);
    const Context as = Context::deserialize(refTable, inp);
    SEXP store = Rf_allocVector(EXTERNALSXP, functionSize);
    void* payload = DATAPTR(store);
    Function* fun = new (payload) Function(functionSize, NULL, {}, sig, as);
    fun->numArgs_ = InInteger(inp);
    fun->info.gc_area_length += fun->numArgs_;
    for (unsigned i = 0; i < fun->numArgs_ + 1; i++) {
        fun->setEntry(i, R_NilValue);
    }
    PROTECT(store);
    AddReadRef(refTable, store);
    SEXP body = Code::deserialize(refTable, inp)->container();
    fun->body(body);
    PROTECT(body);
    int protectCount = 2;
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
    body()->serialize(refTable, out);
    for (unsigned i = 0; i < numArgs_; i++) {
        Code* arg = defaultArg(i);
        OutInteger(out, (int)(arg != NULL));
        if (arg != NULL)
            defaultArg(i)->serialize(refTable, out);
    }
    OutInteger(out, flags.to_i());
}

void Function::disassemble(std::ostream& out) {
    std::cout << "[sigature] ";
    signature().print(std::cout);
    if (!context_.empty())
        out << "| context: [" << context_ << "]";
    std::cout << "\n";
    std::cout << "[flags]    ";
#define V(F)                                                                   \
    if (flags.includes(F))                                                     \
        std::cout << #F << " ";
    RIR_FUNCTION_FLAGS(V)
#undef V
    std::cout << "\n";
    std::cout << "[stats]    ";
    std::cout << "invoked: " << invocationCount()
              << ", deopt: " << deoptCount();
    std::cout << "\n";
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
