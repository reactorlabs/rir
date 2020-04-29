#include "Function.h"
#include "R/Serialize.h"
#include "compiler/translations/rir_2_pir/rir_2_pir_compiler.h"

namespace rir {

Function* Function::deserialize(SEXP refTable, R_inpstream_t inp) {
    size_t functionSize = InInteger(inp);
    const FunctionSignature sig = FunctionSignature::deserialize(refTable, inp);
    SEXP store = Rf_allocVector(EXTERNALSXP, functionSize);
    void* payload = DATAPTR(store);
    Function* fun = new (payload) Function(functionSize, NULL, {}, sig);
    fun->numArgs = InInteger(inp);
    fun->info.gc_area_length += fun->numArgs;
    for (unsigned i = 0; i < fun->numArgs + 1; i++) {
        fun->setEntry(i, R_NilValue);
    }
    PROTECT(store);
    AddReadRef(refTable, store);
    SEXP body = Code::deserialize(refTable, inp)->container();
    fun->body(body);
    PROTECT(body);
    int protectCount = 2;
    for (unsigned i = 0; i < fun->numArgs; i++) {
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
    OutInteger(out, numArgs);
    HashAdd(container(), refTable);
    body()->serialize(refTable, out);
    for (unsigned i = 0; i < numArgs; i++) {
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
void Function::clearDisabledAssumptions(Assumptions& given) const {
    if (flags.contains(Function::DisableArgumentTypeSpecialization))
        given.clearTypeFlags();
    if (flags.contains(Function::DisableNumArgumentsSepzialization))
        given.clearNargs();
    if (flags.contains(Function::DisableAllSpecialization))
        given.clearExcept(pir::Rir2PirCompiler::minimalAssumptions);

    if (GLOBAL_SPECIALIZATION_LEVEL < 100)
        given.setSpecializationLevel(GLOBAL_SPECIALIZATION_LEVEL);
}

} // namespace rir
