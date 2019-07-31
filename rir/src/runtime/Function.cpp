#include "Function.h"
#include "R/Serialize.h"

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
    fun->deopt = InChar(inp);
    fun->markOpt = InChar(inp);
    fun->unoptimizable = InChar(inp);
    fun->uninlinable = InChar(inp);
    fun->dead = InChar(inp);
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
    OutChar(out, deopt ? 1 : 0);
    OutChar(out, markOpt ? 1 : 0);
    OutChar(out, unoptimizable ? 1 : 0);
    OutChar(out, uninlinable ? 1 : 0);
    OutChar(out, dead ? 1 : 0);
}

void Function::disassemble(std::ostream& out) {
    body()->disassemble(out);
}

} // namespace rir
