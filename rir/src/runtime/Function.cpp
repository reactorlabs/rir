#include "Function.h"
#include "R/Serialize.h"

namespace rir {

Function* Function::deserialize(SEXP refTable, R_inpstream_t inp) {
    size_t functionSize = InInteger(inp);
    SEXP store = Rf_allocVector(EXTERNALSXP, functionSize);
    void* payload = DATAPTR(store);
    AddReadRef(refTable, store);
    SEXP body = Code::deserialize(refTable, inp)->container();
    PROTECT(body);
    std::vector<SEXP> defaultArgs;
    int numArgs = InInteger(inp);
    int protectCount = 1;
    for (int i = 0; i < numArgs; i++) {
        if ((bool)InInteger(inp)) {
            SEXP arg = Code::deserialize(refTable, inp)->container();
            PROTECT(arg);
            protectCount++;
            defaultArgs.push_back(arg);
        } else
            defaultArgs.push_back(nullptr);
    }
    const FunctionSignature sig = FunctionSignature::deserialize(refTable, inp);
    Function* fun =
        new (payload) Function(functionSize, body, defaultArgs, sig);
    UNPROTECT(protectCount);
    return fun;
}

void Function::serialize(SEXP refTable, R_outpstream_t out) const {
    OutInteger(out, size);
    HashAdd(container(), refTable);
    body()->serialize(refTable, out);
    OutInteger(out, numArgs);
    for (unsigned i = 0; i < numArgs; i++) {
        Code* arg = defaultArg(i);
        OutInteger(out, (int)(arg != NULL));
        if (arg != NULL)
            defaultArg(i)->serialize(refTable, out);
    }
    signature().serialize(refTable, out);
}

void Function::disassemble(std::ostream& out) {
    body()->disassemble(out);
}

} // namespace rir
