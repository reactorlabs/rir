#include "Function.h"
#include "R/Serialize.h"

namespace rir {

Function* Function::deserialize(SEXP refTable, R_inpstream_t inp) {
    size_t functionSize = InInteger(inp);
    SEXP body = Code::deserialize(refTable, inp)->container();
    std::vector<SEXP> defaultArgs;
    int numArgs = InInteger(inp);
    for (int i = 0; i < numArgs; i++) {
        if ((bool)InInteger(inp))
            defaultArgs.push_back(
                Code::deserialize(refTable, inp)->container());
        else
            defaultArgs.push_back(nullptr);
    }
    const FunctionSignature sig = FunctionSignature::deserialize(refTable, inp);
    SEXP store = Rf_allocVector(EXTERNALSXP, functionSize);
    void* payload = DATAPTR(store);
    return new (payload) Function(functionSize, body, defaultArgs, sig);
}

void Function::serialize(SEXP refTable, R_outpstream_t out) const {
    OutInteger(out, size);
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
