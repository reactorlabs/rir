#include "LazyArglist.h"
#include "R/Protect.h"
#include "R/Serialize.h"

namespace rir {

// ? idk why but this came up in the gitlab:
//     style: Parameter 'p' can be declared with const [constParameter]
// this is not true
// cppcheck-suppress constParameter
R_bcstack_t deserializeStackArg(Protect& p, SEXP refTable, R_inpstream_t inp) {
    R_bcstack_t res;
    res.tag = InInteger(inp);
    res.flags = InInteger(inp);
    auto isSexpArg = InBool(inp);
    if (isSexpArg) {
        res.u.sxpval = p(ReadItem(refTable, inp));
    } else {
        InBytes(inp, &res.u, sizeof(res.u));
    }
    return res;
}

void serializeStackArg(const R_bcstack_t& stackArg, SEXP refTable, R_outpstream_t out) {
    auto isSexpArg = stackArg.tag == 0;
    OutInteger(out, stackArg.tag);
    OutInteger(out, stackArg.flags);
    OutBool(out, isSexpArg);
    if (isSexpArg) {
        WriteItem(stackArg.u.sxpval, refTable, out);
    } else {
        OutBytes(out, &stackArg.u, sizeof(stackArg.u));
    }
}

LazyArglist* LazyArglist::deserialize(SEXP refTable, R_inpstream_t inp) {
    Protect p;
    int size = InInteger(inp);
    auto callId = InSize(inp);
    auto length = InUInt(inp);
    auto onStack = InBool(inp);
    auto args = new R_bcstack_t[length];
    if (onStack) {
        for (size_t i = 0; i < length; ++i) {
            args[i] = deserializeStackArg(p, refTable, inp);
        }
    } else {
        for (size_t i = 0; i < length; ++i) {
            args[i] = {0, 0, {.sxpval = p(ReadItem(refTable, inp))}};
        }
    }
    auto ast = p(ReadItem(refTable, inp));
    auto reordering = p(ReadItem(refTable, inp));

    SEXP store = p(Rf_allocVector(EXTERNALSXP, size));
    auto arglist = new (DATAPTR(store)) LazyArglist(callId, reordering, length, args, ast, onStack);

    // Otherwise it's owned by LazyArglist. But is this a leak?
    if (!onStack) {
        delete[] args;
    }

    return arglist;
}

void LazyArglist::serialize(SEXP refTable, R_outpstream_t out) const {
    OutInteger(out, (int)size());
    OutSize(out, callId);
    OutUInt(out, length);
    // actualNargs is a lazily-computed value, and we don't want laziness to
    // affect serialization
    OutBool(out, stackArgs != nullptr);
    if (stackArgs) {
        for (size_t i = 0; i < length; ++i) {
            serializeStackArg(stackArgs[i], refTable, out);
        }
    } else {
        for (size_t i = 0; i < length; ++i) {
            auto heapArg = heapArgs[i];
            // This invariant isn't clear but it holds
            SLOWASSERT(heapArg == getEntry(i + 1));
            WriteItem(heapArg, refTable, out);
        }
        WriteItem(ast, refTable, out);
        WriteItem(reordering, refTable, out);
    }
}

size_t LazyArglist::size() const {
    return sizeof(LazyArglist) + (stackArgs ? 0 : length * sizeof(SEXP));
}

} // namespace rir