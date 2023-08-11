#include "LazyArglist.h"
#include "R/Protect.h"
#include "R/Serialize.h"
#include "serializeHash/hash/UUIDPool.h"
#include "serializeHash/serialize/serializeR.h"

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
        res.u.sxpval = p(UUIDPool::readItem(refTable, inp));
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
        UUIDPool::writeItem(stackArg.u.sxpval, false, refTable, out);
    } else {
        OutBytes(out, &stackArg.u, sizeof(stackArg.u));
    }
}

R_bcstack_t deserializeStackArg(Protect& p, AbstractDeserializer& deserializer) {
    R_bcstack_t res;
    res.tag = deserializer.readBytesOf<int>();
    res.flags = deserializer.readBytesOf<int>();
    auto isSexpArg = deserializer.readBytesOf<bool>();
    if (isSexpArg) {
        res.u.sxpval = p(deserializer.read());
    } else {
        deserializer.readBytes(&res.u, sizeof(res.u));
    }
    return res;
}

void serializeStackArg(const R_bcstack_t& stackArg, AbstractSerializer& serializer) {
    auto isSexpArg = stackArg.tag == 0;
    serializer.writeBytesOf(stackArg.tag);
    serializer.writeBytesOf(stackArg.flags);
    serializer.writeBytesOf(isSexpArg);
    if (isSexpArg) {
        serializer.write(stackArg.u.sxpval);
    } else {
        serializer.writeBytes(&stackArg.u, sizeof(stackArg.u));
    }
}

void hashStackArg(const R_bcstack_t& stackArg, Hasher& hasher) {
    auto isSexpArg = stackArg.tag == 0;
    hasher.hashBytesOf(stackArg.tag);
    hasher.hashBytesOf(stackArg.flags);
    hasher.hashBytesOf(isSexpArg);
    if (isSexpArg) {
        hasher.hash(stackArg.u.sxpval);
    } else {
        hasher.hashBytes(&stackArg.u, sizeof(stackArg.u));
    }
}

void addConnectedStackArg(const R_bcstack_t& stackArg, ConnectedCollector& collector) {
    auto isSexpArg = stackArg.tag == 0;
    if (isSexpArg) {
        collector.add(stackArg.u.sxpval, false);
    }
}

LazyArglist* LazyArglist::deserializeR(SEXP refTable, R_inpstream_t inp) {
    Protect p;
    int size = InInteger(inp);
    SEXP store = p(Rf_allocVector(EXTERNALSXP, size));
    AddReadRef(refTable, store);
    useRetrieveHashIfSet(inp, store);

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
            args[i] = {0, 0, {.sxpval = p(UUIDPool::readItem(refTable, inp))}};
        }
    }
    auto ast = p(UUIDPool::readItem(refTable, inp));
    auto reordering = p(UUIDPool::readItem(refTable, inp));

    auto arglist = new (DATAPTR(store)) LazyArglist(callId, reordering, length, args, ast, onStack);

    // Otherwise it's owned by LazyArglist. But is this a leak?
    if (!onStack) {
        delete[] args;
    }

    return arglist;
}

void LazyArglist::serializeR(SEXP refTable, R_outpstream_t out) const {
    HashAdd(container(), refTable);
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
            UUIDPool::writeItem(heapArg, false, refTable, out);
        }
        UUIDPool::writeItem(ast, false, refTable, out);
        UUIDPool::writeItem(reordering, true, refTable, out);
    }
}

LazyArglist* LazyArglist::deserialize(AbstractDeserializer& deserializer) {
    Protect p;
    auto size = deserializer.readBytesOf<R_xlen_t>();
    SEXP store = p(Rf_allocVector(EXTERNALSXP, size));
    deserializer.addRef(store);

    auto callId = deserializer.readBytesOf<ArglistOrder::CallId>();
    auto length = deserializer.readBytesOf<uint32_t>();
    auto onStack = deserializer.readBytesOf<bool>();
    auto args = new R_bcstack_t[length];
    if (onStack) {
        for (size_t i = 0; i < length; ++i) {
            args[i] = deserializeStackArg(p, deserializer);
        }
    } else {
        for (size_t i = 0; i < length; ++i) {
            args[i] = {0, 0, {.sxpval = p(deserializer.read())}};
        }
    }
    auto ast = p(deserializer.read(SerialFlags::Ast));
    auto reordering = p(deserializer.read());

    auto arglist = new (DATAPTR(store)) LazyArglist(callId, reordering, length, args, ast, onStack);

    // Otherwise it's owned by LazyArglist. But is this a leak?
    if (!onStack) {
        delete[] args;
    }

    return arglist;
}

void LazyArglist::serialize(AbstractSerializer& serializer) const {
    serializer.writeBytesOf((R_xlen_t)size());
    serializer.writeBytesOf(callId);
    serializer.writeBytesOf(length);
    // actualNargs is a lazily-computed value, and we don't want laziness to
    // affect serialization
    serializer.writeBytesOf<bool>(stackArgs != nullptr);
    if (stackArgs) {
        for (size_t i = 0; i < length; ++i) {
            serializeStackArg(stackArgs[i], serializer);
        }
    } else {
        for (size_t i = 0; i < length; ++i) {
            auto heapArg = heapArgs[i];
            // This invariant isn't clear but it holds
            SLOWASSERT(heapArg == getEntry(i + 1));
            serializer.write(heapArg);
        }
        serializer.write(ast, SerialFlags::Ast);
        serializer.write(reordering);
    }
}

void LazyArglist::hash(Hasher& hasher) const {
    hasher.hashBytesOf(callId);
    hasher.hashBytesOf(length);
    // actualNargs is a lazily-computed value, and we don't want laziness to
    // affect hashing
    hasher.hashBytesOf(stackArgs != nullptr);
    if (stackArgs) {
        for (size_t i = 0; i < length; ++i) {
            hashStackArg(stackArgs[i], hasher);
        }
    } else {
        for (size_t i = 0; i < length; ++i) {
            auto heapArg = heapArgs[i];
            // This invariant isn't clear but it holds
            SLOWASSERT(heapArg == getEntry(i + 1));
            hasher.hash(heapArg);
        }
        hasher.hash(ast, false);
        hasher.hash(reordering);
    }
}

void LazyArglist::addConnected(ConnectedCollector& collector) const {
    if (stackArgs) {
        for (size_t i = 0; i < length; ++i) {
            addConnectedStackArg(stackArgs[i], collector);
        }
    } else {
        for (size_t i = 0; i < length; ++i) {
            auto heapArg = heapArgs[i];
            // This invariant isn't clear but it holds
            SLOWASSERT(heapArg == getEntry(i + 1));
            collector.add(heapArg, false);
        }
        collector.add(ast, false);
        collector.add(reordering, true);
    }
}

size_t LazyArglist::size() const {
    return sizeof(LazyArglist) + (stackArgs ? 0 : length * sizeof(SEXP));
}

} // namespace rir