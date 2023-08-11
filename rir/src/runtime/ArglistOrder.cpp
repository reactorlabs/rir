#include "ArglistOrder.h"
#include "R/Protect.h"
#include "R/Serialize.h"
#include "serializeHash/serialize/serializeR.h"

namespace rir {

ArglistOrder* ArglistOrder::deserializeR(__attribute__((unused)) SEXP refTable, R_inpstream_t inp) {
    Protect p;
    auto size = InInteger(inp);
    auto store = p(Rf_allocVector(EXTERNALSXP, size));
    useRetrieveHashIfSet(inp, store);
    auto arglistOrder = new (DATAPTR(store)) ArglistOrder(InInteger(inp));
    for (int i = 0, offset = sizeof(ArglistOrder); offset < size; i++, offset += sizeof(*data)) {
        arglistOrder->data[i] = (ArglistOrder::ArgIdx)InInteger(inp);
    }
    return arglistOrder;
}

void ArglistOrder::serializeR(__attribute__((unused)) SEXP refTable, R_outpstream_t out) const {
    auto size = (int)this->size();
    OutInteger(out, size);
    OutInteger(out, (int)nCalls);
    for (int i = 0, offset = sizeof(ArglistOrder); offset < size; i++, offset += sizeof(*data)) {
        OutInteger(out, (int)data[i]);
    }
}

ArglistOrder* ArglistOrder::deserialize(AbstractDeserializer& deserializer) {
    Protect p;
    auto size = deserializer.readBytesOf<R_xlen_t>();
    auto store = p(Rf_allocVector(EXTERNALSXP, size));
    // Needs ref for sanity check (assertion) even though it's not actually
    // needed
    deserializer.addRef(store);
    auto arglistOrder = new (DATAPTR(store)) ArglistOrder(deserializer.readBytesOf<int>());
    for (int i = 0, offset = sizeof(ArglistOrder); offset < size; i++, offset += sizeof(*data)) {
        arglistOrder->data[i] = (ArglistOrder::ArgIdx)deserializer.readBytesOf<int>();
    }
    return arglistOrder;
}

void ArglistOrder::serialize(AbstractSerializer& serializer) const {
    auto size = (R_xlen_t)this->size();
    serializer.writeBytesOf(size);
    serializer.writeBytesOf((int)nCalls);
    for (int i = 0, offset = sizeof(ArglistOrder); offset < size; i++, offset += sizeof(*data)) {
        serializer.writeBytesOf((int)data[i]);
    }
}

void ArglistOrder::hash(Hasher& hasher) const {
    auto size = (int)this->size();
    hasher.hashBytesOf(nCalls);
    for (int i = 0, offset = sizeof(ArglistOrder); offset < size; i++, offset += sizeof(*data)) {
        hasher.hashBytesOf(data[i]);
    }
}

void ArglistOrder::addConnected(__attribute__((unused)) ConnectedCollector& collector) const {
    // No connected SEXPs in ArglistOrder
}

} // namespace rir