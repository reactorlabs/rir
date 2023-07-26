#include "ArglistOrder.h"
#include "R/Protect.h"
#include "R/Serialize.h"
#include "serializeHash/serialize/serialize.h"

namespace rir {

ArglistOrder* ArglistOrder::deserialize(__attribute__((unused)) SEXP refTable, R_inpstream_t inp) {
    Protect p;
    int size = InInteger(inp);
    SEXP store = p(Rf_allocVector(EXTERNALSXP, size));
    useRetrieveHashIfSet(inp, store);
    auto arglistOrder = new (DATAPTR(store)) ArglistOrder(InInteger(inp));
    for (int i = 0, offset = sizeof(ArglistOrder); offset < size; i++, offset += sizeof(*data)) {
        arglistOrder->data[i] = (ArglistOrder::ArgIdx)InInteger(inp);
    }
    return arglistOrder;
}

void ArglistOrder::serialize(__attribute__((unused)) SEXP refTable, R_outpstream_t out) const {
    int size = (int)this->size();
    OutInteger(out, size);
    OutInteger(out, (int)nCalls);
    for (int i = 0, offset = sizeof(ArglistOrder); offset < size; i++, offset += sizeof(*data)) {
        OutInteger(out, (int)data[i]);
    }
}

void ArglistOrder::hash(Hasher& hasher) const {
    int size = (int)this->size();
    hasher.hashBytesOf(nCalls);
    for (int i = 0, offset = sizeof(ArglistOrder); offset < size; i++, offset += sizeof(*data)) {
        hasher.hashBytesOf(data[i]);
    }
}

void ArglistOrder::addConnected(__attribute__((unused)) ConnectedCollector& collector) const {
    // No connected SEXPs in ArglistOrder
}

} // namespace rir