#include "DispatchTable.h"

namespace rir {

DispatchTable* DispatchTable::deserialize(SEXP refTable, R_inpstream_t inp) {
    DispatchTable* table = create();
    PROTECT(table->container());
    AddReadRef(refTable, table->container());
    table->size_ = InInteger(inp);
    for (size_t i = 0; i < table->size(); i++) {
        table->setEntry(i,UUIDPool::readItem(refTable, inp));
    }
    UNPROTECT(1);
    return table;
}

void DispatchTable::serialize(SEXP refTable, R_outpstream_t out) const {
    HashAdd(container(), refTable);
    OutInteger(out, (int)size());
    for (size_t i = 0; i < size(); i++) {
        UUIDPool::writeItem(getEntry(i), refTable, out);
    }
}

} // namespace rir