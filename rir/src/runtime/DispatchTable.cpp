#include "DispatchTable.h"
#include "interpreter/serialize.h"

namespace rir {

DispatchTable* DispatchTable::deserialize(SEXP refTable, R_inpstream_t inp) {
    DispatchTable* table = create();
    PROTECT(table->container());
    AddReadRef(refTable, table->container());
    useRetrieveHashIfSet(inp, table->container());
    table->size_ = InInteger(inp);
    for (size_t i = 0; i < table->size(); i++) {
        table->setEntry(i,ReadItem(refTable, inp));
    }
    UNPROTECT(1);
    return table;
}

void DispatchTable::serialize(SEXP refTable, R_outpstream_t out) const {
    // Some stuff is mutable or not part of the structural identity, so we don't
    // want to hash it. However, we still need to serialize recursive items. To
    // do this, we temporarily replace out with a void stream.
    R_outpstream_st nullOut = nullOutputStream();
    auto noHashOut = isHashing(out) ? &nullOut : out;

    HashAdd(container(), refTable);
    OutInteger(noHashOut, (int)size());
    for (size_t i = 0; i < size(); i++) {
        // Only hash baseline so the hash doesn't change
        WriteItem(getEntry(i), refTable, i == 0 ? out : noHashOut);
    }
}

void DispatchTable::print(std::ostream& out, bool hashInfo) const {
    out << "DispatchTable(size = " << size() << "):\n";
    for (size_t i = 0; i < size(); i++) {
        std::cout << "Entry " << i << ":\n";
        get(i)->print(out, hashInfo);
    }
}

} // namespace rir