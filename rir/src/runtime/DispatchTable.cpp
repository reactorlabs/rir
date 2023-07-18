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
    // We don't want to include other entries in the hash, but we need to add
    //  them to the connected worklist to recursively intern them. Otherwise
    //  we will error when serializing them because we need the other entries'
    //  hashes
    R_outpstream_st nullOut = nullOutputStream();
    auto noHashOut = isHashing(out) ? &nullOut : out;

    HashAdd(container(), refTable);
    OutInteger(noHashOut, (int)size());
    assert(size() > 0);
    // Only hash baseline so the hash doesn't change when new entries get added
    // (since semantics won't, and other rir objects will reference optimized
    //  versions directly when they rely on them)
    WriteItem(getEntry(0), refTable, out);
    for (size_t i = 1; i < size(); i++) {
        WriteItem(getEntry(i), refTable, noHashOut);
    }
}

void DispatchTable::print(std::ostream& out, bool hashInfo) const {
    out << "DispatchTable(size = " << size() << "):\n";
    for (size_t i = 0; i < size(); i++) {
        out << "Entry " << i << ":\n";
        get(i)->print(out, hashInfo);
    }
}

} // namespace rir