#include "DispatchTable.h"
#include "serializeHash/serialize/serialize.h"

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
    HashAdd(container(), refTable);
    OutInteger(out, (int)size());
    assert(size() > 0);
    for (size_t i = 0; i < size(); i++) {
        WriteItem(getEntry(i), refTable, out);
    }
}

void DispatchTable::hash(Hasher& hasher) const {
    assert(size() > 0);
    // Only hash baseline so the hash doesn't change when new entries get added
    // (since semantics won't, and other rir objects will reference optimized
    //  versions directly when they rely on them)
    hasher.hash(getEntry(0));
}

void DispatchTable::addConnected(ConnectedCollector& collector) const {
    assert(size() > 0);
    for (size_t i = 0; i < size(); i++) {
        collector.add(getEntry(i));
    }
}

void DispatchTable::print(std::ostream& out, RirObjectPrintStyle style) const {
    assert((style == RirObjectPrintStyle::Default ||
            style == RirObjectPrintStyle::Detailed) &&
           "Unknown print style");

    out << "DispatchTable(size = " << size() << "):\n";
    for (size_t i = 0; i < size(); i++) {
        out << "Entry " << i << ":\n";
        get(i)->print(out, style);
    }
}

} // namespace rir