#include "DispatchTable.h"
#include "hash/contextualHashing.h"
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
    HashAdd(container(), refTable);
    NO_HASH({
        OutInteger(out, (int)size());
    });
    BIG_HASH({
        assert(size() > 0);
        WriteItem(getEntry(0), refTable, out);
    });
    NO_HASH({
        for (size_t i = 1; i < size(); i++) {
            // Only hash baseline so the hash doesn't change
            WriteItem(getEntry(i), refTable, out);
        }
    });
}

void DispatchTable::print(std::ostream& out, bool hashInfo) const {
    out << "DispatchTable(size = " << size() << "):\n";
    for (size_t i = 0; i < size(); i++) {
        out << "Entry " << i << ":\n";
        get(i)->print(out, hashInfo);
    }
}

} // namespace rir