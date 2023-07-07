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
    HashAdd(container(), refTable);
    OutInteger(out, (int)size());
    for (size_t i = 0; i < size(); i++) {
        WriteItem(getEntry(i), refTable, out);
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