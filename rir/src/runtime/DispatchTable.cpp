#include "DispatchTable.h"
#include "runtime/log/printPrettyGraph.h"
#include "serializeHash/hash/UUIDPool.h"
#include "serializeHash/serialize/serialize.h"

namespace rir {

DispatchTable* DispatchTable::onlyBaseline(Function* baseline,
                                           const Context& userDefinedContext,
                                           size_t capacity) {
    auto dt = create(capacity);
    dt->setEntry(0, baseline->container());
    dt->size_ = 1;
    dt->userDefinedContext_ = userDefinedContext;
    return dt;
}

SEXP DispatchTable::onlyBaselineClosure(Function* baseline,
                                        const Context& userDefinedContext,
                                        size_t capacity) {
    PROTECT(baseline->container());
    auto dt = onlyBaseline(baseline, userDefinedContext, capacity);
    auto what = Rf_allocSExp(CLOSXP);
    SET_FORMALS(what, R_NilValue);
    SET_BODY(what, dt->container());
    SET_CLOENV(what, R_GlobalEnv);
    UNPROTECT(1);
    return what;
}

DispatchTable* DispatchTable::deserialize(SEXP refTable, R_inpstream_t inp) {
    DispatchTable* table = create();
    PROTECT(table->container());
    AddReadRef(refTable, table->container());
    useRetrieveHashIfSet(inp, table->container());
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
    assert(size() > 0);
    for (size_t i = 0; i < size(); i++) {
        UUIDPool::writeItem(getEntry(i), refTable, out);
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
        collector.add(getEntry(i), true);
    }
}

void DispatchTable::print(std::ostream& out, bool isDetailed) const {
    out << "DispatchTable(size = " << size() << "):\n";
    for (size_t i = 0; i < size(); i++) {
        out << "Entry " << i << ":\n";
        get(i)->print(out, isDetailed);
    }
}

void DispatchTable::printPrettyGraphContent(const PrettyGraphInnerPrinter& print) const {
    print.addName([&](std::ostream& s) { s << "DispatchTable(" << size() << ")"; });
    for (size_t i = 0; i < size(); i++) {
        print.addEdgeTo(getEntry(i), true, "entry", [&](std::ostream& s) {
            s << "entry " << i;
        });
    }
}

} // namespace rir