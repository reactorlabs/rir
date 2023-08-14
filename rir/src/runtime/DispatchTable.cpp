#include "DispatchTable.h"
#include "R/Protect.h"
#include "runtime/log/printPrettyGraph.h"
#include "serializeHash/hash/UUIDPool.h"
#include "serializeHash/serialize/serialize.h"
#include "serializeHash/serialize/serializeR.h"

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
    PROTECT(dt->container());
    auto what = Rf_allocSExp(CLOSXP);
    PROTECT(what);
    SET_FORMALS(what, R_NilValue);
    SET_BODY(what, dt->container());
    SET_CLOENV(what, R_GlobalEnv);
    UNPROTECT(3);
    return what;
}

DispatchTable* DispatchTable::deserializeR(SEXP refTable, R_inpstream_t inp) {
    DispatchTable* table = create();
    PROTECT(table->container());
    AddReadRef(refTable, table->container());
    useRetrieveHashIfSet(inp, table->container());
    InBytes(inp, (void*)&table->userDefinedContext_, sizeof(table->userDefinedContext_));
    table->size_ = InInteger(inp);
    for (size_t i = 0; i < table->size(); i++) {
        table->setEntry(i,UUIDPool::readItem(refTable, inp));
    }
    UNPROTECT(1);
    return table;
}

void DispatchTable::serializeR(SEXP refTable, R_outpstream_t out) const {
    HashAdd(container(), refTable);
    OutBytes(out, (void*)&userDefinedContext_, sizeof(userDefinedContext_));
    OutInteger(out, (int)size());
    assert(size() > 0);
    for (size_t i = 0; i < size(); i++) {
        UUIDPool::writeItem(getEntry(i), false, refTable, out);
    }
}

DispatchTable* DispatchTable::deserialize(AbstractDeserializer& deserializer) {
    Protect p;
    auto dt = create();
    p(dt->container());
    deserializer.addRef(dt->container());
    if (deserializer.willRead(SerialFlags::DtContext)) {
        dt->userDefinedContext_ = Context(
            deserializer.readBytesOf<unsigned long>(SerialFlags::DtContext));
    }
    DESERIALIZE(dt->size_, readBytesOf<int>, SerialFlags::DtOptimized);
    size_t n = deserializer.willRead(SerialFlags::DtOptimized) ? dt->size() : 1;
    for (size_t i = 0; i < n; i++) {
        dt->setEntry(i,deserializer.read(i == 0 ? SerialFlags::DtBaseline : SerialFlags::DtOptimized));
    }
    return dt;
}

void DispatchTable::serialize(AbstractSerializer& serializer) const {
    serializer.writeBytesOf(userDefinedContext_.toI(), SerialFlags::DtContext);
    serializer.writeBytesOf((int)size(), SerialFlags::DtOptimized);
    size_t n = serializer.willWrite(SerialFlags::DtOptimized) ? size() : 1;
    for (size_t i = 0; i < n; i++) {
        serializer.write(getEntry(i), i == 0 ? SerialFlags::DtBaseline : SerialFlags::DtOptimized);
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
        collector.add(getEntry(i), false);
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

void DispatchTable::debugCompare(const rir::DispatchTable* dt1,
                                 const rir::DispatchTable* dt2,
                                 std::stringstream& differences) {
    if (dt1->size() != dt2->size()) {
        differences << "DispatchTable size differs: " << dt1->size() << " vs " << dt2->size() << "\n";
    }
    for (size_t i = 0; i < dt1->size() && i < dt2->size(); i++) {
        std::stringstream funDifferencesStream;
        Function::debugCompare(
            Function::unpack(dt1->getEntry(i)),
            Function::unpack(dt2->getEntry(i)),
            funDifferencesStream
        );
        std::string funDifferences = funDifferencesStream.str();
        if (!funDifferences.empty()) {
            differences << "DispatchTable entry " << i << " differs:\n" << funDifferences;
        }
    }
}

} // namespace rir