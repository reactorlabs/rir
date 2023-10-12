#include "DispatchTable.h"
#include "R/Protect.h"
#include "runtime/log/printPrettyGraph.h"
#include "serializeHash/serialize/serialize.h"

namespace rir {

DispatchTable* DispatchTable::deserialize(AbstractDeserializer& deserializer) {
    Protect p;
    auto dt = create();
    p(dt->container());
    // Magic is already set
    deserializer.addRef(dt->container());
    if (deserializer.willRead(SerialFlags::DtContext)) {
        dt->userDefinedContext_ = Context(
            deserializer.readBytesOf<unsigned long>(SerialFlags::DtContext));
    }
    dt->size_ = deserializer.willRead(SerialFlags::DtOptimized)
                    ? deserializer.readBytesOf<int>(SerialFlags::DtOptimized)
                    : 1;
    for (size_t i = 0; i < dt->size(); i++) {
        auto version = deserializer.read(i == 0 ? SerialFlags::DtBaseline : SerialFlags::DtOptimized);
        Function::unpack(version)->dispatchTable(dt);
        dt->setEntry(i, version);
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

void DispatchTable::hash(HasherOld& hasher) const {
    assert(size() > 0);
    // Only hash baseline so the hash doesn't change when new entries get added
    // (since semantics won't, and other rir objects will reference optimized
    //  versions directly when they rely on them)
    hasher.hash(getEntry(0));
}

void DispatchTable::addConnected(ConnectedCollectorOld& collector) const {
    assert(size() > 0);
    for (size_t i = 0; i < size(); i++) {
        collector.add(getEntry(i), false);
    }
}

void DispatchTable::print(std::ostream& out, bool isDetailed) const { // NOLINT(*-no-recursion)
    std::cout << "== dispatch table " << this << " ==\n";

    for (size_t entry = 0; entry < size(); ++entry) {
        Function* f = get(entry);
        std::cout << "= version " << entry << " (" << f << ") =\n";
        f->disassemble(std::cout);
    }

    if (isDetailed && !baseline()->isDeserializing()) {
        auto code = baseline()->body();
        auto pc = code->code();
        auto printHeader = true;

        Opcode* prev = nullptr;
        Opcode* pprev = nullptr;

        while (pc < code->endCode()) {
            auto bc = BC::decode(pc, code);
            if (bc.bc == Opcode::close_) {
                if (printHeader) {
                    out << "== nested closures ==\n";
                    printHeader = false;
                }

                // prev is the push_ of srcref
                // pprev is the push_ of body
                auto body = BC::decodeShallow(pprev).immediateConst();
                auto dt = DispatchTable::unpack(body);
                dt->print(std::cout, isDetailed);
            }
            pprev = prev;
            prev = pc;
            pc = BC::next(pc);
        }
    }
}

void DispatchTable::printPrettyGraphContent(const PrettyGraphInnerPrinter& print) const {
    print.addName([&](std::ostream& s) { s << "DispatchTable(" << size() << ")"; });
    for (size_t i = 0; i < size(); i++) {
        print.addEdgeTo(getEntry(i), true, "entry", [&](std::ostream& s) {
            s << "entry " << i;
        });
    }

    // Add edges to nested closures
    {
        auto code = baseline()->body();
        auto pc = code->code();

        Opcode* prev = nullptr;
        Opcode* pprev = nullptr;

        while (pc < code->endCode()) {
            auto bc = BC::decode(pc, code);
            if (bc.bc == Opcode::close_) {
                // prev is the push_ of srcref
                // pprev is the push_ of body
                auto childBody = BC::decodeShallow(pprev).immediateConst();
                print.addEdgeTo(childBody, true, "nested-closure");
            }
            pprev = prev;
            prev = pc;
            pc = BC::next(pc);
        }
    }
}

void DispatchTable::debugCompare(const rir::DispatchTable* dt1,
                                 const rir::DispatchTable* dt2,
                                 std::stringstream& differences,
                                 bool compareExtraPoolRBytecodes) {
    if (dt1->size() != dt2->size()) {
        differences << "DispatchTable size differs: " << dt1->size() << " vs " << dt2->size() << "\n";
    }
    for (size_t i = 0; i < dt1->size() && i < dt2->size(); i++) {
        std::stringstream funDifferencesStream;
        Function::debugCompare(
            Function::unpack(dt1->getEntry(i)),
            Function::unpack(dt2->getEntry(i)),
            funDifferencesStream,
            compareExtraPoolRBytecodes
        );
        std::string funDifferences = funDifferencesStream.str();
        if (!funDifferences.empty()) {
            differences << "DispatchTable entry " << i << " differs:\n" << funDifferences;
        }
    }
}

} // namespace rir