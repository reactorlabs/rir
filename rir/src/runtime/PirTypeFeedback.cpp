#include "PirTypeFeedback.h"
#include "Code.h"
#include "R/Protect.h"
#include "compiler/pir/instruction.h"
#include "serializeHash/hash/UUIDPool.h"
#include "serializeHash/serialize/serializeR.h"
#include <unordered_map>

namespace rir {

const size_t PirTypeFeedback::MAX_SLOT_IDX;

PirTypeFeedback::PirTypeFeedback(
    const std::unordered_set<Code*>& codes,
    const std::unordered_map<size_t, const pir::TypeFeedback&>& slots)
    : RirRuntimeObject(sizeof(*this), codes.size()) {
    assert((size_t)XLENGTH(container()) >=
           requiredSize(codes.size(), slots.size()));
    memset(entry, MAX_SLOT_IDX, sizeof(entry));
    assert(slots.size() < MAX_SLOT_IDX);
    assert(codes.size() < MAX_SLOT_IDX);
    static_assert(MAX_SLOT_IDX <= 0xff * sizeof(uint8_t), "");

    // Store all origins in the gc area, to keep them live
    // TODO, is this really needed? or is there any guarantee that my baseline
    // and all inlinee's baseline code objects stay live? also this should
    // probably be a weak map instead...
    std::unordered_map<Code*, uint8_t> srcCodeMap;
    size_t idx = 0;
    for (auto c : codes) {
        srcCodeMap[c] = idx;
        setEntry(idx++, c->container());
    }

    idx = 0;

    std::unordered_map<Opcode*, size_t> reverseMapping;

    for (auto s : slots) {
        auto slot = s.first;
        auto typeFeedback = s.second;
        assert(slot < MAX_SLOT_IDX);

        auto e = reverseMapping.find(typeFeedback.feedbackOrigin.pc());
        if (e != reverseMapping.end()) {
            entry[slot] = e->second;
            assert(mdEntries()[e->second].previousType == typeFeedback.type);
        } else {
            assert(codes.count(typeFeedback.feedbackOrigin.srcCode()));
            new (&mdEntries()[idx]) MDEntry;
            mdEntries()[idx].srcCode =
                srcCodeMap.at(typeFeedback.feedbackOrigin.srcCode());
            mdEntries()[idx].offset = typeFeedback.feedbackOrigin.offset();
            mdEntries()[idx].previousType = typeFeedback.type;
            reverseMapping[typeFeedback.feedbackOrigin.pc()] = idx;
            entry[slot] = idx++;
        }
    }
}

Code* PirTypeFeedback::getSrcCodeOfSlot(size_t slot) {
    auto code = getEntry(getMDEntryOfSlot(slot).srcCode);
    return Code::unpack(code);
}

Opcode* PirTypeFeedback::getOriginOfSlot(size_t slot) {
    return getSrcCodeOfSlot(slot)->code() + getBCOffsetOfSlot(slot);
}

PirTypeFeedback* PirTypeFeedback::deserializeR(SEXP refTable, R_inpstream_t inp) {
    Protect p;
    int size = InInteger(inp);
    SEXP store = p(Rf_allocVector(EXTERNALSXP, size));
    AddReadRef(refTable, store);
    useRetrieveHashIfSet(inp, store);

    int numCodes = InInteger(inp);
    int numEntries = InInteger(inp);
    auto typeFeedback = new (DATAPTR(store)) PirTypeFeedback(numCodes);
    InBytes(inp, typeFeedback->entry, sizeof(typeFeedback->entry));
    for (int i = 0; i < numCodes; i++) {
        typeFeedback->setEntry(i, p(UUIDPool::readItem(refTable, inp)));
    }
    InBytes(inp, typeFeedback->mdEntries(), (int)sizeof(MDEntry) * numEntries);
    return typeFeedback;
}

void PirTypeFeedback::serializeR(SEXP refTable, R_outpstream_t out) const {
    HashAdd(container(), refTable);
    OutInteger(out, (int)size());
    auto numCodes = this->numCodes();
    auto numEntries = this->numEntries();
    OutInteger(out, numCodes);
    OutInteger(out, numEntries);
    OutBytes(out, entry, sizeof(entry));
    for (int i = 0; i < numCodes; i++) {
        UUIDPool::writeItem(getEntry(i), false, refTable, out);
    }
    OutBytes(out, mdEntries(), (int)sizeof(MDEntry) * numEntries);
}

PirTypeFeedback* PirTypeFeedback::deserialize(AbstractDeserializer& deserializer) {
    Protect p;
    auto size = deserializer.readBytesOf<R_xlen_t>();
    SEXP store = p(Rf_allocVector(EXTERNALSXP, size));
    deserializer.addRef(store);

    auto numCodes = deserializer.readBytesOf<int>();
    auto numEntries = deserializer.readBytesOf<int>();
    auto typeFeedback = new (DATAPTR(store)) PirTypeFeedback(numCodes);
    deserializer.readBytes(typeFeedback->entry, sizeof(typeFeedback->entry));
    for (int i = 0; i < numCodes; i++) {
        typeFeedback->setEntry(i, p(deserializer.read()));
    }
    deserializer.readBytes(typeFeedback->mdEntries(), (int)sizeof(MDEntry) * numEntries);
    return typeFeedback;
}

void PirTypeFeedback::serialize(AbstractSerializer& serializer) const {
    serializer.writeBytesOf((R_xlen_t)size());
    auto numCodes = this->numCodes();
        auto numEntries = this->numEntries();
    serializer.writeBytesOf(numCodes);
    serializer.writeBytesOf(numEntries);
    serializer.writeBytes(entry, sizeof(entry));
    for (int i = 0; i < numCodes; i++) {
        serializer.write(getEntry(i));
    }
    serializer.writeBytes(mdEntries(), (int)sizeof(MDEntry) * numEntries);
}

void PirTypeFeedback::hash(Hasher& hasher) const {
    auto numCodes = this->numCodes();
    auto numEntries = this->numEntries();
    hasher.hashBytesOf(numCodes);
    hasher.hashBytesOf(numEntries);
    hasher.hashBytes(entry, sizeof(entry));
    for (int i = 0; i < numCodes; i++) {
        hasher.hash(getEntry(i));
    }
    hasher.hashBytes(mdEntries(), (int)sizeof(MDEntry) * numEntries);
}

void PirTypeFeedback::addConnected(ConnectedCollector& collector) const {
    auto numCodes = this->numCodes();
    for (int i = 0; i < numCodes; i++) {
        collector.add(getEntry(i), false);
    }
}

int PirTypeFeedback::numCodes() const {
    return (int)info.gc_area_length;
}

int PirTypeFeedback::numEntries() const {
    int numEntries = 0;
    for (auto id : entry) {
        if (id < MAX_SLOT_IDX && id > numEntries) {
            numEntries = id + 1;
        }
    }
    return numEntries;
}

size_t PirTypeFeedback::size() const {
    return requiredSize(numCodes(), numEntries());
}

} // namespace rir
