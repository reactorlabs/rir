#include "PirTypeFeedback.h"
#include "Code.h"
#include "R/Protect.h"
#include "compiler/pir/instruction.h"
#include "hash/RirUIDPool.h"
#include "runtime/TypeFeedback.h"
#include <iostream>
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
    std::unordered_map<Function*, uint8_t> functionMap;
    size_t idx = 0;
    for (auto c : codes) {
        functionMap[c->function()] = idx;
        setEntry(idx++, c->function()->container());
    }

    idx = 0;

    std::unordered_map<FeedbackOrigin, size_t> reverseMapping;

    for (auto s : slots) {
        auto slot = s.first;
        auto typeFeedback = s.second;
        assert(slot < MAX_SLOT_IDX);

        auto e = reverseMapping.find(typeFeedback.feedbackOrigin);

        if (e != reverseMapping.end()) {
            entry[slot] = e->second;
            assert(mdEntries()[e->second].previousType == typeFeedback.type);
        } else {
            assert(codes.count(typeFeedback.feedbackOrigin.function()->body()));
            new (&mdEntries()[idx]) MDEntry;
            mdEntries()[idx].funIdx =
                functionMap.at(typeFeedback.feedbackOrigin.function());
            mdEntries()[idx].rirIdx = typeFeedback.feedbackOrigin.index();
            mdEntries()[idx].previousType = typeFeedback.type;
            reverseMapping[typeFeedback.feedbackOrigin] = idx;
            entry[slot] = idx++;
        }
    }
}

FeedbackIndex PirTypeFeedback::rirIdx(size_t slot) {
    return getMDEntryOfSlot(slot).rirIdx;
}

PirTypeFeedback* PirTypeFeedback::deserialize(SEXP refTable, R_inpstream_t inp) {
    Protect p;
    int size = InInteger(inp);
    int numCodes = InInteger(inp);
    int numEntries = InInteger(inp);
    SEXP store = p(Rf_allocVector(EXTERNALSXP, size));
    auto typeFeedback = new (DATAPTR(store)) PirTypeFeedback(numCodes);
    InBytes(inp, typeFeedback->entry, sizeof(typeFeedback->entry));
    for (int i = 0; i < numCodes; i++) {
        typeFeedback->setEntry(i, p(RirUIDPool::readItem(refTable, inp)));
    }
    InBytes(inp, typeFeedback->mdEntries(), (int)sizeof(MDEntry) * numEntries);
    return typeFeedback;
}

void PirTypeFeedback::serialize(SEXP refTable, R_outpstream_t out) const {
    OutInteger(out, (int)size());
    auto numCodes = this->numCodes();
    auto numEntries = this->numEntries();
    OutInteger(out, numCodes);
    OutInteger(out, numEntries);
    OutBytes(out, entry, sizeof(entry));
    for (int i = 0; i < numCodes; i++) {
        RirUIDPool::writeItem(getEntry(i), refTable, out);
    }
    OutBytes(out, mdEntries(), (int)sizeof(MDEntry) * numEntries);
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
