#include "PirRegisterMap.h"
#include "Code.h"

namespace rir {

const size_t PirRegisterMap::MAX_SLOT_IDX;

PirRegisterMap::PirRegisterMap(
    const std::unordered_set<Code*>& codes,
    const std::unordered_map<size_t, std::pair<Code*, Opcode*>>& slots)
    : RirRuntimeObject(sizeof(*this), codes.size()) {
    assert((size_t)XLENGTH(container()) >=
           requiredSize(codes.size(), slots.size()));
    memset(entry, MAX_SLOT_IDX, sizeof(entry));
    assert(slots.size() < MAX_SLOT_IDX);

    // Store all origins in the gc area, to keep them live
    // TODO, is this really needed? or is there any guarantee that my baseline
    // and all inlinee's baseline code objects stay live? also this should
    // probably be a weak map instead...
    size_t idx = 0;
    for (auto c : codes)
        setEntry(idx++, c->container());

    idx = 0;
    for (auto s : slots) {
        auto slot = s.first;
        auto origin = s.second;
        assert(slot < MAX_SLOT_IDX);
        assert(codes.count(origin.first));
        new (&mdEntries()[idx]) MDEntry;
        mdEntries()[idx].origin = origin.second;
        entry[slot] = idx++;
    }
}

} // namespace rir
