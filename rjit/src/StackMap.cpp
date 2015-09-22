#include "StackMap.h"

namespace rjit {

// Statepoints are identified by their pc address as seen on the runtime
// stack while scanning for roots.
void StackMap::registerStatepoint(uintptr_t function, uintptr_t offset,
                                  stackmap_t stackmap,
                                  unsigned stackmapOffset) {
    statepoint.emplace(function + offset,
                       StatepointRecord({stackmap, stackmapOffset, function}));
}

// Patchpoints are identified by their unique id given at compile time
void StackMap::registerPatchpoint(uintptr_t id, stackmap_t stackmap,
                                  unsigned stackmapOffset) {
    patchpoint.emplace(id, PatchpointRecord({stackmap, stackmapOffset}));
}

bool StackMap::isStatepoint(uintptr_t pc) { return statepoint.count(pc); }

StackMapParserT::RecordAccessor StackMap::getStatepoint(uintptr_t pc) {
    assert(statepoint.count(pc));

    StatepointRecord& e = statepoint.at(pc);
    StackMapParserT p(e.stackmap);
    const auto& r = p.getRecord(e.idx);
    return r;
}

unsigned StackMap::getStackSize(uintptr_t pc) {
    StatepointRecord& e = statepoint.at(pc);
    StackMapParserT p(e.stackmap);
    for (auto& f : p.functions()) {
        if (f.getFunctionAddress() == e.fun)
            return f.getStackSize();
    }
    assert(false);
    return 0;
}

StackMapParserT::RecordAccessor StackMap::getPatchpoint(uint64_t id) {
    assert(patchpoint.count(id));

    PatchpointRecord& e = patchpoint.at(id);
    StackMapParserT p(e.stackmap);
    const auto& r = p.getRecord(e.idx);
    assert(r.getID() != genericStatepointID);
    assert(r.getID() == id);
    return r;
}

unsigned StackMap::genericStatepointID = 0xABCDEF00;
std::unordered_map<uintptr_t, StackMap::StatepointRecord> StackMap::statepoint;
std::unordered_map<uint64_t, StackMap::PatchpointRecord> StackMap::patchpoint;
}
