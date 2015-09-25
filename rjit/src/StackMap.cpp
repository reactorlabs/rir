#include "StackMap.h"

#include "llvm/IR/Instructions.h"
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Attributes.h>

using namespace llvm;

namespace rjit {

uint64_t StackMap::nextStackmapId = 3;

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

// record stackmaps will parse the stackmap section of the current module and
// index all entries.
void StackMap::recordStackmaps(stackmap_t sm,
                               std::unordered_map<uint64_t, uintptr_t>& fids) {
    int i = 0;

    StackMapParserT p(sm);

    for (const auto& r : p.records()) {
        assert(r.getID() != (uint64_t)-1 &&
               r.getID() != StackMap::genericStatepointID);

        if (fids.count(r.getID())) {
            auto function = fids.at(r.getID());
            StackMap::registerStatepoint(function, r.getInstructionOffset(), sm,
                                         i);
        } else {
            // No such function id -> must be patchpoint
            StackMap::registerPatchpoint(r.getID(), sm, i);
        }
        i++;
    }
}

unsigned StackMap::genericStatepointID = 0xABCDEF00;
std::unordered_map<uintptr_t, StackMap::StatepointRecord> StackMap::statepoint;
std::unordered_map<uint64_t, StackMap::PatchpointRecord> StackMap::patchpoint;
}
