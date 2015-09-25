#ifndef STACK_MAP_H
#define STACK_MAP_H

#include <llvm/IR/Module.h>
#include <llvm/ADT/ArrayRef.h>
#include "StackMapParser.h"

#include <unordered_map>
#include <iostream>

namespace rjit {

typedef llvm::ArrayRef<uint8_t> stackmap_t;

// TODO: find target endianness
typedef llvm::StackMapV1Parser<llvm::support::little> StackMapParserT;

class StackMap {
  public:
    struct StatepointRecord {
        stackmap_t stackmap;
        unsigned idx;
        uintptr_t fun;
    };

    struct PatchpointRecord {
        stackmap_t stackmap;
        unsigned idx;
    };

    static bool isStatepoint(uintptr_t pc);

    static StackMapParserT::RecordAccessor getStatepoint(uintptr_t pc);

    static unsigned getStackSize(uintptr_t pc);

    static StackMapParserT::RecordAccessor getPatchpoint(uint64_t id);

    static unsigned genericStatepointID;

    // record stackmaps will parse the stackmap section of the current module
    // and
    // index all entries.
    static void recordStackmaps(stackmap_t stackmap,
                                std::unordered_map<uint64_t, uintptr_t>& fids);

    static uint64_t nextStackmapId;

  private:
    // Statepoints are identified by their pc address as seen on the runtime
    // stack while scanning for roots.
    static void registerStatepoint(uintptr_t function, uintptr_t offset,
                                   stackmap_t stackmap,
                                   unsigned stackmapOffset);

    // Patchpoints are identified by their unique id given at compile time
    static void registerPatchpoint(uintptr_t id, stackmap_t stackmap,
                                   unsigned stackmapOffset);

    static std::unordered_map<uintptr_t, StatepointRecord> statepoint;

    static std::unordered_map<uint64_t, PatchpointRecord> patchpoint;
};
}

#endif
