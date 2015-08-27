#ifndef STACK_MAP_H
#define STACK_MAP_H

#include <llvm/IR/Module.h>
#include <llvm/ADT/ArrayRef.h>
#include "stack_map_parser.h"

#include <unordered_map>
#include <iostream>

#include <R.h>
#include <Rdefines.h>

typedef llvm::ArrayRef<uint8_t> stackmap_t;

// TODO: find target endianness
typedef llvm::StackMapV1Parser<llvm::support::little> StackMapParserT;

static unsigned statepointID = 0xABCDEF00;

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

    // Statepoints are identified by their pc address as seen on the runtime
    // stack while scanning for roots.
    static void registerStatepoint(uintptr_t function,
                                   uintptr_t offset,
                                   stackmap_t stackmap,
                                   unsigned stackmapOffset) {
        statepoint.emplace(function + offset,
                           StatepointRecord({
                               stackmap,
                               stackmapOffset,
                               function}));
    }

    // Patchpoints are identified by their unique id given at compile time
    static void registerPatchpoint(uintptr_t id,
                                   stackmap_t stackmap,
                                   unsigned stackmapOffset) {
        patchpoint.emplace(id, PatchpointRecord({stackmap, stackmapOffset}));
    }

    static bool isStatepoint(uintptr_t pc) {
        return statepoint.count(pc);
    }

    static StackMapParserT::RecordAccessor getStatepoint(uintptr_t pc) {
        assert(statepoint.count(pc));
        
        StatepointRecord & e = statepoint.at(pc);
        StackMapParserT p(e.stackmap);
        const auto &r = p.getRecord(e.idx);
        return r;
    }

    static void stackScanner(void (*forward_node)(SEXP));

    static StackMapParserT::RecordAccessor getPatchpoint(uint64_t id) {
        assert(patchpoint.count(id));
        
        PatchpointRecord & e = patchpoint.at(id);
        StackMapParserT p(e.stackmap);
        const auto &r = p.getRecord(e.idx);
        assert(r.getID() != statepointID);
        assert(r.getID() == id);
        return r;
    }

private:
    static std::unordered_map<uintptr_t, StatepointRecord> statepoint;
    
    static std::unordered_map<uint64_t, PatchpointRecord> patchpoint;
};

#endif
