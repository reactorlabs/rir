#include "JITMemoryManager.h"
#include "JITSymbolResolver.h"
#include <iostream>
#include <unordered_map>

namespace rjit {

uint64_t JITMemoryManager::getSymbolAddress(const std::string& name) {
    // For future transition to orcjit we use a symbolResolver for this:
    return JITSymbolResolver::singleton.findSymbol(name).getAddress();
}

// This is mostly a 1:1 copy from DynldMemoryManager which additionally exports
// stackmapAddr
// TODO: maybe find a better allocation strategy, which keeps data together?
uint8_t* JITMemoryManager::allocateDataSection(uintptr_t size,
                                               unsigned alignment,
                                               unsigned sectionID,
                                               llvm::StringRef sectionName,
                                               bool readonly) {

    uint8_t* res;
    if (readonly)
        res = allocateSection(RODataMem, size, alignment);
    else
        res = allocateSection(RWDataMem, size, alignment);

    if (sectionName.str() == ".llvm_stackmaps" ||
        sectionName.str() == "__llvm_stackmaps") {
        stackmapAddr_ = res;
        stackmapSize_ = size;
    }

    return res;
}

uint8_t* JITMemoryManager::allocateSection(MemoryGroup& MemGroup,
                                           uintptr_t Size, unsigned Alignment) {

    if (!Alignment)
        Alignment = 16;

    assert(!(Alignment & (Alignment - 1)) &&
           "Alignment must be a power of two.");

    uintptr_t RequiredSize =
        Alignment * ((Size + Alignment - 1) / Alignment + 1);
    uintptr_t Addr = 0;

    // Look in the list of free memory regions and use a block there if one
    // is available.
    for (int i = 0, e = MemGroup.FreeMem.size(); i != e; ++i) {
        llvm::sys::MemoryBlock& MB = MemGroup.FreeMem[i];
        if (MB.size() >= RequiredSize) {
            Addr = (uintptr_t)MB.base();
            uintptr_t EndOfBlock = Addr + MB.size();
            // Align the address.
            Addr = (Addr + Alignment - 1) & ~(uintptr_t)(Alignment - 1);
            // Store cutted free memory block.
            MemGroup.FreeMem[i] = llvm::sys::MemoryBlock(
                (void*)(Addr + Size), EndOfBlock - Addr - Size);
            return (uint8_t*)Addr;
        }
    }

    // No pre-allocated free block was large enough. Allocate a new memory
    // region.
    // Note that all sections get allocated as read-write.  The permissions will
    // be updated later based on memory group.
    //
    // FIXME: It would be useful to define a default allocation size (or add
    // it as a constructor parameter) to minimize the number of allocations.
    //
    // FIXME: Initialize the Near member for each memory group to avoid
    // interleaving.
    std::error_code ec;
    llvm::sys::MemoryBlock MB = sys::Memory::allocateMappedMemory(
        RequiredSize, &MemGroup.Near,
        sys::Memory::MF_READ | sys::Memory::MF_WRITE, ec);
    if (ec) {
        // FIXME: Add error propagation to the interface.
        return nullptr;
    }

    // Save this address as the basis for our next request
    MemGroup.Near = MB;

    MemGroup.AllocatedMem.push_back(MB);
    Addr = (uintptr_t)MB.base();
    uintptr_t EndOfBlock = Addr + MB.size();

    // Align the address.
    Addr = (Addr + Alignment - 1) & ~(uintptr_t)(Alignment - 1);

    // The allocateMappedMemory may allocate much more memory than we need. In
    // this case, we store the unused memory as a free memory block.
    unsigned FreeSize = EndOfBlock - Addr - Size;
    if (FreeSize > 16)
        MemGroup.FreeMem.push_back(
            sys::MemoryBlock((void*)(Addr + Size), FreeSize));

    // Return aligned address
    return (uint8_t*)Addr;
}

std::error_code
JITMemoryManager::applyMemoryGroupPermissions(MemoryGroup& MemGroup,
                                              unsigned Permissions) {

    for (int i = 0, e = MemGroup.AllocatedMem.size(); i != e; ++i) {
        std::error_code ec;
        ec = sys::Memory::protectMappedMemory(MemGroup.AllocatedMem[i],
                                              Permissions);
        if (ec) {
            return ec;
        }
    }

    return std::error_code();
}

bool JITMemoryManager::finalizeMemory(std::string* ErrMsg) {
    // FIXME: Should in-progress permissions be reverted if an error occurs?
    std::error_code ec;

    // Don't allow free memory blocks to be used after setting protection flags.
    CodeMem.FreeMem.clear();

    // Make code memory executable.
    ec = applyMemoryGroupPermissions(CodeMem, sys::Memory::MF_WRITE |
                                                  sys::Memory::MF_READ |
                                                  sys::Memory::MF_EXEC);
    if (ec) {
        if (ErrMsg) {
            *ErrMsg = ec.message();
        }
        return true;
    }

    // Don't allow free memory blocks to be used after setting protection flags.
    RODataMem.FreeMem.clear();

    // Make read-only data memory read-only.
    ec = applyMemoryGroupPermissions(RODataMem, sys::Memory::MF_READ |
                                                    sys::Memory::MF_EXEC);
    if (ec) {
        if (ErrMsg) {
            *ErrMsg = ec.message();
        }
        return true;
    }

    // Read-write data memory already has the correct permissions

    // Some platforms with separate data cache and instruction cache require
    // explicit cache flush, otherwise JIT code manipulations (like resolved
    // relocations) will get to the data cache but not to the instruction cache.
    invalidateInstructionCache();

    return false;
}

} // namespace rjit
