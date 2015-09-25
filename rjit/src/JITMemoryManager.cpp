#include "JITMemoryManager.h"

namespace rjit {

uint8_t* new_stackmap_addr = nullptr;
uintptr_t new_stackmap_size;

uint64_t JITMemoryManager::getSymbolAddress(const std::string& name) {
    assert(false);
}

uint8_t* JITMemoryManager::allocateDataSection(uintptr_t size,
                                               unsigned alignment,
                                               unsigned sectionID,
                                               llvm::StringRef sectionName,
                                               bool readonly) {

    auto res = SectionMemoryManager::allocateDataSection(
        size, alignment, sectionID, sectionName, readonly);

    if (sectionName.str() == ".llvm_stackmaps") {
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

bool JITMemoryManager::finalizeMemory(std::string* ErrMsg) {
    if (SectionMemoryManager::finalizeMemory(ErrMsg))
        return true;

    for (int i = 0, e = CodeMem.AllocatedMem.size(); i != e; ++i) {
        std::error_code ec = llvm::sys::Memory::protectMappedMemory(
            CodeMem.AllocatedMem[i], llvm::sys::Memory::MF_READ |
                                         sys::Memory::MF_EXEC |
                                         llvm::sys::Memory::MF_WRITE);
        if (ec) {
            if (ErrMsg) {
                *ErrMsg = ec.message();
            }
            return true;
        }
    }

    return false;
}

} // namespace rjit
