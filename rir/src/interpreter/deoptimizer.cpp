#include "deoptimizer.h"

static DeoptInfo* deoptTable;
static uint32_t* deoptTableOffset;
static size_t deoptTableSize = 0;
static size_t deoptTableEntries = 0;
static size_t deoptTableSizeUsed = 0;
static size_t deoptTableEntriesUsed = 0;
static size_t deoptTableGrow = 10;

using namespace rir;

static DeoptInfo* DeoptInfo_alloc() {
    size_t needed = sizeof(DeoptInfo);
    if (deoptTableSizeUsed + needed >= deoptTableSize) {
        size_t newDeoptTableSize = deoptTableSize + deoptTableGrow * needed;
        DeoptInfo* newDeoptTable = (DeoptInfo*)malloc(newDeoptTableSize);
        assert(newDeoptTable);
        if (deoptTableSize)
            memcpy(newDeoptTable, deoptTable, deoptTableSize);
        memset((char*)newDeoptTable + deoptTableSize, 0,
               newDeoptTableSize - deoptTableSize);
        if (deoptTableSize > 0)
            free(deoptTable);
        deoptTable = newDeoptTable;
        deoptTableSize = newDeoptTableSize;
    }
    DeoptInfo* i = (DeoptInfo*)((char*)deoptTable + deoptTableSizeUsed);
    i->size = 0;
    deoptTableSizeUsed += needed;
    return i;
}

uint32_t Deoptimizer_register(Opcode* oldPc) {
    DeoptInfo* i = DeoptInfo_alloc();
    i->oldPc = oldPc;

    if (deoptTableEntriesUsed == deoptTableEntries) {
        size_t newDeoptTableEntries = deoptTableEntries + deoptTableGrow;
        uint32_t* newDeoptTableOffset =
            (uint32_t*)malloc(sizeof(uint32_t) * newDeoptTableEntries);
        assert(newDeoptTableOffset);
        if (deoptTableEntries > 0)
            memcpy(newDeoptTableOffset, deoptTableOffset,
                   sizeof(uint32_t) * deoptTableEntries);
        memset(newDeoptTableOffset + deoptTableEntries, 0,
               sizeof(uint32_t) * (newDeoptTableEntries - deoptTableEntries));
        if (deoptTableEntries > 0)
            free(deoptTableOffset);
        deoptTableOffset = newDeoptTableOffset;
        deoptTableEntries = newDeoptTableEntries;
    }
    uint32_t off = (char*)i - (char*)deoptTable;
    uint32_t idx = deoptTableEntriesUsed++;
    deoptTableOffset[idx] = off;
    return idx;
}

static DeoptInfo* DeoptInfo_get(uint32_t idx) {
    uint32_t off = deoptTableOffset[idx];
    return (DeoptInfo*)((char*)deoptTable + off);
}

void Deoptimizer_print(uint32_t idx, std::ostream& out) {
    assert(idx != NO_DEOPT_INFO);
    DeoptInfo* i = DeoptInfo_get(idx);
    out << "[d#" << idx << ": " << static_cast<void*>(i->oldPc) << "]";
}

Opcode* Deoptimizer_pc(uint32_t idx) {
    assert(idx != NO_DEOPT_INFO);
    DeoptInfo* i = DeoptInfo_get(idx);
    return i->oldPc;
}
