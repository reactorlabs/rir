#include "deoptimizer.h"

static DeoptInfo* deoptTable;
static uint32_t* deoptTableOffset;
static size_t deoptTableSize = 0;
static size_t deoptTableEntries = 0;
static size_t deoptTableSizeUsed = 0;
static size_t deoptTableEntriesUsed = 0;
static size_t deoptTableGrow = 10;

static DeoptInfo* DeoptInfo_alloc() {
    size_t needed = sizeof(DeoptInfo);
    if (deoptTableSizeUsed + needed >= deoptTableSize) {
        size_t newDeoptTableSize = deoptTableSize + deoptTableGrow * needed;
        DeoptInfo* newDeoptTable = (DeoptInfo*)malloc(newDeoptTableSize);
        assert(newDeoptTable);
        memcpy(newDeoptTable, deoptTable, deoptTableSize);
        memset((char*)newDeoptTable + deoptTableSize, 0,
               newDeoptTableSize - deoptTableSize);
        deoptTable = newDeoptTable;
        deoptTableSize = newDeoptTableSize;
    }
    DeoptInfo* i = (DeoptInfo*)((char*)deoptTable + deoptTableSizeUsed);
    i->size = 0;
    deoptTableSizeUsed += needed;
    return i;
}

uint32_t Deoptimizer_register(OpcodeT* oldPc) {
    DeoptInfo* i = DeoptInfo_alloc();
    i->oldPc = oldPc;

    if (deoptTableEntriesUsed == deoptTableEntries) {
        size_t newDeoptTableEntries = deoptTableEntries + deoptTableGrow;
        uint32_t* newDeoptTableOffset =
            (uint32_t*)malloc(sizeof(uint32_t) * newDeoptTableEntries);
        assert(newDeoptTableOffset);
        memcpy(newDeoptTableOffset, deoptTableOffset,
               sizeof(uint32_t) * deoptTableEntries);
        memset(newDeoptTableOffset + deoptTableEntries, 0,
               sizeof(uint32_t) * (newDeoptTableEntries - deoptTableEntries));
        deoptTableEntries = newDeoptTableEntries;
        deoptTableOffset = newDeoptTableOffset;
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

void Deoptimizer_print(uint32_t idx) {
    assert(idx != NO_DEOPT_INFO);
    DeoptInfo* i = DeoptInfo_get(idx);
    Rprintf("[d#%d: %p]", idx, i->oldPc);
}

OpcodeT* Deoptimizer_pc(uint32_t idx) {
    assert(idx != NO_DEOPT_INFO);
    DeoptInfo* i = DeoptInfo_get(idx);
    return i->oldPc;
}
