#ifndef RIR_RIR_REGISTER_MAP_H
#define RIR_RIR_REGISTER_MAP_H

#include "RirRuntimeObject.h"
#include "compiler/pir/type.h"
#include "runtime/TypeFeedback.h"

#include <iostream>
#include <unordered_map>
#include <unordered_set>

namespace rir {

#pragma pack(push)
#pragma pack(1)

constexpr static size_t PIR_TYPE_FEEDBACK_MAGIC = 0x31573;

struct Code;

namespace pir {
struct TypeFeedback;
}

struct PirTypeFeedback
    : public RirRuntimeObject<PirTypeFeedback, PIR_TYPE_FEEDBACK_MAGIC> {
  public:
    constexpr static size_t MAX_SLOT_IDX = 64;

    static PirTypeFeedback*
    New(const std::unordered_set<Code*>& origins,
        const std::unordered_map<size_t, const pir::TypeFeedback&>& slots) {
        SEXP cont = Rf_allocVector(EXTERNALSXP,
                                   requiredSize(origins.size(), slots.size()));
        PirTypeFeedback* res =
            new (DATAPTR(cont)) PirTypeFeedback(origins, slots);
        return res;
    }

    PirTypeFeedback(
        const std::unordered_set<Code*>& codes,
        const std::unordered_map<size_t, const pir::TypeFeedback&>& slots);

    ObservedValues& getSampleOfSlot(size_t slot) {
        return getMDEntryOfSlot(slot).feedback;
    }
    unsigned getBCOffsetOfSlot(size_t slot) {
        return getMDEntryOfSlot(slot).offset;
    }
    Code* getSrcCodeOfSlot(size_t slot);
    Opcode* getOriginOfSlot(size_t slot);

    static size_t requiredSize(size_t origins, size_t entries) {
        return sizeof(PirTypeFeedback) + sizeof(SEXP) * origins +
               sizeof(MDEntry) * entries;
    }

    struct MDEntry {
        uint8_t srcCode;
        unsigned offset;
        ObservedValues feedback;
        pir::PirType previousType;
        unsigned sampleCount = 0;
        bool readyForReopt = false;
        bool needReopt = false;
    };

    void
    forEachSlot(const std::function<void(size_t, MDEntry&)>& iterationBody) {
        for (size_t id = 0; id < MAX_SLOT_IDX; id++) {
            if (entry[id] < MAX_SLOT_IDX) {
                iterationBody(id, getMDEntryOfSlot(id));
            }
        }
    }

  private:
    MDEntry& getMDEntryOfSlot(size_t slot) {
        assert(slot < MAX_SLOT_IDX);
        auto idx = entry[slot];
        assert(idx != MAX_SLOT_IDX);
        return mdEntries()[idx];
    }

    MDEntry* mdEntries() {
        return reinterpret_cast<MDEntry*>((uintptr_t)this + info.gc_area_start +
                                          sizeof(SEXP) * info.gc_area_length);
    }

    uint8_t entry[MAX_SLOT_IDX];
};

#pragma pack(pop)
} // namespace rir

#endif
