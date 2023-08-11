#ifndef RIR_RIR_REGISTER_MAP_H
#define RIR_RIR_REGISTER_MAP_H

#include "RirRuntimeObject.h"
#include "compiler/pir/type.h"
#include "runtime/TypeFeedback.h"
#include "serializeHash/hash/getConnected.h"
#include "serializeHash/hash/hashRoot.h"
#include "serializeHash/serializeUni.h"

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
struct CallFeedback;
} // namespace pir

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

    FeedbackIndex rirIdx(size_t slot);

    static size_t requiredSize(size_t origins, size_t entries) {
        return sizeof(PirTypeFeedback) + sizeof(SEXP) * origins +
               sizeof(MDEntry) * entries;
    }

    struct MDEntry {
        uint8_t funIdx;
        FeedbackIndex rirIdx;
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

    static PirTypeFeedback* deserializeR(SEXP refTable, R_inpstream_t inp);
    void serializeR(SEXP refTable, R_outpstream_t out) const;
    static PirTypeFeedback* deserialize(AbstractDeserializer& deserializer);
    void serialize(AbstractSerializer& deserializer) const;
    void hash(Hasher& hasher) const;
    void addConnected(ConnectedCollector& collector) const;

  private:
    explicit PirTypeFeedback(int numCodes)
        : RirRuntimeObject(sizeof(*this), numCodes),
          entry() {}

    MDEntry& getMDEntryOfSlot(size_t slot) {
        assert(slot < MAX_SLOT_IDX);
        auto idx = entry[slot];
        assert(idx != MAX_SLOT_IDX);
        return mdEntries()[idx];
    }

    MDEntry* mdEntries() const {
        return reinterpret_cast<MDEntry*>((uintptr_t)this + info.gc_area_start +
                                          sizeof(SEXP) * info.gc_area_length);
    }

    int numCodes() const;
    int numEntries() const;
    size_t size() const;

    uint8_t entry[MAX_SLOT_IDX];
};

#pragma pack(pop)
} // namespace rir

#endif
