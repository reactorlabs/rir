#ifndef RIR_RUNTIME_FEEDBACK
#define RIR_RUNTIME_FEEDBACK

#include "../compiler/pir/value.h"
#include "R/r.h"
#include <array>
#include <cstdint>
#include <unordered_map>

namespace rir {

#pragma pack(push)
#pragma pack(1)

struct ObservedCalles {
    static constexpr unsigned CounterBits = 30;
    static constexpr unsigned CounterOverflow = (1 << CounterBits) - 1;
    static constexpr unsigned TargetBits = 2;
    static constexpr unsigned MaxTargets = (1 << TargetBits) - 1;

    // numTargets is sized such that the largest number it can hold is
    // MaxTargets. If it is set to MaxTargets then the targets array is full. We
    // do not distinguish between the case where we have seen MaxTarget
    // different targets and the case where we have seen more than that.
    // Effectively this means we have seen MaxTargets or more.
    uint32_t numTargets : TargetBits;
    uint32_t taken : CounterBits;

    std::array<SEXP, MaxTargets> targets;

    void record(SEXP callee) {
        if (taken < CounterOverflow)
            taken++;
        if (numTargets < MaxTargets) {
            int i = 0;
            for (; i < numTargets; ++i)
                if (targets[i] == callee)
                    break;
            if (i == numTargets)
                targets[numTargets++] = callee;
        }
    }
};

struct ObservedType {
    uint8_t sexptype : 5;
    uint8_t scalar : 1;
    uint8_t object : 1;
    uint8_t attribs : 1;
    ObservedType() {}
    explicit ObservedType(SEXP s);
    bool operator==(const ObservedType& other) {
        return memcmp(this, &other, sizeof(ObservedType)) == 0;
    }

    bool isObj() const { return object; }
};
static_assert(sizeof(ObservedCalles) == 7 * sizeof(uint32_t),
              "Size needs to fit inside a record_ bc immediate args");

struct ObservedValues {
    static constexpr unsigned MaxTypes = 3;
    uint8_t numTypes;

    std::array<ObservedType, MaxTypes> seen;

    ObservedValues() : numTypes(0) {}

    void record(SEXP e) {
        ObservedType type(e);
        if (numTypes < MaxTypes) {
            int i = 0;
            for (; i < numTypes; ++i)
                if (seen[i] == type)
                    break;
            if (i == numTypes)
                seen[numTypes++] = type;
        }
    }

    bool observedObject() {
        for (const auto& record : seen) {
            if (record.isObj()) {
                return true;
            }
        }
        return false;
    }
};
static_assert(sizeof(ObservedValues) == sizeof(uint32_t),
              "Size needs to fit inside a record_ bc immediate args");

namespace pir {

struct ProfiledValues {
    std::unordered_map<Value*, ObservedCalles>* calles;
    std::unordered_map<Value*, ObservedValues>* types;

    ProfiledValues()
        : calles(new std::unordered_map<Value*, ObservedCalles>()),
          types(new std::unordered_map<Value*, ObservedValues>()){};

    ~ProfiledValues() {
        delete types;
        delete calles;
    }

    bool hasTypesFor(Value* value) {
        return types->count(value) && types->at(value).numTypes;
    }
}; // namespace pir

#pragma pack(pop)

} // namespace pir
} // namespace rir
#endif
