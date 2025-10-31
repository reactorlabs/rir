#ifndef REPORT_SUBSUMED_H
#define REPORT_SUBSUMED_H

#include <string>
#include <unordered_map>

namespace rir {
namespace report {

struct SlotSubsumer {
    std::string name;
    uint32_t idx;
};

using SubsumedSlots = std::unordered_map<uint32_t, SlotSubsumer>;

} // namespace report
} // namespace rir

#endif
