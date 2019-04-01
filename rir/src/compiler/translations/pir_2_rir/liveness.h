#pragma once

#include "../../pir/pir.h"
#include "../../util/cfg.h"

#include <unordered_map>
#include <vector>

namespace rir {
namespace pir {

struct BBLiveness {
    uint8_t live = false;
    unsigned begin = -1;
    unsigned end = -1;
};

struct Liveness : public std::vector<BBLiveness> {
    bool interfere(const Liveness& other) const {
        assert(size() == other.size());
        for (size_t i = 0; i < size(); ++i) {
            const BBLiveness& mine = (*this)[i];
            const BBLiveness& their = other[i];
            if (mine.live && their.live) {
                if (mine.begin == their.begin ||
                    (mine.begin < their.begin && mine.end >= their.begin) ||
                    (mine.begin > their.begin && their.end >= mine.begin))
                    return true;
            }
        }
        return false;
    }
};

struct LivenessIntervals : public std::unordered_map<Value*, Liveness> {
    LivenessIntervals(unsigned bbsSize, CFG const& cfg);
    bool live(Instruction* where, Value* what) const;
};

} // namespace pir
} // namespace rir
