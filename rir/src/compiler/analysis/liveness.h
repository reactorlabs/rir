#pragma once

#include "../pir/pir.h"
#include "../util/cfg.h"

#include <unordered_map>
#include <vector>

/*
 * The liveness analysis _does not_ use the static analysis framework. This is
 * intentional:
 *
 *   1. The framework does not handle phis properly in this case.
 *   2. The framework can compute liveness sets, but not liveness intervals.
 *   3. Because we're in SSA, we don't actually need to compute a fixed point.
 *
 * Liveness intervals are stored as:
 *   Instruction* -> BB id -> { Dead | Live [start, end) }
 *
 * An instruction maps to a vector, where each entry represents a BB's liveness
 * interval. `start` is included, `end` is excluded. Liveness is for the
 * position _after_ an instruction. E.g., for the range [0, 2), the variable is
 * live after the first (index 0) and second (index 1) instructions.
 *
 * Because of SSA, definitions are guaranteed to dominate uses. Therefore, the
 * basic idea of the algorithm is:
 *   - run the analysis backwards
 *   - the first use of a variable is the end of the interval
 *   - the definition of a variable is the beginning of the interval
 *   - phi inputs are only propagated into their corresponding predecessors
 *
 * This is roughly Algorithm 4 ("ByUse") and Algorithm 5 in:
 *   Computing Liveness Sets for SSA-Form Programs
 *   https://hal.inria.fr/inria-00558509v1/document
 *
 * That paper also describes other, fancier algorithms, but it may be more
 * difficult to construct the liveness intervals.
 *
 * Another fancy algorithm is:
 *   Fast Liveness Checking for SSA-Form Programs
 *   http://www.rw.cdl.uni-saarland.de/~grund/papers/cgo08-liveness.pdf
 */

namespace rir {
namespace pir {

struct BBLiveness {
    uint8_t live = false;
    unsigned begin = -1;
    unsigned end = -1;
};

class LivenessIntervals {
    std::unordered_map<Value*, std::vector<BBLiveness>> intervals;

  public:
    LivenessIntervals(unsigned bbsSize, CFG const& cfg);
    bool live(Instruction* where, Value* what) const;
    bool interfere(Value* v1, Value* v2) const;
    size_t count(Value* v) const { return intervals.count(v); }
};

} // namespace pir
} // namespace rir
