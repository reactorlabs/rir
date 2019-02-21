#include "phi_placement.h"
#include "../pir/pir_impl.h"
#include "../util/cfg.h"

namespace rir {
namespace pir {

BB* PhiPlacement::find(const CFG& cfg, BB* searchBlock,
                       const std::vector<BB*>& inputs) {
    auto hasAllInputs = [&](BB* candidate) {
        for (auto& input : inputs) {
            // we cannot move the phi above its src
            if (input == candidate)
                return false;
            if (!cfg.isPredecessor(input, candidate))
                return false;
        }
        return true;
    };
    assert(hasAllInputs(searchBlock));

    // First move up the phi until at least one input comes from a different
    // block.
    while (true) {
        auto& preds = cfg.immediatePredecessors(searchBlock);
        assert(!preds.empty());

        for (auto pre : preds)
            if (!hasAllInputs(pre))
                goto checkCandidate;

        searchBlock = *preds.begin();
        assert(hasAllInputs(searchBlock));
    }
checkCandidate:

    // Verify that we do not create an ambiguous phi. The problematic case we
    // are looking for here is:
    //
    //    a1     {}
    //     \    /
    //       a2
    //       |
    //      phi(a1, a2)
    //
    // This can happen if the optimizer knows that a2 comes from the {} block,
    // but at the same time the instruction already got delayed into the merge
    // block.
    for (auto& input : inputs)
        if (cfg.isImmediatePredecessor(input, searchBlock))
            for (auto& input2 : inputs)
                if (input != input2)
                    if (cfg.isPredecessor(input2, input))
                        return nullptr;

    return searchBlock;
}

} // namespace pir
} // namespace rir
