#include "utils/TypeFeedbackStrategies.h"
#include "runtime/DispatchTable.h"

namespace rir {
std::vector<Context> MergeSmallerCandidatesMergingStrategy::getCompiledContexts(
    const Context& context) const {
    std::vector<Context> compiledContexts;
    for (size_t i = 1; i < dp_->size(); ++i) {
        auto f = dp_->get(i);
        if (f->context().smaller(context) && !f->disabled() &&
            f->context() != context)
            compiledContexts.push_back(f->context());
    }
    return compiledContexts;
}

}; // namespace rir
