#ifndef RIR_TYPEFEEDBACKSTRATEGIES_H
#define RIR_TYPEFEEDBACKSTRATEGIES_H

#include "runtime/Context.h"
#include "runtime/GenericDispatchTable.h"
#include "runtime/TypeFeedback.h"

namespace rir {
static constexpr unsigned MaxFeedbacks = 64;
typedef GenericDispatchTable<Context, TypeFeedback, MaxFeedbacks>
    TypeFeedbackDispatchTable;

struct MergingStrategy {
    virtual TypeFeedback* merge(const Context& ctx, Function* function,
                                std::pair<const Context&, TypeFeedback*> e,
                                const TypeFeedbackDispatchTable* tfdp) = 0;
};

struct NoMergingStrategy : public MergingStrategy {
    TypeFeedback* merge(const Context& ctx, Function* function,
                        std::pair<const Context&, TypeFeedback*> e,
                        const TypeFeedbackDispatchTable* tfdp) override {
        return e.second;
    }
};

struct MergeAllSmallerMergingStrategy : public MergingStrategy {
    TypeFeedback* merge(const Context& ctx, Function* function,
                        std::pair<const Context&, TypeFeedback*> e,
                        const TypeFeedbackDispatchTable* tfdp) override {
        auto mergeCondition = [&](const Context& entryCtx) {
            return entryCtx.smaller(ctx) && entryCtx != e.first;
        };
        auto mergeImplementation = [&](const TypeFeedback* feedback) {
            e.second->mergeWith(feedback, function);
        };
        tfdp->filterForeach(mergeCondition, mergeImplementation);
        return e.second;
    }
};

struct DispatchTable;

// Merges only contexts that will dispatch to this compiled version
struct MergeSmallerCandidatesMergingStrategy : public MergingStrategy {
    explicit MergeSmallerCandidatesMergingStrategy(const DispatchTable* dp)
        : dp_(dp) {}
    TypeFeedback* merge(const Context& ctx, Function* function,
                        std::pair<const Context&, TypeFeedback*> e,
                        const TypeFeedbackDispatchTable* tfdp) override {
        auto tf = e.second;
        auto compiledContexts = getCompiledContexts(ctx);
        auto mergeCondition = [&](const Context& entryCtx) {
            for (Context& c : compiledContexts)
                if (entryCtx.smaller(c))
                    return false;
            return entryCtx.smaller(ctx) && entryCtx != e.first;
        };
        auto mergeImplementation = [&](const TypeFeedback* feedback) {
            tf->mergeWith(feedback, function);
        };
        tfdp->filterForeach(mergeCondition, mergeImplementation);
        return tf;
    }
    std::vector<Context> getCompiledContexts(const Context& context) const;

  protected:
    const DispatchTable* dp_;
};

struct FillingStrategy {
    virtual TypeFeedback* fill(const Context& ctx, Function* function,
                               std::pair<const Context&, TypeFeedback*> e,
                               const TypeFeedbackDispatchTable* tfdp) = 0;
};

struct NoFillingStrategy : public FillingStrategy {
    virtual TypeFeedback* fill(const Context& ctx, Function* function,
                               std::pair<const Context&, TypeFeedback*> e,
                               const TypeFeedbackDispatchTable* tfdp) {
        return e.second;
    }
};

struct TopFeedbackFillingStrategy : public FillingStrategy {
    virtual TypeFeedback* fill(const Context& ctx, Function* function,
                               std::pair<const Context&, TypeFeedback*> e,
                               const TypeFeedbackDispatchTable* tfdp) {
        e.second->fillWith(tfdp->last().second);
        return e.second;
    }
};

struct TraversalFillingStrategy : public FillingStrategy {
    virtual TypeFeedback* fill(const Context& ctx, Function* function,
                               std::pair<const Context&, TypeFeedback*> e,
                               const TypeFeedbackDispatchTable* tfdp) {
        auto fillCond = [&](const Context& c) { return e.first.smaller(c); };
        auto fillImpl = [&](const TypeFeedback* tf) { e.second->fillWith(tf); };
        tfdp->filterForeach(fillCond, fillImpl);
        return e.second;
    }
};

}; // namespace rir

#endif // RIR_TYPEFEEDBACKSTRATEGIES_H
