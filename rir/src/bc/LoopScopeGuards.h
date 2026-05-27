#ifndef RIR_BC_LOOP_SCOPE_GUARDS_H
#define RIR_BC_LOOP_SCOPE_GUARDS_H

#include "bc/CodeContext.h"

namespace rir {

// RAII guard for the range-based iter var optimization: records the type of `i`
// once per call per outer-loop iteration. A clear placeholder is emitted before
// the loop (in the enclosing scope) so that on the next outer iteration `i` can
// re-record if the range type changed. Only used for range-based for-loops.
struct RangeBasedIterVarScope {
    CodeContext* ctx_;
    bool active_ = false;
    bool nested_ = false;
    unsigned placeholderPos_ = 0;

    RangeBasedIterVarScope(CodeContext* ctx, SEXP sym, bool active)
        : ctx_(ctx), active_(active) {
        if (!active_)
            return;
        nested_ = ctx_->defUseAnalysis.loopDepth() > 0;
        if (nested_) {
            placeholderPos_ = ctx_->cs.currentPos();
            ctx_->cs << BC::clearRecordTypeOnceBitsRange(0, 0);
        }
        ctx_->defUseAnalysis.pushRangeBasedForLoopVar(sym, nested_,
                                                      placeholderPos_);
    }

    void finish() {
        if (!active_)
            return;
        ctx_->defUseAnalysis.moveRangeVarToPending();
        if (ctx_->defUseAnalysis.rangeVarAssignmentReady())
            assignBits();
    }

    void assignBits() {
        auto& pending = ctx_->defUseAnalysis.pendingRangeVarEntries_;
        unsigned& bitmapSize = ctx_->recordTypeOnceBitmapSize;
        for (auto it = pending.rbegin(); it != pending.rend(); ++it) {
            auto& e = *it;
            unsigned base = bitmapSize;
            int count = e.pendingCount;
            for (int i = 0; i < count; ++i)
                ctx_->cs.patchImmediate(
                    e.useSites[i].pos,
                    RECORD_TYPE_ONCE_PACK(e.useSites[i].slot, base + i));
            if (e.nested) {
                if (count > 0)
                    ctx_->cs.patchImmediate(
                        e.clearTemplatePos,
                        RECORD_TYPE_ONCE_RANGE_PACK(base, count));
                else
                    ctx_->cs.remove(e.clearTemplatePos);
            }
            bitmapSize += count;
        }
        pending.clear();
    }
};

// RAII guard for the clearable scope mechanism: user-defined variables assigned
// in an outer loop and used inside this loop need their record_type_once_ bits
// cleared before each run of this loop. A clear placeholder is emitted before
// the loop (in the enclosing scope) and patched with the actual bit range when
// the loop scope is popped. Used for all loop kinds when nested.
struct ClearableScopeGuard {
    CodeContext* ctx_;
    bool active_ = false;

    ClearableScopeGuard(CodeContext* ctx, bool active) : ctx_(ctx) {
        active_ = active && ctx_->defUseAnalysis.loopDepth() > 0;
        if (active_) {
            unsigned placeholderPos = ctx_->cs.currentPos();
            ctx_->cs << BC::clearRecordTypeOnceBitsRange(0, 0);
            ctx_->defUseAnalysis.pushClearableScope(placeholderPos);
        }
    }

    void finish() {
        if (!active_)
            return;
        auto entry = ctx_->defUseAnalysis.popClearableScope();
        int count = (int)entry.useSites.size();
        unsigned& bitmapSize = ctx_->recordTypeOnceBitmapSize;
        unsigned base = bitmapSize;
        for (int i = 0; i < count; ++i)
            ctx_->cs.patchImmediate(
                entry.useSites[i].pos,
                RECORD_TYPE_ONCE_PACK(entry.useSites[i].slot, base + i));
        if (count > 0) {
            ctx_->cs.patchImmediate(entry.clearTemplatePos,
                                    RECORD_TYPE_ONCE_RANGE_PACK(base, count));
            bitmapSize += count;
        } else {
            ctx_->cs.remove(entry.clearTemplatePos);
        }
    }
};

} // namespace rir

#endif // RIR_BC_LOOP_SCOPE_GUARDS_H
