#ifndef RIR_COMPILER_CFG_H
#define RIR_COMPILER_CFG_H

#include "R/r.h"
#include "bc/BC.h"
#include <map>
#include <unordered_set>
#include <vector>

namespace rir {

class Code;

// ============================================================================
// Scope-based recording tracker for parameters
// ============================================================================

class CompilerCFGBuilder {
  public:
    struct RecordingPoint {
        SEXP var; // Variable being recorded (nullptr for non-parameter)
        uint32_t slot_idx;         // Feedback slot index
        std::vector<size_t> scope; // Scope stack at time of recording (for
                                   // nesting level tracking)

        RecordingPoint() : var(nullptr), slot_idx(UINT32_MAX) {}
    };

  private:
    // Track recording points in order of emission
    std::vector<RecordingPoint> recording_points_;

    // Parameters extracted from formals
    std::unordered_set<SEXP> parameters_;

    // Parameters excluded from optimization (assigned or shadowed by inner
    // functions)
    std::unordered_set<SEXP> excluded_parameters_;

    // Scope tracking for nesting level analysis
    std::vector<size_t> scope_stack_; // Current scope path during compilation
    size_t next_scope_id_;            // Next scope ID to assign

  public:
    CompilerCFGBuilder() : next_scope_id_(1) {
        // Entry scope is always scope 0
        scope_stack_.push_back(0);
    }

    // ========================================================================
    // Called by compiler during bytecode emission
    // ========================================================================

    // Called when emitting record_type_ instruction
    void onRecordType(SEXP var, uint32_t slot_idx);

    // Mark a parameter as excluded from optimization
    void markParameterExcluded(SEXP var);

    // Scope management (for tracking nesting levels)
    void pushScope(); // Enter a new scope (conditional branch)
    void popScope();  // Exit a scope (merge point)

    // ========================================================================
    // Setup and finalization
    // ========================================================================

    // Set parameters from function formals
    void setParameters(const std::unordered_set<SEXP>& params) {
        parameters_ = params;
    }

    // Finalize after all bytecode is emitted
    void finalize() {
        // Nothing needed for now
    }

    // ========================================================================
    // Analysis results
    // ========================================================================

    enum class RecordingStrategy {
        RECORD, // Always record
        SKIP    // Never record, copy from another slot
    };

    struct RecordingDecision {
        uint32_t slot_idx;
        RecordingStrategy strategy;
        uint32_t
            copy_from_slot; // if SKIP, copy from here (UINT32_MAX if RECORD)
        SEXP var;           // associated variable
    };

    std::vector<RecordingDecision> computeMinimalRecordings();

    // Get statistics
    struct Stats {
        size_t num_parameters;
        size_t total_recordings;
        size_t parameter_recordings;
        size_t skipped_recordings;
    };
    Stats getStats() const;

    // Debug printing
    void printDecisions(const std::vector<RecordingDecision>& decisions,
                        std::ostream& out, SEXP formals, SEXP bodyAst,
                        Function* function) const;
};

} // namespace rir

#endif
