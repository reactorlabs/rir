#include "CompilerCFG.h"
#include "runtime/Code.h"
#include <iostream>
#include <sstream>

namespace rir {

// ============================================================================
// Recording tracking during compilation
// ============================================================================

void CompilerCFGBuilder::onRecordType(SEXP var, uint32_t slot_idx) {
    RecordingPoint point;
    point.var = var;
    point.slot_idx = slot_idx;
    point.scope = scope_stack_; // Capture current scope

    recording_points_.push_back(point);
}

void CompilerCFGBuilder::markParameterExcluded(SEXP var) {
    if (parameters_.count(var)) {
        excluded_parameters_.insert(var);
    }
}

void CompilerCFGBuilder::pushScope() {
    // Enter a new scope (e.g., inside a conditional branch)
    scope_stack_.push_back(next_scope_id_++);
}

void CompilerCFGBuilder::popScope() {
    // Exit a scope (e.g., merge point after conditional)
    if (scope_stack_.size() > 1) {
        scope_stack_.pop_back();
    }
}

// ============================================================================
// Minimal Recording Analysis
// ============================================================================

std::vector<CompilerCFGBuilder::RecordingDecision>
CompilerCFGBuilder::computeMinimalRecordings() {
    std::vector<RecordingDecision> decisions;
    decisions.resize(recording_points_.size());

    // Track recording decisions: parameter -> scope -> first RECORD slot idx
    // We only track RECORD decisions (not SKIP), so we can always find the
    // source
    std::unordered_map<SEXP, std::map<std::vector<size_t>, uint32_t>>
        param_scope_records;

    // Process each recording point
    for (size_t i = 0; i < recording_points_.size(); i++) {
        const auto& point = recording_points_[i];
        RecordingDecision& decision = decisions[i];
        decision.slot_idx = point.slot_idx;
        decision.var = point.var;
        decision.strategy = RecordingStrategy::RECORD;
        decision.copy_from_slot = UINT32_MAX;

        // Only optimize parameters that are not excluded (assigned or shadowed)
        if (!point.var || !parameters_.count(point.var) ||
            excluded_parameters_.count(point.var)) {
            decision.strategy = RecordingStrategy::RECORD;
            continue;
        }

        SEXP param = point.var;
        const auto& current_scope = point.scope;

        // Search for a RECORD in the EXACT same scope only
        uint32_t source_slot = UINT32_MAX;

        if (param_scope_records.count(param) &&
            param_scope_records[param].count(current_scope)) {
            source_slot = param_scope_records[param][current_scope];
        }

        if (source_slot != UINT32_MAX) {
            // Found a source RECORD - SKIP and copy from it
            decision.strategy = RecordingStrategy::SKIP;
            decision.copy_from_slot = source_slot;
        } else {
            // No source found - this is the first RECORD for this scope
            decision.strategy = RecordingStrategy::RECORD;
            param_scope_records[param][current_scope] = point.slot_idx;
        }
    }

    return decisions;
}

// ============================================================================
// Statistics and Debugging
// ============================================================================

CompilerCFGBuilder::Stats CompilerCFGBuilder::getStats() const {
    Stats stats;
    stats.num_parameters = parameters_.size();
    stats.total_recordings = recording_points_.size();

    size_t param_count = 0;
    for (const auto& point : recording_points_) {
        if (point.var && parameters_.count(point.var)) {
            param_count++;
        }
    }
    stats.parameter_recordings = param_count;
    stats.skipped_recordings = 0; // Will be computed from decisions

    return stats;
}

void CompilerCFGBuilder::printDecisions(
    const std::vector<RecordingDecision>& decisions, std::ostream& out,
    SEXP formals, SEXP bodyAst, Function* function) const {
    out << "\n========== Recording Analysis ==========\n";

    // Print the function formals
    out << "Function formals:\n";
    Rf_PrintValue(formals);

    // Print the function body AST
    out << "Function body (AST):\n";
    Rf_PrintValue(bodyAst);
    out << "\n";

    // Print the RIR bytecode
    out << "RIR bytecode:\n";
    function->disassemble(out);
    out << "\n";

    // Print parameters
    out << "Parameters: " << parameters_.size() << " { ";
    for (SEXP param : parameters_) {
        out << CHAR(PRINTNAME(param)) << " ";
    }
    out << "}\n";

    // Print excluded parameters
    if (!excluded_parameters_.empty()) {
        out << "Excluded parameters (assigned or shadowed): { ";
        for (SEXP param : excluded_parameters_) {
            out << CHAR(PRINTNAME(param)) << " ";
        }
        out << "}\n";
    }
    out << "\n";

    // Print decisions (only for parameters)
    out << "=== Recording Decisions (parameters only) ===\n";
    size_t record_count = 0;
    size_t skip_count = 0;

    for (size_t i = 0; i < decisions.size(); i++) {
        const auto& decision = decisions[i];
        const auto& point = recording_points_[i];

        // Only show decisions for parameters
        if (!decision.var || !parameters_.count(decision.var)) {
            continue;
        }

        out << "Slot " << decision.slot_idx << ": ";

        // Print variable name
        out << CHAR(PRINTNAME(decision.var));

        // Print scope
        out << " [scope:";
        for (size_t scope_id : point.scope) {
            out << " " << scope_id;
        }
        out << "]";

        // Print decision
        if (decision.strategy == RecordingStrategy::RECORD) {
            out << " RECORD";
            record_count++;
        } else {
            out << " SKIP (copy from slot " << decision.copy_from_slot << ")";
            skip_count++;
        }

        // Print excluded status
        if (excluded_parameters_.count(decision.var)) {
            out << " [excluded]";
        }

        out << "\n";
    }

    // Print summary
    out << "\n=== Statistics ===\n";
    out << "Total recordings: " << decisions.size() << "\n";
    out << "RECORD: " << record_count << "\n";
    out << "SKIP: " << skip_count << "\n";
    if (decisions.size() > 0) {
        double reduction = 100.0 * skip_count / decisions.size();
        out << "Optimization: " << reduction << "% reduction\n";
    }
    out << "========================================\n\n";
}

} // namespace rir
