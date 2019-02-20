#include "stack_use.h"
#include "../../pir/pir_impl.h"
#include "ir/BC_inc.h"

namespace rir {
namespace pir {

void StackUseAnalysisState::AbstractStack::push(Value* v) {
    for (auto have : *this)
        assert(have != v && "Stack already contains the value to be pushed.");
    push_back(v);
}

void StackUseAnalysisState::AbstractStack::eraseValue(Value* v) {
    for (auto it = rbegin(); it != rend(); ++it) {
        if (*it == v) {
            erase(std::next(it).base());
            return;
        }
    }
    assert(false && "Stack doesn't contain the value to be erased.");
}

void StackUseAnalysisState::AbstractStack::erasePhiInput(Phi* phi) {
    for (auto it = rbegin(); it != rend(); ++it) {
        bool found = false;
        phi->eachArg([&](BB*, Value* in) {
            if (*it == in)
                found = true;
        });
        if (found) {
            erase(std::next(it).base());
            return;
        }
    }
    assert(false && "Stack doesn't contain any input for this phi.");
}

void StackUseAnalysisState::AbstractStack::matchContents(
    AbstractStack const& other) const {
    assert(size() == other.size() && "The stacks better have the same height.");

    // Check that the stacks have the same contents, or the differences all
    // relate to phi inputs
    for (size_t i = 0; i < size(); ++i) {
        if (operator[](i) != other.operator[](i)) {
            if (operator[](i)->isInstruction() &&
                other.operator[](i)->isInstruction()) {
                auto my = Instruction::Cast(operator[](i))->hasSingleUse();
                auto their =
                    Instruction::Cast(other.operator[](i))->hasSingleUse();
                assert(my && Phi::Cast(my));
                assert(their && Phi::Cast(their));
                assert(my == their && "Stack mismatch.");
            } else {
                assert(false &&
                       "Found a phi that has a non-instruction as input.");
            }
        }
    }
}

AbstractResult
StackUseAnalysisState::merge(const StackUseAnalysisState& other) {

    // Don't care about merges of two exit blocks
    if ((i && i->bb()->isExit()) && (other.i && other.i->bb()->isExit()))
        return AbstractResult::None;

    stack.matchContents(other.stack);

    return AbstractResult::None;
}

void StackUseAnalysisState::print(std::ostream& out, bool tty) const {
    out << "== Result: {";
    if (i)
        i->printRef(out);
    else
        out << "??";
    out << " -> [ ";
    for (auto v : stack) {
        v->printRef(out);
        out << " ";
    }
    out << "], toDrop = { ";
    for (auto v : toDrop) {
        v->printRef(out);
        out << " ";
    }
    out << "}, isDead = " << (isDead ? "true" : "false") << "}\n";
}

AbstractResult StackUseAnalysis::apply(StackUseAnalysisState& state,
                                       Instruction* i) const {

    state.i = i;
    state.isDead = false;
    state.toDrop.clear();

    // Check if there are values on the stack that are dead, eg. environment
    // that was elided
    auto isLivePhiInput = [&](Value* v) -> bool {
        if (Instruction::Cast(v)) {
            if (auto use = Instruction::Cast(v)->hasSingleUse()) {
                return Phi::Cast(use);
            }
        }
        return false;
    };
    auto isDead = [&](Value* v) -> bool {
        // v is an argument of i
        if (i->anyArg([&](Value* arg) { return arg == v; }))
            return false;
        // v is needed later
        if (liveness.live(i, v))
            return false;
        // This is a hack because phi's inputs are represented by the phi itself
        // where we cannot decide which input to use (for phis that are not the
        // first instruction in a bb)
        if (isLivePhiInput(v))
            return false;
        return true;
    };

    state.stack.eachSlot([&](Value* v) {
        if (isDead(v))
            state.toDrop.push_back(v);
    });
    for (auto v : state.toDrop)
        state.stack.eraseValue(v);

    // Consume arguments if this is their last use
    if (auto phi = Phi::Cast(i)) {
        // For phi's, we know that the inputs are only used once, by the phi
        state.stack.erasePhiInput(phi);
    } else {
        // We only have each value once, so keep track in case a value is
        // used by the instruction multiple times (eg. deopts)
        std::unordered_set<Value*> erasedArgs;
        i->eachArg([&](Value* v) {
            if (!v->isInstruction())
                return;
            // For all else, check if this is the last use of the value
            if (!liveness.live(i, v) && erasedArgs.count(v) == 0) {
                state.stack.eraseValue(v);
                erasedArgs.insert(v);
            }
        });
    }

    // Add the value produced by this instruction if it's not dead
    if (i->type != PirType::voyd()) {
        if (isDead(i))
            state.isDead = true;
        else
            state.stack.push(i);
    }

    return AbstractResult::None;
}

} // namespace pir
} // namespace rir
