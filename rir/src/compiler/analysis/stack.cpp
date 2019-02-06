#include "stack.h"
#include "../../ir/BC_inc.h"
#include "../pir/pir_impl.h"

namespace {
using namespace rir::pir;

struct StackValuesAnalysisState {
    std::unordered_set<Value*> stackValues;

    AbstractResult merge(const StackValuesAnalysisState& other) {
        AbstractResult res;

        for (auto v : other.stackValues) {
            if (stackValues.count(v) == 0) {
                res.update();
                stackValues.insert(v);
            }
        }

        return res;
    }

    void print(std::ostream& out, bool tty) const {
        out << "== Result: { ";
        for (auto v : stackValues) {
            v->printRef(out);
            out << " ";
        }
        out << "}\n";
    }
};

class TheStackValuesAnalysis
    : public BackwardStaticAnalysis<StackValuesAnalysisState,
                                    AnalysisDebugLevel::None> {
  public:
    CFG const& cfg;

    mutable std::unordered_map<Instruction*, Phi*> fixup;

    TheStackValuesAnalysis(ClosureVersion* cls, Code* code, CFG const& cfg,
                           LogStream& log)
        : BackwardStaticAnalysis("Stack Values", cls, code, cfg, log),
          cfg(cfg) {}

    AbstractResult apply(StackValuesAnalysisState& state,
                         Instruction* i) const override;
};

AbstractResult TheStackValuesAnalysis::apply(StackValuesAnalysisState& state,
                                             Instruction* i) const {
    AbstractResult effect;

    // If we find a value that is an input to a phi, we remove the phi and
    // remember that we have to fix the phi's that now occur before their
    // definitions (only possible before the phi's own block, in case the phi is
    // not at the beginning of the block -- we don't have the phis at the
    // beginning and we don't have parallel semantics for them...)
    for (auto val : state.stackValues) {
        if (auto phi = Phi::Cast(val)) {
            phi->eachArg([&](BB*, Value* phiInput) {
                if (i == phiInput) {
                    fixup[i] = phi;
                }
            });
        }
    }
    if (fixup.count(i))
        state.stackValues.erase(state.stackValues.find(fixup[i]));

    if (auto phi = Phi::Cast(i)) {
        // For phi, we don't remove it, we leave it as a placeholder for its
        // input, since we don't know yet which input will be the correct one
        effect.update();
        state.stackValues.insert(phi);
    } else {
        // For non-phi, we have a source of a value, so we remove it
        if (state.stackValues.count(i) != 0) {
            effect.update();
            state.stackValues.erase(state.stackValues.find(i));
        }
        // And add its requirements instead
        i->eachArg([&](Value* v) {
            if (v->isInstruction() && state.stackValues.count(v) == 0) {
                effect.update();
                state.stackValues.insert(v);
            }
        });
    }

    return effect;
}

/*
 * For every instruction, get the set of values that are still needed at some
 * point after executing it.
 */
class StackValuesAnalysis {
  public:
    std::unordered_map<Instruction*, std::unordered_set<Value*>> stackValues;

    StackValuesAnalysis(ClosureVersion* fun, Code* code, LogStream& log,
                        CFG const& cfg) {
        TheStackValuesAnalysis analysis(fun, code, cfg, log);
        analysis();

        analysis.foreach<
            TheStackValuesAnalysis::PositioningStyle::BeforeInstruction>(
            [&](const StackValuesAnalysisState& state, Instruction* i) {
                stackValues[i] = std::move(state.stackValues);
            });

        for (auto fix : analysis.fixup) {
            auto input = fix.first;
            auto phi = fix.second;
            bool found = false;
            Visitor::check(input->bb(), [&](Instruction* i, BB* bb) {
                if (bb == phi->bb())
                    return false;
                if (i == input)
                    found = true;
                if (found && stackValues[i].count(phi)) {
                    stackValues[i].erase(stackValues[i].find(phi));
                    stackValues[i].insert(input);
                }
                return true;
            });
        }
    }

    void print() const {
        std::cout << "Stack values\n";
        for (auto x : stackValues) {
            std::cout << "  ";
            x.first->printRef(std::cout);
            std::cout << ": ";
            for (auto v : x.second) {
                v->printRef(std::cout);
                std::cout << " ";
            }
            std::cout << "\n";
        }
    }
};

struct StackAnalysisState {

    Instruction* i = nullptr;
    StackAnalysis::AbstractStack stack;
    std::vector<Value*> toDrop; // list of values to be removed from stack
                                // before executing this instruction
    bool isDead; // if this instruction should be popped after it's executed

    AbstractResult merge(const StackAnalysisState& other) {

        // Don't care about merges of two exit blocks
        if ((i && i->bb()->isExit()) && (other.i && other.i->bb()->isExit()))
            return AbstractResult::None;

        if (stack.size() != other.stack.size()) {
            std::cout << "me:    ";
            if (i) i->printRef(std::cout);
            std::cout << " - ";
            for (auto x : stack.data) {
                x->printRef(std::cout); std::cout << " ";
            }
            if (i) i->bb()->print(std::cout, true);
            std::cout << "\nother: ";
            if (other.i) other.i->printRef(std::cout);
            std::cout << " - ";
            for (auto x : other.stack.data) {
                x->printRef(std::cout); std::cout << " ";
            }
            std::cout << "\n";
            if (other.i) other.i->bb()->print(std::cout, true);
        }

        stack.matchContents(other.stack);

        return AbstractResult::None;
    }

    void print(std::ostream& out, bool tty) const {
        out << "== Result: { ";
        if (i)
            i->printRef(out);
        else
            out << "??";
        out << " -> ";
        for (auto v : stack.data) {
            v->printRef(out);
            out << " ";
        }
        out << "}  toDrop = { ";
        for (auto v : toDrop) {
            v->printRef(out);
            out << " ";
        }
        out << "}\n";
    }
};

class TheStackAnalysis
    : public StaticAnalysis<StackAnalysisState, AnalysisDebugLevel::None> {
  public:
    StackValuesAnalysis const& sva;

    TheStackAnalysis(ClosureVersion* cls, Code* code, LogStream& log,
                     StackValuesAnalysis const& sva)
        : StaticAnalysis("Stack Usage", cls, code, log), sva(sva) {}

    AbstractResult apply(StackAnalysisState& state,
                         Instruction* i) const override;
};

AbstractResult TheStackAnalysis::apply(StackAnalysisState& state,
                                       Instruction* i) const {

    state.i = i;
    state.isDead = false;
    state.toDrop.clear();

    // Check if there are values on the stack that are dead, eg. environment
    // that was elided
    auto isLivePhiInput = [&](Value* v) -> bool {
        if (!Instruction::Cast(v))
            return false;
        if (auto use = Instruction::Cast(v)->hasSingleUse()) {
            return Phi::Cast(use);
        }
        return false;
    };
    auto isDead = [&](Value* v) -> bool {
        // v is an argument of i
        if (i->anyArg([&](Value* arg) { return arg == v; }))
            return false;
        // v is needed later
        if (sva.stackValues.at(i).count(v))
            return false;
        // This is a hack because phi's inputs are represented by the phi itself
        // where we cannot decide which input to use (eg. a phi that is not the
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
        state.stack.erase(v);

    // Consume arguments if this is their last use
    if (auto phi = Phi::Cast(i)) {
        // For phi's, we know that the inputs are only used once, by the phi
        state.stack.erasePhiInput(phi);
    } else {
        std::unordered_set<Value*> erasedArgs;
        i->eachArg([&](Value* v) {
            if (!v->isInstruction())
                return;
            // For all else, check if this is the last use of the value
            if (sva.stackValues.at(i).count(v) == 0 &&
                erasedArgs.count(v) == 0) {
                state.stack.erase(v);
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

} // namespace

namespace rir {
namespace pir {

size_t StackAnalysis::AbstractStack::size() const { return data.size(); }

void StackAnalysis::AbstractStack::eachSlot(StackSlotsIterator it) const {
    for (size_t i = 0; i < data.size(); ++i)
        it(data[i]);
}

void StackAnalysis::AbstractStack::push(Value* v) {
    for (auto have : data)
        assert(have != v && "Stack already contains the value to be pushed.");
    data.push_back(v);
}

void StackAnalysis::AbstractStack::erase(Value* v) {
    for (auto it = data.rbegin(); it != data.rend(); ++it) {
        if (*it == v) {
            data.erase(std::next(it).base());
            return;
        }
    }

    for (auto v : data) {
        v->printRef(std::cout);
        std::cout << " ";
    }
    std::cout << "   ---   ";
    v->printRef();
    std::cout << "\n";

    assert(false && "Stack doesn't contain the value to be erased.");
}

void StackAnalysis::AbstractStack::erasePhiInput(Phi* phi) {
    for (auto it = data.rbegin(); it != data.rend(); ++it) {
        bool found = false;
        phi->eachArg([&](BB*, Value* in) {
            if (*it == in)
                found = true;
        });
        if (found) {
            data.erase(std::next(it).base());
            return;
        }
    }

    for (auto v : data) {
        v->printRef(std::cout);
        std::cout << " ";
    }
    std::cout << "   ---   ";
    phi->printRef(std::cout);
    std::cout << "\n";

    assert(false && "Stack doesn't contain any input for this phi.");
}

void StackAnalysis::AbstractStack::matchContents(AbstractStack const& other) const {
    assert(size() == other.size() && "The stacks better have the same height.");
    for (size_t i = 0; i < data.size(); ++i) {
        if (data[i] != other.data[i]) {
            if (data[i]->isInstruction() && other.data[i]->isInstruction()) {
                auto my = Instruction::Cast(data[i])->hasSingleUse();
                auto their = Instruction::Cast(other.data[i])->hasSingleUse();
                assert(my && Phi::Cast(my));
                assert(their && Phi::Cast(their));
                assert(my == their && "Stack mismatch.");
            } else {
                assert(false && ">> dunno what to do.. <<");
            }
        }
    }
}

void StackAnalysis::operator()(ClosureVersion* fun, Code* code,
                               LogStream& log) {

    StackValuesAnalysis sva(fun, code, log, cfg);
    // sva.print();
    // code->printCode(std::cout, true);
    TheStackAnalysis analysis(fun, code, log, sva);
    analysis();

    analysis.foreach<TheStackAnalysis::PositioningStyle::AfterInstruction>(
        [&](StackAnalysisState const& state, Instruction* i) {
            assert(stacks.count(i) == 0);
            stacks[i] = std::move(state.stack);

            assert(toDrop.count(i) == 0);
            if (!state.toDrop.empty())
                toDrop[i] = std::move(state.toDrop);

            if (state.isDead)
                dead.insert(i);
        });
    // print();
}

} // namespace pir
} // namespace rir
