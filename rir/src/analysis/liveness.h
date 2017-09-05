#pragma once

#include <algorithm>
#include <unordered_set>

#include "../ir/CodeEditor.h"
#include "../analysis_framework/framework.h"
#include "../analysis_framework/analysis.h"


namespace rir {
    
/** Our abstract value.
 *
 * Liveness analysis uses powerset lattice, the elements being the variables of the analyzed function.
 * At all points in the function we have one abstract value which tells the set of variables that are
 * live at that point, i.e. their values are read by someone later.
 */

class ASet {
public:

    ASet() = default;
    ASet(ASet const & other) = default;

    static ASet const & Absent() { return bottom(); }

    bool mergeWith(ASet const & other) {
        // This is just set union

        auto originalSize = variables.size();

        for (auto const & var : other.variables) {
            variables.insert(var);
        }

        return originalSize != variables.size();
    }

    // Bottom is empty set
    static ASet const & bottom() {
        static ASet value;
        return value;
    }

    void print() const {
        Rprintf("{ ");
        for (auto const & var : variables) {
            Rprintf("%s ", CHAR(PRINTNAME((var))));
        }
        Rprintf("}");
    }

    std::unordered_set<SEXP> variables;

};

template <typename AVALUE>
class LivenessAbstractState : public State
{
public:
    
    LivenessAbstractState() = default;

    LivenessAbstractState(LivenessAbstractState const &) = default;
    
    virtual ~LivenessAbstractState()
    {}
    
    /** Creates a deep copy of the state and returns it.
     */
    virtual LivenessAbstractState * clone() const
    {
        return new LivenessAbstractState(*this);
    }

    /** Merges the information from the other state, returns true if changed.
     */
    virtual bool mergeWith(const State *other)
    {
        return this->state.mergeWith(dynamic_cast<const LivenessAbstractState*>(other)->getState());
    }
    
    const ASet& getState() const
    {
        return state;
    }
    
    void setState(const ASet& s)
    {
        state = s;
    }
    
    void insert(SEXP e)
    {
        state.variables.insert(e);
    }
    
    void erase(SEXP e)
    {
        state.variables.erase(e);
    }
    
private:
    
    AVALUE state;
};

class LivenessAnalysis : public BackwardAnalysisIns<LivenessAbstractState<ASet>>,
                         public InstructionDispatcher::Receiver {
public:
    LivenessAnalysis() :
        dispatcher_(*this) {
    }

protected:

    LivenessAbstractState<ASet> * initialState() override {
        auto * result = new LivenessAbstractState<ASet>();
        // No instructions use stack, we only have one abstract value stored here
        result->setState(ASet::bottom());
        return result;
    }

    void ldarg_(CodeEditor::Iterator ins) override {
        BC bc = *ins;
        current().insert(bc.immediateConst());
    }

    void ldvar_(CodeEditor::Iterator ins) override {
        BC bc = *ins;
        current().insert(bc.immediateConst());
    }

    void stvar_(CodeEditor::Iterator ins) override {
        BC bc = *ins;
        current().erase(bc.immediateConst());
    }

    void ret_(CodeEditor::Iterator ins) override {
    }

    void return_(CodeEditor::Iterator ins) override {
    }

    void any(CodeEditor::Iterator ins) override {}

    Dispatcher & dispatcher() override {
        return dispatcher_;
    }

private:
    InstructionDispatcher dispatcher_;

};


}  // namespace rir
