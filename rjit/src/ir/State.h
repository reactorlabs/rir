#ifndef IR_STATE_H
#define IR_STATE_H

#include <memory>

#include "llvm.h"
#include "RIntlns.h"
#include "Ir.h"

namespace rjit {
namespace ir {



template<typename AVALUE>
class AState {
public:
    typedef AVALUE Value;

    AState() = default;
    AState(AState const &) = default;
    AState(AState && other):
        registers_(std::move(other.registers_)),
        variables_(std::move(other.variables_)) {
    }

    bool has(ir::Value index) const {
        return registers_.find(index) != registers_.end();
    }

    bool has(SEXP symbol) const {
        return variables_.find(symbol) != variables_.end();
    }

    AVALUE & operator [] (ir::Value index) {
        return registers_[index];
    }

    AVALUE & operator [] (SEXP symbol) {
        return variables_[symbol];
    }

    AState & operator = (AState<AVALUE> const &) = default;

    /** Merges the other state into itself. Returns true if the state changed during the merge.
     */
    bool mergeWith(AState<AVALUE> const & other) {
        bool result = mergeMaps(registers_, other.registers_);
        return mergeMaps(variables_, other.variables_) or result;
    }

    void invalidateVariables(AVALUE top) {
        for (auto i = variables_.begin(), e = variables_.end(); i != e; ++i)
            i->second = top;
    }

    /** Moves information about the old register to the new register and and discards the old one.
     */
    void replaceWith(ir::Value old, ir::Value with) {
        assert (registers_.find(with) == registers_.end() and "Target value is assumed to be new");
        auto i = registers_.find(old);
        if (i != registers_.end()) {
            registers_[with] = registers_[old];
            registers_.erase(i);
        }
    }

protected:

    template<typename MAP>
    bool mergeMaps(MAP & myMap, MAP const & theirMap) {
        bool result = false;
        for (auto their : theirMap)
            result = myMap[their.first].mergeWith(their.second) or result;
        // we do not care about any values in ourselves that are not part of incomming as they will merge to themselves
        return result;
    }

    std::map<llvm::Value *, AVALUE> registers_;
    std::map<SEXP, AVALUE> variables_;



};

} // namespace ir
} // namespace rjit

#endif // IR_STATE_H


