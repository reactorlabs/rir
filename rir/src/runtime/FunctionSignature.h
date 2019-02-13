#pragma once

#include "R/r.h"

#include "runtime/Assumptions.h"

#include <iomanip>
#include <iostream>
#include <vector>

namespace rir {

struct FunctionSignature {
    enum class Environment {
        CallerProvided,
        CalleeCreated,
    };

    enum class OptimizationLevel {
        Baseline,
        Optimized,
        Contextual,
    };

    struct ArgumentType {
        bool isEvaluated = false;
        unsigned char type = PROMSXP;
        int length = -1;

        ArgumentType() = default;
        explicit ArgumentType(SEXP s) : type(TYPEOF(s)) {
            if (type == PROMSXP) {
                isEvaluated = PRVALUE(s) != R_UnboundValue;
            }
            // Only get the length if the promise is forced,
            // otherwise stays set to -1 (unknown)
            if (isEvaluated)
                length = XLENGTH(s);
        }
        ArgumentType(bool evaled, unsigned int type) :
            isEvaluated(evaled),
            type(static_cast<unsigned char>(type)) {}

        bool matches(ArgumentType const& other) const {
            return isEvaluated == other.isEvaluated && type == other.type &&
                   length == other.length;
        }

        void print(std::ostream& out = std::cout) const {
            if (isEvaluated)
                out << "eager ";
            out << Rf_type2char(type);
        }
    };

    void pushDefaultArgument() {
        arguments.emplace_back();
        assert(formalNargs() > 0);
    }

    void pushArgument(ArgumentType arg) { arguments.emplace_back(arg); }

    void print(std::ostream& out = std::cout) const {
        if (formalNargs() > 0) {
            out << "argTypes: (";
            for (auto i = arguments.begin(); i != arguments.end(); ++i) {
                i->print(out);
                if (i + 1 != arguments.end())
                    out << ", ";
            }
            out << ") ";
        }
        if (optimization != OptimizationLevel::Baseline)
            out << "optimized code ";
        if (envCreation == Environment::CallerProvided)
            out << "needsEnv ";
        if (!assumptions.empty()) {
            out << "| assumptions: [" << assumptions << "]";
        }
    }

    FunctionSignature() = delete;
    FunctionSignature(Environment envCreation, OptimizationLevel optimization)
        : envCreation(envCreation), optimization(optimization) {}
    FunctionSignature(Environment envCreation, OptimizationLevel optimization,
                      const Assumptions& assumptions)
        : envCreation(envCreation), optimization(optimization),
          assumptions(assumptions) {}

    size_t formalNargs() const { return arguments.size(); }
    size_t expectedNargs() const {
        return arguments.size() - assumptions.numMissing();
    }

    const Environment envCreation;
    const OptimizationLevel optimization;
    std::vector<ArgumentType> arguments;
    const Assumptions assumptions;
};

} // namespace rir
