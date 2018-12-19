#pragma once

#include "R/r.h"

#include "compiler/pir/assumptions.h"

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
        assert(nargs() > 0);
    }

    void pushArgument(ArgumentType arg) { arguments.emplace_back(arg); }

    void print(std::ostream& out = std::cout) const {
        if (nargs() > 0) {
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
            out << "| assumptions: [";
            for (auto i = assumptions.begin(); i != assumptions.end(); ++i) {
                out << *i;
                if (i + 1 != assumptions.end())
                    out << ",";
            }
            out << "]";
        }
    }

    FunctionSignature() = delete;
    FunctionSignature(Environment envCreation, OptimizationLevel optimization)
        : envCreation(envCreation), optimization(optimization) {}
    FunctionSignature(Environment envCreation, OptimizationLevel optimization,
                      const pir::Assumptions& assumptions)
        : envCreation(envCreation), optimization(optimization),
          assumptions(assumptions) {}

    size_t nargs() const { return arguments.size(); }

    const Environment envCreation;
    const OptimizationLevel optimization;
    std::vector<ArgumentType> arguments;
    const pir::Assumptions assumptions;
};

} // namespace rir
