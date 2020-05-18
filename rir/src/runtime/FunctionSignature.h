#pragma once

#include "R/Serialize.h"
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

    // TODO: currently unused, since we track the types in the assumptions.
    // What should we do with it.
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

        static ArgumentType deserialize(SEXP refTable, R_inpstream_t inp) {
            ArgumentType x;
            InBytes(inp, &x, sizeof(ArgumentType));
            return x;
        }

        void serialize(SEXP refTable, R_outpstream_t out) const {
            OutBytes(out, this, sizeof(ArgumentType));
        }

        void print(std::ostream& out = std::cout) const {
            if (isEvaluated)
                out << "eager ";
            out << Rf_type2char(type);
        }
    };

    static FunctionSignature deserialize(SEXP refTable, R_inpstream_t inp) {
        Environment envc = (Environment)InInteger(inp);
        OptimizationLevel opt = (OptimizationLevel)InInteger(inp);
        const Assumptions as = Assumptions::deserialize(refTable, inp);
        unsigned numArgs = InInteger(inp);
        FunctionSignature sig(envc, opt, as);
        sig.numArguments = numArgs;
        return sig;
    }

    void serialize(SEXP refTable, R_outpstream_t out) const {
        OutInteger(out, (int)envCreation);
        OutInteger(out, (int)optimization);
        assumptions.serialize(refTable, out);
        OutInteger(out, numArguments);
    }

    void pushArgument(SEXP name, SEXP expr) {
        numArguments++;
        if (name == R_DotsSymbol)
            hasDotsArgs = true;
        if (expr != R_MissingArg)
            hasDefaultArgs = true;
    }

    void print(std::ostream& out = std::cout) const {
        if (optimization != OptimizationLevel::Baseline)
            out << "optimized code ";
        if (envCreation == Environment::CallerProvided)
            out << "needsEnv ";
        if (!assumptions.empty()) {
            out << "| assumptions: [" << assumptions << "]";
        }
    }

  public:
    FunctionSignature() = delete;
    FunctionSignature(Environment envCreation, OptimizationLevel optimization)
        : envCreation(envCreation), optimization(optimization) {}
    FunctionSignature(Environment envCreation, OptimizationLevel optimization,
                      const Assumptions& assumptions)
        : envCreation(envCreation), optimization(optimization),
          assumptions(assumptions) {}

    size_t formalNargs() const { return numArguments; }
    size_t expectedNargs() const {
        return numArguments - assumptions.numMissing();
    }

    const Environment envCreation;
    const OptimizationLevel optimization;
    unsigned numArguments = 0;
    const Assumptions assumptions;
    bool hasDotsArgs = false;
    bool hasDefaultArgs = false;
};

} // namespace rir
