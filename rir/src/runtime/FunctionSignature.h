#pragma once

#include "R/Serialize.h"
#include "R/r.h"

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

    static FunctionSignature deserialize(SEXP refTable, R_inpstream_t inp) {
        Environment envc = (Environment)InInteger(inp);
        OptimizationLevel opt = (OptimizationLevel)InInteger(inp);
        unsigned numArgs = InInteger(inp);
        FunctionSignature sig(envc, opt);
        sig.numArguments = numArgs;
        sig.dotsPosition = InInteger(inp);
        return sig;
    }

    void serialize(SEXP refTable, R_outpstream_t out) const {
        OutInteger(out, (int)envCreation);
        OutInteger(out, (int)optimization);
        OutInteger(out, numArguments);
        OutInteger(out, dotsPosition);
    }

    void pushFormal(SEXP arg, SEXP name) {
        if (arg != R_MissingArg)
            hasDefaultArgs = true;
        if (name == R_DotsSymbol) {
            hasDotsFormals = true;
            dotsPosition = numArguments;
        }
        numArguments++;
    }

    void print(std::ostream& out = std::cout) const {
        if (optimization != OptimizationLevel::Baseline)
            out << "optimized code ";
        if (envCreation == Environment::CallerProvided)
            out << "needsEnv ";
    }

  public:
    FunctionSignature() = delete;
    FunctionSignature(Environment envCreation, OptimizationLevel optimization)
        : envCreation(envCreation), optimization(optimization) {}

    size_t formalNargs() const { return numArguments; }

    const Environment envCreation;
    const OptimizationLevel optimization;
    unsigned numArguments = 0;
    bool hasDotsFormals = false;
    bool hasDefaultArgs = false;
    size_t dotsPosition = -1;
};

} // namespace rir
