#pragma once

#include "R/Serialize.h"
#include "R/r.h"
#include "utils/ByteBuffer.h"

#include <iomanip>
#include <iostream>
#include <vector>
#include <sstream>

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
        sig.hasDotsFormals = InInteger(inp);
        sig.hasDefaultArgs = InInteger(inp);
        return sig;
    }

    void serialize(SEXP refTable, R_outpstream_t out) const {
        OutInteger(out, (int)envCreation);
        OutInteger(out, (int)optimization);
        OutInteger(out, numArguments);
        OutInteger(out, dotsPosition);
        OutInteger(out, hasDotsFormals);
        OutInteger(out, hasDefaultArgs);
    }

    static FunctionSignature deserialize(ByteBuffer& buffer) {
        auto envc = (Environment)buffer.getInt();
        auto opt = (OptimizationLevel)buffer.getInt();
        FunctionSignature sig(envc, opt);
        sig.numArguments = buffer.getInt();
        sig.dotsPosition = buffer.getInt();
        sig.hasDotsFormals = buffer.getInt();
        sig.hasDefaultArgs = buffer.getInt();
        return sig;
    }

    void serialize(ByteBuffer& buffer) const {
        buffer.putInt((uint32_t)envCreation);
        buffer.putInt((uint32_t)optimization);
        buffer.putInt(numArguments);
        buffer.putInt(dotsPosition);
        buffer.putInt(hasDotsFormals);
        buffer.putInt(hasDefaultArgs);
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

    /// Compare two signatures and print the differences to the given stream.
    static void debugCompare(const FunctionSignature& f1,
                             const FunctionSignature& f2,
                             std::stringstream& differences) {
        if (f1.envCreation != f2.envCreation) {
            differences << "envCreation: " << (int)f1.envCreation << " != "
                        << (int)f2.envCreation << std::endl;
        }
        if (f1.optimization != f2.optimization) {
            differences << "optimization: " << (int)f1.optimization << " != "
                        << (int)f2.optimization << std::endl;
        }
        if (f1.numArguments != f2.numArguments) {
            differences << "numArguments: " << f1.numArguments << " != "
                        << f2.numArguments << std::endl;
        }
        if (f1.hasDotsFormals != f2.hasDotsFormals) {
            differences << "hasDotsFormals: " << f1.hasDotsFormals << " != "
                        << f2.hasDotsFormals << std::endl;
        }
        if (f1.hasDefaultArgs != f2.hasDefaultArgs) {
            differences << "hasDefaultArgs: " << f1.hasDefaultArgs << " != "
                        << f2.hasDefaultArgs << std::endl;
        }
        if (f1.dotsPosition != f2.dotsPosition) {
            differences << "dotsPosition: " << f1.dotsPosition << " != "
                        << f2.dotsPosition << std::endl;
        }
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
