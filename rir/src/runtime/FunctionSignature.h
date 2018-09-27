#pragma once

#include "R/r.h"

#include <iomanip>
#include <iostream>
#include <vector>

namespace rir {

struct FunctionSignature {

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
            out << std::setw(22) << " "
                << "isEvaluated=" << (isEvaluated ? "true" : "false")
                << ", type=" << Rf_type2char(type) << ", length=" << length
                << "\n";
        }
    };

    void pushDefaultArgument() { arguments.emplace_back(); }

    void pushArgument(ArgumentType arg) { arguments.emplace_back(arg); }

    static FunctionSignature const * defaultSignature() {
        static FunctionSignature signature;
        return &signature;
    }

    bool matches(FunctionSignature const& other) const {
        if (argsOnStack != other.argsOnStack)
            return false;
        if (arguments.size() != other.arguments.size())
            return false;
        for (unsigned i = 0; i < arguments.size(); ++i)
            if (!arguments[i].matches(other.arguments[i]))
                return false;
        return true;
    }

    void print(std::ostream& out = std::cout) const {
        out << std::left << std::setw(20) << "    environment?"
            << (createEnvironment ? "true" : "false") << "\n";
        out << std::left << std::setw(20) << "    on stack?"
            << (argsOnStack ? "true" : "false") << "\n";
        out << std::left << std::setw(20) << "    args"
            << "size=" << arguments.size() << "\n";
        for (auto arg : arguments)
            arg.print(out);
        out << "\n";
    }

    FunctionSignature() = default;

    bool createEnvironment = true;
    bool argsOnStack = false;
    std::vector<ArgumentType> arguments;
};

} // namespace rir
