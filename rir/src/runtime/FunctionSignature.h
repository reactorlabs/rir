#pragma once

#include "R/r.h"

namespace rir {

struct FunctionSignature {

    struct ArgumentType {
        bool isEvaluated = false;
        unsigned char type = PROMSXP;
        int length = -1;

        ArgumentType() = default;
        ArgumentType(SEXP s) : type(TYPEOF(s)) {
            if (type == PROMSXP) {
                isEvaluated = PRVALUE(s) != R_UnboundValue;
            }
            if (isEvaluated)
                length = XLENGTH(s);
        }

        bool matches(ArgumentType const& other) const {
            return isEvaluated == other.isEvaluated && type == other.type &&
                   length == other.length;
        }

        void print() const {
            Rprintf("                   isEvaluated=%s, type=%u, length=%d\n",
                    isEvaluated ? "true" : "false", type, length);
        }
    };

    void pushDefaultArgument() { arguments.emplace_back(); }

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

    void print() const {
        Rprintf("    on stack?      %s\n", argsOnStack ? "true" : "false");
        Rprintf("    args (%u):\n", arguments.size());
        for (auto arg : arguments)
            arg.print();
        Rprintf("\n");
    }

    bool argsOnStack = false;
    std::vector<ArgumentType> arguments;
};

} // namespace rir
