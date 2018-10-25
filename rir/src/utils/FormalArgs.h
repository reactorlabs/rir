#pragma once

#include "../R/RList.h"

#include <vector>

namespace rir {

class FormalArgs {
  public:
    std::vector<SEXP> names;
    std::vector<SEXP> defaultArgs;
    bool hasDefaultArgs, hasDots;

    explicit FormalArgs(SEXP formals) : hasDefaultArgs(false), hasDots(false) {
        for (auto it = RList(formals).begin(); it != RList::end(); ++it) {
            names.push_back(it.tag());
            defaultArgs.push_back(*it);

            if (it.tag() == R_DotsSymbol)
                hasDots = true;
            if (*it != R_MissingArg)
                hasDefaultArgs = true;
        }
    }
};

} // namespace rir
