#pragma once

#include "../R/RList.h"
#include "../runtime/Function.h"

#include <vector>

namespace rir {

class FormalArgs {
    std::vector<SEXP> names_;
    std::vector<SEXP> defaultArgs_;
    bool hasDefaultArgs_, hasDots_;
    SEXP original_;

  public:
    FormalArgs(const FormalArgs&) = delete;
    FormalArgs& operator=(const FormalArgs&) = delete;

    FormalArgs(rir::Function* function, SEXP formals)
        : hasDefaultArgs_(false), hasDots_(false), original_(formals) {
        unsigned i = 0;
        for (auto it = RList(formals).begin(); it != RList::end(); ++it, ++i) {
            names_.push_back(it.tag());

            if (it.tag() == R_DotsSymbol)
                hasDots_ = true;

            auto arg = function->defaultArg(i);
            if (*it != R_MissingArg) {
                assert(arg != nullptr && "Rir compiled function is missing a "
                                         "compiled default argument");
                hasDefaultArgs_ = true;
                defaultArgs_.push_back(arg->container());
            } else {
                assert(arg == nullptr &&
                       "Rir compiled function has a default argument that is "
                       "not in the formals list");
                defaultArgs_.push_back(R_MissingArg);
            }
        }
    }

    const std::vector<SEXP>& names() const { return names_; }

    const std::vector<SEXP>& defaultArgs() const { return defaultArgs_; }

    bool hasDefaultArgs() const { return hasDefaultArgs_; }

    bool hasDots() const { return hasDots_; }

    size_t nargs() const { return names_.size(); }

    SEXP original() const { return original_; }
};

} // namespace rir
