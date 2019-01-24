#pragma once

#include "../R/RList.h"

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

    explicit FormalArgs(SEXP formals)
        : hasDefaultArgs_(false), hasDots_(false), original_(formals) {
        for (auto it = RList(formals).begin(); it != RList::end(); ++it) {
            names_.push_back(it.tag());
            defaultArgs_.push_back(*it);

            if (it.tag() == R_DotsSymbol)
                hasDots_ = true;
            if (*it != R_MissingArg)
                hasDefaultArgs_ = true;
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
