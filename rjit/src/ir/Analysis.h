#ifndef ANALYSIS_H
#define ANALYSIS_H

#include "ir/HandlerPassWrapper.h"

namespace rjit {
namespace ir {

template <typename Handler>
class ForwardAnalysis : public HandlerPassWrapper<Handler> {
  public:
    Handler handler;

    bool runOnFunction(Function& f) override {
        if (f.isDeclaration() || f.empty())
            return false;

        return runOnFunction_(f);
    }

    virtual bool runOnFunction_(Function& f) { return dispatch_(f); }

    virtual bool dispatch_(Function& f) {
        for (auto& b : f) {
            BasicBlock::iterator i = b.begin();
            while (i != b.end()) {
                if (!handler.dispatch(i))
                    i++;
            }
        }

        return false;
    }
};
}
}

#endif
