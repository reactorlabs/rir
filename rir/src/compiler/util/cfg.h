#ifndef PIR_CFG_H
#define PIR_CFG_H

#include "../pir/pir.h"
#include <unordered_set>
#include <vector>

namespace rir {
namespace pir {

class CFG {
    typedef std::vector<BB*> BBList;

    std::vector<BBList> predecessors_;
    std::vector<BBList> transitivePredecessors;
    BBList exits_;

  public:
    CFG(Code*);
    bool isPredecessor(BB* a, BB* b) const;
    bool isImmediatePredecessor(BB* a, BB* b) const;
    bool hasSinglePred(BB* a) const;
    bool isMergeBlock(BB* a) const;
    const BBList& immediatePredecessors(BB* a) const;
    const BBList& exits() const { return exits_; }
};

class DominanceGraph {
    typedef std::unordered_set<BB*> BBList;

  public:
    std::vector<BBList> dominating;

    size_t size() const { return dominating.size(); }
    DominanceGraph(Code*);

    bool dominates(BB* a, BB* b) const;
};
}
}

#endif
