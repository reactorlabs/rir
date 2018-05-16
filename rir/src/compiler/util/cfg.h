#ifndef PIR_CFG_H
#define PIR_CFG_H

#include "../pir/pir.h"
#include <unordered_set>
#include <vector>

namespace rir {
namespace pir {

class CFG {
    typedef std::unordered_set<BB*> BBList;

  public:
    std::vector<BBList> predecessors;
    std::vector<BBList> transitivePredecessors;
    BBList exits;

    CFG(BB*);
};

class DominanceGraph {
    typedef std::unordered_set<BB*> BBList;

  public:
    std::vector<BBList> dominating;

    size_t size() const { return dominating.size(); }
    DominanceGraph(BB*);

    bool dominates(BB* a, BB* b) const;
};
}
}

#endif
