#ifndef PIR_CFG_H
#define PIR_CFG_H

#include "../pir/pir.h"
#include "utils/Set.h"
#include <unordered_map>
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
    explicit CFG(Code*);
    bool isPredecessor(BB* a, BB* b) const;
    const BBList& exits() const { return exits_; }
};

class DominanceGraph {
  public:
    typedef std::vector<BB*> BBList;
    typedef std::unordered_set<BB*> BBSet;

  private:
    class DomTree {
      public:
        std::vector<BB*> container;
        bool seen = false;
        bool merge(const DomTree& other);
        void push_back(BB* bb) { container.push_back(bb); }
    };
    std::vector<DomTree> dominating;

  public:
    size_t size() const { return dominating.size(); }
    explicit DominanceGraph(Code*);

    // Given a Code and a set of BBs, return the set of BBs dominated by the
    // input set.
    static BBSet dominatedSet(Code* start, const BBSet& bbs);

    bool dominates(BB* a, BB* b) const;
    bool immediatelyDominates(BB* a, BB* b) const;

    bool hasImmediateDominator(BB* bb) const;
    BB* immediateDominator(BB*) const;
    const BBList& dominators(BB*) const;
    void dominatorTreeNext(BB* bb, const std::function<void(BB*)>&) const;
};

class DominanceFrontier {
  public:
    typedef SmallSet<BB*> BBList;

  private:
    std::vector<BBList> frontier;

  public:
    DominanceFrontier(Code* code, const DominanceGraph&);
    const BBList& at(BB* bb) const;
};

class UsesTree {
  public:
    typedef SmallSet<Instruction*> DependenciesList;
    explicit UsesTree(Code*);
    const DependenciesList& at(Instruction* i) const;
    const std::unordered_map<Instruction*, DependenciesList>::const_iterator
    begin() const {
        return uses.begin();
    }
    const std::unordered_map<Instruction*, DependenciesList>::const_iterator
    end() const {
        return uses.end();
    }

  private:
    std::unordered_map<Instruction*, DependenciesList> uses;
    DependenciesList empty;
};

} // namespace pir
} // namespace rir
#endif
