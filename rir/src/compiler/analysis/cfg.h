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
    // This array, indexed by BB id, represents the dominator tree. It stores
    // the immediate dominator of each node, i.e., for a node `v`,
    // `idom[v->id]` is the immediate dominator of `v`. The dominators of `v`
    // can be found by walking through this array. E.g., `idom[v->id]`
    // dominates `v`, `idom[idom[v->id]->id]` dominates `v`, and so on.
    BBList idom;

  public:
    explicit DominanceGraph(Code*);

    size_t size() const { return idom.size(); }

    // Given a Code and a set of BBs, return the set of BBs dominated by the
    // input set.
    static BBSet dominatedSet(Code* start, const BBSet& bbs);

    // `a` dominates `b` if every path from the entry node to `b` contains `a`.
    // Note that a node dominates itself. Nodes may dominate multiple nodes,
    // and be dominated by multiple nodes.
    bool dominates(BB* a, BB* b) const;

    // `a` strictly dominates `b` if `a` dominates `b` and `a` does not equal
    // `b`.
    bool strictlyDominates(BB* a, BB* b) const;

    // `a` immediately dominates `b` if `a` strictly dominates `b`, but does not
    // strictly dominate any other node that strictly dominates `b`. Every node,
    // except for the entry node, has exactly one immediate dominator.
    bool immediatelyDominates(BB* a, BB* b) const;

    bool hasImmediateDominator(BB* bb) const;
    BB* immediateDominator(BB*) const;
    const BBSet dominators(BB*) const;
    void dominatorTreeNext(BB* bb, const std::function<void(BB*)>&) const;
};

class DominanceFrontier {
  public:
    typedef SmallSet<BB*> BBList;

  private:
    std::vector<BBList> frontier;

  public:
    DominanceFrontier(Code* code, const DominanceGraph&);

    // The dominance frontier at node `bb` is the set of all nodes `n` such
    // that `bb` dominates an immediate predecessor of `n`, but does not
    // dominate `n`. I.e., the dominance frontier of `bb` is where the
    // dominance of `bb` "stops."
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
