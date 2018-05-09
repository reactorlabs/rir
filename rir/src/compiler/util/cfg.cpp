#include "cfg.h"
#include "../pir/pir_impl.h"
#include "../util/visitor.h"

#include <algorithm>
#include <set>
#include <stack>
#include <unordered_map>

namespace {
using namespace rir::pir;

class BBUnionSet : public std::set<BB*> {
  public:
    bool merge(BBUnionSet& other) {
        if (!std::includes(begin(), end(), other.begin(), other.end())) {
            insert(other.begin(), other.end());
            return true;
        }
        return false;
    }
};

void computeCfg(CFG* cfg, BB* start) {
    std::set<BB*> todo;
    todo.insert(start);

    std::unordered_map<BB*, BBUnionSet> pred;
    std::unordered_map<BB*, BBUnionSet> directPred;

    size_t maxId = start->id;

    while (!todo.empty()) {
        BB* cur = *todo.begin();
        todo.erase(todo.begin());

        auto apply = [&](BB* bb) {
            if (!bb)
                return;

            if (bb->id > maxId)
                maxId = bb->id;

            if (!directPred[bb].count(cur))
                directPred[bb].insert(cur);

            if (!pred[bb].count(cur)) {
                pred[bb].insert(cur);
                todo.insert(bb);
            }

            if (pred[bb].merge(pred[cur]))
                todo.insert(bb);
        };

        apply(cur->next0);
        apply(cur->next1);
        if (!cur->next0 && !cur->next1)
            cfg->exits.insert(cur);
    }

    cfg->predecessors.resize(maxId + 1);
    cfg->transitivePredecessors.resize(maxId + 1);

    for (auto e : pred) {
        auto i = e.first->id;
        cfg->transitivePredecessors[i].insert(e.second.begin(), e.second.end());
    }
    for (auto e : directPred) {
        auto i = e.first->id;
        cfg->predecessors[i].insert(e.second.begin(), e.second.end());
    }
}

class BBInterSet : public std::unordered_set<BB*> {
  public:
    bool seen = false;
    bool merge(BBInterSet& other) {
        if (*this == other)
            return false;
        bool changed = false;
        auto it = begin();
        while (it != end()) {
            if (other.find(*it) == other.end()) {
                it = erase(it);
                changed = true;
            } else {
                it++;
            }
        }
        return changed;
    }
};

// Static Analysis computes the set of all dominating bb's, for every bb
// reachable from start. Runs until none of the sets grow anymore.
void computeDominanceGraph(DominanceGraph* cfg, BB* start) {
    std::stack<BB*> todo;
    todo.push(start);

    std::unordered_map<BB*, BBInterSet> dom;

    size_t maxId = 0;

    while (!todo.empty()) {
        BB* cur = todo.top();
        if (maxId < cur->id)
            maxId = cur->id;

        auto& front = dom[cur];

        todo.pop();

        auto apply = [&](BB* bb) {
            if (!bb)
                return;

            auto& d = dom[bb];
            if (!dom[bb].seen) {
                d.seen = true;
                d.insert(front.begin(), front.end());
                d.insert(cur);
                todo.push(bb);
                return;
            }

            if (d.merge(front))
                todo.push(bb);
        };
        apply(cur->next0);
        apply(cur->next1);
    }

    cfg->dominating.resize(maxId + 1);

    for (auto e : dom) {
        unsigned i = e.first->id;
        cfg->dominating[i].insert(e.second.begin(), e.second.end());
    }
}
}

namespace rir {
namespace pir {

CFG::CFG(BB* start) { computeCfg(this, start); }

DominanceGraph::DominanceGraph(BB* start) {
    computeDominanceGraph(this, start);
}

bool DominanceGraph::dominates(BB* a, BB* b) const {
    return dominating[b->id].find(a) != dominating[b->id].end();
}
}
}
