#include "cfg.h"
#include "../pir/pir_impl.h"
#include "../util/visitor.h"

#include <algorithm>
#include <stack>
#include <unordered_map>

namespace {
using namespace rir::pir;

class BBSet : public std::unordered_set<BB*> {
  public:
    bool seen = false;

    bool merge(BBSet& other) {
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

void computeCfg(CFG* cfg, BB* start) {
    std::stack<BB*> todo;

    std::unordered_map<BB*, BBSet> pred;

    size_t maxId = 0;

    Visitor::run(start, [&](BB* cur) {
        if (maxId < cur->id)
            maxId = cur->id;

        auto apply = [&](BB* bb) {
            if (!bb)
                return;

            pred[bb].insert(cur);
        };
        apply(cur->next0);
        apply(cur->next1);
        if (cur->next0 == nullptr && cur->next1 == nullptr) {
            cfg->exits.push_back(cur);
        }
    });

    cfg->predecessors.resize(maxId + 1);

    for (auto e : pred) {
        auto i = e.first->id;
        cfg->predecessors[i].insert(cfg->predecessors[i].end(),
                                    e.second.begin(), e.second.end());
    }
}

// Static Analysis computes the set of all dominating bb's, for every bb
// reachable from start. Runs until none of the sets grow anymore.
void computeDominanceGraph(DominanceGraph* cfg, BB* start) {
    std::stack<BB*> todo;
    todo.push(start);

    std::unordered_map<BB*, BBSet> dom;

    size_t maxId = 0;

    while (!todo.empty()) {
        BB* cur = todo.top();
        if (maxId < cur->id)
            maxId = cur->id;

        BBSet& front = dom[cur];

        todo.pop();

        auto apply = [&](BB* bb) {
            if (!bb)
                return;

            BBSet& d = dom[bb];
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
