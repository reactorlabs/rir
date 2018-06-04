#include "cfg.h"
#include "../pir/pir_impl.h"
#include "../util/visitor.h"

#include <algorithm>
#include <set>
#include <stack>
#include <unordered_map>

namespace {
using namespace rir::pir;

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
void computeDominanceGraph(DominanceGraph* cfg, Code* start) {
    std::stack<BB*> todo;
    todo.push(start->entry);

    std::vector<BBInterSet> dom(start->nextBBId);

    while (!todo.empty()) {
        BB* cur = todo.top();
        auto front = dom[cur->id];
        front.insert(cur);

        todo.pop();

        auto apply = [&](BB* bb) {
            if (!bb)
                return;

            auto& d = dom[bb->id];
            if (!dom[bb->id].seen) {
                d.seen = true;
                d.insert(front.begin(), front.end());
                todo.push(bb);
                return;
            }

            if (d.merge(front))
                todo.push(bb);
        };
        apply(cur->next0);
        apply(cur->next1);
    }

    cfg->dominating.resize(start->nextBBId);

    for (size_t i = 0; i < start->nextBBId; ++i) {
        cfg->dominating[i].insert(dom[i].begin(), dom[i].end());
    }
}
}

namespace rir {
namespace pir {

CFG::CFG(Code* start) {
    predecessors_.resize(start->nextBBId);
    transitivePredecessors.resize(start->nextBBId);

    Visitor::run(start->entry, [&](BB* bb) {
        auto apply = [&](BB* next) {
            if (!next)
                return;
            if (!isImmediatePredecessor(bb, next)) {
                predecessors_[next->id].push_back(bb);
                transitivePredecessors[next->id].push_back(bb);
            }
        };
        apply(bb->next0);
        apply(bb->next1);
        if (!bb->next0 && !bb->next1)
            exits_.push_back(bb);
    });

    std::function<void(BB*, BB*)> complete = [&](BB* bb, BB* pre1) {
        for (auto pre2 : immediatePredecessors(pre1)) {
            if (!isPredecessor(pre2, bb)) {
                transitivePredecessors[bb->id].push_back(pre2);
                complete(bb, pre2);
            }
        }
    };

    Visitor::run(start->entry, [&](BB* bb) {
        for (auto pre1 : immediatePredecessors(bb))
            complete(bb, pre1);
    });
}

bool CFG::isMergeBlock(BB* a) const { return predecessors_[a->id].size() > 1; }

bool CFG::hasSinglePred(BB* a) const {
    return predecessors_[a->id].size() == 1;
}

bool CFG::isPredecessor(BB* a, BB* b) const {
    for (auto p : transitivePredecessors[b->id])
        if (p == a)
            return true;
    return false;
}

bool CFG::isImmediatePredecessor(BB* a, BB* b) const {
    for (auto p : predecessors_[b->id])
        if (p == a)
            return true;
    return false;
}

const CFG::BBList& CFG::immediatePredecessors(BB* a) const {
    return predecessors_[a->id];
}

DominanceGraph::DominanceGraph(Code* start) {
    computeDominanceGraph(this, start);
}

bool DominanceGraph::dominates(BB* a, BB* b) const {
    return dominating[b->id].find(a) != dominating[b->id].end();
}
}
}
