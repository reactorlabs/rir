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
}

namespace rir {
namespace pir {

CFG::CFG(Code* start)
    : predecessors_(start->nextBBId), transitivePredecessors(start->nextBBId) {
    Visitor::run(start->entry, [&](BB* bb) {
        auto apply = [&](BB* next) {
            if (!next)
                return;
            if (!isImmediatePredecessor(bb, next)) {
                predecessors_[next->id].push_back(bb);
                transitivePredecessors[next->id].push_back(bb);
            }
        };
        apply(bb->trueBranch());
        apply(bb->falseBranch());
        if (!bb->trueBranch() && !bb->falseBranch())
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
    auto& preds = transitivePredecessors[b->id];
    return std::any_of(
        preds.begin(), preds.end(),
        std::bind(std::equal_to<BB*>(), std::placeholders::_1, a));
}

bool CFG::isImmediatePredecessor(BB* a, BB* b) const {
    auto& preds = predecessors_[b->id];
    return std::any_of(
        preds.begin(), preds.end(),
        std::bind(std::equal_to<BB*>(), std::placeholders::_1, a));
}

const CFG::BBList& CFG::immediatePredecessors(BB* a) const {
    return predecessors_[a->id];
}

DominanceGraph::DominanceGraph(Code* start) : dominating(start->nextBBId) {
    // Static Analysis computes the set of all dominating bb's, for every bb
    // reachable from start. Runs until none of the sets grow anymore.

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
        apply(cur->trueBranch());
        apply(cur->falseBranch());
    }

    for (size_t i = 0; i < start->nextBBId; ++i) {
        dominating[i].insert(dom[i].begin(), dom[i].end());
    }
}

bool DominanceGraph::dominates(BB* a, BB* b) const {
    return dominating[b->id].find(a) != dominating[b->id].end();
}
}
}
