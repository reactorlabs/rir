#include "cfg.h"
#include "../pir/pir_impl.h"
#include "../util/visitor.h"

#include <algorithm>
#include <stack>

namespace {
using namespace rir::pir;

class BBSet : public std::set<size_t> {
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

static void computeCfg(CFG* cfg, BB* start) {
    std::stack<BB*> todo;

    std::vector<BBSet> pred;
    std::vector<BB*> bbPtr;

    pred.resize(start->id + 10);
    bbPtr.resize(start->id + 10);

    size_t maxId = 0;

    Visitor::run(start, [&](BB* cur) {
        if (maxId < cur->id)
            maxId = cur->id;

        if (pred.size() <= cur->id) {
            pred.resize(cur->id + 5);
            bbPtr.resize(cur->id + 5);
        }

        bbPtr[cur->id] = cur;

        auto apply = [&](BB* bb) {
            if (!bb)
                return;

            if (pred.size() <= bb->id) {
                pred.resize(bb->id + 5);
                bbPtr.resize(bb->id + 5);
            }

            pred[bb->id].insert(cur->id);
        };
        apply(cur->next0);
        apply(cur->next1);
        if (cur->next0 == nullptr && cur->next1 == nullptr) {
            cfg->exits.push_back(cur);
        }
    });

    cfg->preds.resize(maxId + 1);

    for (size_t i = 0; i < maxId + 1; ++i) {
        for (auto p : pred[i])
            cfg->preds[i].push_back(bbPtr[p]);
    }
}

static void computeDominanceGraph(DominanceGraph* cfg, BB* start) {
    std::stack<BB*> todo;
    todo.push(start);

    std::vector<BBSet> dom;
    std::vector<BBSet> pred;
    std::vector<BB*> bbPtr;

    dom.resize(start->id + 10);
    pred.resize(start->id + 10);
    bbPtr.resize(start->id + 10);

    size_t maxId = 0;

    do {
        BB* cur = todo.top();
        if (maxId < cur->id)
            maxId = cur->id;

        if (dom.size() <= cur->id) {
            dom.resize(cur->id + 5);
            pred.resize(cur->id + 5);
            bbPtr.resize(cur->id + 5);
        }

        BBSet& front = dom[cur->id];
        bbPtr[cur->id] = cur;

        todo.pop();

        auto apply = [&](BB* bb) {
            if (!bb)
                return;

            if (dom.size() <= bb->id) {
                dom.resize(bb->id + 5);
                pred.resize(bb->id + 5);
                bbPtr.resize(bb->id + 5);
            }

            pred[bb->id].insert(cur->id);

            BBSet& d = dom[bb->id];
            if (!dom[bb->id].seen) {
                d.seen = true;
                d.insert(front.begin(), front.end());
                d.insert(cur->id);
                todo.push(bb);
                return;
            }

            if (d.merge(front))
                todo.push(bb);
        };
        apply(cur->next0);
        apply(cur->next1);
    } while (!todo.empty());

    cfg->doms.resize(maxId + 1);

    for (size_t i = 0; i < maxId + 1; ++i) {
        for (auto p : dom[i])
            cfg->doms[i].push_back(bbPtr[p]);
    }
}
}

namespace rir {
namespace pir {

CFG::CFG(BB* start) { computeCfg(this, start); }
DominanceGraph::DominanceGraph(BB* start) {
    computeDominanceGraph(this, start);
}
}
}
