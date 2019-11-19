#include "cfg.h"
#include "../pir/pir_impl.h"
#include "../util/visitor.h"

#include <algorithm>
#include <set>
#include <stack>

namespace rir {
namespace pir {

CFG::CFG(Code* start)
    : predecessors_(start->nextBBId), transitivePredecessors(start->nextBBId) {
    Visitor::run(start->entry, [&](BB* bb) {
        auto apply = [&](BB* next) {
            if (!next)
                return;
            if (!bb->predecessors().count(next)) {
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
        for (auto pre2 : pre1->predecessors()) {
            if (!isPredecessor(pre2, bb)) {
                transitivePredecessors[bb->id].push_back(pre2);
                complete(bb, pre2);
            }
        }
    };

    Visitor::run(start->entry, [&](BB* bb) {
        for (auto pre1 : bb->predecessors())
            complete(bb, pre1);
    });
}

bool CFG::isPredecessor(BB* a, BB* b) const {
    auto& preds = transitivePredecessors[b->id];
    return std::any_of(
        preds.begin(), preds.end(),
        std::bind(std::equal_to<BB*>(), std::placeholders::_1, a));
}

bool DominanceGraph::DomTree::merge(const DomTree& other) {
    bool changed = false;
    auto it = container.begin();
    while (it != container.end()) {
        if (std::find(other.container.begin(), other.container.end(), *it) ==
            other.container.end()) {
            it = container.erase(it);
            changed = true;
        } else {
            it++;
        }
    }
    return changed;
}

DominanceGraph::DominanceGraph(Code* start) : dominating(start->nextBBId) {
    // Static Analysis computes the set of all dominating bb's, for every bb
    // reachable from start. Runs until none of the sets grow anymore.

    std::stack<BB*> todo;
    todo.push(start->entry);

    while (!todo.empty()) {
        BB* cur = todo.top();
        DomTree curState = dominating[cur->id];
        curState.push_back(cur);
        curState.seen = true;

        todo.pop();

        auto apply = [&](BB* bb) {
            if (!bb)
                return;

            auto& d = dominating[bb->id];
            if (!d.seen) {
                d = curState;
                todo.push(bb);
                return;
            }

            if (d.merge(curState))
                todo.push(bb);
        };
        apply(cur->trueBranch());
        apply(cur->falseBranch());
    }
}

DominanceGraph::BBSet DominanceGraph::dominatedSet(Code* start,
                                                   const BBSet& input) {
    // Given a set of BBs, compute the set of BBs dominated by the input set.
    // Inductive definition:
    // - a BB is dominated by the input set if it is contained in the input set.
    // - a BB is dominated by the input set if all its inputs are dominated by
    //   the input set.

    BBSet result;
    BBSet seen;
    std::stack<BB*> todo;
    todo.push(start->entry);

    while (!todo.empty()) {
        BB* cur = todo.top();
        todo.pop();

        // Is the current BB dominated by the input set?
        bool curState = result.count(cur);

        // If not _and_ the current BB is in the input set, then the BB is
        // dominated; put it in the result set and update our current state.
        if (!curState && input.count(cur)) {
            result.insert(cur);
            curState = true;
        }

        auto apply = [&](BB* bb) {
            if (!bb) {
                return;
            }

            // Have we already processed this child BB?
            if (!seen.count(bb)) {
                seen.insert(bb);

                // Our parent is dominated, so tentatively assume we're
                // dominated; then add ourself to the worklist.
                if (curState) {
                    result.insert(bb);
                }
                todo.push(bb);
                return;
            }

            if (!curState) {
                auto position = result.find(bb);
                if (position != result.end()) {
                    // Our parent is _not_ dominated, but we're in the result
                    // set. We were wrong, so remove ourself from the result;
                    // then add ourself to the worklist.
                    result.erase(position);
                    todo.push(bb);
                }
            }
        };
        apply(cur->trueBranch());
        apply(cur->falseBranch());
    }

    return result;
}

bool DominanceGraph::dominates(BB* a, BB* b) const {
    const auto& doms = dominating[b->id].container;
    return std::find(doms.begin(), doms.end(), a) != doms.end();
}

bool DominanceGraph::immediatelyDominates(BB* a, BB* b) const {
    const auto& doms = dominating[b->id].container;
    if (doms.empty())
        return false;
    return doms[doms.size() - 1] == a;
}

bool DominanceGraph::hasImmediateDominator(BB* bb) const {
    const auto& doms = dominating[bb->id].container;
    return doms.size() > 0;
}

BB* DominanceGraph::immediateDominator(BB* bb) const {
    const auto& doms = dominating[bb->id].container;
    return doms[doms.size() - 1];
}

const DominanceGraph::BBList& DominanceGraph::dominators(BB* bb) const {
    return dominating[bb->id].container;
}

void DominanceGraph::dominatorTreeNext(
    BB* bb, const std::function<void(BB*)>& apply) const {
    BreadthFirstVisitor::run(bb, [&](BB* b) {
        if (immediatelyDominates(bb, b))
            apply(b);
    });
};

const DominanceFrontier::BBList& DominanceFrontier::at(BB* bb) const {
    return frontier[bb->id];
}

DominanceFrontier::DominanceFrontier(Code* code, const DominanceGraph& dom) {
    frontier.resize(code->nextBBId);
    Visitor::run(code->entry, [&](BB* n) {
        for (const auto& p : n->predecessors()) {
            auto r = p;
            while (r != dom.immediateDominator(n)) {
                frontier[r->id].insert(n);
                r = dom.immediateDominator(r);
            }
        }
    });
}

UsesTree::UsesTree(Code* start) {
    Visitor::run(start->entry, [&](Instruction* instruction) {
        instruction->eachArg([&](Value* v) {
            if (auto usage = Instruction::Cast(v)) {
                uses[usage].insert(instruction);
            }
        });
    });
}

const UsesTree::DependenciesList& UsesTree::at(Instruction* i) const {
    if (uses.count(i))
        return uses.at(i);
    else
        return empty;
}
}
}
