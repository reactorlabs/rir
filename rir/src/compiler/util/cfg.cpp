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
        auto succs = bb->successors();
        for (auto suc : succs)
            apply(suc);
        if (succs.size() == 0)
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

DominanceGraph::DominanceGraph(Code* start) : idom(start->nextBBId) {
    int size = start->nextBBId;
    int N = 0;

    std::vector<SmallSet<BB*>> bucket(size);
    std::vector<int> dfnum(size, -1);
    BBList semi(size);
    BBList ancestor(size);
    BBList samedom(size);
    BBList parent(size);
    BBList best(size);
    BBList vertex;
    vertex.reserve(size); // can't assume BBs are contiguously numbered

    std::function<void()> DFS = [&]() {
        std::stack<BB*> nodes;
        std::stack<BB*> parents;
        nodes.push(start->entry);
        parents.push(nullptr);

        while (!nodes.empty()) {
            BB* n = nodes.top();
            BB* p = parents.top();
            nodes.pop();
            parents.pop();

            if (dfnum[n->id] == -1) {
                dfnum[n->id] = N;
                vertex.push_back(n);
                parent[n->id] = p;
                N++;

                for (const auto& w : n->successors()) {
                    nodes.push(w);
                    parents.push(n);
                }
            }
        }
    };

    std::function<BB*(BB*)> ancestorWithLowestSemi = [&](BB* v) {
        BB* a = ancestor[v->id];
        if (ancestor[a->id] != nullptr) {
            BB* b = ancestorWithLowestSemi(a);
            ancestor[v->id] = ancestor[a->id];
            if (dfnum[semi[b->id]->id] < dfnum[semi[best[v->id]->id]->id]) {
                best[v->id] = b;
            }
        }
        return best[v->id];
    };

    std::function<void(BB*, BB*)> link = [&](BB* p, BB* n) {
        ancestor[n->id] = p;
        best[n->id] = n;
    };

    // Number nodes by depth-first number.
    DFS();

    // Iterate over nodes (skipping the root) in reverse DFS order.
    for (int i = N - 1; i >= 1; --i) {
        BB* n = vertex[i];
        BB* p = parent[n->id];
        BB* s = p;

        // Calculate the semidominator of n, based on the Semidominator Theorem
        for (const auto& v : n->predecessors()) {
            BB* s1;
            if (dfnum[v->id] <= dfnum[n->id]) {
                s1 = v;
            } else {
                s1 = semi[ancestorWithLowestSemi(v)->id];
            }
            if (dfnum[s1->id] < dfnum[s->id]) {
                s = s1;
            }
        }

        // Calculation of n's dominator is deferred until the path from s to n
        // has been linked into the forest.
        semi[n->id] = s;
        bucket[s->id].insert(n);
        link(p, n);

        // Now that the path from p to v has been linked into the spanning
        // forest, calculate the dominator of v, based on the first clause of
        // the Dominator Theorem, or else defer calculation until y's dominator
        // is known.
        for (const auto& v : bucket[p->id]) {
            BB* y = ancestorWithLowestSemi(v);
            if (semi[y->id] == semi[v->id]) {
                idom[v->id] = p;
            } else {
                samedom[v->id] = y;
            }
        }
        bucket[p->id].clear();
    }

    for (int i = 1; i < N; ++i) {
        BB* n = vertex[i];
        // Perform the deferred dominator calculations, based on the second
        // clause of the Dominator THeorem.
        if (samedom[n->id] != nullptr) {
            idom[n->id] = idom[samedom[n->id]->id];
        }
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
        for (auto suc : cur->successors())
            apply(suc);
    }

    return result;
}

bool DominanceGraph::dominates(BB* a, BB* b) const {
    BB* dominator = b;
    while (dominator != nullptr) {
        if (dominator == a) {
            return true;
        }
        dominator = idom[dominator->id];
    }
    return false;
}

bool DominanceGraph::strictlyDominates(BB* a, BB* b) const {
    return a != b && dominates(a, b);
}

bool DominanceGraph::immediatelyDominates(BB* a, BB* b) const {
    return idom[b->id] == a;
}

bool DominanceGraph::hasImmediateDominator(BB* bb) const {
    return idom[bb->id] != nullptr;
}

BB* DominanceGraph::immediateDominator(BB* bb) const {
    assert(idom[bb->id] != nullptr);
    return idom[bb->id];
}

const DominanceGraph::BBSet DominanceGraph::dominators(BB* bb) const {
    BBSet result{bb};

    BB* dominator = idom[bb->id];
    while (dominator != nullptr) {
        result.insert(dominator);
        dominator = idom[dominator->id];
    }

    return result;
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
    // Algorithm taking from Figure 5 of Cooper, Harvey, and Kennedy, 2001.
    // A simple, fast dominance algorithm. Software Practice & Experience.
    // http://www.hipersoft.rice.edu/grads/publications/dom14.pdf
    frontier.resize(code->nextBBId);
    Visitor::run(code->entry, [&](BB* n) {
        // Nodes in dominance frontier represent join points, so skip if there
        // is only one predecessor.
        if (n->predecessors().size() < 2) {
            return;
        }
        for (const auto& p : n->predecessors()) {
            // Start at p and walk up the dominator tree.
            auto r = p;
            // Stop when we reach n's immediate dominator.
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
} // namespace pir
} // namespace rir
