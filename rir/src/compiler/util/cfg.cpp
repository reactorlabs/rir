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
    // We use the Lengauer-Tarjan algorithm [LT79] for computing dominators.
    // The algorithmic complexity is O(N log N), where N is the number of edges
    // plus the number of nodes.
    //
    // A later paper [GTW06] studied various practical dominator algorithms,
    // and suggested that the simple Lengauer-Tarjan algorithm was the most
    // consistently fast algorithm in their experiments, and also less
    // sensitive to pathological examples.
    //
    // This implementation is a fairly close translation of the pseudocode from
    // [AP04]. The details of the algorithm are fairly technical, but can be
    // found in the same place. (Note: [AP04] is clearer than [LT79] at
    // explaining the algorithm.)
    //
    //
    // The algorithm can be divided into four steps.
    //
    //   Step 1.
    //     Perform a depth-first search of the graph, numbering vertices in
    //     order of discovery. This number (dfnum) allows us to determine if a
    //     node `a` is an ancestor (in the DFS tree) of `b`, assuming there is
    //     a path from `a` to `b` in the CFG.
    //
    //     `a` is an ancestor of `b` if dfnum[a] < dfnum[b]. `a` is a proper
    //     ancestor of `b` if `a` is an ancestor of `b` and `a` is not equal to
    //     `b`.
    //
    //   Step 2.
    //     Iterate over nodes in decreasing dfnum. Calculate semidominators
    //     using the Semidominator Theorem. A node's semidominator is often
    //     that node's immediate dominator. As each node is visited, we insert
    //     it into the spanning forest.
    //
    //     If `s` is the semidominator of `n`, then it is the node with the
    //     smallest dfnum with a path to `n` whose nodes (other than `s` and
    //     `n`) are not ancestors of `n`. In other words, there is a path in
    //     the CFG (but not the DFS tree) that departs from `s` and then
    //     rejoins the tree at `n`.
    //
    //     Semidominator Theorem.
    //       For all predecessors of `n`:
    //        1. If `v` is a proper ancestor of `n` (i.e. dfnum[v] < dfnum[n]),
    //           then `v` is a candidate for semi(n).
    //        2. If `v` is a nonancestor of `n` (i.e. dfnum[v] > dfnum[n]),
    //           then for each ancestor `u` of `v` (or u = v) that is not an
    //           ancestor of `n`, semi(u) is a candidate for semi(n).
    //       The candidate with smallest dfnum is the semidominator of `n`.
    //
    //   Step 3.
    //     Implicitly define the immediate dominator by applying the first
    //     clause of the Dominator Theorem.
    //
    //   Step 4.
    //     Explicitly define the immediate dominator by applying the second
    //     clause of the Dominator Theorem. Do this while iterating over nodes
    //     in increasing dfnum.
    //
    //     Dominator Theorem.
    //       On the DFS tree path below semi(n) and above or including `n`, let
    //       `y` be the node the the smallest numbered semidominator, i.e. with
    //       minimum dfnum[semi(y)]. Then:
    //         1. idom(n) = semi(n)    if semi(y) == semi(n)
    //         2. idom(n) = idom(y)    if semi(y) != semi(n)
    //
    //
    // As the algorithm runs, it maintains a spanning forest of nodes -- at the
    // end, the forest will be fully connected as a single spanning tree. The
    // algorithm is designed so that when a node `n` is being processed, only
    // nodes with a higher dfnum than `n`, i.e. nonancestors of `n`, will be in
    // the forest.
    //
    // This spanning forest is represented by the `ancestor` array, updated by
    // the `link` function, and queried by the `ancestorWithLowestSemi`
    // function.
    //
    // Note that the naive implementation of `ancestorWithLowestSemi` is O(N),
    // but this implementation performs "path compression," so its amortized
    // complexity is O(log N). There is an even more sophisticated version with
    // better algorithmic complexity [LT79], that performs "balanced path
    // compression," but in practice, it is actually slower [GTW06].
    //
    // As `ancestorWithLowestSemi` searches the spanning forest, it updates
    // the `ancestor` array to compress the path. However, as it does so, it
    // must remember the best result in the path that was skipped, i.e. the
    // node with semidominator with lowest dfnum.
    //
    //
    // References:
    //
    // [AP04] Appel, A. and Palsberg, J. (2004). "Efficient Computation of the
    // Dominator Tree." In "Modern Compiler Implementation in Java," 2nd ed.
    //
    // [GTW06] Georgiadis, L., Tarjan, R. E., and Werneck R. F. (2006).
    // "Finding dominators in practice." J. Graph Algorithms Appl.
    // http://doi.org/10.7155/jgaa.00119
    //
    // [LT79] Lengauer, T. and Tarjan, R. E. (1979). "A fast algorithm for
    // finding dominators in a flowgraph." ACM Trans. Program. Lang. Syst.
    // https://doi.org/10.1145/357062.357071

    /***************************************************************************
     * Declarations and initializations.
     **************************************************************************/

    // Note that this is an overapproximation of the actual number of BBs,
    // because some BBs may have been deleted.
    int size = start->nextBBId;

    // Counter for numbering BBs, by depth-first search order.
    int N = 0;

    // Indexed by BB id. `dfnum[n->id]` is the dfnum of node `n`.
    std::vector<int> dfnum(size);

    // Indexed by dfnum. `vertex[i]` is the node whose dfnum is `i`. Note that
    // `vertex[dfnum[n->id]] == n`.
    std::vector<BB*> vertex(size);

    // Indexed by BB id. `parent[n->id]` is the node that is the parent of `n`
    // in the DFS tree.
    BBList parent(size);

    // Indexed by BB id. `semi[n->id]` is the semidominator of `n`.
    BBList semi(size);

    // Indexed by BB id. `n` has the same dominator as `samedom[n->id]`, i.e.,
    // idom(n) == idom(samedom(n)). This is used for the deferred application
    // of the Dominator Theorem, clause 2.
    BBList samedom(size);

    // Indexed by BB id. `bucket[s->id]` is the set of nodes that `s`
    // semidominates. Used to defer the dominator calculation until after
    // linking.
    std::vector<SmallSet<BB*>> bucket(size);

    // Indexed by BB id. Spanning forest for the CFG. `ancestor[n->id]` is an
    // ancestor (not necessarily parent) of `n`, i.e., any node above `n` in
    // the spanning forest. This is gradually built as the algorithm runs; at
    // the end, it will be a spanning tree.
    BBList ancestor(size);

    // Indexed by BB id. While `ancestor[n->id]` points to some ancestor of
    // `n`, it may skip over some nodes. We need to remember the node along
    // that possibly skipped path (which includes `n` but not
    // `ancestor[n->id]`) which has the semidominator with lowest dfnum; that
    // node is `best[n->id]`.
    BBList best(size);

    // Add the edge `n -> p` (where `p` is the parent of `n`) to the spanning
    // forest, represented by `ancestor`.
    std::function<void(BB*, BB*)> link = [&](BB* p, BB* n) {
        // Initially `ancestor[n->id]` points to `n`'s parent.
        ancestor[n->id] = p;

        // Initially only `n` is in the path to its ancestor, so it is our
        // current best.
        best[n->id] = n;
    };

    // Search upward in the spanning forest, represented by `ancestor`, for the
    // nonroot ancestor of `v` whose semidominator has the smallest dfnum. Note
    // that `ancestor` is built as the algorithm runs, so some nodes are (by
    // design) missing from the forest.
    std::function<BB*(BB*)> ancestorWithLowestSemi = [&](BB* v) {
        BB* a = ancestor[v->id];
        if (ancestor[a->id] != nullptr) {
            BB* b = ancestorWithLowestSemi(a);

            // Compress the path as we walk up the spanning forest. We want to
            // skip `a`, which is currently `v`'s ancestor.
            ancestor[v->id] = ancestor[a->id];

            // However, `b` might be the new "best" node, i.e. the node whose
            // semidominator has the lowest dfnum. We need to update
            // `best[v->id]` if this is the case.
            BB* curBest = best[v->id];
            if (dfnum[semi[b->id]->id] < dfnum[semi[curBest->id]->id]) {
                best[v->id] = b;
            }
        }
        return best[v->id];
    };

    /***************************************************************************
     * Algorithm starts here.
     **************************************************************************/

    // Step 1. Number nodes by depth-first number. We don't need a function
    // because our DFS is iterative, with an explicit stack -- in fact, we use
    // two stacks in parallel, for nodes and their parents.
    std::stack<BB*> nodes;
    std::stack<BB*> parents;
    nodes.push(start->entry);
    parents.push(nullptr);

    while (!nodes.empty()) {
        assert(nodes.size() == parents.size());
        BB* n = nodes.top();
        BB* p = parents.top();
        nodes.pop();
        parents.pop();

        // We haven't visited or numbered `n` yet.
        if (dfnum[n->id] == 0) {
            dfnum[n->id] = N;
            vertex[N] = n;
            parent[n->id] = p;
            N++;

            for (const auto& w : n->successors()) {
                nodes.push(w);
                parents.push(n);
            }
        }
    }

    // Step 2. Iterate over nodes (skipping the root) in descending dfnum
    // order.
    for (int i = N - 1; i >= 1; --i) {
        BB* n = vertex[i];
        BB* p = parent[n->id];

        // Parent of `n` is a candidate semidominator (since it's a proper
        // ancestor).
        BB* s = p;

        // Calculate `s`, the semidominator of `n`, using the Semidominator
        // Theorem. We need to consider all predecessors `v` of `n`.
        for (const auto& v : n->predecessors()) {
            BB* s1;
            if (dfnum[v->id] <= dfnum[n->id]) {
                // Semidominator Theorem, clause 1: `v` is an ancestor of `n`,
                // so it is a candidate semidominator.
                s1 = v;
            } else {
                // Semidominator Theorem, clause 2: `v` is a nonancestor of
                // `n`. For each `u` that is an ancestor of `v` (or u = v) but
                // not an ancestor of `n`, the semi(u) with the lowest dfnum is
                // a candidate. Note that when `n` is processed, only nodes
                // with a higher dfnum than `n` (i.e. nonancestors) will be in
                // the forest.
                BB* u = ancestorWithLowestSemi(v);
                s1 = semi[u->id];
            }
            if (dfnum[s1->id] < dfnum[s->id]) {
                // Take the one with the lowest dfnum as the new candidate
                // semidominator. After this loop, we will have found the
                // candidate with the lowest dfnum.
                s = s1;
            }
        }

        // Calculation of `n`'s dominator is deferred until the path from `s`
        // to `n` has been linked into the forest. We don't yet know `y`, the
        // lowest semidominator on the path from `s` to `n` is. Therefore, we
        // put `n` into the bucket of all nodes that `s` semidominates, so we
        // can process it after linking.
        semi[n->id] = s;
        bucket[s->id].insert(n);
        link(p, n);

        // Step 3. Now that the path from `p` to `v` has been linked into the
        // spanning forest, calculate the dominator of `v`, based on the first
        // clause of the Dominator Theorem, or else defer calculation until
        // y`'s dominator is known.
        for (const auto& v : bucket[p->id]) {
            // Find `y`, the ancestor of `v` whose semidominator has the lowest
            // dfnum. Note that the spanning forest only contains nodes with a
            // higher dfnum than `n`.
            BB* y = ancestorWithLowestSemi(v);
            if (semi[y->id] == semi[v->id]) {
                // Dominator Theorem, clause 1: semi(v) = p is the immediate
                // dominator of `v`.
                idom[v->id] = p;
            } else {
                // Dominator Theorem, clause 2: idom(y) is the immediate
                // dominator of `v`. But we don't yet know what idom(y) is, so
                // defer this calculation.
                samedom[v->id] = y;
            }
        }
        bucket[p->id].clear();
    }

    // Step 4. Iterate over the nodes (skipping the root) in ascending dfnum
    // order.
    for (int i = 1; i < N; ++i) {
        BB* n = vertex[i];
        // Perform the deferred dominator calculations, based on the second
        // clause of the Dominator Theorem.
        if (samedom[n->id] != nullptr) {
            // idom(n) = idom(y), and we had previously assigned `y` to
            // samedom(n).
            BB* y = samedom[n->id];
            idom[n->id] = idom[y->id];
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
    // Start with node `b`, because `a` dominates `b` if `a` equals `b`. Then
    // walk up the dominator tree, comparing each visited node to `a`.
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
    // A node bb dominates itself.
    BBSet result{bb};

    // Walk the dominator tree and insert each immediate dominator into the
    // result set.
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
