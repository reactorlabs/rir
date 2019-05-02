#include "loop_detection.h"
#include "../util/visitor.h"

namespace rir {
namespace pir {

bool LoopDetection::Loop::comesBefore(Instruction* a, Instruction* b) const {
    if (a->bb() == b->bb()) {
        return a->id().idx() < b->id().idx();
    } else {
        // ordering vector is reversed: larger means precedes
        return ordering_.at(a->bb()) > ordering_.at(b->bb());
    }
}

LoopDetection::LoopDetection(Code* code, bool determineNesting) {
    CFG cfg(code);
    DominanceGraph dom(code);
    // map of header nodes to tail nodes
    std::unordered_map<BB*, BBList> tailNodes;

    // find back edges, i.e. edges n->h where h dominates n
    Visitor::run(code->entry, [&](BB* maybeHeader) {
        for (const auto& n : cfg.immediatePredecessors(maybeHeader)) {
            if (dom.dominates(maybeHeader, n)) {
                if (tailNodes.count(maybeHeader)) {
                    tailNodes[maybeHeader].push_back(n);
                } else {
                    tailNodes.emplace(maybeHeader, BBList({n}));
                }
            }
        }
    });

    // for each loop header, find the nodes in the loop
    for (auto& l : tailNodes) {
        const auto& header = l.first;
        auto& todo = l.second;
        std::unordered_map<BB*, size_t> ordering;
        size_t counter = 0;
        BBSet body = {header}; // header is part of loop body

#ifdef DEBUG_LOOP_DETECTION
        std::cerr << "\n========== Detected loop ==========";
        std::cerr << "\n\tHeader: " << header->id;
        std::cerr << "\n\tTails:";
        for (const auto& t : todo) {
            std::cerr << " " << t->id;
        }
        std::cerr << "\n\tBody (in order of discovery):";
#endif

        // depth-first search starting from the loop tail(s)
        while (!todo.empty()) {
            BB* cur = todo.back();
#ifdef DEBUG_LOOP_DETECTION
            std::cerr << " " << cur->id;
#endif
            todo.pop_back();
            if (!body.count(cur)) {
                body.insert(cur);
                ordering[cur] = counter++;
                for (const auto& p : cfg.immediatePredecessors(cur)) {
                    todo.push_back(p);
                }
            }
        }

        // header node is topologically last
        ordering[header] = counter++;
        loops.emplace_back(Loop{header, body, ordering});
    }

    // reconstruct the loop hierarchy
    if (determineNesting) {
        // sort in ascending order of loop body size
        std::sort(begin(), end(), [](const Loop& a, const Loop& b) {
            return a.size() < b.size();
        });

#ifdef DEBUG_LOOP_DETECTION
        std::cerr << "\n========== Determined loop hierarchy ==========\n";
#endif

        // compare each loop l to every other loop l'
        // the smallest l' such that l is inside l' is the (nearest) outer loop
        for (auto it = begin(); it != end(); ++it) {
            for (auto it2 = it + 1; it2 != end(); ++it2) {
                if (it2->contains(it->header())) {
#ifdef DEBUG_LOOP_DETECTION
                    std::cerr << "\tLoop " << it->header()->id
                              << " is directly inside loop "
                              << it2->header()->id << "\n";
#endif
                    it->setOuterLoop(&*it2);
                    break;
                }
            }
        }
    }

    // TODO: compute or add the loop preheader
}

} // namespace pir
} // namespace rir
