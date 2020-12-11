#include "loop_detection.h"
#include "../util/visitor.h"

namespace rir {
namespace pir {

LoopDetection::LoopDetection(Code* code, bool determineNesting) {
    DominanceGraph dom(code);
    // map of header nodes to tail nodes
    std::unordered_map<BB*, BBList> tailNodes;

    // find back edges, i.e. edges tail->header where header dominates tail
    Visitor::run(code->entry, [&](BB* maybeHeader) {
        for (const auto& maybeTail : maybeHeader->predecessors()) {
            if (dom.strictlyDominates(maybeHeader, maybeTail)) {
                if (tailNodes.count(maybeHeader)) {
                    tailNodes[maybeHeader].push_back(maybeTail);
                } else {
                    tailNodes.emplace(maybeHeader, BBList({maybeTail}));
                }
            }
        }
    });

    // for each loop header, find the nodes in the loop
    for (auto& l : tailNodes) {
        const auto& header = l.first;
        auto& todo = l.second;
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
                for (const auto& p : cur->predecessors()) {
                    todo.push_back(p);
                }
            }
        }

        loops.emplace_back(Loop{header, body});
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
}

void LoopDetection::Loop::print(std::ostream& out, bool tty) {
    out << "==========\nLoop Start:\n";
    for (auto bb : body_) {
        bb->print(out, tty);
    }
    out << "==========\nLoop End";
}

} // namespace pir
} // namespace rir
