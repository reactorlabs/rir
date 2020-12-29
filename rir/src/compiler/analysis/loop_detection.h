#ifndef PIR_LOOP_DETECTION_H
#define PIR_LOOP_DETECTION_H

#include "compiler/analysis/cfg.h"
#include "compiler/pir/pir.h"
#include "compiler/util/bb_transform.h"

#include <unordered_map>
#include <unordered_set>

//#define DEBUG_LOOP_DETECTION

// TODO: Currently we use the straightforward algorithm in the Dragon book to
// identify loops. Note that the approach cannot handle irreducible CFGs. Then
// we take the naive approach to compute the loop-nesting forest (we sort loops
// by size and compare them to each other).
// There are fancier and more efficient algorithms, e.g.:
//   - P. Havlak
//     Nesting of reducible and irreducible loops
//     https://doi.org/10.1145/262004.262005
//   - G. Ramalingam
//     Identifying loops in almost linear time
//     https://doi.org/10.1145/316686.316687
//   - G. Ramalingam
//     On loops, dominators, and dominance frontiers
//     https://doi.org/10.1145/570886.570887

namespace rir {
namespace pir {

class LoopDetection {
  public:
    typedef std::vector<BB*> BBList;
    typedef std::unordered_set<BB*> BBSet;
    typedef std::function<bool(Instruction*)> InstrActionPredicate;

    // A natural loop is defined by a header h, and a back edge n->h where h
    // dominates n. If there are multiple back edges to the same header, the
    // body is the union of all the nodes computed for each back edge.
    class Loop {
        // target of loop entry edge(s) and back edge(s)
        BB* header_;
        // set of all nodes in the loop, including the header and tails
        BBSet body_;
        // pointer (or nullptr) to (nearest) outer loop containing this loop
        Loop* outer_;
        // true if this loop does not contain another loop
        bool isInnermost_;

      public:
        Loop(BB* header, const BBSet& body)
            : header_(header), body_(std::move(body)), outer_(nullptr),
              isInnermost_(true) {}

        Loop(const Loop&) = delete;
        Loop& operator=(const Loop&) = delete;
        Loop(Loop&&) = default;
        Loop& operator=(Loop&&) = default;

        BB* header() const { return header_; }
        size_t size() const { return body_.size(); }
        bool contains(BB* node) const { return body_.count(node); }
        bool isInnermost() const { return isInnermost_; }
        void setOuterLoop(Loop* o) {
            outer_ = o;
            o->isInnermost_ = false;
        }

        /*
         * TODO: finds a preheader when there is one.
         * We should create a preheader when there is non.
         */
        BB* preheader() {
            BBList outOfLoopPredecessor;
            for (const auto& pred : header()->predecessors()) {
                if (!body_.count(pred))
                    outOfLoopPredecessor.push_back(pred);
            }

            return outOfLoopPredecessor.size() != 1
                       ? nullptr
                       : outOfLoopPredecessor.front();
        }

        bool check(const InstrActionPredicate& action) const {
            for (auto bb : body_) {
                for (auto instruction : *bb) {
                    if (!action(instruction))
                        return false;
                }
            }
            return true;
        }

        typedef BBSet::iterator iterator;
        typedef BBSet::const_iterator const_iterator;

        iterator begin() { return body_.begin(); }
        iterator end() { return body_.end(); }
        const_iterator begin() const { return body_.begin(); }
        const_iterator end() const { return body_.end(); }
        void print(std::ostream& out, bool tty);
    };

    explicit LoopDetection(Code* code, bool determineNesting = false);

    LoopDetection(const LoopDetection&) = delete;
    LoopDetection& operator=(const LoopDetection&) = delete;
    LoopDetection(LoopDetection&&) = default;
    LoopDetection& operator=(LoopDetection&&) = default;

    typedef std::vector<Loop>::iterator iterator;
    typedef std::vector<Loop>::const_iterator const_iterator;

    iterator begin() { return loops.begin(); }
    iterator end() { return loops.end(); }
    const_iterator begin() const { return loops.begin(); }
    const_iterator end() const { return loops.end(); }

  private:
    std::vector<Loop> loops;
};

} // namespace pir
} // namespace rir

#endif
