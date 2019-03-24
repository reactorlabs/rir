#include "liveness.h"
#include "../../pir/bb.h"
#include "../../pir/instruction.h"

#include <map>
#include <set>

namespace rir {
namespace pir {

LivenessIntervals::LivenessIntervals(unsigned bbsSize, CFG const& cfg) {

    // temp list of live out sets for every BB
    std::unordered_map<BB*, std::set<Value*>> liveAtEnd(bbsSize);

    std::set<BB*> todo;
    for (auto e : cfg.exits())
        todo.insert(e);

    while (!todo.empty()) {
        BB* bb = *todo.begin();
        todo.erase(todo.begin());

        // keep track of currently live variables
        std::set<Value*> accumulated;
        std::map<BB*, std::set<Value*>> accumulatedPhiInput;

        // Mark all (backwards) incoming live variables
        for (auto v : liveAtEnd[bb]) {
            assert(count(v));
            auto& liveRange = at(v)[bb->id];
            if (!liveRange.live || liveRange.end < bb->size()) {
                liveRange.live = true;
                liveRange.end = bb->size();
                accumulated.insert(v);
            }
        }

        // Run BB in reverse
        size_t pos = bb->size();
        if (!bb->isEmpty()) {
            auto ip = bb->end();
            do {
                --ip;
                --pos;
                Instruction* i = *ip;

                auto markIfNotSeen = [&](Value* v) {
                    if (!count(v)) {
                        // First time we see this variable, need to allocate
                        // vector of all liveranges
                        operator[](v).resize(bbsSize);
                        assert(!operator[](v)[bb->id].live);
                    }
                    auto& liveRange = operator[](v)[bb->id];
                    if (!liveRange.live) {
                        liveRange.live = true;
                        liveRange.end = pos;
                        return true;
                    }
                    return false;
                };

                // First set all arguments to be live
                if (auto phi = Phi::Cast(i)) {
                    phi->eachArg([&](BB* in, Value* v) {
                        if (markIfNotSeen(v))
                            accumulatedPhiInput[in].insert(v);
                    });
                } else {
                    i->eachArg([&](Value* v) {
                        if (markIfNotSeen(v))
                            accumulated.insert(v);
                    });
                }

                // Mark the end of the current instructions liveness
                if (accumulated.count(i)) {
                    assert(count(i));
                    auto& liveRange = operator[](i)[bb->id];
                    assert(liveRange.live);
                    liveRange.begin = pos;
                    accumulated.erase(accumulated.find(i));
                }
            } while (ip != bb->begin());
        }
        assert(pos == 0);

        // Mark everything that is live at the beginning of the BB.
        auto markLiveEntry = [&](Value* v) {
            assert(count(v));
            auto& liveRange = operator[](v)[bb->id];
            assert(liveRange.live);
            liveRange.begin = 0;
        };

        for (auto v : accumulated)
            markLiveEntry(v);
        for (auto pi : accumulatedPhiInput)
            for (auto v : pi.second)
                markLiveEntry(v);

        // Merge everything that is live at the beginning of the BB into the
        // incoming vars of all predecessors
        //
        // Phi inputs should only be merged to BB that are successors of the
        // input BBs
        auto merge = [&](BB* bb, const std::set<Value*>& live) {
            auto& liveOut = liveAtEnd[bb];
            if (!std::includes(liveOut.begin(), liveOut.end(), live.begin(),
                               live.end())) {
                liveOut.insert(live.begin(), live.end());
                todo.insert(bb);
            }
        };
        auto mergePhiInp = [&](BB* bb) {
            for (auto in : accumulatedPhiInput) {
                auto inBB = in.first;
                auto inLive = in.second;
                if (bb == inBB || cfg.isPredecessor(inBB, bb)) {
                    merge(bb, inLive);
                }
            }
        };
        for (auto pre : cfg.immediatePredecessors(bb)) {
            bool firstTime = !liveAtEnd.count(pre);
            if (firstTime) {
                liveAtEnd[pre] = accumulated;
                mergePhiInp(pre);
                todo.insert(pre);
            } else {
                merge(pre, accumulated);
                mergePhiInp(pre);
            }
        }
    }
}

bool LivenessIntervals::live(Instruction* where, Value* what) const {
    if (!what->isInstruction() || count(what) == 0)
        return false;
    auto bbLiveness = at(what)[where->bb()->id];
    if (!bbLiveness.live)
        return false;
    unsigned idx = where->bb()->indexOf(where);
    return bbLiveness.begin <= idx && idx < bbLiveness.end;
}

} // namespace pir
} // namespace rir
