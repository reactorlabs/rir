#include "liveness.h"
#include "../pir/bb.h"
#include "../pir/instruction.h"
#include "../util/visitor.h"
#include "compiler/analysis/cfg.h"

#include <map>
#include <set>

namespace rir {
namespace pir {

LivenessIntervals::LivenessIntervals(Code* code, unsigned bbsSize) {
    // temp list of live out sets for every BB
    // ordered set so we can use std::includes
    std::unordered_map<BB*, std::set<Value*>> liveAtEnd(bbsSize);

    DominanceGraph dom(code);
    // this is a backwards analysis, starting from CFG exits
    std::unordered_set<BB*> todo;
    Visitor::run(code->entry, [&](BB* bb) {
        if (bb->isExit())
            todo.insert(bb);
    });

restart:
    while (!todo.empty()) {
        BB* bb = *todo.begin();
        todo.erase(todo.begin());

        // keep track of currently live variables
        // ordered sets so we can use std::includes
        std::set<Value*> accumulated;
        std::unordered_map<BB*, std::set<Value*>> accumulatedPhiInput;

        // Mark all (backwards) incoming live variables
        for (const auto& v : liveAtEnd[bb]) {
            assert(count(v));
            auto& liveRange = intervals[v][bb->id];
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
                        intervals[v].resize(bbsSize);
                        assert(!intervals[v][bb->id].live);
                    }
                    auto& liveRange = intervals[v][bb->id];
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
                        if (markIfNotSeen(v)) {
                            accumulated.insert(v);
                        }
                    });
                }

                // Mark the end of the current instructions liveness
                if (accumulated.count(i)) {
                    assert(count(i));
                    auto& liveRange = intervals[i][bb->id];
                    assert(liveRange.live);
                    liveRange.begin = pos;
                    accumulated.erase(accumulated.find(i));
                }

                if (accumulated.size() > maxLive)
                    maxLive = accumulated.size();

            } while (ip != bb->begin());
        }
        assert(pos == 0);

        // Mark everything that is live at the beginning of the BB.
        // Note that we need a separate `liveAtEntry` flag; begin = 0 cannot
        // distinguish between liveness before or after the first instruction.
        auto markLiveEntry = [&](Value* v) {
            assert(count(v));
            auto& liveRange = intervals[v][bb->id];
            assert(liveRange.live);
            liveRange.liveAtEntry = true;
            liveRange.begin = 0;
        };
        for (const auto& v : accumulated)
            markLiveEntry(v);
        for (const auto& pi : accumulatedPhiInput)
            for (const auto& v : pi.second)
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
            for (const auto& in : accumulatedPhiInput) {
                auto& inBB = in.first;
                auto& inLive = in.second;
                if (bb == inBB) {
                    merge(bb, inLive);
                }
            }
        };
        for (const auto& pre : bb->predecessors()) {
            if (!liveAtEnd.count(pre)) {
                liveAtEnd[pre] = accumulated;
                mergePhiInp(pre);
                todo.insert(pre);
            } else {
                merge(pre, accumulated);
                mergePhiInp(pre);
            }
        }
    }

    // enqueue nodes that are alive and have a non-live successor
    // to be processed on the next iteration
    for (auto p : liveAtEnd) {
        auto bb = p.first;
        for (auto n : bb->successors()) {
            if (!liveAtEnd.count(n))
                todo.insert(n);
        }
    }

    if (!todo.empty())
        goto restart;

#ifdef DEBUG_LIVENESS
    for (const auto& kv : intervals) {
        const auto& instr = Instruction::Cast(kv.first);
        const auto& liveVec = kv.second;

        std::cerr << "========== Liveness info for ";
        if (instr) {
            std::cerr << "instr:\n";
            instr->print(std::cerr, false);
        } else {
            std::cerr << "non-instr val:\n";
            kv.first->printRef(std::cerr);
        }
        std::cerr << "\n";

        for (size_t i = 0; i < liveVec.size(); ++i) {
            const auto& bbl = liveVec[i];
            if (bbl.live) {
                std::cerr << "\tLive in BB" << i << ": "
                          << (bbl.liveAtEntry ? "!" : "") << "[" << bbl.begin
                          << ", " << bbl.end << ")\n";
            }
        }
    }
#endif
}

bool LivenessIntervals::live(Instruction* where, Value* what) const {
    if (!what->isInstruction() || count(what) == 0)
        return false;
    const auto& bbLiveness = intervals.at(what)[where->bb()->id];
    if (!bbLiveness.live)
        return false;
    unsigned idx = where->bb()->indexOf(where);
    return bbLiveness.begin <= idx && idx < bbLiveness.end;
}

bool LivenessIntervals::live(const BB::Instrs::iterator& where,
                             Value* what) const {
    if (!what->isInstruction() || count(what) == 0)
        return false;
    const auto& bbLiveness = intervals.at(what)[(*where)->bb()->id];
    if (!bbLiveness.live)
        return false;
    unsigned idx = where - (*where)->bb()->begin();
    return bbLiveness.begin <= idx && idx < bbLiveness.end;
}

bool LivenessIntervals::interfere(Value* v1, Value* v2) const {
    const auto& l1 = intervals.at(v1);
    const auto& l2 = intervals.at(v2);
    assert(l1.size() == l2.size());

    for (size_t i = 0; i < l1.size(); ++i) {
        const auto& int1 = l1[i];
        const auto& int2 = l2[i];
        if (int1.live && int2.live) {
            if (int1.begin <= int1.end) {
                if (int2.begin <= int2.end) {
                    if (int1.begin < int2.end && int2.begin < int1.end)
                        return true;
                } else {
                    if (int1.begin < int2.end || int2.begin < int1.end)
                        return true;
                }
            } else {
                if (int2.begin <= int2.end) {
                    if (int1.begin < int2.end || int2.begin < int1.end)
                        return true;
                } else {
                    return true;
                }
            }
        }
    }
    return false;
}

bool LivenessIntervals::liveAtBBEntry(BB* bb, Value* what) const {
    if (count(what) == 0)
        return false;
    const auto& bbLiveness = intervals.at(what)[bb->id];
    return bbLiveness.live && bbLiveness.liveAtEntry;
}

} // namespace pir
} // namespace rir
