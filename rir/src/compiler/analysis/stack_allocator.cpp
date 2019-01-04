#include "stack_allocator.h"
#include "../../ir/BC_inc.h"
#include "../pir/pir_impl.h"

#include <iterator>

namespace {
using namespace rir::pir;

struct StackAllocatorState {

    using MergeCompensation = StackAllocator::MergeCompensation;

    CFG const* cfg;
    MergeCompensation* mergeCompensation;

    Instruction* i;
    std::vector<Value*> requirements, additionalRequirements;

    AbstractResult merge(const StackAllocatorState& other) {

        // is it a problem if we have multi-pred and multi-succ at the same
        // time? ie not split-edge?

        assert(i);
        assert(other.i);

        BB* myBB = i->bb();
        BB* otherBB = other.i->bb();

        BB* commonPred = nullptr;

        for (auto pred : cfg->immediatePredecessors(myBB)) {
            for (auto otherPred : cfg->immediatePredecessors(otherBB)) {
                if (pred == otherPred) {
                    assert(!commonPred && "multiple common predecessors?");
                    commonPred = pred;
                }
            }
        }
        assert(commonPred);

        // deal with the already seen path by just adding pops...
        assert((*mergeCompensation)[commonPred].count(myBB) == 0);

        for (unsigned n = 0; n < additionalRequirements.size(); ++n) {
            (*mergeCompensation)[commonPred][myBB].push_back(
                std::make_pair(rir::Opcode::pop_, 0));
        }

        // deal with the newly found path
        assert((*mergeCompensation)[commonPred].count(otherBB) == 0);

        std::vector<Value*> source = requirements;
        source.insert(source.end(), additionalRequirements.begin(),
                      additionalRequirements.end());
        std::vector<Value*> target = other.requirements;

        //-------------------------

        {
            auto wl = source;

            std::unordered_map<Value*, unsigned> have, need;
            for (auto v : source)
                ++have[v];
            for (auto v : target)
                ++need[v];

            auto pop = [&]() {
                (*mergeCompensation)[commonPred][otherBB].push_back(
                    std::make_pair(rir::Opcode::pop_, 0));
                wl.pop_back();
            };

            auto pick = [&](unsigned offset) {
                if (offset == 0)
                    return;
                if (offset == 1) {
                    if ((*mergeCompensation)[commonPred][otherBB].size() &&
                        (*mergeCompensation)[commonPred][otherBB]
                                .back()
                                .first == rir::Opcode::swap_)
                        (*mergeCompensation)[commonPred][otherBB].pop_back();
                    else
                        (*mergeCompensation)[commonPred][otherBB].push_back(
                            std::make_pair(rir::Opcode::swap_, 0));
                } else {
                    if ((*mergeCompensation)[commonPred][otherBB].size() &&
                        (*mergeCompensation)[commonPred][otherBB]
                                .back()
                                .first == rir::Opcode::put_ &&
                        (*mergeCompensation)[commonPred][otherBB]
                                .back()
                                .second == offset)
                        (*mergeCompensation)[commonPred][otherBB].pop_back();
                    else
                        (*mergeCompensation)[commonPred][otherBB].push_back(
                            std::make_pair(rir::Opcode::pick_, offset));
                }
                wl.push_back(wl[wl.size() - offset - 1]);
                wl.erase(wl.end() - offset - 2);
            };

            auto put = [&](unsigned offset) {
                if (offset == 0)
                    return;
                if (offset == 1) {
                    if ((*mergeCompensation)[commonPred][otherBB].size() &&
                        (*mergeCompensation)[commonPred][otherBB]
                                .back()
                                .first == rir::Opcode::swap_)
                        (*mergeCompensation)[commonPred][otherBB].pop_back();
                    else
                        (*mergeCompensation)[commonPred][otherBB].push_back(
                            std::make_pair(rir::Opcode::swap_, 0));
                } else {
                    if ((*mergeCompensation)[commonPred][otherBB].size() &&
                        (*mergeCompensation)[commonPred][otherBB]
                                .back()
                                .first == rir::Opcode::pick_ &&
                        (*mergeCompensation)[commonPred][otherBB]
                                .back()
                                .second == offset)
                        (*mergeCompensation)[commonPred][otherBB].pop_back();
                    else
                        (*mergeCompensation)[commonPred][otherBB].push_back(
                            std::make_pair(rir::Opcode::put_, offset));
                }
                wl.insert(wl.end() - offset - 1, wl.back());
                wl.pop_back();
            };

            auto pull = [&](unsigned offset) {
                if (offset == 0) {
                    (*mergeCompensation)[commonPred][otherBB].push_back(
                        std::make_pair(rir::Opcode::dup_, 0));
                } else {
                    (*mergeCompensation)[commonPred][otherBB].push_back(
                        std::make_pair(rir::Opcode::pull_, offset));
                }
                wl.push_back(wl[wl.size() - offset - 1]);
            };

            {
                unsigned offset = 1;
                auto it = source.rbegin();
                while (it != source.rend()) {
                    auto next = it + 1;

                    if (need[*it] < have[*it]) {
                        pick(offset - 1);
                        pop();
                        --offset;
                        --have[*it];
                    } else if (need[*it] > have[*it]) {
                        while (need[*it] > have[*it]) {
                            pull(offset - 1);
                            ++offset;
                            ++have[*it];
                        }
                    }

                    ++offset;
                    it = next;
                }
            }

            assert(wl.size() == target.size());

            {
                unsigned offset = 1;
                auto it = target.begin();
                while (it != target.end()) {
                    auto next = it + 1;

                    int i = 0;
                    while (*it != *(wl.rbegin() + i))
                        ++i;
                    pick(i);
                    put(wl.size() - offset);

                    ++offset;
                    it = next;
                }
            }
        }

        return AbstractResult::None;
    }

    void print(std::ostream& out, bool tty) const {
        out << "== Result:";
        out << "\n        instruction: ";
        if (i)
            i->printRef(out);
        else
            out << "null";
        out << "\n       requirements: ";
        for (auto v : requirements) {
            v->printRef(out);
            out << " ";
        }
        out << "\n   restRequirements: ";
        for (auto v : additionalRequirements) {
            v->printRef(out);
            out << " ";
        }
        out << "\n";
    }
};

class TheStackAllocator
    : public BackwardStaticAnalysis<StackAllocatorState,
                                    AnalysisDebugLevel::None> {
  private:
    using ApplyCompensation = StackAllocator::ApplyCompensation;
    using MergeCompensation = StackAllocator::MergeCompensation;

    CFG const& cfg;
    StackValuesAnalysis const& sva;
    ApplyCompensation& applyCompensation;
    MergeCompensation& mergeCompensation;

  public:
    TheStackAllocator(Closure* cls, Code* code, StackAllocatorState init,
                      LogStream& log, CFG const& cfg,
                      StackValuesAnalysis const& sva, ApplyCompensation& ac,
                      MergeCompensation& mc)
        : BackwardStaticAnalysis("Stack Allocator", cls, code, init, cfg, log),
          cfg(cfg), sva(sva), applyCompensation(ac), mergeCompensation(mc) {}

    AbstractResult apply(StackAllocatorState& state,
                         Instruction* i) const override;
};

AbstractResult TheStackAllocator::apply(StackAllocatorState& state,
                                        Instruction* i) const {

    state.i = i;
    BB* bb = i->bb();

    state.requirements.insert(state.requirements.end(),
                              state.additionalRequirements.begin(),
                              state.additionalRequirements.end());
    state.additionalRequirements.clear();

    // replace phi instructions with the actual values
    // TODO: maybe enough only when changing bb?
    for (auto& val : state.requirements) {
        if (auto phi = Phi::Cast(val)) {
            if (phi->bb() == bb)
                continue;
            phi->eachArg([&](BB* b, Value* v) {
                if (bb == b)
                    val = v;
            });
            assert(!Phi::Cast(val) && "phi that doesn't have this bb as input");
        }
    }

    // if the type is void, this instruction doesn't leave anything on the stack
    // otherwise, it is the source of a value, so we find all uses of this value
    // and compensate for them here
    if (i->type != PirType::voyd()) {

        unsigned count = 0;
        for (auto v : state.requirements)
            if (v == i)
                ++count;

        if (count == 0 && !MkEnv::Cast(i) && !LdFunctionEnv::Cast(i)) {
            applyCompensation[i].push_back(
                std::make_pair(rir::Opcode::pop_, 0));
        } else {
            unsigned offset = state.requirements.size() - count + 1;
            auto it = state.requirements.begin();
            while (it != state.requirements.end()) {
                auto next = it + 1;

                if (i == *it) {
                    if (--count)
                        applyCompensation[i].push_back(
                            std::make_pair(rir::Opcode::dup_, 0));
                    unsigned pos = offset - !count;
                    if (pos > 1) {
                        if (applyCompensation[i].size() &&
                            applyCompensation[i].back().first ==
                                rir::Opcode::pick_ &&
                            applyCompensation[i].back().second == pos)
                            applyCompensation[i].pop_back();
                        else
                            applyCompensation[i].push_back(
                                std::make_pair(rir::Opcode::put_, pos));
                    } else if (pos == 1 && !count) {
                        if (applyCompensation[i].size() &&
                            applyCompensation[i].back().first ==
                                rir::Opcode::swap_)
                            applyCompensation[i].pop_back();
                        else
                            applyCompensation[i].push_back(
                                std::make_pair(rir::Opcode::swap_, 0));
                    }
                    next = state.requirements.erase(it);
                } else {
                    --offset;
                }

                it = next;
            }
        }
    }

    if (Phi::Cast(i)) {
        state.requirements.push_back(i);
    } else {
        i->eachArg([&](Value* v) {
            if (v->isInstruction() && !Env::isAnyEnv(v))
                state.requirements.push_back(v);
        });
    }

    if (i == bb->first()) {

        if (cfg.immediatePredecessors(bb).size() == 1) {
            BB* pred = cfg.immediatePredecessors(bb).front();
            if (sva.stackValues.count(pred)) {
                auto vals = sva.stackValues.at(pred);
                for (auto have : state.requirements) {
                    if (vals.count(have))
                        vals.erase(vals.find(have));
                }
                state.additionalRequirements.insert(
                    state.additionalRequirements.end(), vals.begin(),
                    vals.end());
            }
        }
    }

    return AbstractResult::None;
}

} // namespace

namespace rir {
namespace pir {

StackAllocator::StackAllocator(Closure* function, Code* code, LogStream& log) {

    CFG cfg(code);

    StackValuesAnalysis sva(function, code, log, cfg);

    StackAllocatorState initialState;
    initialState.i = nullptr;
    initialState.cfg = &cfg;
    initialState.mergeCompensation = &mergeCompensation;

    TheStackAllocator allocate(function, code, initialState, log, cfg, sva,
                               applyCompensation, mergeCompensation);
    allocate();
}

} // namespace pir
} // namespace rir
