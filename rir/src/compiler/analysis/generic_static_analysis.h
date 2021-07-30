#ifndef PIR_GENERIC_STATIC_ANALYSIS
#define PIR_GENERIC_STATIC_ANALYSIS

#include "../pir/bb.h"
#include "../pir/closure_version.h"
#include "../pir/instruction.h"
#include "../pir/promise.h"
#include "../util/visitor.h"
#include "R/r.h"
#include "abstract_result.h"
#include "compiler/analysis/cfg.h"
#include "compiler/log/stream_logger.h"

#include <stack>
#include <unordered_map>

namespace rir {
namespace pir {

/*
 * Generic implementation of a static analysis.
 *
 * In "mergepoint" we keep a list of abstract states for every basic block. The
 * first state is the abstract state at the beginning of the basic block. We
 * add additional entries after every function return.
 *
 * To implement a concrete static analysis, the "apply" method needs to be
 * implemented, which supplies the implementation for every instruction. Apply
 * is supposed to modify the abstract state, but not (!) the analysis itself
 * (that is why it is marked const). The reason is that after we reach a
 * fixed-point, it should be possible to reconstruct the state of the analysis
 * at every instruction. To do so, a dominating state is loaded from
 * "mergepoint" and then "apply" is used to seek to the desired instruction
 * pointer (see "collect" for an example).
 *
 * AbstractState basically has to have a merge function.
 * Anything else depends on the requirements of the apply function, which is
 * provided by the subclass that specializes StaticAnalysis.
 */

enum class AnalysisDebugLevel {
    None,
    Taint,
    Exit,
    Merge,
    BB,
    Instruction,
};

struct DummyState {
    void resetChanged() {}
    bool changed() { return false; }
};

template <class AbstractState,                    // Flow sensitive
          class GlobalAbstractState = DummyState, // Flow insensitive
          bool Forward = true,
          AnalysisDebugLevel DEBUG_LEVEL = AnalysisDebugLevel::None>
class StaticAnalysis {
  public:
    enum PositioningStyle { BeforeInstruction, AfterInstruction };

    virtual ~StaticAnalysis() {}

  private:
    const std::string name;

    struct BBSnapshot {
        bool seen = false;
        size_t incoming = 0;
        // entry stores the state *before* the first instruction in the BB
        AbstractState entry;
        // extra stores the state *after* calling apply on the given instruction
        std::unordered_map<Instruction*, AbstractState> extra;
    };
    typedef std::vector<BBSnapshot> AnalysisSnapshots;
    AnalysisSnapshots snapshots;

    // For fixed-point computation, allowed to modify global state
    virtual AbstractResult compute(AbstractState& state, Instruction* i) {
        return apply(state, i);
    }
    // For lookup, after fixed-point was found
    virtual AbstractResult apply(AbstractState&, Instruction*) const = 0;
    // Compute BB entry
    virtual AbstractResult applyEntry(AbstractState&, BB*) const {
        return AbstractResult::None;
    };

#ifdef PIR_ANALYSIS_USE_LOOKUP_CACHE
    constexpr static size_t MAX_CACHE_SIZE =
        std::max(1UL, 128 / sizeof(AbstractState));

    mutable std::unordered_map<Instruction*, AbstractState> beforeCache;
    mutable std::unordered_map<Instruction*, AbstractState> afterCache;
    mutable std::deque<Instruction*> beforeCacheQueue;
    mutable std::deque<Instruction*> afterCacheQueue;
    void addToCache(PositioningStyle pos, Instruction* i,
                    const AbstractState& state) const {
        auto& cache = pos == BeforeInstruction ? beforeCache : afterCache;
        auto& cacheQueue =
            pos == BeforeInstruction ? beforeCacheQueue : afterCacheQueue;
        if (cache.count(i)) {
            cache.erase(cache.find(i));
            cache.emplace(i, state);
            return;
        }
        if (cacheQueue.size() > MAX_CACHE_SIZE) {
            auto oldest = cacheQueue.front();
            cacheQueue.pop_front();
            cache.erase(cache.find(oldest));
        }
        cache.emplace(i, state);
        cacheQueue.push_back(i);
    }
#endif
    std::unordered_map<BB*, AbstractState> exitpoints;
    AbstractState exitpoint;

  protected:
    GlobalAbstractState* globalState = nullptr;

    bool done = false;
    LogStream& log;

    ClosureVersion* closure;
    Code* code;
    std::vector<BB*> entrypoints;

  public:
    StaticAnalysis(const std::string& name, ClosureVersion* cls, Code* code,
                   LogStream& log)
        : name(name), log(log), closure(cls), code(code) {
        snapshots.resize(code->nextBBId);
        seedEntries();
    }
    StaticAnalysis(const std::string& name, ClosureVersion* cls, Code* code,
                   const AbstractState& initialState,
                   GlobalAbstractState* globalState, LogStream& log)
        : name(name), globalState(globalState), log(log), closure(cls),
          code(code) {
        snapshots.resize(code->nextBBId);
        seedEntries();
        for (auto& e : entrypoints)
            snapshots[e->id].entry = initialState;
    }

    void setInitialState(const std::function<void(AbstractState&)>& apply) {
        for (auto& e : entrypoints)
            apply(snapshots[e->id].entry);
    }

    const GlobalAbstractState& getGlobalState() { return *globalState; }

    const AbstractState& result() const {
        if (!done)
            const_cast<StaticAnalysis*>(this)->operator()();
        assert(done);
        return exitpoint;
    }

    const AbstractState resultIgnoringUnreachableExits(Instruction* instruction,
                                                       const CFG& cfg) const {
        if (!done)
            const_cast<StaticAnalysis*>(this)->operator()();
        assert(done);
        bool foundAny = false;
        AbstractState exitState;
        for (auto& exit : exitpoints) {
            if (instruction->bb() == exit.first ||
                cfg.isPredecessor(instruction->bb(), exit.first)) {
                if (foundAny) {
                    exitState.mergeExit(exit.second);
                } else {
                    exitState = exit.second;
                    foundAny = true;
                }
            }
        }
        return exitState;
    }

    void logHeader() const {
        if (DEBUG_LEVEL > AnalysisDebugLevel::None) {
            log << "=========== Starting " << name << " Analysis on "
                << closure->name() << " ";
            if (code == closure)
                log << "body";
            else
                log << "Prom(" << static_cast<Promise*>(code)->id << ")";
            log << "\n";
        }
    }

    void logInitialState(const AbstractState& state, const BB* bb) const {
        if (DEBUG_LEVEL >= AnalysisDebugLevel::BB) {
            log << "======= Entering BB" << bb->id << ", initial state\n";
            log(state);
        }
    }

    void logChange(const AbstractState& post, const AbstractResult& res,
                   const Instruction* i) {
        if (DEBUG_LEVEL >= AnalysisDebugLevel::Instruction &&
            res >= AbstractResult::None) {
            log << "===== After applying instruction ";
            log(i);
            if (res == AbstractResult::Tainted) {
                log << " (State got tainted)";
            }
            log << " we have\n";
            log(post);
        }
    }

    void logTaintChange(const AbstractState& pre, const AbstractState& post,
                        const AbstractResult& res, const Instruction* i) {
        assert(DEBUG_LEVEL == AnalysisDebugLevel::Taint);
        if (res >= AbstractResult::Tainted) {
            if (res == AbstractResult::Tainted) {
                log << "===== Before applying instruction ";
                log(i);
                log << " we have\n";
                log(pre);
            }
            log << "===== After applying instruction ";
            log(i);
            if (res == AbstractResult::Tainted) {
                log << " (State got tainted)";
            }
            log << " we have\n";
            log(post);
        }
    }

    void logExit(const AbstractState& state) {
        if (DEBUG_LEVEL >= AnalysisDebugLevel::Exit) {
            log << "===== Exit state is\n";
            log(state);
        }
    }

    AbstractState before(Instruction* i,
                         AbstractState* afterPreviousInstr = nullptr) const {
        return at<PositioningStyle::BeforeInstruction>(i, afterPreviousInstr);
    }

    AbstractState after(Instruction* i,
                        AbstractState* afterPreviousInstr = nullptr) const {
        return at<PositioningStyle::AfterInstruction>(i, afterPreviousInstr);
    }

  private:
    template <PositioningStyle POS>
    AbstractState at(Instruction* i,
                     AbstractState* afterPreviousInstr = nullptr) const {
        if (!done)
            const_cast<StaticAnalysis*>(this)->operator()();
        assert(done);

#ifdef PIR_ANALYSIS_USE_LOOKUP_CACHE
        if (beforeCache.count(i) && POS == BeforeInstruction) {
            return beforeCache.at(i);
        }
        if (afterCache.count(i) && POS == AfterInstruction) {
            return afterCache.at(i);
        }
#endif

        if (POS == PositioningStyle::BeforeInstruction && afterPreviousInstr)
            return *afterPreviousInstr;

        BB* bb = i->bb();
        if (Forward)
            return findSnapshot<POS>(bb->begin(), bb->end(), bb, i,
                                     afterPreviousInstr);

        return findSnapshot<POS>(bb->rbegin(), bb->rend(), bb, i,
                                 afterPreviousInstr);
    }

    template <PositioningStyle POS, typename Iter>
    AbstractState
    findSnapshot(Iter begin, Iter end, BB* bb, Instruction* i,
                 AbstractState* afterPreviousInstr = nullptr) const {
        const BBSnapshot& bbSnapshots = snapshots[bb->id];

        // Find the snapshot closest to the desired state
        size_t tried = 0;
        auto snapshotPos = begin;
        for (auto pos = begin; pos != end && tried < bbSnapshots.extra.size();
             ++pos) {
            if (POS == BeforeInstruction && i == *pos)
                break;
            if (bbSnapshots.extra.count(*pos)) {
                snapshotPos = pos;
                tried++;
            }
            if (POS == AfterInstruction && i == *pos)
                break;
        }

        auto state =
            tried == 0 ? bbSnapshots.entry : bbSnapshots.extra.at(*snapshotPos);

        // If we found a snapshot in extra, this gives us the state *after*
        // applying, hence we either found the result or need to move to the
        // next instruction
        if (tried) {
            if (i == *snapshotPos) {
                assert(POS == AfterInstruction);
#ifdef PIR_ANALYSIS_USE_LOOKUP_CACHE
                addToCache(AfterInstruction, i, state);
                if (snapshotPos + 1 != end)
                    addToCache(BeforeInstruction, *(snapshotPos + 1), state);
#endif
                return state;
            }
            ++snapshotPos;
            assert(snapshotPos != end);
        }

        // No snapshot found for the current position. If we have the state
        // after the previous instruction, then this is the next fastest way to
        // compute it.
        if (afterPreviousInstr && *snapshotPos != i) {
            assert(POS == PositioningStyle::AfterInstruction);
            auto state = *afterPreviousInstr;
            apply(state, i);
            assert(!bbSnapshots.extra.count(i));
            return state;
        }

        // Apply until we arrive at the position
        for (auto pos = snapshotPos; pos != end; ++pos) {
            if (POS == BeforeInstruction && i == *pos) {
#ifdef PIR_ANALYSIS_USE_LOOKUP_CACHE
                addToCache(BeforeInstruction, i, state);
                if (pos != begin)
                    addToCache(AfterInstruction, *(pos - 1), state);
#endif
                return state;
            }
            apply(state, *pos);
            if (POS == AfterInstruction && i == *pos) {
#ifdef PIR_ANALYSIS_USE_LOOKUP_CACHE
                addToCache(AfterInstruction, i, state);
                if (pos + 1 != end)
                    addToCache(BeforeInstruction, *(pos + 1), state);
#endif
                return state;
            }
        }

        assert(false);
        return AbstractState();
    }

  public:
    typedef std::function<void(const AbstractState&, Instruction*)> Collect;

    template <PositioningStyle POS>
    void foreach (Collect collect) const {
        assert(done);
        assert(Forward && "Only exists in forward mode");
        assert(entrypoints.size() == 1);

        Visitor::run(entrypoints[0], [&](BB* bb) {
            const BBSnapshot& bbSnapshots = snapshots[bb->id];
            AbstractState state = bbSnapshots.entry;
            for (auto i : *bb) {
                if (POS == BeforeInstruction)
                    collect(state, i);

                const auto& entry = bbSnapshots.extra.find(i);
                if (entry != bbSnapshots.extra.end())
                    state = entry->second;
                else
                    apply(state, i);

                if (POS == AfterInstruction)
                    collect(state, i);
            }
        });
    }

    void operator()() {
        bool reachedExit = false;

        std::vector<bool> changed(snapshots.size(), false);
        for (auto e : entrypoints)
            changed[e->id] = true;

        logHeader();

        typedef std::pair<BB*, Instruction*> Position;
        std::vector<Position> recursiveTodo;
        do {
            done = true;
            for (auto e : entrypoints) {
                if (globalState)
                    globalState->resetChanged();

                Visitor::runInDirection(Forward, e, [&](BB* bb) {
                    size_t id = bb->id;

                    if (!changed[id])
                        return;

                    if (applyEntry(snapshots[id].entry, bb) >
                        AbstractResult::None)
                        changed[id] = true;

                    AbstractState state = snapshots[id].entry;
                    logInitialState(state, bb);

                    auto apply = [&](Instruction* i) {
                        AbstractResult res;
                        if (DEBUG_LEVEL == AnalysisDebugLevel::Taint) {
                            AbstractState old = state;
                            res = compute(state, i);
                            if (!Deopt::Cast(i)) {
                                AbstractState old2 = old;
                                auto changed = old2.merge(state);
                                if (changed > AbstractResult::None)
                                    logTaintChange(old, state, res, i);
                            }
                        } else {
                            res = compute(state, i);
                            logChange(state, res, i);
                        }

                        auto& snapshot = snapshots[bb->id];
                        if (res.needRecursion) {
                            auto& extra = snapshot.extra;
                            const auto& entry = extra.find(i);
                            if (entry != extra.end()) {
                                entry->second.merge(state);
                                state = entry->second;
                            } else {
                                extra.emplace(i, state);
                            }
                            recursiveTodo.push_back(Position(bb, i));
                        }

                        if (res.keepSnapshot || snapshot.extra.count(i)) {
                            snapshot.extra[i] = state;
                        }
                    };

                    if (Forward)
                        for (auto i : *bb)
                            apply(i);
                    else
                        for (auto i : VisitorHelpers::reverse(*bb))
                            apply(i);

                    if (Forward ? bb->isExit() : bb == code->entry) {
                        logExit(state);

                        auto exitStateIt = exitpoints.find(bb);
                        if (exitStateIt == exitpoints.end())
                            exitpoints.emplace(bb, state);
                        else
                            exitStateIt->second = state;

                        if (reachedExit) {
                            exitpoint.mergeExit(state);
                        } else {
                            exitpoint = state;
                            reachedExit = true;
                        }

                        changed[id] = false;
                        return;
                    }

                    if (Forward)
                        for (auto suc : bb->successors())
                            mergeBranch(bb, suc, state, changed);
                    else
                        for (auto suc : bb->predecessors())
                            mergeBranch(bb, suc, state, changed);

                    changed[id] = false;
                });
                if (!recursiveTodo.empty()) {
                    for (auto& rec : recursiveTodo) {
                        auto bb = rec.first->id;
                        auto& extra = snapshots[bb].extra;
                        const auto& entry = extra.find(rec.second);
                        if (entry != extra.end()) {
                            auto mres = entry->second.mergeExit(exitpoint);
                            if (mres > AbstractResult::None) {
                                logChange(entry->second, mres, rec.second);
                                changed[bb] = true;
                                done = false;
                            }
                        } else {
                            extra.emplace(rec.second, exitpoint);
                            changed[bb] = true;
                            done = false;
                        }
                    }
                    recursiveTodo.clear();
                }
                if (globalState && globalState->changed())
                    done = false;
            }
        } while (!done);
    }

    void mergeBranch(BB* in, BB* branch, const AbstractState& state,
                     std::vector<bool>& changed) {
        auto id = branch->id;
        auto& thisState = snapshots.at(id);
        if (!thisState.seen) {
            thisState.entry = state;
            thisState.seen = true;
            thisState.incoming = in->id;
            done = false;
            changed[id] = true;
        } else if (in->id == thisState.incoming) {
            thisState.entry = state;
            changed[id] = changed[in->id];
        } else {
            thisState.incoming = -1;
            AbstractState old;
            if (DEBUG_LEVEL >= AnalysisDebugLevel::Taint) {
                old = thisState.entry;
            }
            AbstractResult mres = thisState.entry.merge(state);
            if (mres > AbstractResult::None) {
                if (DEBUG_LEVEL >= AnalysisDebugLevel::Merge ||
                    (mres == AbstractResult::Tainted &&
                     DEBUG_LEVEL == AnalysisDebugLevel::Taint)) {
                    log << "===== Merging BB" << in->id << " into BB" << id
                        << (mres == AbstractResult::Tainted ? " tainted" : "")
                        << (mres == AbstractResult::LostPrecision
                                ? " lost precision"
                                : "")
                        << ", updated state:\n";
                    log << "===- In state is:\n";
                    log(state);
                    log << "===- Old state is:\n";
                    log(old);
                    log << "===- Merged state is:\n";
                    log(thisState.entry);
                }
                done = false;
                changed[id] = true;
            } else if (DEBUG_LEVEL >= AnalysisDebugLevel::Merge) {
                log << "===== Merging into trueBranch BB" << id
                    << " reached fixpoint\n";
            }
        }
    }

    bool fixedPointReached() { return done; }

  private:
    void seedEntries() {
        if (Forward) {
            entrypoints.push_back(code->entry);
            return;
        }

        // TODO add headers of infinite loops
        Visitor::run(code->entry, [&](BB* bb) {
            if (bb->isExit()) {
                entrypoints.push_back(bb);
            }
        });
    }
};

} // namespace pir
} // namespace rir

#endif
