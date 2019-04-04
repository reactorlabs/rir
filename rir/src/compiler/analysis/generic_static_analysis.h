#ifndef PIR_GENERIC_STATIC_ANALYSIS
#define PIR_GENERIC_STATIC_ANALYSIS

#include "../debugging/stream_logger.h"
#include "../pir/bb.h"
#include "../pir/closure_version.h"
#include "../pir/instruction.h"
#include "../pir/promise.h"
#include "../util/cfg.h"
#include "../util/visitor.h"
#include "R/r.h"
#include "abstract_result.h"

#include <stack>
#include <unordered_map>

namespace rir {
namespace pir {

/*
 * Generic implementation of a (forward) static analysis.
 *
 * In "mergepoint" we keep a list of abstract states for every basic block. The
 * first state is the abstract state at the beginning of the basic block. We
 * add additional entries after every function return.
 *
 * To implement a concrete static analysis, the "apply" method needs to be
 * implemented, which supplies the implementation for every instruction. Apply
 * is supposed to modify the abstract state, but not (!) the analysis itself
 * (that is why it is marked const). The reason is, that after we reached a
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

template <class AbstractState,
          AnalysisDebugLevel DEBUG_LEVEL = AnalysisDebugLevel::None>
class StaticAnalysis {
  public:
    enum PositioningStyle { BeforeInstruction, AfterInstruction };

  private:
    const std::string name;

    struct BBSnapshot {
        bool seen = false;
        AbstractState entry;
        std::unordered_map<Instruction*, AbstractState> extra;
    };
    typedef std::vector<BBSnapshot> AnalysisSnapshots;
    AnalysisSnapshots snapshots;

    virtual AbstractResult apply(AbstractState&, Instruction*) const = 0;

    constexpr static size_t MAX_CACHE_SIZE = 128 / sizeof(AbstractState);

    std::unordered_map<Instruction*, AbstractState> cache;
    std::deque<Instruction*> cacheQueue;
    void addToCache(Instruction* i, const AbstractState& state) const {
        if (cache.count(i)) {
            const_cast<StaticAnalysis*>(this)->cache.erase(cache.find(i));
            const_cast<StaticAnalysis*>(this)->cache.emplace(i, state);
            return;
        }
        if (cacheQueue.size() > MAX_CACHE_SIZE) {
            auto oldest = cacheQueue.front();
            const_cast<StaticAnalysis*>(this)->cacheQueue.pop_front();
            const_cast<StaticAnalysis*>(this)->cache.erase(cache.find(oldest));
        }
        const_cast<StaticAnalysis*>(this)->cache.emplace(i, state);
        const_cast<StaticAnalysis*>(this)->cacheQueue.push_back(i);
    }

  protected:
    AbstractState exitpoint;

    bool done = false;
    LogStream& log;

    ClosureVersion* closure;
    Code* code;
    BB* entry;

  public:
    StaticAnalysis(const std::string& name, ClosureVersion* cls, Code* code,
                   LogStream& log)
        : name(name), log(log), closure(cls), code(code), entry(code->entry) {
        snapshots.resize(code->nextBBId);
    }
    StaticAnalysis(const std::string& name, ClosureVersion* cls, Code* code,
                   const AbstractState& initialState, LogStream& log)
        : name(name), log(log), closure(cls), code(code), entry(code->entry) {
        snapshots.resize(code->nextBBId);
        snapshots[entry->id].entry = initialState;
    }

    const AbstractState& result() const {
        if (!done)
            const_cast<StaticAnalysis*>(this)->operator()();
        assert(done);
        return exitpoint;
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
            if (res == AbstractResult::Tainted) {
                log << " (State got tainted)";
            } else {
                log(i);
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
            if (res == AbstractResult::Tainted) {
                log << " (State got tainted)";
            } else {
                log(i);
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

    template <PositioningStyle POS>
    AbstractState at(Instruction* i) const {
        if (!done)
            const_cast<StaticAnalysis*>(this)->operator()();
        assert(done);

        BB* bb = i->bb();

        if (cache.count(i)) {
            auto state = cache.at(i);
            if (PositioningStyle::AfterInstruction == POS)
                apply(state, i);
            return state;
        }

        const BBSnapshot& bbSnapshots = snapshots[bb->id];

        // Find the last snapshot before the instruction we want to know about
        size_t tried = 0;
        auto snapshotPos = bb->begin();
        for (auto pos = bb->begin(), end = bb->end();
             pos != end && tried < bbSnapshots.extra.size(); ++pos) {
            if (POS == BeforeInstruction && i == *pos)
                break;
            if (bbSnapshots.extra.count(*pos)) {
                snapshotPos = pos;
                tried++;
            }
            if (POS == AfterInstruction && i == *pos)
                break;
        }

        // Apply until we arrive at the position
        auto state = snapshotPos == bb->begin()
                         ? bbSnapshots.entry
                         : bbSnapshots.extra.at(*snapshotPos);
        for (auto pos = snapshotPos, end = bb->end(); pos != end; ++pos) {
            if (POS == BeforeInstruction && i == *pos) {
                addToCache(i, state);
                return state;
            }
            apply(state, *pos);
            if (POS == AfterInstruction && i == *pos) {
                if (pos + 1 != bb->end())
                    addToCache(*(pos + 1), state);
                return state;
            }
        }

        assert(false);
        return AbstractState();
    }

    typedef std::function<void(const AbstractState&, Instruction*)> Collect;

    template <PositioningStyle POS>
    void foreach (Collect collect) const {
        assert(done);

        Visitor::run(entry, [&](BB* bb) {
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
        changed[entry->id] = true;

        logHeader();

        typedef std::pair<BB*, Instruction*> Position;
        std::vector<Position> recursiveTodo;
        do {
            done = true;
            Visitor::run(entry, [&](BB* bb) {
                size_t id = bb->id;

                if (!changed[id])
                    return;

                AbstractState state = snapshots[id].entry;
                logInitialState(state, bb);

                for (auto i : *bb) {
                    AbstractResult res;
                    if (DEBUG_LEVEL == AnalysisDebugLevel::Taint) {
                        AbstractState old = state;
                        res = apply(state, i);
                        logTaintChange(old, state, res, i);
                    } else {
                        res = apply(state, i);
                        logChange(state, res, i);
                    }

                    if (res.needRecursion) {
                        auto& extra = snapshots[bb->id].extra;
                        const auto& entry = extra.find(i);
                        if (entry != extra.end()) {
                            entry->second.merge(state);
                            state = entry->second;
                        } else {
                            extra.emplace(i, state);
                        }
                        recursiveTodo.push_back(Position(bb, i));
                    }

                    if (res.keepSnapshot || snapshots[bb->id].extra.count(i)) {
                        snapshots[bb->id].extra[i] = state;
                    }
                }

                if (bb->isExit()) {
                    logExit(state);
                    if (reachedExit) {
                        exitpoint.mergeExit(state);
                    } else {
                        exitpoint = state;
                        reachedExit = true;
                    }
                    changed[id] = false;
                    return;
                }

                mergeBranch(bb, bb->trueBranch(), state, changed);
                mergeBranch(bb, bb->falseBranch(), state, changed);

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
        } while (!done);
    }

    void mergeBranch(BB* in, BB* branch, const AbstractState& state,
                     std::vector<bool>& changed) {
        if (!branch)
            return;

        auto id = branch->id;
        if (!snapshots[id].seen) {
            snapshots[id].entry = state;
            snapshots[id].seen = true;
            done = false;
            changed[id] = true;
        } else {
            AbstractState old;
            if (DEBUG_LEVEL >= AnalysisDebugLevel::Taint) {
                old = snapshots[id].entry;
            }
            AbstractResult mres = snapshots[id].entry.merge(state);
            if (mres > AbstractResult::None) {
                if (DEBUG_LEVEL >= AnalysisDebugLevel::Merge ||
                    (mres == AbstractResult::Tainted &&
                     DEBUG_LEVEL == AnalysisDebugLevel::Taint)) {
                    log << "===== Merging BB" << in->id << " into BB" << id
                        << (mres == AbstractResult::Tainted ? " tainted" : "")
                        << (mres == AbstractResult::LostPrecision
                                ? " lost precision"
                                : "")
                        << " updated state:\n";
                    log << "===- In state is:\n";
                    log(state);
                    log << "===- Old state is:\n";
                    log(old);
                    log << "===- Merged state is:\n";
                    log(snapshots[id].entry);
                }
                done = false;
                changed[id] = true;
            } else if (DEBUG_LEVEL >= AnalysisDebugLevel::Merge) {
                log << "===== Merging into trueBranch BB" << id
                    << " reached fixpoint\n";
            }
        }
    }
};

template <class AbstractState,
          AnalysisDebugLevel DEBUG_LEVEL = AnalysisDebugLevel::None>
class BackwardStaticAnalysis {
  public:
    enum PositioningStyle { BeforeInstruction, AfterInstruction };

  private:
    const std::string name;

    struct BBSnapshot {
        bool seen = false;
        AbstractState entry;
        std::unordered_map<Instruction*, AbstractState> extra;
    };
    typedef std::vector<BBSnapshot> AnalysisSnapshots;
    AnalysisSnapshots snapshots;

    virtual AbstractResult apply(AbstractState&, Instruction*) const = 0;

    constexpr static size_t MAX_CACHE_SIZE = 128 / sizeof(AbstractState);

    std::unordered_map<Instruction*, AbstractState> cache;
    std::deque<Instruction*> cacheQueue;
    void addToCache(Instruction* i, const AbstractState& state) const {
        if (cache.count(i)) {
            const_cast<BackwardStaticAnalysis*>(this)->cache.erase(
                cache.find(i));
            const_cast<BackwardStaticAnalysis*>(this)->cache.emplace(i, state);
            return;
        }
        if (cacheQueue.size() > MAX_CACHE_SIZE) {
            auto oldest = cacheQueue.front();
            const_cast<BackwardStaticAnalysis*>(this)->cacheQueue.pop_front();
            const_cast<BackwardStaticAnalysis*>(this)->cache.erase(
                cache.find(oldest));
        }
        const_cast<BackwardStaticAnalysis*>(this)->cache.emplace(i, state);
        const_cast<BackwardStaticAnalysis*>(this)->cacheQueue.push_back(i);
    }

  protected:
    AbstractState exitpoint;

    bool done = false;
    CFG const& cfg;
    LogStream& log;

    ClosureVersion* closure;
    Code* code;
    std::vector<BB*> entrypoints;

  public:
    BackwardStaticAnalysis(const std::string& name, ClosureVersion* cls,
                           Code* code, CFG const& cfg, LogStream& log)
        : name(name), cfg(cfg), log(log), closure(cls), code(code) {
        snapshots.resize(code->nextBBId);
        for (auto e : cfg.exits()) {
            entrypoints.push_back(e);
        }
    }
    BackwardStaticAnalysis(const std::string& name, ClosureVersion* cls,
                           Code* code, const AbstractState& initialState,
                           CFG const& cfg, LogStream& log)
        : name(name), cfg(cfg), log(log), closure(cls), code(code) {
        snapshots.resize(code->nextBBId);
        for (auto e : cfg.exits()) {
            entrypoints.push_back(e);
            snapshots[e->id].entry = initialState;
        }
    }

    const AbstractState& result() const {
        assert(done);
        return exitpoint;
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
            if (res == AbstractResult::Tainted) {
                log << " (State got tainted)";
            } else {
                log(i);
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
            if (res == AbstractResult::Tainted) {
                log << " (State got tainted)";
            } else {
                log(i);
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

    template <PositioningStyle POS>
    AbstractState at(Instruction* i) const {
        if (!done)
            const_cast<BackwardStaticAnalysis*>(this)->operator()();
        assert(done);

        BB* bb = i->bb();

        if (cache.count(i)) {
            auto state = cache.at(i);
            if (PositioningStyle::AfterInstruction == POS)
                apply(state, i);
            return state;
        }

        const BBSnapshot& bbSnapshots = snapshots[bb->id];

        // Find the last snapshot before the instruction we want to know about
        size_t tried = 0;
        auto snapshotPos = bb->rbegin();
        for (auto pos = bb->rbegin(), end = bb->rend();
             pos != end && tried < bbSnapshots.extra.size(); ++pos) {
            if (POS == BeforeInstruction && i == *pos)
                break;
            if (bbSnapshots.extra.count(*pos)) {
                snapshotPos = pos;
                tried++;
            }
            if (POS == AfterInstruction && i == *pos)
                break;
        }

        // Apply until we arrive at the position
        auto state = snapshotPos == bb->rbegin()
                         ? bbSnapshots.entry
                         : bbSnapshots.extra.at(*snapshotPos);
        for (auto pos = snapshotPos, end = bb->rend(); pos != end; ++pos) {
            if (POS == BeforeInstruction && i == *pos) {
                addToCache(i, state);
                return state;
            }
            apply(state, *pos);
            if (POS == AfterInstruction && i == *pos) {
                if (pos + 1 != bb->rend())
                    addToCache(*(pos + 1), state);
                return state;
            }
        }

        assert(false);
        return AbstractState();
    }

    typedef std::function<void(const AbstractState&, Instruction*)> Collect;

    template <PositioningStyle POS>
    void foreach (Collect collect) const {
        assert(done);

        std::vector<bool> seen(snapshots.size(), false);

        for (auto e : entrypoints) {
            Visitor::runBackward(e, cfg, [&](BB* bb) {
                if (seen[bb->id])
                    return;
                seen[bb->id] = true;
                const BBSnapshot& bbSnapshots = snapshots[bb->id];
                AbstractState state = bbSnapshots.entry;
                for (auto i : VisitorHelpers::reverse(*bb)) {
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
                Visitor::runBackward(e, cfg, [&](BB* bb) {
                    size_t id = bb->id;

                    if (!changed[id])
                        return;

                    AbstractState state = snapshots[id].entry;
                    logInitialState(state, bb);

                    for (auto i : VisitorHelpers::reverse(*bb)) {
                        AbstractResult res;
                        if (DEBUG_LEVEL == AnalysisDebugLevel::Taint) {
                            AbstractState old = state;
                            res = apply(state, i);
                            logTaintChange(old, state, res, i);
                        } else {
                            res = apply(state, i);
                            logChange(state, res, i);
                        }

                        if (res.needRecursion) {
                            auto& extra = snapshots[bb->id].extra;
                            const auto& entry = extra.find(i);
                            if (entry != extra.end()) {
                                entry->second.merge(state);
                                state = entry->second;
                            } else {
                                extra.emplace(i, state);
                            }
                            recursiveTodo.push_back(Position(bb, i));
                        }

                        if (res.keepSnapshot ||
                            snapshots[bb->id].extra.count(i)) {
                            snapshots[bb->id].extra[i] = state;
                        }
                    }

                    if (bb == code->entry) {
                        logExit(state);
                        if (reachedExit) {
                            exitpoint.mergeExit(state);
                        } else {
                            exitpoint = state;
                            reachedExit = true;
                        }
                        changed[id] = false;
                        return;
                    }

                    for (auto p : cfg.immediatePredecessors(bb))
                        mergeBranch(bb, p, state, changed);

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
            }
        } while (!done);
    }

    void mergeBranch(BB* in, BB* branch, const AbstractState& state,
                     std::vector<bool>& changed) {
        if (!branch)
            return;

        auto id = branch->id;
        if (!snapshots[id].seen) {
            snapshots[id].entry = state;
            snapshots[id].seen = true;
            done = false;
            changed[id] = true;
        } else {
            AbstractState old;
            if (DEBUG_LEVEL >= AnalysisDebugLevel::Taint) {
                old = snapshots[id].entry;
            }
            AbstractResult mres = snapshots[id].entry.merge(state);
            if (mres > AbstractResult::None) {
                if (DEBUG_LEVEL >= AnalysisDebugLevel::Merge ||
                    (mres == AbstractResult::Tainted &&
                     DEBUG_LEVEL == AnalysisDebugLevel::Taint)) {
                    log << "===== Merging BB" << in->id << " into BB" << id
                        << (mres == AbstractResult::Tainted ? " tainted" : "")
                        << (mres == AbstractResult::LostPrecision
                                ? " lost precision"
                                : "")
                        << " updated state:\n";
                    log << "===- In state is:\n";
                    log(state);
                    log << "===- Old state is:\n";
                    log(old);
                    log << "===- Merged state is:\n";
                    log(snapshots[id].entry);
                }
                done = false;
                changed[id] = true;
            } else if (DEBUG_LEVEL >= AnalysisDebugLevel::Merge) {
                log << "===== Merging into trueBranch BB" << id
                    << " reached fixpoint\n";
            }
        }
    }
};

} // namespace pir
} // namespace rir

#endif
