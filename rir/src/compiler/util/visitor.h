#ifndef COMPILER_VISITOR_H
#define COMPILER_VISITOR_H

#include "../pir/bb.h"
#include "../pir/code.h"
#include "../pir/instruction.h"
#include "../pir/pir.h"
#include "../util/cfg.h"

#include <deque>
#include <functional>
#include <random>
#include <stack>
#include <unordered_set>

namespace rir {
namespace pir {

namespace VisitorHelpers {
typedef std::function<bool(BB*)> BBActionPredicate;
typedef std::function<void(BB*)> BBAction;

/*
 * PredicateWrapper abstracts over BBAction and BBActionPredicate, to be
 * able to use them in the same generic implementation.
 * In the case of BBAction the return value will always be true.
 *
 */
template <typename ActionKind>
struct PredicateWrapper {};

template <>
struct PredicateWrapper<BBAction> {
    const BBAction action;
    bool operator()(BB* bb) const {
        action(bb);
        return true;
    }
};

template <>
struct PredicateWrapper<BBActionPredicate> {
    const BBActionPredicate action;
    bool operator()(BB* bb) const { return action(bb); }
};

/*
 * Helpers to remember which BB has already been visited. There is a fast
 * version (IDMarker) that uses a bitvector, but relies on stable BB ids. And
 * there is a slow version (PointerMarker) using a set.
 *
 */
struct IDMarker {
    std::vector<bool> done;
    IDMarker() : done(128, false){};

    void set(BB* bb) {
        while (bb->id >= done.size())
            done.resize(done.size() * 2);
        done[bb->id] = true;
    }

    bool check(BB* bb) { return bb->id < done.size() && done[bb->id]; }
};

struct PointerMarker {
    std::unordered_set<BB*> done;
    void set(BB* bb) { done.insert(bb); }
    bool check(BB* bb) { return done.find(bb) != done.end(); }
};

/*
 * Support for reverse iteration in range-based for loops.
 */
template <typename T>
struct reverse_wrapper {
    T& iterable;
};

template <typename T>
auto begin(reverse_wrapper<T> w) {
    return w.iterable.rbegin();
}

template <typename T>
auto end(reverse_wrapper<T> w) {
    return w.iterable.rend();
}

template <typename T>
reverse_wrapper<T> reverse(T&& iterable) {
    return {iterable};
}

}; // namespace VisitorHelpers

enum class Order { Depth, Breadth, Random, Lowering };

template <Order ORDER, class Marker>
class VisitorImplementation {
  public:
    typedef std::function<bool(Instruction*)> InstrActionPredicate;
    typedef std::function<void(Instruction*)> InstrAction;
    typedef std::function<bool(Instruction*, BB*)> InstrBBActionPredicate;
    typedef std::function<void(Instruction*, BB*)> InstrBBAction;
    using BBActionPredicate = VisitorHelpers::BBActionPredicate;
    using BBAction = VisitorHelpers::BBAction;

    /*
     * Instruction Visitors
     *
     */
    static void run(BB* bb, const InstrAction& action) {
        run(bb, [action](BB* bb) {
            for (auto i : *bb)
                action(i);
        });
    }

    static bool check(BB* bb, const InstrActionPredicate& action) {
        return check(bb, [action](BB* bb) {
            bool holds = true;
            for (auto i : *bb) {
                if (!action(i)) {
                    holds = false;
                    break;
                }
            }
            return holds;
        });
    }

    static void run(BB* bb, const InstrBBAction& action) {
        run(bb, [action](BB* bb) {
            for (auto i : *bb)
                action(i, bb);
        });
    }

    static bool check(BB* bb, const InstrBBActionPredicate& action) {
        return check(bb, [action](BB* bb) {
            bool holds = true;
            for (auto i : *bb) {
                if (!action(i, bb)) {
                    holds = false;
                    break;
                }
            }
            return holds;
        });
    }

    static void runBackward(BB* bb, CFG const& cfg, const InstrAction& action) {
        runBackward(bb, cfg, [action](BB* bb) {
            for (auto i : VisitorHelpers::reverse(*bb))
                action(i);
        });
    }

    static bool checkBackward(BB* bb, CFG const& cfg,
                              const InstrActionPredicate& action) {
        return checkBackward(bb, cfg, [action](BB* bb) {
            bool holds = true;
            for (auto i : VisitorHelpers::reverse(*bb)) {
                if (!action(i)) {
                    holds = false;
                    break;
                }
            }
            return holds;
        });
    }

    static void runBackward(BB* bb, CFG const& cfg,
                            const InstrBBAction& action) {
        runBackward(bb, cfg, [action](BB* bb) {
            for (auto i : VisitorHelpers::reverse(*bb))
                action(i, bb);
        });
    }

    static bool checkBackward(BB* bb, CFG const& cfg,
                              const InstrBBActionPredicate& action) {
        return checkBackward(bb, cfg, [action](BB* bb) {
            bool holds = true;
            for (auto i : VisitorHelpers::reverse(*bb)) {
                if (!action(i, bb)) {
                    holds = false;
                    break;
                }
            }
            return holds;
        });
    }

    /*
     * BB Visitors
     *
     */
    static void run(BB* bb, const BBAction& action) {
        forwardGenericRun<false>(bb, action);
    }

    static void runPostChange(BB* bb, const BBAction& action) {
        forwardGenericRun<true>(bb, action);
    }

    static bool check(BB* bb, const BBActionPredicate& action) {
        return forwardGenericRun<false>(bb, action);
    }

    static void runBackward(BB* bb, CFG const& cfg, const BBAction& action) {
        backwardGenericRun<false>(bb, cfg, action);
    }

    static bool checkBackward(BB* bb, CFG const& cfg,
                              const BBActionPredicate& action) {
        return backwardGenericRun<false>(bb, cfg, action);
    }

  protected:
    typedef std::function<void(BB*)> Schedule;
    typedef std::function<void(Schedule, BB*)> ScheduleNext;

    template <bool PROCESS_NEW_NODES, typename ActionKind>
    static bool forwardGenericRun(BB* bb, const ActionKind& action) {
        struct Scheduler {
            RIR_INLINE std::array<BB*, 2> operator()(BB* cur) const {
                if (ORDER == Order::Lowering) {
                    // Curently we emit only brtrue in pir2rir, therefore we
                    // always want next1 to be the fallthrough case.
                    return {cur->next1, cur->next0};
                } else {
                    return {cur->next0, cur->next1};
                }
            }
        };
        const Scheduler scheduler;
        return genericRun<PROCESS_NEW_NODES>(bb, scheduler, action);
    }

    template <bool PROCESS_NEW_NODES, typename ActionKind>
    static bool backwardGenericRun(BB* bb, CFG const& cfg,
                                   const ActionKind& action) {
        struct Scheduler {
            const CFG& cfg;
            RIR_INLINE const std::vector<BB*> operator()(BB* cur) const {
                return cfg.immediatePredecessors(cur);
            }
        };
        const Scheduler scheduler = {cfg};
        return genericRun<PROCESS_NEW_NODES>(bb, scheduler, action);
    }

    template <bool PROCESS_NEW_NODES, typename Scheduler, typename ActionKind>
    static bool RIR_INLINE genericRun(BB* bb, Scheduler& scheduleNext,
                                      const ActionKind& action) {
        typedef VisitorHelpers::PredicateWrapper<ActionKind> PredicateWrapper;
        const PredicateWrapper predicate = {action};

        BB* cur = bb;
        std::deque<BB*> todo;
        std::deque<BB*> delayed;
        Marker done;
        BB* next = nullptr;
        done.set(cur);

        while (cur) {
            next = nullptr;
            auto schedule = [&next, &done, &delayed, &todo](BB* bb) {
                if (!bb || done.check(bb))
                    return;
                bool deoptBranch =
                    !bb->isEmpty() && ScheduledDeopt::Cast(bb->last());
                if (ORDER == Order::Lowering && deoptBranch) {
                    delayed.push_back(bb);
                } else if (!next && todo.empty()) {
                    next = bb;
                } else {
                    enqueue(todo, bb);
                }
                done.set(bb);
            };

            if (PROCESS_NEW_NODES) {
                if (!predicate(cur))
                    return false;
                for (BB* bb : scheduleNext(cur))
                    schedule(bb);
            } else {
                for (BB* bb : scheduleNext(cur))
                    schedule(bb);
                if (!predicate(cur))
                    return false;
            }

            // cppcheck-suppress knownConditionTrueFalse
            if (!next) {
                if (!todo.empty()) {
                    next = todo.front();
                    todo.pop_front();
                } else if (!delayed.empty()) {
                    next = delayed.front();
                    delayed.pop_front();
                }
            }

            cur = next;
        }
        assert(todo.empty());

        return true;
    }

  private:
    static bool coinFlip() {
        static std::mt19937 gen(42);
        static std::bernoulli_distribution coin(0.5);
        return coin(gen);
    };

    static void enqueue(std::deque<BB*>& todo, BB* bb) {
        // For analysis random search is faster
        if (ORDER == Order::Breadth || ORDER == Order::Lowering ||
            (ORDER == Order::Random && coinFlip()))
            todo.push_back(bb);
        else
            todo.push_front(bb);
    }
};

class Visitor
    : public VisitorImplementation<Order::Random, VisitorHelpers::IDMarker> {};

class BreadthFirstVisitor
    : public VisitorImplementation<Order::Breadth, VisitorHelpers::IDMarker> {};

class DepthFirstVisitor
    : public VisitorImplementation<Order::Depth, VisitorHelpers::IDMarker> {};

class LoweringVisitor
    : public VisitorImplementation<Order::Lowering, VisitorHelpers::IDMarker> {
};

template <class Marker = VisitorHelpers::IDMarker>
class DominatorTreeVisitor {
    using BBAction = VisitorHelpers::BBAction;

    const DominanceGraph& dom;

  public:
    explicit DominatorTreeVisitor(const DominanceGraph& dom) : dom(dom) {}

    void run(Code* code, BBAction action) {
        Marker done;

        std::stack<BB*> todo;
        std::stack<BB*> delayedTodo;

        todo.push(code->entry);
        done.set(code->entry);

        BB* cur;
        while (!todo.empty() || !delayedTodo.empty()) {
            if (!todo.empty()) {
                cur = todo.top();
                todo.pop();
            } else {
                cur = delayedTodo.top();
                delayedTodo.pop();
            }

            auto apply = [&](BB* next) {
                if (!next || done.check(next))
                    return;
                if (dom.dominates(cur, next)) {
                    todo.push(next);
                } else {
                    delayedTodo.push(next);
                }
                done.set(next);
            };

            apply(cur->next0);
            apply(cur->next1);

            action(cur);
        }
    }
};

} // namespace pir
} // namespace rir

#endif
