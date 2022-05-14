#ifndef COMPILER_VISITOR_H
#define COMPILER_VISITOR_H

#include "../pir/bb.h"
#include "../pir/code.h"
#include "../pir/instruction.h"
#include "../pir/pir.h"
#include "utils/random.h"

#include <deque>
#include <functional>
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
  private:
    std::vector<bool> done;

  public:
    explicit IDMarker(size_t sz) : done(sz, false){};

    void set(BB* bb) {
        if (bb->id >= done.size()) {
            done.resize(bb->owner->nextBBId * 1.1);
        }

        assert(bb->id < done.size());
        done.at(bb->id) = true;
    }

    bool check(BB* bb) const { return bb->id < done.size() && done.at(bb->id); }
};

struct PointerMarker {
    explicit PointerMarker(size_t sz) {}
    std::unordered_set<BB*> done;
    void set(BB* bb) { done.insert(bb); }
    bool check(BB* bb) const { return done.find(bb) != done.end(); }
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

} // namespace VisitorHelpers

enum class Order { Depth, Breadth, Random };

template <Order ORDER, class Marker, bool VISIT_DEOPT_BRANCH = true>
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

    static void runInDirection(bool forward, BB* bb, const BBAction& action) {
        if (forward)
            run(bb, action);
        else
            runBackward(bb, action);
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

    static void runBackward(BB* bb, const InstrAction& action) {
        runBackward(bb, [action](BB* bb) {
            for (auto i : VisitorHelpers::reverse(*bb))
                action(i);
        });
    }

    static bool checkBackward(BB* bb, const InstrActionPredicate& action) {
        return checkBackward(bb, [action](BB* bb) {
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

    static void runBackward(BB* bb, const InstrBBAction& action) {
        runBackward(bb, [action](BB* bb) {
            for (auto i : VisitorHelpers::reverse(*bb))
                action(i, bb);
        });
    }

    static bool checkBackward(BB* bb, const InstrBBActionPredicate& action) {
        return checkBackward(bb, [action](BB* bb) {
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
        forwardGenericRun<false, BBAction>(bb, nullptr, action);
    }

    static void run(BB* bb, BB* stop, const BBAction& action) {
        forwardGenericRun<false, BBAction>(bb, stop, action);
    }

    static void runPostChange(BB* bb, const BBAction& action) {
        forwardGenericRun<true, BBAction>(bb, nullptr, action);
    }

    static bool check(BB* bb, const BBActionPredicate& action) {
        return forwardGenericRun<false, BBActionPredicate>(bb, nullptr, action);
    }

    static void runBackward(BB* bb, const BBAction& action) {
        backwardGenericRun<false, BBAction>(bb, nullptr, action);
    }

    static bool checkBackward(BB* bb, const BBActionPredicate& action) {
        return backwardGenericRun<false, BBActionPredicate>(bb, nullptr,
                                                            action);
    }

  protected:
    typedef std::function<void(BB*)> Schedule;
    typedef std::function<void(Schedule, BB*)> ScheduleNext;

    template <bool PROCESS_NEW_NODES, typename ActionKind>
    static bool forwardGenericRun(BB* bb, BB* stop, const ActionKind& action) {
        struct Scheduler {
            BB::Successors operator()(BB* cur) const {
                if (!VISIT_DEOPT_BRANCH)
                    return cur->nonDeoptSuccessors();
                return cur->successors();
            }
        };
        constexpr Scheduler scheduler;
        return genericRun<PROCESS_NEW_NODES>(bb, stop, scheduler, action);
    }

    template <bool PROCESS_NEW_NODES, typename ActionKind>
    static bool backwardGenericRun(BB* bb, BB* stop, const ActionKind& action) {
        struct Scheduler {
            const SmallSet<BB*> operator()(BB* cur) const {
                return cur->predecessors();
            }
        };
        constexpr Scheduler scheduler;
        return genericRun<PROCESS_NEW_NODES>(bb, stop, scheduler, action);
    }

    template <bool PROCESS_NEW_NODES, typename Scheduler, typename ActionKind>
    static bool inline genericRun(BB* bb, BB* stop, Scheduler& scheduleNext,
                                  const ActionKind& action) {
        typedef VisitorHelpers::PredicateWrapper<ActionKind> PredicateWrapper;
        const PredicateWrapper predicate = {action};

        BB* cur = bb;
        std::deque<BB*> todo;
        Marker done(bb->owner->nextBBId);
        BB* next = nullptr;
        done.set(cur);
        Random random;

        while (cur) {
            next = nullptr;
            auto schedule = [&](BB* bb) {
                if (!bb || done.check(bb) || cur == stop)
                    return;
                if (!next && todo.empty()) {
                    next = bb;
                } else {
                    enqueue(todo, bb, random);
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

            if (!next) {
                if (!todo.empty()) {
                    next = todo.front();
                    todo.pop_front();
                }
            }

            cur = next;
        }
        assert(todo.empty());

        return true;
    }

  private:
    static bool coinFlip(Random& random) { return random() > (ULONG_MAX / 2L); }

    static void enqueue(std::deque<BB*>& todo, BB* bb, Random& random) {
        // For analysis random search is faster
        if (ORDER == Order::Breadth ||
            (ORDER == Order::Random && coinFlip(random)))
            todo.push_back(bb);
        else
            todo.push_front(bb);
    }
};

class Visitor
    : public VisitorImplementation<Order::Random, VisitorHelpers::IDMarker> {};

class VisitorNoDeoptBranch
    : public VisitorImplementation<Order::Random, VisitorHelpers::IDMarker,
                                   false> {};

class BreadthFirstVisitor
    : public VisitorImplementation<Order::Breadth, VisitorHelpers::IDMarker> {};

template <class Marker = VisitorHelpers::IDMarker>
class DepthFirstVisitor : public VisitorImplementation<Order::Depth, Marker> {};

template <class Marker = VisitorHelpers::IDMarker>
class DominatorTreeVisitor {
    using BBAction = VisitorHelpers::BBAction;

    const DominanceGraph& dom;

  public:
    explicit DominatorTreeVisitor(const DominanceGraph& dom) : dom(dom) {}

    void run(BB* entry, BBAction action) const;
};

} // namespace pir
} // namespace rir

#endif
