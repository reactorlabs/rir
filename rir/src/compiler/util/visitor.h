#ifndef COMPILER_VISITOR_H
#define COMPILER_VISITOR_H

#include "../pir/bb.h"
#include "../pir/pir.h"

#include <deque>
#include <functional>
#include <random>
#include <unordered_set>

namespace rir {
namespace pir {

class Visitor {
  public:
    typedef std::function<bool(BB*)> BBActionPredicate;
    typedef std::function<void(BB*)> BBAction;

    template <bool STABLE = false>
    static void run(BB* bb, BBAction action) {
        BB* cur = bb;
        std::deque<BB*> todo;
        std::vector<bool> done(64, false);
        markDone(done, cur);

        while (cur) {
            BB* next = nullptr;

            if (cur->next0 && !isDone(done, cur->next0)) {
                if (todo.empty())
                    next = cur->next0;
                else
                    enqueue<STABLE>(todo, cur->next0);
                markDone(done, cur->next0);
            }

            if (cur->next1 && !isDone(done, cur->next1)) {
                if (!next && todo.empty()) {
                    next = cur->next1;
                } else {
                    enqueue<STABLE>(todo, cur->next1);
                }
                markDone(done, cur->next1);
            }

            if (!next) {
                if (!todo.empty()) {
                    next = todo.front();
                    todo.pop_front();
                }
            }

            action(cur);

            cur = next;
        }
        assert(todo.empty());
    }

    template <bool STABLE = false>
    static bool check(BB* bb, BBActionPredicate action) {
        BB* cur = bb;
        std::deque<BB*> todo;
        std::vector<bool> done(128, false);
        markDone(done, cur);

        while (cur) {
            BB* next = nullptr;

            if (cur->next0 && !isDone(done, cur->next0)) {
                if (todo.empty())
                    next = cur->next0;
                else
                    enqueue<STABLE>(todo, cur->next0);
                markDone(done, cur->next0);
            }

            if (cur->next1 && !isDone(done, cur->next1)) {
                if (!next && todo.empty()) {
                    next = cur->next1;
                } else {
                    enqueue<STABLE>(todo, cur->next1);
                }
                markDone(done, cur->next1);
            }

            if (!next) {
                if (!todo.empty()) {
                    next = todo.front();
                    todo.pop_front();
                }
            }

            if (!action(cur))
                return false;

            cur = next;
        }
        assert(todo.empty());

        return true;
    }

    // This visitor does not rely on stable bb ids, use for renumbering
    static void runWithChangingIds(BB* bb, BBAction action) {
        BB* cur = bb;
        std::deque<BB*> todo;
        std::unordered_set<BB*> done;
        done.insert(cur);

        while (cur) {
            BB* next = nullptr;

            if (cur->next0 && done.find(cur->next0) == done.end()) {
                if (todo.empty())
                    next = cur->next0;
                else
                    enqueue<true>(todo, cur->next0);
                done.insert(cur->next0);
            }

            if (cur->next1 && done.find(cur->next1) == done.end()) {
                if (!next && todo.empty()) {
                    next = cur->next1;
                } else {
                    enqueue<true>(todo, cur->next1);
                }
                done.insert(cur->next1);
            }

            if (!next) {
                if (!todo.empty()) {
                    next = todo.front();
                    todo.pop_front();
                }
            }

            action(cur);

            cur = next;
        }
        assert(todo.empty());
    }

  private:
    static std::random_device rd;
    static std::mt19937 gen;
    static std::bernoulli_distribution coin;

    static void markDone(std::vector<bool>& done, BB* bb) {
        while (bb->id >= done.size())
            done.resize(done.size() * 2);
        done[bb->id] = true;
    }

    static bool isDone(std::vector<bool>& done, BB* bb) {
        return bb->id < done.size() && done[bb->id];
    }

    template <bool STABLE>
    static void enqueue(std::deque<BB*>& todo, BB* bb) {
        // For analysis random search is faster
        if (STABLE || coin(gen)) {
            todo.push_back(bb);
        } else {
            todo.push_front(bb);
        }
    }
};
}
}

#endif
