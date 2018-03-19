#ifndef COMPILER_VISITOR_H
#define COMPILER_VISITOR_H

#include "../pir/bb.h"
#include "../pir/pir.h"

#include <deque>
#include <functional>
#include <unordered_set>

namespace rir {
namespace pir {

class Visitor {
  public:
    typedef std::function<bool(BB*)> BBReturnAction;
    typedef std::function<void(BB*)> BBAction;

    template <bool STABLE = false>
    static void run(BB* bb, BBAction action) {
        BB* cur = bb;
        std::deque<BB*> todo;
        std::vector<bool> done(64, false);
        ok(done, cur);

        while (cur) {
            BB* next = nullptr;

            if (cur->next0 && !isOk(done, cur->next0)) {
                if (todo.empty())
                    next = cur->next0;
                else
                    q<STABLE>(todo, cur->next0);
                ok(done, cur->next0);
            }

            if (cur->next1 && !isOk(done, cur->next1)) {
                if (!next && todo.empty()) {
                    next = cur->next1;
                } else {
                    q<STABLE>(todo, cur->next1);
                }
                ok(done, cur->next1);
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
    static bool check(BB* bb, BBReturnAction action) {
        BB* cur = bb;
        std::deque<BB*> todo;
        std::vector<bool> done(128, false);
        ok(done, cur);

        while (cur) {
            BB* next = nullptr;

            if (cur->next0 && !isOk(done, cur->next0)) {
                if (todo.empty())
                    next = cur->next0;
                else
                    q<STABLE>(todo, cur->next0);
                ok(done, cur->next0);
            }

            if (cur->next1 && !isOk(done, cur->next1)) {
                if (!next && todo.empty()) {
                    next = cur->next1;
                } else {
                    q<STABLE>(todo, cur->next1);
                }
                ok(done, cur->next1);
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
                    q<true>(todo, cur->next0);
                done.insert(cur->next0);
            }

            if (cur->next1 && done.find(cur->next1) == done.end()) {
                if (!next && todo.empty()) {
                    next = cur->next1;
                } else {
                    q<true>(todo, cur->next1);
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
    static void ok(std::vector<bool>& done, BB* bb) {
        while (bb->id >= done.size())
            done.resize(done.size() * 2);
        done[bb->id] = true;
    }

    static bool isOk(std::vector<bool>& done, BB* bb) {
        return bb->id < done.size() && done[bb->id];
    }

    template <bool STABLE>
    static void q(std::deque<BB*>& todo, BB* bb) {
        // For analysis random search is faster
        if (STABLE || ((uintptr_t)bb & (1 << 5))) {
            todo.push_back(bb);
        } else {
            todo.push_front(bb);
        }
    }
};
}
}

#endif
