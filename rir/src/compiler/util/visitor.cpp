#include "visitor.h"
#include "cfg.h"
#include "utils/Set.h"
#include <unordered_map>

namespace rir {
namespace pir {

template <>
void DominatorTreeVisitor<VisitorHelpers::IDMarker>::run(
    BB* entry, BBAction action) const {
    VisitorHelpers::IDMarker done;
    std::deque<BB*> todo;
    todo.push_back(entry);

    while (!todo.empty()) {
        BB* cur = todo.front();
        todo.pop_front();
        if (!done.check(cur)) {
            done.set(cur);
            std::vector<BB*> temp;
            dom.dominatorTreeNext(cur, [&](BB* bb) { temp.push_back(bb); });
            todo.insert(todo.begin(), temp.begin(), temp.end());
            action(cur);
        }
    }
}

template <>
void DominatorTreeVisitor<VisitorHelpers::PointerMarker>::run(
    BB* entry, BBAction action) const {
    // DominanceGraph assumes stable BB ids. For pointer marker strategy (which
    // allows renumbering) we need to cache the dominator tree first.
    std::unordered_map<BB*, SmallSet<BB*>> cache;
    {
        VisitorHelpers::IDMarker done;
        std::stack<BB*> todo;
        todo.push(entry);

        while (!todo.empty()) {
            BB* cur = todo.top();
            todo.pop();
            if (!done.check(cur)) {
                done.set(cur);
                dom.dominatorTreeNext(cur, [&](BB* bb) {
                    cache[cur].insert(bb);
                    todo.push(bb);
                });
            }
        }
    }
    {
        VisitorHelpers::PointerMarker done;
        std::deque<BB*> todo;
        todo.push_back(entry);

        while (!todo.empty()) {
            BB* cur = todo.front();
            todo.pop_front();
            if (!done.check(cur)) {
                done.set(cur);
                // Relies on SmallSet maintaining insertion order
                todo.insert(todo.begin(), cache[cur].begin(), cache[cur].end());
                action(cur);
            }
        }
    }
}

} // namespace pir
} // namespace rir
