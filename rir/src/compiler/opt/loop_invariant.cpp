#include "../pir/pir_impl.h"
#include "../transform/bb.h"
#include "../util/cfg.h"
#include "../util/safe_builtins_list.h"
#include "../util/visitor.h"
#include "R/r.h"
#include "pass_definitions.h"

namespace rir {
namespace pir {

typedef std::pair<BB*, BB*> BackEdge;
typedef std::function<bool(Instruction*)> InstrActionPredicate;

auto noEnvironmentTainting = [](Instruction* i) {
    if (LdFun::Cast(i))
        return true;

    if (auto call = CallBuiltin::Cast(i))
        return SafeBuiltinsList::nonObject(call->builtinId);

    if (CallSafeBuiltin::Cast(i))
        return true;

    if (CallInstruction::CastCall(i))
        return false;

    return !(i->mayUseReflection());
};

struct NaturalLoop {
    BB* header;
    std::vector<BB*> tails; // nodes that precede the header
    std::unordered_set<BB*> body;

    bool isSafeToHoistLoads() { return run(noEnvironmentTainting); }

    bool overwrites(SEXP binding) {
        return !run([&](Instruction* i) {
            // A store overwrites the binding
            SEXP varName = nullptr;
            if (auto store = StVar::Cast(i))
                varName = store->varName;
            else if (auto store = StVarSuper::Cast(i))
                varName = store->varName;

            if (varName && varName == binding)
                return false;

            // An environment overwrites the binding
            if (auto env = MkEnv::Cast(i)) {
                auto safe = true;
                env->eachLocalVar([&](SEXP name, Value* v) {
                    if (name == binding) {
                        safe = false;
                        return;
                    }
                });
                if (!safe)
                    return false;
            }

            return true;
        });
    }

    bool run(const InstrActionPredicate& action) {
        auto answer = true;
        for (auto bb : body) {
            for (auto instruction : *bb) {
                if (!action(instruction)) {
                    answer = false;
                    break;
                }
            }
        }
        return answer;
    }

    /*
     * TODO: This just finds a preheader when there is one.
     * We should create a preheader when there is non.
     */
    BB* outOfLoopPredecessor(const CFG& cfg) {
        std::vector<BB*> outOfLoopPredecessor;
        for (auto pred : cfg.immediatePredecessors(header)) {
            if (!body.count(pred))
                outOfLoopPredecessor.push_back(pred);
        }

        if (outOfLoopPredecessor.size() != 1) {
            return nullptr;
        } else {
            return outOfLoopPredecessor.front();
        }
    }
};

void addNaturalLoop(const CFG& cfg, std::vector<size_t>& nesting,
                    std::vector<size_t>& order, NaturalLoop& loop) {
    std::vector<bool> seen(cfg.size());
    std::stack<BB*> todo;
    size_t counter = 0;

    // If a node is part of the loop, mark it as seen, increment its global
    // "nesting" count, and keep track of the order we discovered nodes
    auto addToLoop = [&](BB* bb) {
        seen[bb->id] = true;
        loop.body.insert(bb);
        nesting[bb->id] += 1;
        order[bb->id] = counter++;
    };

    // Loop header is part of body, mark as seen
    addToLoop(loop.header);

    // Start reverse DFS from the nodes that precede the header
    for (const auto& n : loop.tails) {
        todo.push(n);
    }

    while (!todo.empty()) {
        BB* cur = todo.top();
        todo.pop();

        if (!seen[cur->id]) {
            addToLoop(cur);
            for (const auto& p : cfg.immediatePredecessors(cur)) {
                todo.push(p);
            }
        }
    }

    // loop header should be the "last" in terms of order, so reset its count
    order[loop.header->id] = counter++;
}

void tryHoistingFnLookups(const CFG& cfg, NaturalLoop& loop) {
    BB* targetBB = loop.outOfLoopPredecessor(cfg);
    if (targetBB && loop.isSafeToHoistLoads()) {
        for (auto bb : loop.body) {
            auto ip = bb->begin();
            while (ip != bb->end()) {
                Instruction* i = *ip;
                auto next = ip + 1;

                SEXP binding = nullptr;
                if (auto ldFun = LdFun::Cast(i)) {
                    binding = ldFun->varName;
                } else if (auto ldVar = LdVar::Cast(i)) {
                    binding = ldVar->varName;
                }

                if (binding && !loop.overwrites(binding))
                    next = bb->moveToEnd(ip, targetBB);

                ip = next;
            }
        }
    }
}

void LoopInvariant::apply(RirCompiler&, ClosureVersion* function,
                          LogStream& log) const {
    CFG cfg(function);
    DominanceGraph dom(function);
    std::vector<size_t> nesting(function->nextBBId);
    std::vector<size_t> order(function->nextBBId);

    // Find back edges, i.e. edges n->h where h dominates n
    std::vector<BackEdge> backEdges;
    Visitor::run(function->entry, [&](BB* maybeHeader) {
        for (const auto& n : cfg.immediatePredecessors(maybeHeader)) {
            if (dom.dominates(maybeHeader, n)) {
                backEdges.emplace_back(std::make_pair(n, maybeHeader));
            }
        }
    });

    // Create entries in the loop map, but only for unique headers
    std::unordered_map<BB*, NaturalLoop> loops;
    for (const auto& be : backEdges) {
        BB* header = be.second;
        if (!loops.count(header)) {
            NaturalLoop l = {header, {be.first}, {}};
            loops.emplace(header, l);
        } else {
            loops[header].tails.push_back(be.first);
        }
    }

    // Fill out the nodes of each loop and then run the optimization
    for (auto& l : loops) {
        addNaturalLoop(cfg, nesting, order, l.second);
        tryHoistingFnLookups(cfg, l.second);
    }
}
} // namespace pir
} // namespace rir
