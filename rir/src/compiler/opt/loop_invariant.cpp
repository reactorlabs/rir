#include "../analysis/loop_detection.h"
#include "../pir/pir_impl.h"
#include "../util/cfg.h"
#include "../util/safe_builtins_list.h"
#include "pass_definitions.h"
#include <unordered_map>

namespace rir {
namespace pir {

bool taintsEnvironment(Instruction* i) {
    // For these instructions we test later they don't change the particular
    // binding
    if (StVar::Cast(i) || StVarSuper::Cast(i) || MkEnv::Cast(i))
        return false;

    /*If there is a force for the load, and we can prove the loop does not
    overload the binding, then the force is redudant*/
    if (auto force = Force::Cast(i)) {
        return LdVar::Cast(force->input()->followCastsAndForce()) == nullptr;
    }

    if (auto call = CallBuiltin::Cast(i)) {
        if (SafeBuiltinsList::nonObject(call->builtinId)) {
            auto taints = false;
            call->eachCallArg([&](Value* arg) {
                if (arg->type.maybeObj())
                    taints = true;
            });
            return taints;
        }
    }

    if (i->changesEnv()) {
        std::cout << "change env:";
        i->print(std::cout);
    }
    return i->changesEnv();
}

bool isSafeToHoistLoads(const LoopDetection::Loop& loop) {
    for (auto bb : loop) {
        for (auto instruction : *bb) {
            if (taintsEnvironment(instruction))
                return false;
        }
    }
    return true;
}

bool instructionOverwritesBinding(Instruction* i, SEXP binding) {
    SEXP varName = nullptr;
    if (auto store = StVar::Cast(i))
        varName = store->varName;
    else if (auto store = StVarSuper::Cast(i))
        varName = store->varName;

    if (varName && varName == binding)
        return true;

    // An environment overwrites the binding
    if (auto env = MkEnv::Cast(i)) {
        bool overwrites = false;
        env->eachLocalVar([&](SEXP name, Value*) {
            if (name == binding) {
                overwrites = true;
                return;
            }
        });
        return overwrites;
    }

    return false;
}

bool loopOverwritesBinding(LoopDetection::Loop& loop, SEXP binding) {
    for (auto bb : loop) {
        for (auto instruction : *bb) {
            if (instructionOverwritesBinding(instruction, binding))
                return true;
        }
    }
    return false;
}

bool replaceWithOuterLoopEquivalent(Instruction* instruction,
                                    DominanceGraph& dom, BB* start) {
    std::vector<Instruction*> betweenLoadandLoop;
    Instruction* found = nullptr;
    auto current = start;
    SEXP binding = nullptr;
    if (auto ldFun = LdFun::Cast(instruction)) {
        binding = ldFun->varName;
    } else if (auto ldVar = LdVar::Cast(instruction)) {
        binding = ldVar->varName;
    }

    while (current != nullptr && found != nullptr) {
        auto it = current->rbegin();
        while (it != current->rend()) {
            auto currentInst = *it;
            SEXP otherBinding = nullptr;
            if (auto ldFun = LdFun::Cast(currentInst)) {
                binding = ldFun->varName;
            } else if (auto ldVar = LdVar::Cast(currentInst)) {
                binding = ldVar->varName;
            }
            if (currentInst->tag == instruction->tag &&
                binding == otherBinding &&
                instruction->env() == currentInst->env()) {
                found = *it;
                break;
            } else {
                betweenLoadandLoop.push_back(currentInst);
            }
            it++;
        }
        current = dom.immediateDominator(current);
    }

    if (found != nullptr) {
        for (auto instruction : betweenLoadandLoop) {
            if (instructionOverwritesBinding(instruction, binding) ||
                taintsEnvironment(instruction))
                return false;
        }
    }

    instruction->replaceUsesWith(found);
    instruction->bb()->remove(instruction);
    return true;
}

void LoopInvariant::apply(RirCompiler&, ClosureVersion* function,
                          LogStream& log) const {
    LoopDetection loops(function);
    CFG cfg(function);

    for (auto& loop : loops) {
        std::unordered_map<Instruction*, BB*> loads;
        BB* targetBB = loop.preheader(cfg);
        std::cout << "entre";
        targetBB->print(std::cout, false);
        loop.print(std::cout, false);
        std::cout << isSafeToHoistLoads(loop);
        std::cout << "\n";
        auto safeToHoist = false;
        if (targetBB && isSafeToHoistLoads(loop)) {
            safeToHoist = true;
            for (auto bb : loop) {
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

                    if (binding && !loopOverwritesBinding(loop, binding))
                        loads.emplace(i, bb);
                    else
                        safeToHoist = false;

                    ip = next;
                }
            }
        }

        if (safeToHoist) {
            DominanceGraph dom(function);
            for (auto loadAndBB : loads) {
                auto load = loadAndBB.first;
                auto bb = loadAndBB.second;
                // This should be the case if loop peeling succeded
                if (!replaceWithOuterLoopEquivalent(load, dom, targetBB)) {
                    bb->moveToEnd(bb->atPosition(load), targetBB);
                }
            }
        }
    }
}
} // namespace pir
} // namespace rir
