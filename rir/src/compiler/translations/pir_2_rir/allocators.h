#ifndef PIR_2_RIR_ALLOC_H
#define PIR_2_RIR_ALLOC_H

#include "interpreter/cache.h"
#include "stack_use.h"
#include <set>

namespace rir {
namespace pir {

/*
 * SSAAllocator assigns each instruction to a local variable number, or the
 * stack. It uses the following algorithm:
 *
 * 1. Split phis with moves. This translates the IR to CSSA (see toCSSA).
 * 2. Compute liveness (see liveness.h):
 * 3. For now, just put everything on stack. (step 4 is thus skipped...)
 * 4. Assign the remaining Instructions to local RIR variable numbers
 *    (see computeAllocation):
 *    1. Coalesce all remaining phi with their inputs. This is save since we are
 *       already in CSSA. Directly allocate a register on the fly, such that.
 *    2. Traverse the dominance tree and eagerly allocate the remaining ones
 * 5. For debugging, verify the assignment with a static analysis that simulates
 *    the variable and stack usage (see verify).
 */

class SSAAllocator {
  public:
    DominanceGraph dom;
    Code* code;
    size_t bbsSize;

    LivenessIntervals livenessIntervals;
    StackUseAnalysis sa;

    typedef size_t SlotNumber;
    const static SlotNumber unassignedSlot = 0;
    const static SlotNumber stackSlot = -1;

    std::unordered_map<Value*, SlotNumber> allocation;

    explicit SSAAllocator(Code* code, ClosureVersion* cls, LogStream& log)
        : dom(code), code(code), bbsSize(code->nextBBId),
          livenessIntervals(code, bbsSize),
          sa(cls, code, log, livenessIntervals) {
#ifdef DEBUG_LIVENESS
        std::cerr << "^^^^^^^^^^ "
                  << "SSAAllocator ran liveness analysis"
                  << " ^^^^^^^^^^\n";
        code->printGraphCode(std::cerr, false);
#endif

        computeStackAllocation();
        computeAllocation();
    }

    void computeStackAllocation() {

        static auto toStack = [](Instruction* i) -> bool {
            if (MkEnv::Cast(i))
                return false;
            return true;
        };

        std::unordered_set<Value*> phis;

        Visitor::run(code->entry, [&](Instruction* i) {
            auto p = Phi::Cast(i);
            if (!p || allocation.count(p))
                return;
            if (toStack(p)) {
                allocation[p] = stackSlot;
                p->eachArg(
                    [&](BB*, Value* v) { allocation[v] = allocation[p]; });
            } else {
                phis.insert(p);
                p->eachArg([&](BB*, Value* v) { phis.insert(v); });
            }
        });

        Visitor::run(code->entry, [&](Instruction* i) {
            if (allocation.count(i))
                return;
            if (toStack(i) && phis.count(i) == 0) {
                allocation[i] = stackSlot;
            }
        });
    }

    void computeAllocation() {
        std::unordered_map<SlotNumber, std::unordered_set<Value*>> reverseAlloc;
        auto slotIsAvailable = [&](SlotNumber slot, Value* i) {
            for (auto other : reverseAlloc[slot])
                if (livenessIntervals.interfere(other, i))
                    return false;
            return true;
        };

        // Precolor Phi
        Visitor::run(code->entry, [&](Instruction* i) {
            auto p = Phi::Cast(i);
            if (!p || allocation.count(p))
                return;
            SlotNumber slot = unassignedSlot;
            while (true) {
                ++slot;
                bool success = slotIsAvailable(slot, p);
                if (success) {
                    p->eachArg([&](BB*, Value* v) {
                        if (!slotIsAvailable(slot, v))
                            success = false;
                    });
                }
                if (success)
                    break;
            }
            allocation[i] = slot;
            reverseAlloc[slot].insert(i);
            p->eachArg([&](BB*, Value* v) {
                allocation[v] = slot;
                reverseAlloc[slot].insert(v);
            });
        });

        // Traverse the dominance graph in preorder and eagerly assign slots.
        // We assume that no critical paths exist, ie. we preprocessed the graph
        // such that every phi input is only used exactly once (by the phi).
        DominatorTreeVisitor<>(dom).run(code->entry, [&](BB* bb) {
            auto findFreeSlot = [&](Instruction* i) {
                SlotNumber slot = unassignedSlot;
                for (;;) {
                    ++slot;
                    if (slotIsAvailable(slot, i)) {
                        allocation[i] = slot;
                        reverseAlloc[slot].insert(i);
                        break;
                    }
                };
            };

            size_t pos = 0;
            for (auto i : *bb) {
                ++pos;

                if (!allocation.count(i) && livenessIntervals.count(i)) {
                    // Try to reuse input slot, to reduce moving
                    SlotNumber hint = unassignedSlot;
                    if (i->nargs() > 0) {
                        auto o = Instruction::Cast(i->arg(0).val());
                        if (o && allocation.count(o))
                            hint = allocation.at(o);
                    }
                    if (hint != unassignedSlot && hint != stackSlot &&
                        slotIsAvailable(hint, i)) {
                        allocation[i] = hint;
                        reverseAlloc[hint].insert(i);
                    } else {
                        findFreeSlot(i);
                    }
                }
            }
        });
    }

    void print(std::ostream& out) {
        out << "Allocation\n";
        for (auto a : allocation) {
            out << "  ";
            a.first->printRef(out);
            out << ": ";
            if (onStack(a.first))
                out << "stack";
            else
                out << a.second;
            out << "\n";
        }
        out << "  dead: ";
        BreadthFirstVisitor::run(code->entry, [&](Instruction* i) {
            if (!hasSlot(i)) {
                i->printRef(out);
                out << " ";
            }
        });
        out << "\n"
            << "  # slots: " << slots() << "\n";
    }

    void verify() {

        // Explore all possible traces and verify the allocation
        typedef std::pair<BB*, BB*> Jmp;
        typedef std::unordered_map<size_t, Instruction*> RegisterFile;
        typedef std::deque<Instruction*> Stack;
        typedef std::function<void(BB*, BB*, RegisterFile&, Stack&)> VerifyBB;
        std::set<Jmp> branchTaken;

        VerifyBB verifyBB = [&](BB* pred, BB* bb, RegisterFile& reg,
                                Stack& stack) {
            for (auto i : *bb) {
                for (auto drop : sa.toDrop(i)) {
                    for (auto it = stack.begin(); it != stack.end(); ++it) {
                        if (drop == *it) {
                            stack.erase(it);
                            break;
                        }
                    }
                }
                if (auto phi = Phi::Cast(i)) {
                    SlotNumber slot = allocation.at(phi);
                    phi->eachArg([&](BB*, Value* a) {
                        auto ia = Instruction::Cast(a);
                        if (!ia)
                            return;
                        if (!allocation.count(ia)) {
                            std::cerr << "REG alloc fail: ";
                            phi->printRef(std::cerr);
                            std::cerr << " needs ";
                            ia->printRef(std::cerr);
                            std::cerr << " but is not allocated\n";
                            assert(false);
                        } else if (allocation[ia] != slot) {
                            std::cerr << "REG alloc fail: ";
                            phi->printRef(std::cerr);
                            std::cerr << " and it's input ";
                            ia->printRef(std::cerr);
                            std::cerr << " have different allocations: ";
                            if (allocation[phi] == stackSlot)
                                std::cerr << "stack";
                            else
                                std::cerr << allocation[phi];
                            std::cerr << " vs ";
                            if (allocation[ia] == stackSlot)
                                std::cerr << "stack";
                            else
                                std::cerr << allocation[ia];
                            std::cerr << "\n";
                            assert(false);
                        }
                    });
                    // Make sure the argument slot is initialized
                    if (slot != stackSlot && reg.count(slot) == 0) {
                        std::cerr << "REG alloc fail: phi ";
                        phi->printRef(std::cerr);
                        std::cerr << " is reading from an unititialized slot\n";
                        assert(false);
                    }
                    if (slot == stackSlot) {
                        bool found = false;
                        for (auto it = stack.begin(); it != stack.end(); ++it) {
                            phi->eachArg([&](BB* phiInput, Value* phiArg) {
                                if (phiInput == pred && phiArg == *it) {
                                    stack.erase(it);
                                    found = true;
                                }
                            });
                            if (found)
                                break;
                        }
                        if (!found) {
                            std::cerr << "REG alloc fail: phi ";
                            phi->printRef(std::cerr);
                            std::cerr << " input is missing on stack\n";
                            assert(false);
                        }
                    }
                } else {
                    // Make sure all our args are live
                    size_t argNum = 0;
                    i->eachArg([&](Value* a) {
                        auto ia = Instruction::Cast(a);
                        if (!ia || !ia->producesRirResult()) {
                            argNum++;
                            return;
                        }
                        if (!allocation.count(ia)) {
                            std::cerr << "REG alloc fail: ";
                            i->printRef(std::cerr);
                            std::cerr << " needs ";
                            ia->printRef(std::cerr);
                            std::cerr << " but is not allocated\n";
                            assert(false);
                        } else {
                            SlotNumber slot = allocation.at(ia);
                            if (slot == stackSlot) {
                                bool found = false;
                                for (auto it = stack.begin(); it != stack.end();
                                     ++it) {
                                    if (ia == *it) {
                                        found = true;
                                        if (lastUse(i, argNum)) {
                                            stack.erase(it);
                                        }
                                        break;
                                    }
                                }
                                if (!found) {
                                    std::cerr << "REG alloc fail: ";
                                    i->printRef(std::cerr);
                                    std::cerr << " needs ";
                                    ia->printRef(std::cerr);
                                    std::cerr << " but it's missing on stack\n";
                                    assert(false);
                                }
                            } else {
                                // Make sure the argument slot is initialized
                                if (reg.count(slot) == 0) {
                                    std::cerr << "REG alloc fail: ";
                                    i->printRef(std::cerr);
                                    std::cerr << " is reading its argument ";
                                    ia->printRef(std::cerr);
                                    std::cerr << "from an unititialized slot\n";
                                    assert(false);
                                }
                                if (reg.at(slot) != ia) {
                                    std::cerr << "REG alloc fail: ";
                                    i->printRef(std::cerr);
                                    std::cerr << " needs ";
                                    ia->printRef(std::cerr);
                                    std::cerr << " but slot " << slot
                                              << " was overridden by ";
                                    reg.at(slot)->printRef(std::cerr);
                                    std::cerr << "\n";
                                    assert(false);
                                }
                            }
                        }
                        argNum++;
                    });
                }

                // Remember this instruction if it writes to a slot
                if (allocation.count(i)) {
                    if (allocation.at(i) == stackSlot) {
                        if (i->producesRirResult() && !sa.dead(i)) {
                            stack.push_back(i);
                        }
                    } else {
                        reg[allocation.at(i)] = i;
                    }
                }
            }

            if (bb->isExit()) {
                if (stack.size() != 0) {
                    std::cerr << "REG alloc fail: BB" << bb->id
                              << " tries to return with " << stack.size()
                              << " elements on the stack\n";
                    assert(false);
                }
            }

            auto succs = bb->succsessors();
            for (auto suc : succs) {
                if (branchTaken.count({bb, suc}))
                    continue;
                branchTaken.insert({bb, suc});
                if (succs.size() > 1 && *succs.begin() == suc) {
                    // Need to copy here, since we are gonna explore
                    // other branch next
                    RegisterFile regC = reg;
                    Stack stackC = stack;
                    verifyBB(bb, suc, regC, stackC);
                    continue;
                }
                verifyBB(bb, suc, reg, stack);
            }
        };

        {
            RegisterFile f;
            Stack s;
            verifyBB(nullptr, code->entry, f, s);
        }
    }

    size_t operator[](Value* v) const {
        assert(allocation.at(v) != stackSlot);
        return allocation.at(v) - 1;
    }

    size_t slots() const {
        unsigned max = 0;
        for (auto a : allocation) {
            if (a.second != stackSlot && max < a.second)
                max = a.second;
        }
        return max;
    }

    bool onStack(Value* v) const { return allocation.at(v) == stackSlot; }

    bool hasSlot(Value* v) const { return allocation.count(v); }

    size_t getStackOffset(Instruction* instr, std::vector<bool>& used,
                          Value* what, bool remove) const {

        auto stack = sa.stackBefore(instr);
        assert(stack.size() == used.size());

        size_t offset = 0;
        size_t usedIdx = used.size() - 1;
        auto i = stack.rbegin();
        while (i != stack.rend()) {
            if (*i == what) {
                if (remove) {
                    used[usedIdx] = true;
                }
                return offset;
            }
            if (hasSlot(*i) && onStack(*i) && !used[usedIdx]) {
                ++offset;
            }
            ++i;
            --usedIdx;
        }
        assert(false && "Value wasn't found on the stack.");
        return -1;
    }

    size_t stackPhiOffset(Instruction* instr, Phi* phi) const {
        auto stack = sa.stackBefore(instr);
        size_t offset = 0;
        for (auto i = stack.rbegin(); i != stack.rend(); ++i) {
            if (*i == phi || phi->anyArg([&](Value* v) { return *i == v; }))
                return offset;
            if (hasSlot(*i) && onStack(*i))
                ++offset;
        }
        assert(false && "Phi wasn't found on the stack.");
        return -1;
    }

    // Check if v is needed after argument argNumber of instr
    bool lastUse(Instruction* instr, size_t argNumber) const {
        assert(argNumber < instr->nargs());
        Value* v = instr->arg(argNumber).val();
        auto stack = sa.stackAfter(instr);
        for (auto i = stack.begin(); i != stack.end(); ++i)
            if (*i == v)
                return false;
        for (size_t i = argNumber + 1; i < instr->nargs(); ++i)
            if (instr->arg(i).val() == v)
                return false;
        return true;
    }
};

class CachePosition {
  public:
    typedef size_t SlotNumber;
    typedef std::pair<SEXP, Value*> NameAndEnv;
    typedef std::pair<size_t, size_t> StartSize;

    explicit CachePosition(Code* code) {
        std::unordered_map<Value*,
                           std::unordered_map<NameAndEnv, size_t, pairhash>>
            found;

        // Count how many instruction use a binding. We use this as a proxy for
        // how many times it might get accessed at runtime.
        Visitor::run(code->entry, [&](Instruction* i) {
            if (auto ld = LdVar::Cast(i)) {
                found[ld->env()][NameAndEnv(ld->varName, ld->env())]++;
            } else if (auto st = StVar::Cast(i)) {
                found[st->env()][NameAndEnv(st->varName, st->env())]++;
            }
        });

        // First all global envs
        for (const auto& env : found) {
            auto e = Env::Cast(env.first);
            if (e && e->rho != R_BaseEnv && e->rho != R_BaseNamespace) {
                for (const auto& key : env.second) {
                    // If a binding is used only once, then it does not pay off
                    // to cache (yes, it could be used in a loop, this is just a
                    // static approximation).
                    if (key.second <= 2)
                        continue;
                    if (uniqueNumbers.size() == MAX_CACHE_SIZE)
                        break;
                    uniqueNumbers.emplace(key.first, uniqueNumbers.size());
                }
            }
        }
        globalEnvsCacheSize_ = uniqueNumbers.size();

        // At runtime looking up a binding in a local environment is a linear
        // search. Therefore, the smaller then environment, the faster the
        // lookup. E.g. looking up in an env of size one is not much slower than
        // going through the cache. Therefore we will scale the limit on the
        // size of the environment.
        auto minAccessEnvSize = [](size_t s) -> unsigned {
            if (s == 0)
                return 99999; // This env seems empty, caching is just a waste.
            else if (s == 1)
                return 3;
            else if (s < 5)
                return 2;
            return 1;
        };
        for (const auto& env : found) {
            if (MkEnv::Cast(env.first)) {
                envCacheRanges[env.first].first = uniqueNumbers.size();
                auto limit = minAccessEnvSize(env.second.size());
                for (const auto& key : env.second) {
                    if (key.second <= limit)
                        continue;
                    if (uniqueNumbers.size() == MAX_CACHE_SIZE)
                        break;
                    uniqueNumbers.emplace(key.first, uniqueNumbers.size());
                }
                envCacheRanges[env.first].second =
                    uniqueNumbers.size() - envCacheRanges[env.first].first;
            }
        }
    }

    size_t size() const { return uniqueNumbers.size(); }

    SlotNumber isCached(const NameAndEnv& key) const {
        return uniqueNumbers.count(key);
    }

    SlotNumber indexOf(const NameAndEnv& key) const {
        return uniqueNumbers.at(key);
    }

    unsigned globalEnvsCacheSize() const { return globalEnvsCacheSize_; }

    void ifCacheRange(MkEnv* env, std::function<void(StartSize)> apply) const {
        if (!env->stub && envCacheRanges.count(env))
            apply(envCacheRanges.at(env));
    }

  private:
    std::unordered_map<NameAndEnv, SlotNumber, pairhash> uniqueNumbers;
    std::unordered_map<Value*, StartSize> envCacheRanges;
    size_t globalEnvsCacheSize_;
};

} // namespace pir
} // namespace rir
#endif
