#include "allocator.h"
#include "compiler/native/representation_llvm.h"
#include "compiler/util/visitor.h"
#include "interpreter/profiler.h"

#include <set>
#include <unordered_map>
#include <unordered_set>

namespace rir {
namespace pir {

bool NativeAllocator::needsASlot(Instruction* i) const {
    return needsAVariable(i) && Rep::Of(i) == Rep::SEXP;
}

void NativeAllocator::compute() {
    std::unordered_map<Slot, std::unordered_set<Instruction*>> reverseAlloc;

    auto interfere = [&](Instruction* a, Instruction* b) {
        // Ensure we preserve slots for variables with typefeedback to make
        // them accessible to the runtime profiler.
        // TODO: this needs to be replaced by proper mapping of slots.
        if (RuntimeProfiler::enabled() && a != b &&
            (a->typeFeedback().feedbackOrigin.hasSlot() ||
             b->typeFeedback().feedbackOrigin.hasSlot()))
            return true;
        return livenessIntervals.interfere(a, b);
    };
    auto slotIsAvailable = [&](Slot slot, Value* v) {
        if (auto i = Instruction::Cast(v)) {
            for (auto other : reverseAlloc[slot]) {
                if (interfere(other, i))
                    return false;
            }
        }
        return true;
    };

    // Precolor Phi
    Visitor::run(code->entry, [&](Instruction* i) {
        if (auto p = Phi::Cast(i)) {
            if (allocation.count(p) || !needsASlot(p))
                return;
            auto testSlot = [&](size_t slot) {
                bool success = slotIsAvailable(slot, p);
                if (success) {
                    p->eachArg([&](BB*, Value* v) {
                        if (!slotIsAvailable(slot, v))
                            success = false;
                    });
                }
                return success;
            };
            Slot slot = unassignedSlot;
            auto h = hints.find(i);
            if (h != hints.end() && testSlot(h->second)) {
                slot = h->second;
            } else {
                while (true) {
                    ++slot;
                    if ((h == hints.end() || h->second != slot) &&
                        testSlot(slot))
                        break;
                }
            }
            allocation[i] = slot;
            reverseAlloc[slot].insert(i);
            p->eachArg([&](BB*, Value* v) {
                if (auto j = Instruction::Cast(v)) {
                    allocation[j] = slot;
                    reverseAlloc[slot].insert(j);
                    // Backwards propagate the slot as a hint
                    while (j) {
                        if (j->nargs() == 0)
                            break;
                        j = Instruction::Cast(j->arg(0).val());
                        if (!j)
                            break;
                        if (hints.count(j))
                            break;
                        hints[j] = slot;
                    }
                }
            });
        }
    });

    // Traverse the dominance graph in preorder and eagerly assign slots.
    // We assume that no critical paths exist, ie. we preprocessed the graph
    // such that every phi input is only used exactly once (by the phi).
    DominatorTreeVisitor<>(dom).run(code->entry, [&](BB* bb) {
        auto findFreeSlot = [&](Instruction* i) {
            Slot slot = unassignedSlot;
            while (true) {
                ++slot;
                if (slotIsAvailable(slot, i)) {
                    allocation[i] = slot;
                    reverseAlloc[slot].insert(i);
                    break;
                }
            }
        };

        size_t pos = 0;
        for (auto i : *bb) {
            ++pos;

            if (!needsASlot(i))
                continue;

            if (!allocation.count(i) && livenessIntervals.count(i)) {
                // Try to reuse input slot, to reduce moving
                Slot hint = unassignedSlot;
                auto h = hints.find(i);
                if (h != hints.end()) {
                    hint = h->second;
                } else if (i->nargs() > 0) {
                    auto o = Instruction::Cast(i->arg(0).val());
                    if (o && allocation.count(o))
                        hint = allocation.at(o);
                }
                if (hint != unassignedSlot && slotIsAvailable(hint, i)) {
                    allocation[i] = hint;
                    reverseAlloc[hint].insert(i);
                } else {
                    findFreeSlot(i);
                }
            }
        }
    });

    for (auto a : allocation) {
        if (slots_ < a.second)
            slots_ = a.second;
    }
}

void NativeAllocator::verify() {
    // Explore all possible traces and verify the allocation
    typedef std::pair<BB*, BB*> Jmp;
    typedef std::unordered_map<size_t, Instruction*> RegisterFile;
    typedef std::function<void(BB*, BB*, RegisterFile&)> VerifyBB;
    std::set<Jmp> branchTaken;

    VerifyBB verifyBB = [&](BB* pred, BB* bb, RegisterFile& reg) {
        for (auto i : *bb) {
            if (auto phi = Phi::Cast(i)) {
                if (needsASlot(phi)) {
                    Slot slot = allocation.at(phi);
                    phi->eachArg([&](BB*, Value* a) {
                        auto ia = Instruction::Cast(a);
                        if (!ia)
                            return;
                        if (!allocation.count(ia)) {
                            code->printCode(std::cout, true, false);
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
                            std::cerr << allocation[phi];
                            std::cerr << " vs ";
                            std::cerr << allocation[ia];
                            std::cerr << "\n";
                            assert(false);
                        }
                    });
                    // Make sure the argument slot is initialized
                    if (reg.count(slot) == 0) {
                        std::cerr << "REG alloc fail: phi ";
                        phi->printRef(std::cerr);
                        std::cerr << " is reading from an unititialized slot\n";
                        assert(false);
                    }
                }
            } else {
                // Make sure all our args are live
                size_t argNum = 0;
                i->eachArg([&](Value* a) {
                    auto ia = Instruction::Cast(a);
                    if (!ia || !needsASlot(ia)) {
                        argNum++;
                        return;
                    }
                    if (!allocation.count(ia)) {
                        code->printCode(std::cout, true, false);
                        std::cerr << "REG alloc fail: ";
                        i->printRef(std::cerr);
                        std::cerr << " needs ";
                        ia->printRef(std::cerr);
                        std::cerr << " but is not allocated\n";
                        assert(false);
                    } else {
                        Slot slot = allocation.at(ia);
                        // Make sure the argument slot is initialized
                        if (reg.count(slot) == 0) {
                            std::cerr << "REG alloc fail: ";
                            i->printRef(std::cerr);
                            std::cerr << " is reading its argument ";
                            ia->printRef(std::cerr);
                            std::cerr << "from an uninitialized slot\n";
                            assert(false);
                        }
                        if (reg.at(slot) != ia) {
                            code->printCode(std::cerr, true, false);
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
                    argNum++;
                });
            }

            // Remember this instruction if it writes to a slot
            if (allocation.count(i)) {
                reg[allocation.at(i)] = i;
            }
        }

        auto succs = bb->successors();
        for (auto suc : succs) {
            if (branchTaken.count({bb, suc}))
                continue;
            branchTaken.insert({bb, suc});
            if (succs.size() > 1 && *succs.begin() == suc) {
                // Need to copy here, since we are gonna explore
                // other branch next
                RegisterFile regC = reg;
                verifyBB(bb, suc, regC);
                continue;
            }
            verifyBB(bb, suc, reg);
        }
    };

    {
        RegisterFile f;
        verifyBB(nullptr, code->entry, f);
    }
}

NativeAllocator::NativeAllocator(Code* code,
                                 const LivenessIntervals& livenessIntervals)
    : code(code), dom(code), livenessIntervals(livenessIntervals) {
    compute();
    verify();
}

bool NativeAllocator::needsAVariable(Instruction* i) const {
    return !LdArg::Cast(i) && !i->type.isVoid() && !i->type.isVirtualValue() &&
           !i->type.isCompositeValue();
}

NativeAllocator::Slot NativeAllocator::operator[](Instruction* i) const {
    return allocation.at(i) - 1;
}

size_t NativeAllocator::slots() const { return slots_; }

} // namespace pir
} // namespace rir
