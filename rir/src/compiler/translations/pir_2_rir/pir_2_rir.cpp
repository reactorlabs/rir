#include "pir_2_rir.h"
#include "../../analysis/last_env.h"
#include "../../pir/pir_impl.h"
#include "../../pir/value_list.h"
#include "../../transform/bb.h"
#include "../../util/cfg.h"
#include "../../util/visitor.h"
#include "compiler/analysis/reference_count.h"
#include "compiler/analysis/verifier.h"
#include "compiler/parameter.h"
#include "interpreter/instance.h"
#include "ir/CodeStream.h"
#include "ir/CodeVerifier.h"
#include "runtime/DispatchTable.h"
#include "simple_instruction_list.h"
#include "stack_use.h"
#include "utils/FunctionWriter.h"

#include "../../debugging/PerfCounter.h"

#include <algorithm>
#include <chrono>
#include <iomanip>
#include <list>
#include <sstream>

namespace rir {
namespace pir {

namespace {

/*
 * SSAAllocator assigns each instruction to a local variable number, or the
 * stack. It uses the following algorithm:
 *
 * 1. Split phis with moves. This translates the IR to CSSA (see toCSSA).
 * 2. Compute liveness (see liveness.h):
 * 3. For now, just put everything on stack. (step 4 is thus skipped...)
 * 4. Assign the remaining Instructions to local RIR variable numbers
 *    (see computeAllocation):
 *    1. Coalesc all remaining phi with their inputs. This is save since we are
 *       already in CSSA. Directly allocate a register on the fly, such that.
 *    2. Traverse the dominance tree and eagerly allocate the remaining ones
 * 5. For debugging, verify the assignment with a static analysis that simulates
 *    the variable and stack usage (see verify).
 */
class SSAAllocator {
  public:
    CFG cfg;
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
        : cfg(code), dom(code), code(code), bbsSize(code->nextBBId),
          livenessIntervals(bbsSize, cfg),
          sa(cls, code, log, livenessIntervals) {

        computeStackAllocation();
        computeAllocation();
    }

    void computeStackAllocation() {

        static auto toStack = [](Instruction* i) -> bool {
            return Phi::Cast(i) || !MkEnv::Cast(i);
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

            if (bb->trueBranch() &&
                !branchTaken.count(Jmp(bb, bb->trueBranch()))) {
                branchTaken.insert(Jmp(bb, bb->trueBranch()));
                if (!bb->falseBranch()) {
                    verifyBB(bb, bb->trueBranch(), reg, stack);
                } else {
                    // Need to copy here, since we are gonna explore
                    // falseBranch() next
                    RegisterFile regC = reg;
                    Stack stackC = stack;
                    verifyBB(bb, bb->trueBranch(), regC, stackC);
                }
            }
            if (bb->falseBranch() &&
                !branchTaken.count(Jmp(bb, bb->falseBranch()))) {
                branchTaken.insert(Jmp(bb, bb->falseBranch()));
                verifyBB(bb, bb->falseBranch(), reg, stack);
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

class Context {
  public:
    std::stack<CodeStream*> css;
    FunctionWriter& fun;

    explicit Context(FunctionWriter& fun) : fun(fun) {}
    ~Context() { assert(css.empty()); }

    CodeStream& cs() { return *css.top(); }

    rir::Code* finalizeCode(size_t localsCnt) {
        auto res = cs().finalize(localsCnt);
        delete css.top();
        css.pop();
        return res;
    }

    void push(SEXP ast) { css.push(new CodeStream(fun, ast)); }
};

class Pir2Rir {
  public:
    Pir2Rir(Pir2RirCompiler& cmp, ClosureVersion* cls, bool dryRun,
            LogStream& log)
        : compiler(cmp), cls(cls), dryRun(dryRun), log(log) {}
    rir::Code* compileCode(Context& ctx, Code* code);
    rir::Code* getPromise(Context& ctx, Promise* code);

    void lower(Code* code);
    void toCSSA(Code* code);
    rir::Function* finalize();

  private:
    Pir2RirCompiler& compiler;
    ClosureVersion* cls;
    std::unordered_map<Promise*, rir::Code*> promises;
    bool dryRun;
    LogStream& log;

    class CodeBuffer {
      private:
        struct Src {
            enum { None, Sexp, Idx } tag;
            union {
                SEXP sexp;
                unsigned idx;
            } u;
        };
        using RIRInstruction = std::pair<BC, Src>;
        std::list<RIRInstruction> code;
        CodeStream& cs;

        void emplace_back(BC&& bc, Src src) {
            code.emplace_back(std::make_pair(std::move(bc), std::move(src)));
        }

        void peephole() {
            /*TODO: there are still more patterns that could be cleaned up:
             *   pick(2) swap() pick(2) -> swap()
             *   pick(2) pop() swap() pop() pop() -> pop() pop() pop()
             */

            auto plus = [](std::list<RIRInstruction>::iterator it, int n) {
                while (n--)
                    it++;
                return it;
            };

            Src noSource;
            noSource.tag = Src::None;
            int steam = 5;
            bool changed = false;

            do {
                changed = false;
                auto it = code.begin();
                while (it != code.end()) {
                    auto next = plus(it, 1);
                    auto& bc = it->first;

                    if (bc.is(rir::Opcode::pick_)) {
                        unsigned arg = bc.immediate.i;
                        unsigned n = 1;
                        // Have 1 pick(arg), can remove if we find arg + 1 of
                        // them
                        auto last = next;
                        for (; last != code.end(); last++) {
                            auto& bc = last->first;
                            if (bc.is(rir::Opcode::pick_) &&
                                bc.immediate.i == arg)
                                n++;
                            else
                                break;
                        }
                        if (n == arg + 1 && last != code.end()) {
                            next = code.erase(it, last);
                            changed = true;
                        } else if (arg == 1) {
                            next = code.erase(it);
                            next = code.emplace(next, BC::swap(), noSource);
                            changed = true;
                        }
                    } else if (bc.is(rir::Opcode::pull_)) {
                        if (bc.immediate.i == 0) {
                            next = code.erase(it);
                            next = code.emplace(next, BC::dup(), noSource);
                            changed = true;
                        } else if (bc.immediate.i == 1 && next != code.end() &&
                                   next->first.is(rir::Opcode::pull_) &&
                                   next->first.immediate.i == 1) {
                            next = code.erase(it, plus(next, 1));
                            next = code.emplace(next, BC::dup2(), noSource);
                            changed = true;
                        }
                    } else if (bc.is(rir::Opcode::dup_) && next != code.end() &&
                               plus(next, 1) != code.end() &&
                               next->first.is(rir::Opcode::isobj_) &&
                               plus(next, 1)->first.is(rir::Opcode::brtrue_)) {
                        auto target = plus(next, 1)->first.immediate.offset;
                        next = code.erase(it, plus(next, 2));
                        next = code.emplace(next, BC::brobj(target), noSource);
                        changed = true;
                    } else if (bc.is(rir::Opcode::dup_) && next != code.end() &&
                               plus(next, 1) != code.end() &&
                               plus(next, 2) != code.end() &&
                               next->first.is(rir::Opcode::for_seq_size_) &&
                               plus(next, 1)->first.is(rir::Opcode::swap_) &&
                               plus(next, 2)->first.is(rir::Opcode::pop_)) {
                        next = plus(code.erase(it), 1);
                        next = code.erase(next, plus(next, 2));
                        changed = true;
                    } else if (bc.is(rir::Opcode::ldvar_noforce_) &&
                               next != code.end() &&
                               next->first.is(rir::Opcode::force_)) {
                        auto arg = Pool::get(bc.immediate.pool);
                        next = code.erase(it, plus(next, 1));
                        next = code.emplace(next, BC::ldvar(arg), noSource);
                        changed = true;
                    } else if (bc.is(rir::Opcode::pop_)) {
                        unsigned n = 1;
                        auto last = next;
                        for (; last != code.end(); last++) {
                            auto& bc = last->first;
                            if (bc.is(rir::Opcode::pop_))
                                n++;
                            else
                                break;
                        }
                        if (n > 1 && last != code.end()) {
                            next = code.erase(it, last);
                            next = code.emplace(next, BC::popn(n), noSource);
                            changed = true;
                        }
                    }

                    it = next;
                }
            } while (changed && steam-- > 0);
        }

      public:
        void flush() {
            peephole();
            for (auto const& instr : code) {
                cs << instr.first;
                switch (instr.second.tag) {
                case Src::Sexp:
                    cs.addSrc(instr.second.u.sexp);
                    break;
                case Src::Idx:
                    cs.addSrcIdx(instr.second.u.idx);
                    break;
                default:
                    break;
                }
            }
            code.clear();
        }

        explicit CodeBuffer(CodeStream& cs) : cs(cs) {}
        ~CodeBuffer() { assert(code.empty()); }

        void add(BC&& bc) {
            Src s;
            s.tag = Src::None;
            emplace_back(std::move(bc), s);
        }

        void add(BC&& bc, SEXP src) {
            Src s;
            s.tag = src ? Src::Sexp : Src::None;
            s.u.sexp = src;
            emplace_back(std::move(bc), s);
        }

        void add(BC&& bc, unsigned idx) {
            Src s;
            s.tag = Src::Idx;
            s.u.idx = idx;
            emplace_back(std::move(bc), s);
        }

        void add(BC::Label label) {
            flush();
            cs << label;
        }
    };
};

rir::Code* Pir2Rir::compileCode(Context& ctx, Code* code) {
    lower(code);
    toCSSA(code);
#ifdef FULLVERIFIER
    Verify::apply(cls, true);
#else
#ifdef ENABLE_SLOWASSERT
    Verify::apply(cls);
#endif
#endif
    log.CSSA(code);

    SSAAllocator alloc(code, cls, log);
    log.afterAllocator(code, [&](std::ostream& o) { alloc.print(o); });
    alloc.verify();

    auto isJumpThrough = [&](BB* bb) {
        return bb->isEmpty() || (bb->size() == 1 && Nop::Cast(bb->last()) &&
                                 alloc.sa.toDrop(bb->last()).empty());
    };

    // Create labels for all bbs
    std::unordered_map<BB*, BC::Label> bbLabels;
    BreadthFirstVisitor::run(code->entry, [&](BB* bb) {
        if (!isJumpThrough(bb))
            bbLabels[bb] = ctx.cs().mkLabel();
    });

    LastEnv lastEnv(cls, code, log);
    std::unordered_map<Value*, BC::Label> pushContexts;

    std::deque<unsigned> order;
    LoweringVisitor::run(code->entry, [&](BB* bb) {
        if (!isJumpThrough(bb))
            order.push_back(bb->id);
    });

    std::unordered_set<Instruction*> needsEnsureNamed;
    std::unordered_set<Instruction*> needsSetShared;
    std::unordered_set<Instruction*> needsLdVarForUpdate;
    bool refcountAnalysisOverflow = false;
    {
        Visitor::run(code->entry, [&](Instruction* i) {
            switch (i->tag) {
            case Tag::ForSeqSize:
                if (auto arg = Instruction::Cast(i->arg(0).val()))
                    if (arg->minReferenceCount() < Value::MAX_REFCOUNT)
                        needsSetShared.insert(arg);
                break;
            case Tag::Subassign1_1D:
            case Tag::Subassign2_1D:
            case Tag::Subassign1_2D:
            case Tag::Subassign2_2D:
                // Subassigns override the vector, even if the named count
                // is 1. This is only valid, if we are sure that the vector
                // is local, ie. vector and subassign operation come from
                // the same lexical scope.
                if (auto vec = Instruction::Cast(
                        i->arg(1).val()->followCastsAndForce())) {
                    if (auto ld = LdVar::Cast(vec)) {
                        if (auto su = vec->hasSingleUse()) {
                            if (auto st = StVar::Cast(su)) {
                                if (ld->env() != st->env())
                                    needsLdVarForUpdate.insert(vec);
                                break;
                            }
                        }
                        if (ld->env() != i->env())
                            needsLdVarForUpdate.insert(vec);
                    } else {
                        if (vec->minReferenceCount() < 2 &&
                            !vec->hasSingleUse())
                            needsSetShared.insert(vec);
                    }
                }
                break;
            default: {}
            }
        });

        StaticReferenceCount analysis(cls, log);
        if (analysis.result().overflow)
            refcountAnalysisOverflow = true;
        else
            for (auto& u : analysis.result().uses) {
                if (u.second == AUses::Multiple)
                    needsEnsureNamed.insert(u.first);
            }
    }

    CodeBuffer cb(ctx.cs());
    LoweringVisitor::run(code->entry, [&](BB* bb) {
        if (isJumpThrough(bb))
            return;

        order.pop_front();
        cb.add(bbLabels[bb]);

        auto jumpThroughEmpty = [&](BB* bb) {
            while (isJumpThrough(bb))
                bb = bb->next();
            return bb;
        };

        for (auto it = bb->begin(); it != bb->end(); ++it) {
            auto instr = *it;

            // Prepare stack and arguments
            {

                std::vector<bool> removedStackValues(
                    alloc.sa.stackBefore(instr).size(), false);
                size_t pushedStackValues = 0;

                auto debugAddVariableName = [&](Value* v) -> SEXP {
#ifdef ENABLE_SLOWASSERT
                    std::stringstream ss;
                    v->printRef(ss);
                    // Protect error: install can allocate and calling it
                    // many times during pir2rir might in principle cause
                    // some of these to be gc'd.. But unlikely and only
                    // possible in debug mode
                    return Rf_install(ss.str().c_str());
#else
                    return nullptr;
#endif
                };

                auto explicitEnvValue = [](Instruction* instr) {
                    return MkEnv::Cast(instr) || IsEnvStub::Cast(instr);
                };

                auto moveToTOS = [&](size_t offset) {
                    cb.add(BC::pick(offset));
                };

                auto copyToTOS = [&](size_t offset) {
                    cb.add(BC::pull(offset));
                };

                auto getFromStack = [&](Value* what, size_t argNumber) {
                    auto lastUse = alloc.lastUse(instr, argNumber);
                    auto offset =
                        alloc.getStackOffset(instr, removedStackValues, what,
                                             lastUse) +
                        pushedStackValues;
                    if (lastUse)
                        moveToTOS(offset);
                    else
                        copyToTOS(offset);
                };

                auto loadEnv = [&](Value* what, size_t argNumber) {
                    if (what == Env::notClosed()) {
                        cb.add(BC::parentEnv());
                    } else if (what == Env::nil()) {
                        cb.add(BC::push(R_NilValue));
                    } else if (Env::isStaticEnv(what)) {
                        auto env = Env::Cast(what);
                        // Here we could also load env->rho, but if the user
                        // were to change the environment on the closure our
                        // code would be wrong.
                        if (env == cls->owner()->closureEnv())
                            cb.add(BC::parentEnv());
                        else
                            cb.add(BC::push(env->rho));
                    } else {
                        if (!alloc.hasSlot(what)) {
                            std::cerr << "Don't know how to load the env ";
                            what->printRef(std::cerr);
                            std::cerr << " (" << tagToStr(what->tag) << ")\n";
                            assert(false);
                        }
                        if (alloc.onStack(what)) {
                            getFromStack(what, argNumber);
                        } else {
                            cb.add(BC::ldloc(alloc[what]),
                                   debugAddVariableName(what));
                        }
                    }
                    pushedStackValues++;
                };

                auto loadArg = [&](Value* what, size_t argNumber) {
                    if (what == UnboundValue::instance()) {
                        cb.add(BC::push(R_UnboundValue));
                    } else if (what == MissingArg::instance()) {
                        cb.add(BC::push(R_MissingArg));
                    } else if (what == True::instance()) {
                        cb.add(BC::push(R_TrueValue));
                    } else if (what == False::instance()) {
                        cb.add(BC::push(R_FalseValue));
                    } else if (what == NaLogical::instance()) {
                        cb.add(BC::push(R_LogicalNAValue));
                    } else {
                        if (!alloc.hasSlot(what)) {
                            std::cerr << "Don't know how to load the arg ";
                            what->printRef(std::cerr);
                            std::cerr << " (" << tagToStr(what->tag) << ")\n";
                            assert(false);
                        }
                        if (alloc.onStack(what)) {
                            getFromStack(what, argNumber);
                        } else {
                            cb.add(BC::ldloc(alloc[what]),
                                   debugAddVariableName(what));
                        }
                    }
                    pushedStackValues++;
                };

                auto loadPhiArg = [&](Phi* phi) {
                    if (!alloc.hasSlot(phi)) {
                        std::cerr << "Don't know how to load the phi arg ";
                        phi->printRef(std::cerr);
                        std::cerr << " (" << tagToStr(phi->tag) << ")\n";
                        assert(false);
                    }
                    if (alloc.onStack(phi)) {
                        auto offset = alloc.stackPhiOffset(instr, phi);
                        moveToTOS(offset);
                    } else {
                        cb.add(BC::ldloc(alloc[phi]),
                               debugAddVariableName(phi));
                    }
                };

                // Remove values from the stack that are dead here
                auto toDrop = alloc.sa.toDrop(instr);
                for (auto val : VisitorHelpers::reverse(toDrop)) {
                    // If not actually allocated on stack, do nothing
                    if (!alloc.onStack(val))
                        continue;

                    auto offset = alloc.getStackOffset(
                        instr, removedStackValues, val, true);
                    moveToTOS(offset);
                    cb.add(BC::pop());
                }

                if (auto phi = Phi::Cast(instr)) {
                    loadPhiArg(phi);
                } else {
                    size_t argNumber = 0;
                    instr->eachArg([&](Value* what) {
                        if (what == Env::elided() ||
                            what->tag == Tag::Tombstone ||
                            what->type == NativeType::context) {
                            argNumber++;
                            return;
                        }

                        if (instr->hasEnv() && instr->env() == what) {
                            if (explicitEnvValue(instr)) {
                                loadEnv(what, argNumber);
                            } else {
                                auto env = instr->env();
                                if (!lastEnv.envStillValid(instr)) {
                                    loadEnv(env, argNumber);
                                    cb.add(BC::setEnv());
                                } else {
                                    if (alloc.hasSlot(env) &&
                                        alloc.onStack(env) &&
                                        alloc.lastUse(instr, argNumber)) {
                                        auto offset =
                                            alloc.getStackOffset(
                                                instr, removedStackValues, env,
                                                true) +
                                            pushedStackValues;
                                        moveToTOS(offset);
                                        cb.add(BC::pop());
                                    }
                                }
                            }
                        } else {
                            loadArg(what, argNumber);
                        }
                        argNumber++;
                    });
                }
            }

            switch (instr->tag) {

            case Tag::LdConst: {
                cb.add(BC::push_from_pool(LdConst::Cast(instr)->idx));
                break;
            }

            case Tag::LdFun: {
                auto ldfun = LdFun::Cast(instr);
                cb.add(BC::ldfun(ldfun->varName));
                break;
            }

            case Tag::LdVar: {
                auto ldvar = LdVar::Cast(instr);
                if (needsLdVarForUpdate.count(instr))
                    cb.add(BC::ldvarForUpdate(ldvar->varName));
                else
                    cb.add(BC::ldvarNoForce(ldvar->varName));
                break;
            }

            case Tag::ForSeqSize: {
                cb.add(BC::forSeqSize());
                // TODO: currently we always pop the sequence, since we
                // cannot deal with instructions that do not pop the value
                // after use.
                cb.add(BC::swap());
                cb.add(BC::pop());
                break;
            }

            case Tag::LdArg: {
                auto ld = LdArg::Cast(instr);
                cb.add(BC::ldarg(ld->id));
                break;
            }

            case Tag::StVarSuper: {
                auto stvar = StVarSuper::Cast(instr);
                cb.add(BC::stvarSuper(stvar->varName));
                break;
            }

            case Tag::LdVarSuper: {
                auto ldvar = LdVarSuper::Cast(instr);
                cb.add(BC::ldvarNoForceSuper(ldvar->varName));
                break;
            }

            case Tag::StVar: {
                auto stvar = StVar::Cast(instr);
                if (stvar->isStArg)
                    cb.add(BC::starg(stvar->varName));
                else
                    cb.add(BC::stvar(stvar->varName));
                break;
            }

            case Tag::MkArg: {
                auto p = MkArg::Cast(instr)->prom();
                unsigned id = ctx.cs().addPromise(getPromise(ctx, p));
                cb.add(BC::promise(id));
                break;
            }

            case Tag::MkFunCls: {
                // TODO: would be nice to compile the function here. But I am
                // not sure if our compiler backend correctly deals with not
                // closed closures.
                auto mkfuncls = MkFunCls::Cast(instr);
                auto cls = mkfuncls->cls;
                cb.add(BC::push(cls->formals().original()));
                cb.add(BC::push(mkfuncls->originalBody->container()));
                cb.add(BC::push(cls->srcRef()));
                cb.add(BC::close());
                break;
            }

            case Tag::Missing: {
                auto m = Missing::Cast(instr);
                cb.add(BC::missing(m->varName));
                break;
            }

            case Tag::Is: {
                auto is = Is::Cast(instr);
                cb.add(BC::is(is->sexpTag));
                break;
            }

            case Tag::IsType: {
                auto is = IsType::Cast(instr);
                auto t = is->typeTest;
                assert(!t.isVoid() && !t.maybeObj() && !t.maybeLazy() &&
                       !t.maybePromiseWrapped());

                if (t.isA(RType::integer)) {
                    if (t.isScalar())
                        cb.add(BC::is(TypeChecks::IntegerSimpleScalar));
                    else
                        cb.add(BC::is(TypeChecks::IntegerNonObject));
                } else if (t.isA(RType::real)) {
                    if (t.isScalar())
                        cb.add(BC::is(TypeChecks::RealSimpleScalar));
                    else
                        cb.add(BC::is(TypeChecks::RealNonObject));
                } else {
                    assert(false);
                }
                break;
            }

            case Tag::AsInt: {
                auto asInt = AsInt::Cast(instr);
                if (asInt->ceil)
                    cb.add(BC::ceil());
                else
                    cb.add(BC::floor());
                break;
            }

#define EMPTY(Name)                                                            \
    case Tag::Name: {                                                          \
        break;                                                                 \
    }
                EMPTY(CastType);
                EMPTY(Nop);
                EMPTY(PirCopy);
#undef EMPTY

#define SIMPLE(Name, Factory)                                                  \
    case Tag::Name: {                                                          \
        cb.add(BC::Factory());                                                 \
        break;                                                                 \
    }
                SIMPLE(LdFunctionEnv, getEnv);
                SIMPLE(Visible, visible);
                SIMPLE(Invisible, invisible);
                SIMPLE(Identical, identicalNoforce);
                SIMPLE(IsObject, isobj);
                SIMPLE(IsEnvStub, isstubenv);
                SIMPLE(LOr, lglOr);
                SIMPLE(LAnd, lglAnd);
                SIMPLE(Inc, inc);
                SIMPLE(Dec, dec);
                SIMPLE(Force, force);
                SIMPLE(AsTest, asbool);
                SIMPLE(Length, length);
                SIMPLE(ChkMissing, checkMissing);
                SIMPLE(ChkClosure, isfun);
                SIMPLE(Seq, seq);
                SIMPLE(MkCls, close);
#define V(V, name, Name) SIMPLE(Name, name);
                SIMPLE_INSTRUCTIONS(V, _);
#undef V
#undef SIMPLE

#define SIMPLE_WITH_SRCIDX(Name, Factory)                                      \
    case Tag::Name: {                                                          \
        cb.add(BC::Factory(), instr->srcIdx);                                  \
        break;                                                                 \
    }
                SIMPLE_WITH_SRCIDX(Add, add);
                SIMPLE_WITH_SRCIDX(Sub, sub);
                SIMPLE_WITH_SRCIDX(Mul, mul);
                SIMPLE_WITH_SRCIDX(Div, div);
                SIMPLE_WITH_SRCIDX(IDiv, idiv);
                SIMPLE_WITH_SRCIDX(Mod, mod);
                SIMPLE_WITH_SRCIDX(Pow, pow);
                SIMPLE_WITH_SRCIDX(Lt, lt);
                SIMPLE_WITH_SRCIDX(Gt, gt);
                SIMPLE_WITH_SRCIDX(Lte, ge);
                SIMPLE_WITH_SRCIDX(Gte, le);
                SIMPLE_WITH_SRCIDX(Eq, eq);
                SIMPLE_WITH_SRCIDX(Neq, ne);
                SIMPLE_WITH_SRCIDX(Colon, colon);
                SIMPLE_WITH_SRCIDX(AsLogical, asLogical);
                SIMPLE_WITH_SRCIDX(Plus, uplus);
                SIMPLE_WITH_SRCIDX(Minus, uminus);
                SIMPLE_WITH_SRCIDX(Not, not_);
                SIMPLE_WITH_SRCIDX(Extract1_1D, extract1_1);
                SIMPLE_WITH_SRCIDX(Extract2_1D, extract2_1);
                SIMPLE_WITH_SRCIDX(Extract1_2D, extract1_2);
                SIMPLE_WITH_SRCIDX(Extract2_2D, extract2_2);
                SIMPLE_WITH_SRCIDX(Subassign1_1D, subassign1_1);
                SIMPLE_WITH_SRCIDX(Subassign2_1D, subassign2_1);
                SIMPLE_WITH_SRCIDX(Subassign1_2D, subassign1_2);
                SIMPLE_WITH_SRCIDX(Subassign2_2D, subassign2_2);
#undef SIMPLE_WITH_SRCIDX

            case Tag::Call: {
                auto call = Call::Cast(instr);
                cb.add(BC::call(call->nCallArgs(), Pool::get(call->srcIdx),
                                call->inferAvailableAssumptions()));
                break;
            }

            case Tag::NamedCall: {
                auto call = NamedCall::Cast(instr);
                cb.add(BC::call(call->nCallArgs(), call->names,
                                Pool::get(call->srcIdx),
                                call->inferAvailableAssumptions()));
                break;
            }

            case Tag::StaticCall: {
                auto call = StaticCall::Cast(instr);
                SEXP originalClosure = call->cls()->rirClosure();
                auto dt = DispatchTable::unpack(BODY(originalClosure));
                if (auto trg = call->tryOptimisticDispatch()) {
                    // Avoid recursivly compiling the same closure
                    auto fun = compiler.alreadyCompiled(trg);
                    SEXP funCont = nullptr;

                    if (fun) {
                        funCont = fun->container();
                    } else if (!compiler.isCompiling(trg)) {
                        fun = compiler.compile(trg, dryRun);
                        funCont = fun->container();
                        Protect p(funCont);
                        assert(originalClosure &&
                               "Cannot compile synthetic closure");
                        dt->insert(fun);
                    }
                    auto bc = BC::staticCall(call->nCallArgs(),
                                             Pool::get(call->srcIdx),
                                             originalClosure, funCont,
                                             call->inferAvailableAssumptions());
                    auto hint = bc.immediate.staticCallFixedArgs.versionHint;
                    cb.add(std::move(bc));
                    if (!funCont)
                        compiler.needsPatching(trg, hint);
                } else {
                    // Something went wrong with dispatching, let's put the
                    // baseline there
                    cb.add(BC::staticCall(
                        call->nCallArgs(), Pool::get(call->srcIdx),
                        originalClosure, dt->baseline()->container(),
                        call->inferAvailableAssumptions()));
                }
                break;
            }

            case Tag::CallBuiltin: {
                auto blt = CallBuiltin::Cast(instr);
                cb.add(BC::callBuiltin(blt->nCallArgs(), Pool::get(blt->srcIdx),
                                       blt->blt));
                break;
            }

            case Tag::CallSafeBuiltin: {
                auto blt = CallSafeBuiltin::Cast(instr);
                cb.add(BC::callBuiltin(blt->nargs(), Pool::get(blt->srcIdx),
                                       blt->blt));
                break;
            }

            case Tag::PushContext: {
                if (!pushContexts.count(instr))
                    pushContexts[instr] = ctx.cs().mkLabel();
                cb.add(BC::pushContext(pushContexts.at(instr)));
                break;
            }

            case Tag::PopContext: {
                auto push = PopContext::Cast(instr)->push();
                if (!pushContexts.count(push))
                    pushContexts[push] = ctx.cs().mkLabel();
                cb.add(pushContexts.at(push));
                cb.add(BC::popContext());
                break;
            }

            case Tag::MkEnv: {
                auto mkenv = MkEnv::Cast(instr);
                cb.add(BC::mkEnv(mkenv->varName, mkenv->context, mkenv->stub));
                break;
            }

            case Tag::Phi: {
                // Phi functions are no-ops, because after allocation on
                // CSSA form, all arguments and the funcion itself are
                // allocated to the same place
                auto phi = Phi::Cast(instr);
                phi->eachArg([&](BB*, Value* arg) {
                    assert(((alloc.onStack(phi) && alloc.onStack(arg)) ||
                            (alloc[phi] == alloc[arg])) &&
                           "Phi inputs must all be allocated in 1 slot");
                });
                break;
            }

            // BB exitting instructions
            case Tag::Branch: {
                auto trueBranch = jumpThroughEmpty(bb->trueBranch());
                auto falseBranch = jumpThroughEmpty(bb->falseBranch());
                if (trueBranch->id == order.front()) {
                    cb.add(BC::brfalse(bbLabels[falseBranch]));
                    cb.add(BC::br(bbLabels[trueBranch]));
                } else {
                    cb.add(BC::brtrue(bbLabels[trueBranch]));
                    cb.add(BC::br(bbLabels[falseBranch]));
                }
                // This is the end of this BB
                return;
            }

            case Tag::Return: {
                cb.add(BC::ret());
                // end of this BB
                return;
            }

            case Tag::ScheduledDeopt: {
                auto deopt = ScheduledDeopt::Cast(instr);

                size_t nframes = deopt->frames.size();

                SEXP store =
                    Rf_allocVector(RAWSXP, sizeof(DeoptMetadata) +
                                               nframes * sizeof(FrameInfo));
                auto m = new (DATAPTR(store)) DeoptMetadata;
                m->numFrames = nframes;

                size_t i = 0;
                // Frames in the ScheduledDeopt are in pir argument order
                // (from left to right). On the other hand frames in the rir
                // deopt_ instruction are in stack order, from tos down.
                for (auto fi = deopt->frames.rbegin();
                     fi != deopt->frames.rend(); fi++)
                    m->frames[i++] = *fi;

                cb.add(BC::deopt(store));
                // deopt is exit
                return;
            }

            // Invalid, should've been lowered away
            case Tag::FrameState:
            case Tag::Deopt:
            case Tag::Assume:
            case Tag::Checkpoint: {
                assert(false && "Deopt instructions must be lowered into "
                                "standard branches and scheduled deopt, "
                                "before pir_2_rir");
                break;
            }

            // Values, not instructions
#define V(Value) case Tag::Value:
            COMPILER_VALUES(V) {
#undef V
                break;
            }

            // Dummy sentinel enum item
            case Tag::_UNUSED_: {
                break;
            }
            }

            if (instr->minReferenceCount() < 2 && needsSetShared.count(instr))
                cb.add(BC::setShared());
            else if (instr->minReferenceCount() < 1 &&
                     (refcountAnalysisOverflow ||
                      needsEnsureNamed.count(instr)))
                cb.add(BC::ensureNamed());

            // Store the result
            if (alloc.sa.dead(instr)) {
                cb.add(BC::pop());
            } else if (instr->producesRirResult()) {
                if (!alloc.hasSlot(instr)) {
                    cb.add(BC::pop());
                } else if (!alloc.onStack(instr)) {
                    cb.add(BC::stloc(alloc[instr]));
                }
            }
        }

        // This BB has exactly one successor, trueBranch().
        assert(bb->isJmp());
        auto next = jumpThroughEmpty(bb->trueBranch());
        cb.add(BC::br(bbLabels[next]));
    });
    cb.flush();

    auto localsCnt = alloc.slots();
    auto res = ctx.finalizeCode(localsCnt);
    return res;
}

static bool coinFlip() {
    static std::mt19937 gen(Parameter::DEOPT_CHAOS_SEED);
    static std::bernoulli_distribution coin(0.03);
    return coin(gen);
};

void Pir2Rir::lower(Code* code) {

    Visitor::runPostChange(code->entry, [&](BB* bb) {
        auto it = bb->begin();
        while (it != bb->end()) {
            auto next = it + 1;
            if (auto call = CallInstruction::CastCall(*it))
                call->clearFrameState();
            if (auto ldfun = LdFun::Cast(*it)) {
                // The guessed binding in ldfun is just used as a temporary
                // store. If we did not manage to resolve ldfun by now, we
                // have to remove the guess again, since apparently we
                // were not sure it is correct.
                if (ldfun->guessedBinding())
                    ldfun->clearGuessedBinding();
            } else if (auto deopt = Deopt::Cast(*it)) {
                // Lower Deopt instructions + their FrameStates to a
                // ScheduledDeopt.
                auto newDeopt = new ScheduledDeopt();
                newDeopt->consumeFrameStates(deopt);
                bb->replace(it, newDeopt);
            } else if (auto expect = Assume::Cast(*it)) {
                auto condition = expect->condition();
                if (Parameter::DEOPT_CHAOS && coinFlip()) {
                    condition = expect->assumeTrue ? (Value*)False::instance()
                                                   : (Value*)True::instance();
                }
                std::string debugMessage;
                if (Parameter::DEBUG_DEOPTS) {
                    std::stringstream dump;
                    debugMessage = "DEOPT, assumption ";
                    expect->condition()->printRef(dump);
                    debugMessage += dump.str();
                    debugMessage += " failed in\n";
                    dump.str("");
                    code->printCode(dump, false, false);
                    debugMessage += dump.str();
                }
                BBTransform::lowerExpect(
                    code, bb, it, condition, expect->assumeTrue,
                    expect->checkpoint()->bb()->falseBranch(), debugMessage);
                // lowerExpect splits the bb from current position. There
                // remains nothing to process. Breaking seems more robust
                // than trusting the modified iterator.
                break;
            }

            it = next;
        }
    });

    Visitor::run(code->entry, [&](BB* bb) {
        auto it = bb->begin();
        while (it != bb->end()) {
            auto next = it + 1;
            if (FrameState::Cast(*it)) {
                next = bb->remove(it);
            } else if (Checkpoint::Cast(*it)) {
                next = bb->remove(it);
                // Branching removed. Preserve invariant
                bb->next1 = nullptr;
            } else if (MkArg::Cast(*it) && (*it)->unused()) {
                next = bb->remove(it);
            }
            it = next;
        }
    });

    // Insert Nop into all empty blocks to make life easier
    Visitor::run(code->entry, [&](BB* bb) {
        if (bb->isEmpty())
            bb->append(new Nop());
    });
}

void Pir2Rir::toCSSA(Code* code) {
    // For each Phi, insert copies
    BreadthFirstVisitor::run(code->entry, [&](BB* bb) {
        // TODO: move all phi's to the beginning, then insert the copies not
        // after each phi but after all phi's?
        for (auto it = bb->begin(); it != bb->end(); ++it) {
            auto instr = *it;
            if (auto phi = Phi::Cast(instr)) {
                for (size_t i = 0; i < phi->nargs(); ++i) {
                    BB* pred = phi->inputAt(i);
                    // If pred is branch insert a new split block
                    if (!pred->isJmp()) {
                        BB* split = nullptr;
                        if (pred->trueBranch() == phi->bb())
                            split = pred->trueBranch();
                        else if (pred->falseBranch() == phi->bb())
                            split = pred->falseBranch();
                        assert(split &&
                               "Don't know where to insert a phi input copy.");
                        pred = BBTransform::splitEdge(code->nextBBId++, pred,
                                                      split, code);
                    }
                    if (Instruction* iav =
                            Instruction::Cast(phi->arg(i).val())) {
                        auto copy = pred->insert(pred->end(), new PirCopy(iav));
                        phi->arg(i).val() = *copy;
                    } else {
                        auto val = phi->arg(i).val()->asRValue();
                        auto copy = pred->insert(pred->end(), new LdConst(val));
                        phi->arg(i).val() = *copy;
                    }
                }
                auto phiCopy = new PirCopy(phi);
                phi->replaceUsesWith(phiCopy);
                it = bb->insert(it + 1, phiCopy);
            }
        }
    });
}

rir::Code* Pir2Rir::getPromise(Context& ctx, Promise* p) {
    if (!promises.count(p)) {
        ctx.push(src_pool_at(globalContext(), p->srcPoolIdx()));
        promises[p] = compileCode(ctx, p);
    }
    return promises.at(p);
}

rir::Function* Pir2Rir::finalize() {
    // TODO: keep track of source ast indices in the source pool
    // (for now, calls, promises and operators do)
    // + how to deal with inlined stuff?

    FunctionWriter function;
    Context ctx(function);

    FunctionSignature signature(FunctionSignature::Environment::CalleeCreated,
                                FunctionSignature::OptimizationLevel::Optimized,
                                cls->assumptions());

    // PIR does not support default args currently.
    for (size_t i = 0; i < cls->nargs(); ++i) {
        function.addArgWithoutDefault();
        signature.pushDefaultArgument();
    }

    assert(signature.formalNargs() == cls->nargs());
    ctx.push(R_NilValue);
    auto body = compileCode(ctx, cls);
    log.finalPIR(cls);
    function.finalize(body, signature);
#ifdef ENABLE_SLOWASSERT
    CodeVerifier::verifyFunctionLayout(function.function()->container(),
                                       globalContext());
#endif
    log.finalRIR(function.function());
    return function.function();
}

} // namespace

rir::Function* Pir2RirCompiler::compile(ClosureVersion* cls, bool dryRun) {
    auto& log = logger.get(cls);
    done[cls] = nullptr;
    Pir2Rir pir2rir(*this, cls, dryRun, log);
    auto fun = pir2rir.finalize();
    done[cls] = fun;
    log.flush();
    if (fixup.count(cls)) {
        auto fixups = fixup.find(cls);
        for (auto idx : fixups->second)
            Pool::patch(idx, fun->container());
        fixup.erase(fixups);
    }
    return fun;
}

bool Parameter::DEBUG_DEOPTS = getenv("PIR_DEBUG_DEOPTS") &&
                               0 == strncmp("1", getenv("PIR_DEBUG_DEOPTS"), 1);
bool Parameter::DEOPT_CHAOS = getenv("PIR_DEOPT_CHAOS") &&
                              0 == strncmp("1", getenv("PIR_DEOPT_CHAOS"), 1);
bool Parameter::DEOPT_CHAOS_SEED = getenv("PIR_DEOPT_CHAOS_SEED")
                                       ? atoi(getenv("PIR_DEOPT_CHAOS_SEED"))
                                       : std::random_device()();

} // namespace pir
} // namespace rir
