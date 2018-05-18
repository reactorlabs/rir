#include "pir_2_rir.h"
#include "../pir/pir_impl.h"
#include "../util/cfg.h"
#include "../util/visitor.h"
#include "interpreter/runtime.h"
#include "ir/CodeEditor.h"
#include "ir/CodeStream.h"
#include "ir/CodeVerifier.h"
#include "ir/Optimizer.h"
#include "utils/FunctionWriter.h"

#include <algorithm>

// #define DEBUGGING
#define ALLOC_DEBUG 1
#define PHI_REMOVE_DEBUG 1

#ifdef DEBUGGING
#define DEBUGCODE(flag, code)                                                  \
    if (flag)                                                                  \
    code
#else
#define DEBUGCODE(flag, code) /* nothing */
#endif

namespace rir {
namespace pir {

namespace {

/*
 * SSAAllocator assigns each instruction to a local variable number, or the
 * stack. It uses the following algorithm:
 *
 * 1. Split phis with moves. This translates the IR to CSSA (see toCSSA).
 * 2. Compute liveness (see computeLiveness):
 *    Liveness intervals are stored as:
 *        Instruction* -> BB id -> { start : pos, end : pos, live : bool}
 *    Two Instructions interfere iff there is a BB where they are both live
 *    and the start-end overlap.
 * 3. Use simple heuristics to detect Instructions that can stay on the RIR
 *    stack (see computeStackAllocation):
 *    1. Use stack slots for instructions which are used
 *       (i)   exactly once,
 *       (ii)  in stack order,
 *       (iii) within the same BB.
 *    2. Use stack slots for phi which are
 *       (i)  at the beginning of a BB, and
 *       (ii) all inputs are at the end of all immediate predecessor BBs.
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

    typedef size_t SlotNumber;
    const static SlotNumber unassignedSlot = 0;
    const static SlotNumber stackSlot = -1;

    std::unordered_map<Value*, SlotNumber> allocation;

    struct BBLiveness {
        uint8_t live = false;
        unsigned begin = -1;
        unsigned end = -1;
    };
    struct Liveness : public std::vector<BBLiveness> {
        bool interfere(const Liveness& other) const {
            assert(size() == other.size());
            for (size_t i = 0; i < size(); ++i) {
                const BBLiveness& mine = (*this)[i];
                const BBLiveness& their = other[i];
                if (mine.live && their.live) {
                    if (mine.begin == their.begin ||
                        (mine.begin < their.begin && mine.end >= their.begin) ||
                        (mine.begin > their.begin && their.end >= mine.begin))
                        return true;
                }
            }
            return false;
        }
    };
    std::unordered_map<Value*, Liveness> livenessInterval;

    SSAAllocator(Code* code, bool verbose)
        : cfg(code->entry), dom(code->entry), code(code),
          bbsSize(code->maxBBId + 1) {
        computeLiveness(verbose);
        computeStackAllocation();
        computeAllocation();
    }

    // Run backwards analysis to compute livenessintervals
    void computeLiveness(bool verbose = false) {
        // temp list of live out sets for every BB
        std::unordered_map<BB*, std::set<Value*>> liveAtEnd(bbsSize);

        std::set<BB*> todo;
        for (auto e : cfg.exits)
            todo.insert(e);

        while (!todo.empty()) {
            BB* bb = *todo.begin();
            todo.erase(todo.begin());

            // keep track of currently live variables
            std::set<Value*> accumulated;
            std::map<BB*, std::set<Value*>> accumulatedPhiInput;

            // Mark all (backwards) incoming live variables
            for (auto v : liveAtEnd[bb]) {
                assert(livenessInterval.count(v));
                auto& liveRange = livenessInterval.at(v)[bb->id];
                if (!liveRange.live || liveRange.end < bb->size()) {
                    liveRange.live = true;
                    liveRange.end = bb->size();
                    accumulated.insert(v);
                }
            }

            // Run BB in reverse
            size_t pos = bb->size();
            if (!bb->isEmpty()) {
                auto ip = bb->end();
                do {
                    --ip;
                    --pos;
                    Instruction* i = *ip;
                    Phi* phi = Phi::Cast(i);

                    auto markIfNotSeen = [&](Value* v) {
                        if (!livenessInterval.count(v)) {
                            // First time we see this variable, need to allocate
                            // vector of all livereanges
                            livenessInterval[v].resize(bbsSize);
                            assert(!livenessInterval[v][bb->id].live);
                        }
                        auto& liveRange = livenessInterval[v][bb->id];
                        if (!liveRange.live) {
                            liveRange.live = true;
                            liveRange.end = pos;
                            return true;
                        }
                        return false;
                    };

                    // First set all arguments to be live
                    if (phi)
                        phi->eachArg([&](BB* in, Value* v) {
                            if (markIfNotSeen(v))
                                accumulatedPhiInput[in].insert(v);
                        });
                    else
                        i->eachArg([&](Value* v) {
                            if (markIfNotSeen(v))
                                accumulated.insert(v);
                        });

                    // Mark the end of the current instructions liveness
                    if (accumulated.count(i)) {
                        assert(livenessInterval.count(i));
                        auto& liveRange = livenessInterval[i][bb->id];
                        assert(liveRange.live);
                        liveRange.begin = pos;
                        accumulated.erase(accumulated.find(i));
                    }
                } while (ip != bb->begin());
            }
            assert(pos == 0);

            // Mark everything that is live at the beginning of the BB.
            auto markLiveEntry = [&](Value* v) {
                assert(livenessInterval.count(v));
                auto& liveRange = livenessInterval[v][bb->id];
                assert(liveRange.live);
                liveRange.begin = 0;
            };

            for (auto v : accumulated)
                markLiveEntry(v);
            for (auto pi : accumulatedPhiInput)
                for (auto v : pi.second)
                    markLiveEntry(v);

            // Merge everything that is live at the beginning of the BB into the
            // incoming vars of all predecessors
            //
            // Phi inputs should only be merged to BB that are successors of the
            // input BBs
            auto merge = [&](BB* bb, const std::set<Value*>& live) {
                auto& liveOut = liveAtEnd[bb];
                if (!std::includes(liveOut.begin(), liveOut.end(), live.begin(),
                                   live.end())) {
                    liveOut.insert(live.begin(), live.end());
                    todo.insert(bb);
                }
            };
            auto mergePhiInp = [&](BB* bb) {
                for (auto in : accumulatedPhiInput) {
                    auto inBB = in.first;
                    auto inLive = in.second;
                    if (bb == inBB ||
                        cfg.transitivePredecessors[bb->id].count(inBB)) {
                        merge(bb, inLive);
                    }
                }
            };
            for (auto pre : cfg.predecessors[bb->id]) {
                bool firstTime = !liveAtEnd.count(pre);
                if (firstTime) {
                    liveAtEnd[pre] = accumulated;
                    mergePhiInp(pre);
                    todo.insert(pre);
                } else {
                    merge(pre, accumulated);
                    mergePhiInp(pre);
                }
            }
        }

        if (verbose) {
            std::cout << "======= Liveness ========\n";
            for (auto ll : livenessInterval) {
                auto& l = ll.second;
                ll.first->printRef(std::cout);
                std::cout << " is live : ";
                for (size_t i = 0; i < bbsSize; ++i) {
                    if (l[i].live) {
                        std::cout << "BB" << i << " [";
                        std::cout << l[i].begin << ",";
                        std::cout << l[i].end << "]  ";
                    }
                }
                std::cout << "\n";
            }
            std::cout << "======= End Liveness ========\n";
        }
    }

    void computeStackAllocation() {
        Visitor::run(code->entry, [&](BB* bb) {
            {
                // If a phi is at the beginning of a BB, and all inputs are at
                // the end of the immediate predecessors BB, we can allocate it
                // on the stack, since the stack is otherwise empty at the BB
                // boundaries.
                size_t pos = 1;
                for (auto i : *bb) {
                    Phi* phi = Phi::Cast(i);
                    if (!phi)
                        break;
                    bool argsInRightOrder = true;
                    phi->eachArg([&](BB* in, Value* v) {
                        if (in->next0 != bb || in->size() < pos ||
                            *(in->end() - pos) != v) {
                            argsInRightOrder = false;
                        }
                    });
                    if (!argsInRightOrder)
                        break;
                    phi->eachArg(
                        [&](BB*, Value* v) { allocation[v] = stackSlot; });
                    allocation[phi] = stackSlot;
                    pos++;
                }
            }

            // Precolor easy stack load-stores within one BB
            size_t pos = 0;
            std::deque<Instruction*> stack;

            auto tryLoadingArgsFromStack = [&](Instruction* i) {
                if (i->nargs() == 0 || stack.size() < i->nargs())
                    return;

                // Match all args to stack slots.
                size_t newStackSize = stack.size();
                bool foundAll = true;
                auto check = stack.rbegin();
                i->eachArg([&](Value* arg) {
                    while (check != stack.rend() && *check != arg) {
                        ++check;
                        --newStackSize;
                    }

                    if (check == stack.rend()) {
                        foundAll = false;
                    } else {
                        // found arg!
                        ++check;
                        --newStackSize;
                    }
                });

                if (!foundAll)
                    return;

                // pop args from stack, discarding all unmatched values
                // in the process. For example if the stack contains
                // [xxx, A, B, C] and we match [A, C], then we will mark
                // A, C to be in a stack slot, discard B (it will become
                // a local variable later) and resize the stack to [xxx]
                stack.resize(newStackSize);
                i->eachInstructionArg(
                    [&](Instruction* arg) { allocation[arg] = stackSlot; });
            };

            for (auto i : *bb) {
                tryLoadingArgsFromStack(i);
                ++pos;

                if (!allocation.count(i) && !(i->type == PirType::voyd()) &&
                    !Phi::Cast(i) && i->hasSingleUse()) {
                    stack.push_back(i);
                }
            }
        });
    }

    void computeAllocation() {
        std::unordered_map<SlotNumber, std::unordered_set<Value*>> reverseAlloc;
        auto slotIsAvailable = [&](SlotNumber slot, Value* i) {
            for (auto other : reverseAlloc[slot])
                if (livenessInterval.at(other).interfere(
                        livenessInterval.at(i)))
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
        DominatorTreeVisitor<>(dom).run(code, [&](BB* bb) {
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

                if (!allocation.count(i) && livenessInterval.count(i)) {
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

    void print(std::ostream& out = std::cout) {
        out << "======= Allocation ========\n";
        BreadthFirstVisitor::run(code->entry, [&](BB* bb) {
            out << "BB" << bb->id << ": ";
            for (auto a : allocation) {
                auto i = a.first;
                if (Instruction::Cast(i) && Instruction::Cast(i)->bb() != bb)
                    continue;
                i->printRef(out);
                out << "@";
                if (allocation.at(i) == stackSlot)
                    out << "s";
                else
                    out << a.second;
                out << "   ";
            }
            out << "\n";
        });
        out << "======= End Allocation ========\n";
    }

    void verify() {
        // Explore all possible traces and verify the allocation
        typedef std::pair<BB*, BB*> Jmp;
        typedef std::unordered_map<size_t, Instruction*> RegisterFile;
        typedef std::deque<Instruction*> Stack;
        typedef std::function<void(BB*, RegisterFile&, Stack&)> VerifyBB;
        std::set<Jmp> branchTaken;

        VerifyBB verifyBB = [&](BB* bb, RegisterFile& reg, Stack& stack) {
            for (auto i : *bb) {
                Phi* phi = Phi::Cast(i);
                if (phi) {
                    SlotNumber slot = allocation.at(phi);
                    phi->eachInstructionArg([&](BB*, Instruction* i) {
                        if (allocation[i] != slot) {
                            std::cerr << "REG alloc fail: ";
                            phi->printRef(std::cerr);
                            std::cerr << " and it's input ";
                            i->printRef(std::cerr);
                            std::cerr << " have different allocations : ";
                            if (allocation[phi] == stackSlot)
                                std::cerr << "stack";
                            else
                                std::cerr << allocation[phi];
                            std::cerr << " vs ";
                            if (allocation[i] == stackSlot)
                                std::cerr << "stack";
                            else
                                std::cerr << allocation[i];
                            std::cerr << "\n";
                            assert(false);
                        }
                    });
                    if (slot == stackSlot)
                        stack.pop_back();
                } else {
                    // Make sure all our args are live
                    i->eachInstructionArg([&](Instruction* a) {
                        if (!allocation.count(a)) {
                            std::cerr << "REG alloc fail: ";
                            i->printRef(std::cerr);
                            std::cerr << " needs ";
                            a->printRef(std::cerr);
                            std::cerr << " but is not allocated\n";
                            assert(false);
                        } else {
                            Instruction* given = nullptr;
                            SlotNumber slot = allocation.at(a);
                            if (slot == stackSlot) {
                                given = stack.back();
                                stack.pop_back();
                            } else {
                                given = reg.at(slot);
                            }
                            if (given != a) {
                                std::cerr << "REG alloc fail: ";
                                i->printRef(std::cerr);
                                std::cerr << " needs ";
                                a->printRef(std::cerr);
                                if (slot == stackSlot) {
                                    std::cerr << " the stack has ";
                                } else {
                                    std::cerr << " but slot " << slot
                                              << " was overridden by ";
                                }
                                given->printRef(std::cerr);
                                std::cerr << "\n";
                                assert(false);
                            }
                        }
                    });
                }

                // Remember this instruction if it writes to a slot
                if (allocation.count(i)) {
                    if (allocation.at(i) == stackSlot)
                        stack.push_back(i);
                    else
                        reg[allocation.at(i)] = i;
                }
            }

            if (!bb->next0 && !bb->next1) {
                if (stack.size() != 0) {
                    std::cerr << "REG alloc fail: BB " << bb->id
                              << " tries to return with " << stack.size()
                              << " elements on the stack\n";
                    assert(false);
                }
            }

            if (bb->next0 && !branchTaken.count(Jmp(bb, bb->next0))) {
                branchTaken.insert(Jmp(bb, bb->next0));
                if (!bb->next1) {
                    verifyBB(bb->next0, reg, stack);
                } else {
                    // Need to copy here, since we are gonna explore next1 next
                    RegisterFile regC = reg;
                    Stack stackC = stack;
                    verifyBB(bb->next0, regC, stackC);
                }
            }
            if (bb->next1 && !branchTaken.count(Jmp(bb, bb->next1))) {
                branchTaken.insert(Jmp(bb, bb->next1));
                verifyBB(bb->next1, reg, stack);
            }
        };

        {
            RegisterFile f;
            Stack s;
            verifyBB(code->entry, f, s);
        }
    }

    size_t operator[](Value* v) const {
        Instruction* i = Instruction::Cast(v);
        assert(i);
        assert(allocation.at(i) != stackSlot);
        return allocation.at(i) - 1;
    }

    size_t slots() const { return allocation.size(); }

    bool onStack(Value* v) const {
        Instruction* i = Instruction::Cast(v);
        assert(i);
        return allocation.at(i) == stackSlot;
    }

    bool dead(Value* v) const {
        Instruction* i = Instruction::Cast(v);
        assert(i);
        return allocation.count(i) == 0;
    }
};

class Context {
  public:
    std::stack<CodeStream*> css;
    FunctionWriter& fun;

    Context(FunctionWriter& fun) : fun(fun) {}
    ~Context() { assert(css.empty()); }

    CodeStream& cs() { return *css.top(); }

    void pushDefaultArg(SEXP ast) {
        defaultArg.push(true);
        push(ast);
    }
    void pushPromise(SEXP ast) {
        defaultArg.push(false);
        push(ast);
    }
    void pushBody(SEXP ast) {
        defaultArg.push(false);
        push(ast);
    }

    FunIdxT finalizeCode(size_t localsCnt) {
        auto idx = cs().finalize(defaultArg.top(), localsCnt);
        delete css.top();
        defaultArg.pop();
        css.pop();
        return idx;
    }

  private:
    std::stack<bool> defaultArg;
    void push(SEXP ast) { css.push(new CodeStream(fun, ast)); }
};

class Pir2Rir {
  public:
    Pir2Rir(Pir2RirCompiler& cmp, Closure* cls) : compiler(cmp), cls(cls) {}
    size_t compileCode(Context& ctx, Code* code);
    void toCSSA(Code* code);
    rir::Function* finalize();

  private:
    Pir2RirCompiler& compiler;
    Closure* cls;
    std::unordered_map<Promise*, FunIdxT> promises;
    std::unordered_map<Promise*, SEXP> argNames;
};

size_t Pir2Rir::compileCode(Context& ctx, Code* code) {
    toCSSA(code);

    if (compiler.verbose)
        code->print(std::cout);

    SSAAllocator alloc(code, compiler.verbose);

    if (compiler.verbose)
        alloc.print();

    alloc.verify();

    // create labels for all bbs
    std::unordered_map<BB*, LabelT> bbLabels;
    BreadthFirstVisitor::run(code->entry, [&](BB* bb) {
        if (!bb->isEmpty())
            bbLabels[bb] = ctx.cs().mkLabel();
    });

    BreadthFirstVisitor::run(code->entry, [&](BB* bb) {
        if (bb->isEmpty())
            return;

        CodeStream& cs = ctx.cs();
        cs << bbLabels[bb];

        /*
            this attempts to eliminate redundant store-load pairs, ie.
            if there is an instruction that has only one use, and the use is the
            next instruction, and it is its only input, and PirCopy is not
            involved
            TODO: is it always the case that the store and load use the same
            local slot?
        */
        auto store = [&](BB::Instrs::iterator it, Value* what) {
            if (alloc.dead(what)) {
                cs << BC::pop();
                return;
            }
            if (alloc.onStack(what))
                return;
            cs << BC::stloc(alloc[what]);
        };
        auto load = [&](BB::Instrs::iterator it, Value* what) {
            if (alloc.onStack(what))
                return;
            cs << BC::ldloc(alloc[what]);
        };

        Value* currentEnv = nullptr;
        auto loadEnv = [&](BB::Instrs::iterator it, Value* val) {
            assert(Env::isAnyEnv(val));
            if (Env::isStaticEnv(val))
                cs << BC::push(Env::Cast(val)->rho);
            else
                load(it, val);
        };
        auto setEnv = [&](BB::Instrs::iterator it, Value* val) {
            assert(Env::isAnyEnv(val));
            if (val != currentEnv) {
                currentEnv = val;
                loadEnv(it, val);
                cs << BC::setEnv();
            } else if (alloc.allocation[val] == SSAAllocator::stackSlot) {
                cs << BC::pop();
            }
        };

        for (auto it = bb->begin(); it != bb->end(); ++it) {
            auto instr = *it;
            switch (instr->tag) {
            case Tag::LdConst: {
                cs << BC::push(LdConst::Cast(instr)->c);
                store(it, instr);
                break;
            }
            case Tag::LdFun: {
                auto ldfun = LdFun::Cast(instr);
                setEnv(it, ldfun->env());
                cs << BC::ldfun(ldfun->varName);
                store(it, ldfun);
                break;
            }
            case Tag::LdVar: {
                auto ldvar = LdVar::Cast(instr);
                setEnv(it, ldvar->env());
                cs << BC::ldvarNoForce(ldvar->varName);
                store(it, ldvar);
                break;
            }
            case Tag::ForSeqSize: {
                // TODO: not tested
                // make sure that the seq is at TOS? the instr doesn't push it!
                cs << BC::forSeqSize();
                store(it, instr);
                break;
            }
            case Tag::LdArg: {
                cs << BC::ldarg(LdArg::Cast(instr)->id);
                store(it, instr);
                break;
            }
            case Tag::ChkMissing: {
                assert(false && "not yet implemented.");
                break;
            }
            case Tag::ChkClosure: {
                assert(false && "not yet implemented.");
                break;
            }
            case Tag::StVarSuper: {
                // TODO: not tested
                auto stvar = StVarSuper::Cast(instr);
                load(it, stvar->val());
                setEnv(it, stvar->env());
                cs << BC::stvarSuper(stvar->varName);
                break;
            }
            case Tag::LdVarSuper: {
                // TODO: not tested
                auto ldvar = LdVarSuper::Cast(instr);
                setEnv(it, ldvar->env());
                cs << BC::ldvarNoForceSuper(ldvar->varName);
                store(it, ldvar);
                break;
            }
            case Tag::StVar: {
                auto stvar = StVar::Cast(instr);
                load(it, stvar->val());
                setEnv(it, stvar->env());
                cs << BC::stvar(stvar->varName);
                break;
            }
            case Tag::Branch: {
                auto br = Branch::Cast(instr);
                load(it, br->arg<0>().val());

                // jump through empty blocks
                auto next0 = bb->next0;
                while (next0->isEmpty())
                    next0 = next0->next0;
                auto next1 = bb->next1;
                while (next1->isEmpty())
                    next1 = next1->next0;

                cs << BC::brfalse(bbLabels[next0]) << BC::br(bbLabels[next1]);

                // this is the end of this BB
                return;
            }
            case Tag::Return: {
                Return* ret = Return::Cast(instr);
                load(it, ret->arg<0>().val());
                cs << BC::ret();

                // this is the end of this BB
                return;
            }
            case Tag::MkArg: {
                // TODO: handle eager values
                BreadthFirstVisitor::run(bb, [&](Instruction* i) {
                    i->eachArg([&](Value* arg) {
                        if (arg == i)
                            assert(CallInstruction::Cast(i) &&
                                   "MkArg used in a non-call instruction");
                    });
                });
                break;
            }
            case Tag::Seq: {
                // TODO: not tested
                auto seq = Seq::Cast(instr);
                auto start = seq->arg<0>().val();
                auto end = seq->arg<1>().val();
                auto step = seq->arg<2>().val();
                cs << BC::ldloc(alloc[start]) << BC::ldloc(alloc[end])
                   << BC::ldloc(alloc[step]) << BC::seq();
                store(it, seq);
                break;
            }
            case Tag::MkCls: {
                // check if it is used (suspect only MkFunCls is ever created)
                assert(false && "not yet implemented.");
                break;
            }
            case Tag::MkFunCls: {
                // TODO: not tested

                auto mkfuncls = MkFunCls::Cast(instr);
                Pir2Rir pir2rir(compiler, mkfuncls->fun);
                auto rirFun = pir2rir.finalize();

                auto dt = DispatchTable::unpack(mkfuncls->code);
                assert(dt->capacity() == 2 && dt->at(1) == nullptr &&
                       "invalid dispatch table");
                dt->put(1, rirFun);

                cs << BC::push(mkfuncls->fml) << BC::push(mkfuncls->code)
                   << BC::push(mkfuncls->src) << BC::close();
                store(it, mkfuncls);
                break;
            }
            case Tag::CastType: {
                auto cast = CastType::Cast(instr);
                load(it, cast->arg<0>().val());
                store(it, cast);
                break;
            }
            case Tag::Subassign1_1D: {
                assert(false && "not yet implemented.");
                break;
            }
            case Tag::Subassign2_1D: {
                assert(false && "not yet implemented.");
                break;
            }
            case Tag::Extract1_1D: {
                assert(false && "not yet implemented.");
                break;
            }
            case Tag::Extract2_1D: {
                assert(false && "not yet implemented.");
                break;
            }
            case Tag::Extract1_2D: {
                assert(false && "not yet implemented.");
                break;
            }
            case Tag::Extract2_2D: {
                assert(false && "not yet implemented.");
                break;
            }
            case Tag::IsObject: {
                assert(false && "not yet implemented.");
                break;
            }
            case Tag::LdFunctionEnv: {
                // TODO: what should happen? For now get the current env (should
                // be the promise environment that the evaluator was called
                // with) and store it into local and leave it set as current
                cs << BC::getEnv();
                store(it, instr);
                break;
            }
            case Tag::PirCopy: {
                auto cpy = PirCopy::Cast(instr);
                load(it, cpy->arg<0>().val());
                store(it, cpy);
                break;
            }
            case Tag::Is: {
                auto is = Is::Cast(instr);
                load(it, is->arg<0>().val());
                cs << BC::is(is->tag);
                store(it, is);
                break;
            }

#define SIMPLE_INSTR(Name, Factory)                                            \
    case Tag::Name: {                                                          \
        auto simple = Name::Cast(instr);                                       \
        load(it, simple->arg<0>().val());                                      \
        cs << BC::Factory();                                                   \
        cs.addSrcIdx(simple->srcIdx);                                          \
        store(it, simple);                                                     \
        break;                                                                 \
    }
                SIMPLE_INSTR(Inc, inc);
                SIMPLE_INSTR(Force, force);
                SIMPLE_INSTR(AsLogical, asLogical);
                SIMPLE_INSTR(AsTest, asbool);
                // unary operators are simple, too
                SIMPLE_INSTR(Plus, uplus);
                SIMPLE_INSTR(Minus, uminus);
                SIMPLE_INSTR(Not, Not);
                SIMPLE_INSTR(Length, length);

#undef SIMPLE_INSTR

#define BINOP(Name, Factory)                                                   \
    case Tag::Name: {                                                          \
        auto binop = Name::Cast(instr);                                        \
        load(it, binop->arg<0>().val());                                       \
        load(it, binop->arg<1>().val());                                       \
        cs << BC::Factory();                                                   \
        cs.addSrcIdx(binop->srcIdx);                                           \
        store(it, binop);                                                      \
        break;                                                                 \
    }
                BINOP(Add, add);
                BINOP(Sub, sub);
                BINOP(Mul, mul);
                BINOP(Div, div);
                BINOP(IDiv, idiv);
                BINOP(Mod, mod);
                BINOP(Pow, pow);
                BINOP(Lt, lt);
                BINOP(Gt, gt);
                BINOP(Lte, ge);
                BINOP(Gte, le);
                BINOP(Eq, eq);
                BINOP(Neq, ne);
                BINOP(LOr, lglOr);
                BINOP(LAnd, lglAnd);
                BINOP(Colon, colon);
#undef BINOP

            case Tag::Call: {
                auto call = Call::Cast(instr);
                setEnv(it, call->env());
                load(it, call->cls());

                std::vector<FunIdxT> callArgs;
                call->eachCallArg([&](Value* arg) {
                    // TODO: for now, ignore the eager value in MkArg
                    auto mkarg = MkArg::Cast(arg);
                    callArgs.push_back(promises[mkarg->prom]);
                });

                cs.insertCall(Opcode::call_, callArgs, {},
                              Pool::get(call->srcIdx));
                store(it, call);
                break;
            }
            case Tag::StaticCall: {
                auto call = StaticCall::Cast(instr);
                setEnv(it, call->env());

                compiler.compile(call->cls(), call->origin());
                // push callee - this stores it into constant pool - discuss!
                cs << BC::push(call->origin());

                std::vector<FunIdxT> callArgs;
                call->eachCallArg([&](Value* arg) {
                    // TODO: for now, ignore the eager value in MkArg
                    auto mkarg = MkArg::Cast(arg);
                    callArgs.push_back(promises[mkarg->prom]);
                });

                cs.insertCall(Opcode::call_, callArgs, {},
                              Pool::get(call->srcIdx));
                store(it, call);
                break;
            }
            case Tag::StaticEagerCall: {
                auto call = StaticEagerCall::Cast(instr);

                compiler.compile(call->cls(), call->origin());

                setEnv(it, call->env());
                call->eachCallArg([&](Value* arg) { load(it, arg); });
                cs.insertStackCall(Opcode::static_call_stack_,
                                   call->nCallArgs(), {},
                                   Pool::get(call->srcIdx), call->origin());
                store(it, call);
                break;
            }
            case Tag::CallBuiltin: {
                auto blt = CallBuiltin::Cast(instr);
                setEnv(it, blt->env());
                blt->eachCallArg([&](Value* arg) { load(it, arg); });
                cs.insertStackCall(Opcode::static_call_stack_, blt->nCallArgs(),
                                   {}, Pool::get(blt->srcIdx), blt->blt);
                store(it, blt);
                break;
            }
            case Tag::CallSafeBuiltin: {
                auto blt = CallSafeBuiltin::Cast(instr);
                // no environment
                blt->eachArg([&](Value* arg) { load(it, arg); });
                cs.insertStackCall(Opcode::static_call_stack_, blt->nargs(), {},
                                   Pool::get(blt->srcIdx), blt->blt);
                store(it, blt);
                break;
            }
            case Tag::MkEnv: {
                auto mkenv = MkEnv::Cast(instr);
                loadEnv(it, mkenv->parent());
                cs << BC::makeEnv() << BC::dup() << BC::setEnv();
                currentEnv = mkenv;
                // bind all args
                // TODO: maybe later have a separate instruction for this?
                mkenv->eachLocalVar([&](SEXP name, Value* val) {
                    load(it, val);
                    cs << BC::stvar(name);
                });
                store(it, mkenv);
                break;
            }
            case Tag::Phi: {
                auto phi = Phi::Cast(instr);
                phi->eachArg([&](BB*, Value* arg) {
                    assert(((alloc.onStack(phi) && alloc.onStack(arg)) ||
                            (alloc[phi] == alloc[arg])) &&
                           "Phi inputs must all be allocated in 1 slot");
                });
                load(it, phi);
                store(it, phi);
                break;
            }
            case Tag::Deopt: {
                assert(false && "not yet implemented.");
                break;
            }
            // values, not instructions
            case Tag::Missing:
            case Tag::Env:
            case Tag::Nil:
                break;
            // dummy sentinel enum item
            case Tag::_UNUSED_:
                break;
            }
        }

        // this BB has exactly one successor, next0
        // jump through empty blocks
        auto next = bb->next0;
        while (next->isEmpty())
            next = next->next0;
        cs << BC::br(bbLabels[next]);
    });

    return alloc.slots();
}

void Pir2Rir::toCSSA(Code* code) {

    // For each Phi, insert copies
    BreadthFirstVisitor::run(code->entry, [&](BB* bb) {
        // TODO: move all phi's to the beginning, then insert the copies not
        // after each phi but after all phi's
        for (auto it = bb->begin(); it != bb->end(); ++it) {
            auto instr = *it;
            Phi* phi = Phi::Cast(instr);
            if (phi) {
                for (size_t i = 0; i < phi->nargs(); ++i) {
                    BB* pred = phi->input[i];
                    // pred is either jump (insert copy at end) or branch
                    // (insert copy before the branch instr)
                    auto it = pred->isJmp() ? pred->end() : pred->end() - 1;
                    Instruction* iav = Instruction::Cast(phi->arg(i).val());
                    auto copy = pred->insert(it, new PirCopy(iav));
                    phi->arg(i).val() = *copy;
                }
                auto phiCopy = new PirCopy(phi);
                phi->replaceUsesWith(phiCopy);
                it = bb->insert(it + 1, phiCopy);
            }
        }
    });

    DEBUGCODE(PHI_REMOVE_DEBUG, {
        std::cout << "--- phi copies inserted ---\n";
        code->print(std::cout);
    });
}

rir::Function* Pir2Rir::finalize() {
    // TODO: asts are NIL for now, how to deal with that? after inlining
    // functions and asts don't correspond anymore

    FunctionWriter function = FunctionWriter::create();
    Context ctx(function);

    size_t i = 0;
    for (auto arg : cls->defaultArgs) {
        if (!arg)
            continue;
        ctx.pushDefaultArg(R_NilValue);
        size_t localsCnt = compileCode(ctx, arg);
        promises[arg] = ctx.finalizeCode(localsCnt);
        argNames[arg] = cls->argNames[i++];
    }
    for (auto prom : cls->promises) {
        if (!prom)
            continue;
        ctx.pushPromise(R_NilValue);
        size_t localsCnt = compileCode(ctx, prom);
        promises[prom] = ctx.finalizeCode(localsCnt);
    }
    ctx.pushBody(R_NilValue);
    size_t localsCnt = compileCode(ctx, cls);
    ctx.finalizeCode(localsCnt);

    CodeEditor code(function.function->body());

    for (size_t i = 0; i < code.numPromises(); ++i)
        if (code.promise(i))
            Optimizer::optimize(*code.promise(i));
    Optimizer::optimize(code);

    auto opt = code.finalize();

#ifdef ENABLE_SLOWASSERT
    CodeVerifier::verifyFunctionLayout(opt->container(), globalContext());
#endif

    return opt;
}

} // namespace

rir::Function* Pir2RirCompiler::compile(Closure* cls, SEXP origin) {
    auto table = DispatchTable::unpack(BODY(origin));
    if (table->slot(1))
        return table->at(1);

    Pir2Rir pir2rir(*this, cls);
    auto fun = pir2rir.finalize();
    Protect p(fun->container());

    auto oldFun = table->first();

    fun->invocationCount = oldFun->invocationCount;
    // TODO: are these still needed / used?
    fun->envLeaked = oldFun->envLeaked;
    fun->envChanged = oldFun->envChanged;
    // TODO: signatures need a rework
    fun->signature = oldFun->signature;

    table->put(1, fun);

    return fun;
}

} // namespace pir
} // namespace rir
