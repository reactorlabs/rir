#include "pir_2_rir.h"
#include "../pir/pir_impl.h"
#include "../transform/bb.h"
#include "../util/cfg.h"
#include "../util/visitor.h"
#include "interpreter/runtime.h"
#include "ir/CodeStream.h"
#include "ir/CodeVerifier.h"
#include "utils/FunctionWriter.h"

#include <algorithm>
#include <iomanip>
#include <sstream>

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

    explicit SSAAllocator(Code* code)
        : cfg(code), dom(code), code(code), bbsSize(code->nextBBId) {
        computeLiveness();
        computeStackAllocation();
        computeAllocation();
    }

    // Run backwards analysis to compute livenessintervals
    void computeLiveness() {
        // temp list of live out sets for every BB
        std::unordered_map<BB*, std::set<Value*>> liveAtEnd(bbsSize);

        std::set<BB*> todo;
        for (auto e : cfg.exits())
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
                    if (auto phi = Phi::Cast(i)) {
                        phi->eachArg([&](BB* in, Value* v) {
                            if (markIfNotSeen(v))
                                accumulatedPhiInput[in].insert(v);
                        });
                    } else {
                        i->eachArg([&](Value* v) {
                            if (markIfNotSeen(v))
                                accumulated.insert(v);
                        });
                    }

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
                    if (bb == inBB || cfg.isPredecessor(inBB, bb)) {
                        merge(bb, inLive);
                    }
                }
            };
            for (auto pre : cfg.immediatePredecessors(bb)) {
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
                        argsInRightOrder =
                            argsInRightOrder &&
                            /* 1. the phi input block must not be a branch block
                             *    and it must be the direct predecessor of
                             *    the merge block. */
                            in->isJmp() && in->next() == bb &&
                            /* 2. the phi input value must be pushed in stack
                             *    order. i.e. if we are currently looking for
                             *    the inputs to the pos-th phi, then the phi
                             *    input value must have been pushed as the
                             *    pos-th last value. */
                            in->size() >= pos && *(in->end() - pos) == v;
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
            std::deque<Instruction*> stack;

            auto tryLoadingArgsFromStack = [&](Instruction* i) {
                if (i->nargs() == 0 || stack.size() < i->nargs())
                    return;

                // Match all args to stack slots.
                size_t newStackSize = stack.size();
                bool foundAll = true;
                auto check = stack.rbegin();
                i->eachArgRev([&](Value* arg) {
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
                i->eachArgRev([&](Value* arg) { allocation[arg] = stackSlot; });
            };

            for (auto i : *bb) {
                tryLoadingArgsFromStack(i);

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

    void print(std::ostream& out) {

        out << "Liveness intervals:\n";
        for (auto ll : livenessInterval) {
            auto& l = ll.second;
            ll.first->printRef(out);
            out << " is live : ";
            for (size_t i = 0; i < bbsSize; ++i) {
                if (l[i].live) {
                    out << "BB" << i << " [";
                    out << l[i].begin << ",";
                    out << l[i].end << "]  ";
                }
            }
            out << "\n";
        }

        out << "Allocations:\n";
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
        out << "dead: ";
        BreadthFirstVisitor::run(code->entry, [&](BB* bb) {
            for (auto i : *bb) {
                if (allocation.count(i) == 0) {
                    i->printRef(out);
                    out << "   ";
                }
            }
        });
        out << "\nnumber of slots: " << slots() << "\n";
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
                    phi->eachArg([&](BB*, Value* arg) {
                        auto i = Instruction::Cast(arg);
                        if (!i)
                            return;
                        if (!allocation.count(i)) {
                            std::cerr << "REG alloc fail: ";
                            phi->printRef(std::cerr);
                            std::cerr << " needs ";
                            i->printRef(std::cerr);
                            std::cerr << " but is not allocated\n";
                            assert(false);
                        } else if (allocation[i] != slot) {
                            std::cerr << "REG alloc fail: ";
                            phi->printRef(std::cerr);
                            std::cerr << " and it's input ";
                            i->printRef(std::cerr);
                            std::cerr << " have different allocations: ";
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
                    // Make sure the argument slot is initialized
                    if (slot != stackSlot && reg.count(slot) == 0) {
                        std::cerr << "REG alloc fail: phi ";
                        phi->printRef(std::cerr);
                        std::cerr << " is reading from an unititialized slot\n";
                        assert(false);
                    }
                    if (slot == stackSlot)
                        stack.pop_back();
                } else {
                    // Make sure all our args are live
                    i->eachArgRev([&](Value* a) {
                        auto i = Instruction::Cast(a);
                        if (!i)
                            return;
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
                                // Make sure the argument slot is initialized
                                if (reg.count(slot) == 0) {
                                    std::cerr << "REG alloc fail: ";
                                    i->printRef(std::cerr);
                                    std::cerr << " is reading its argument ";
                                    a->printRef(std::cerr);
                                    std::cerr << "from an unititialized slot\n";
                                    assert(false);
                                }
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

            if (bb->isExit()) {
                if (stack.size() != 0) {
                    std::cerr << "REG alloc fail: BB " << bb->id
                              << " tries to return with " << stack.size()
                              << " elements on the stack\n";
                    assert(false);
                }
            }

            if (bb->trueBranch() &&
                !branchTaken.count(Jmp(bb, bb->trueBranch()))) {
                branchTaken.insert(Jmp(bb, bb->trueBranch()));
                if (!bb->falseBranch()) {
                    verifyBB(bb->trueBranch(), reg, stack);
                } else {
                    // Need to copy here, since we are gonna explore
                    // falseBranch() next
                    RegisterFile regC = reg;
                    Stack stackC = stack;
                    verifyBB(bb->trueBranch(), regC, stackC);
                }
            }
            if (bb->falseBranch() &&
                !branchTaken.count(Jmp(bb, bb->falseBranch()))) {
                branchTaken.insert(Jmp(bb, bb->falseBranch()));
                verifyBB(bb->falseBranch(), reg, stack);
            }
        };

        {
            RegisterFile f;
            Stack s;
            verifyBB(code->entry, f, s);
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
    Pir2Rir(Pir2RirCompiler& cmp, Closure* cls, SEXP origin, bool dryRun,
            LogStream& log)
        : compiler(cmp), cls(cls), originCls(origin), dryRun(dryRun), log(log) {
    }
    size_t compileCode(Context& ctx, Code* code);
    rir::Code* getPromise(Context& ctx, Promise* code);

    void lower(Code* code);
    void toCSSA(Code* code);
    rir::Function* finalize();

  private:
    Pir2RirCompiler& compiler;
    Closure* cls;
    SEXP originCls;
    std::unordered_map<Promise*, rir::Code*> promises;
    bool dryRun;
    LogStream& log;
};

size_t Pir2Rir::compileCode(Context& ctx, Code* code) {
    lower(code);
    toCSSA(code);
    log.CSSA(code);

    SSAAllocator alloc(code);
    log.afterAllocator(code, [&](std::ostream& o) { alloc.print(o); });
    alloc.verify();

    // create labels for all bbs
    std::unordered_map<BB*, BC::Label> bbLabels;
    BreadthFirstVisitor::run(code->entry, [&](BB* bb) {
        if (!bb->isEmpty())
            bbLabels[bb] = ctx.cs().mkLabel();
    });

    LoweringVisitor::run(code->entry, [&](BB* bb) {
        if (bb->isEmpty())
            return;

        CodeStream& cs = ctx.cs();
        auto debugAddVariableName = [&cs](Value* v) {
            std::stringstream ss;
            v->printRef(ss);
            cs.addSrc(Rf_install(ss.str().c_str()));
        };

        cs << bbLabels[bb];

        Value* currentEnv = nullptr;

        for (auto it = bb->begin(); it != bb->end(); ++it) {
            auto instr = *it;

            bool hasResult =
                instr->type != PirType::voyd() && !Phi::Cast(instr);

            auto explicitEnvValue = [](Instruction* instr) {
                return MkEnv::Cast(instr);
            };

            // Load Arguments to the stack
            {
                auto loadEnv = [&](BB::Instrs::iterator it, Value* what) {
                    if (what == Env::notClosed()) {
                        cs << BC::parentEnv();
                    } else if (what == Env::nil()) {
                        cs << BC::push(R_NilValue);
                    } else if (Env::isStaticEnv(what)) {
                        auto env = Env::Cast(what);
                        // Here we could also load env->rho, but if the user
                        // were to change the environment on the closure our
                        // code would be wrong.
                        if (originCls && env->rho == CLOENV(originCls))
                            cs << BC::parentEnv();
                        else
                            cs << BC::push(env->rho);
                    } else {
                        if (!alloc.hasSlot(what)) {
                            std::cerr << "Don't know how to load the env ";
                            what->printRef(std::cerr);
                            std::cerr << " (" << tagToStr(what->tag) << ")\n";
                            assert(false);
                        }
                        if (!alloc.onStack(what)) {
                            cs << BC::ldloc(alloc[what]);
                            debugAddVariableName(what);
                        }
                    }
                };

                auto loadArg = [&](BB::Instrs::iterator it, Instruction* instr,
                                   Value* what) {
                    if (what->tag == Tag::Tombstone) {
                        return;
                    }
                    if (what == Missing::instance()) {
                        // if missing flows into instructions with more than one
                        // arg we will need stack shuffling here
                        assert(MkArg::Cast(instr) &&
                               "only mkarg supports missing");
                        cs << BC::push(R_UnboundValue);
                    } else {
                        if (!alloc.hasSlot(what)) {
                            std::cerr << "Don't know how to load the arg ";
                            what->printRef(std::cerr);
                            std::cerr << " (" << tagToStr(what->tag) << ")\n";
                            assert(false);
                        }
                        if (!alloc.onStack(what)) {
                            cs << BC::ldloc(alloc[what]);
                            debugAddVariableName(what);
                        }
                    }
                };

                // Step one: load and set env
                if (!Phi::Cast(instr)) {
                    if (instr->hasEnv() && !explicitEnvValue(instr)) {
                        // If the env is passed on the stack, it needs
                        // to be TOS here. To relax this condition some
                        // stack shuffling would be needed.
                        assert(instr->envSlot() == instr->nargs() - 1);
                        auto env = instr->env();
                        if (currentEnv != env) {
                            loadEnv(it, env);
                            cs << BC::setEnv();
                            currentEnv = env;
                        } else {
                            if (alloc.hasSlot(env) && alloc.onStack(env))
                                cs << BC::pop();
                        }
                    }
                }

                // Step two: load the rest
                if (!Phi::Cast(instr)) {
                    instr->eachArg([&](Value* what) {
                        if (instr->hasEnv() && instr->env() == what) {
                            if (explicitEnvValue(instr))
                                loadEnv(it, what);
                        } else if (what != Env::elided()) {
                            loadArg(it, instr, what);
                        }
                    });
                }
            }

            switch (instr->tag) {
            case Tag::LdConst: {
                cs << BC::push(LdConst::Cast(instr)->c);
                break;
            }
            case Tag::LdFun: {
                auto ldfun = LdFun::Cast(instr);
                cs << BC::ldfun(ldfun->varName);
                break;
            }
            case Tag::LdVar: {
                auto ldvar = LdVar::Cast(instr);
                cs << BC::ldvarNoForce(ldvar->varName);
                break;
            }
            case Tag::ForSeqSize: {
                cs << BC::forSeqSize();
                // TODO: currently we always pop the sequence, since we
                // cannot deal with instructions that do not pop the value
                // after use. If it is used in a later instruction, it will
                // be loaded from a local variable again.
                cs << BC::swap() << BC::pop();
                break;
            }
            case Tag::LdArg: {
                cs << BC::ldarg(LdArg::Cast(instr)->id);
                break;
            }
            case Tag::StVarSuper: {
                auto stvar = StVarSuper::Cast(instr);
                cs << BC::stvarSuper(stvar->varName);
                break;
            }
            case Tag::LdVarSuper: {
                auto ldvar = LdVarSuper::Cast(instr);
                cs << BC::ldvarNoForceSuper(ldvar->varName);
                break;
            }
            case Tag::StVar: {
                auto stvar = StVar::Cast(instr);
                cs << BC::stvar(stvar->varName);
                break;
            }
            case Tag::Branch: {
                // jump through empty blocks
                auto trueBranch = bb->trueBranch();
                while (trueBranch->isEmpty())
                    trueBranch = trueBranch->next();
                auto falseBranch = bb->falseBranch();
                while (falseBranch->isEmpty())
                    falseBranch = falseBranch->next();

                cs << BC::brtrue(bbLabels[trueBranch])
                   << BC::br(bbLabels[falseBranch]);

                // this is the end of this BB
                return;
            }
            case Tag::Return: {
                cs << BC::ret();

                // this is the end of this BB
                return;
            }
            case Tag::MkArg: {
                cs << BC::promise(
                    cs.addPromise(getPromise(ctx, MkArg::Cast(instr)->prom())));
                break;
            }
            case Tag::MkFunCls: {
                auto mkfuncls = MkFunCls::Cast(instr);
                cs << BC::push(mkfuncls->fml) << BC::push(mkfuncls->code)
                   << BC::push(mkfuncls->src) << BC::close();
                break;
            }
            case Tag::Is: {
                auto is = Is::Cast(instr);
                cs << BC::is(is->sexpTag);
                break;
            }
            case Tag::Subassign2_1D: {
                cs << BC::subassign2();
                cs.addSrcIdx(instr->srcIdx);
                break;
            }

#define EMPTY(Name)                                                            \
    case Tag::Name: {                                                          \
        break;                                                                 \
    }
                EMPTY(PirCopy);
                EMPTY(CastType);
#undef EMPTY

            case Tag::LdFunctionEnv: {
                // TODO: what should happen? For now get the current env
                // (should be the promise environment that the evaluator was
                // called with) and store it into local and leave it set as
                // current
                cs << BC::getEnv();
                break;
            }

#define SIMPLE(Name, Factory)                                                  \
    case Tag::Name: {                                                          \
        cs << BC::Factory();                                                   \
        break;                                                                 \
    }
                SIMPLE(Identical, identical);
                SIMPLE(LOr, lglOr);
                SIMPLE(LAnd, lglAnd);
                SIMPLE(Inc, inc);
                SIMPLE(Force, force);
                SIMPLE(AsTest, asbool);
                SIMPLE(Length, length);
                SIMPLE(ChkMissing, checkMissing);
                SIMPLE(ChkClosure, isfun);
                SIMPLE(Seq, seq);
                SIMPLE(MkCls, close);
                SIMPLE(IsObject, isObj);
                SIMPLE(Int3, int3);
                SIMPLE(SetShared, setShared);
                SIMPLE(EnsureNamed, ensureNamed);
#undef SIMPLE

#define SIMPLE_WITH_SRCIDX(Name, Factory)                                      \
    case Tag::Name: {                                                          \
        cs << BC::Factory();                                                   \
        cs.addSrcIdx(instr->srcIdx);                                           \
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
                SIMPLE_WITH_SRCIDX(Not, Not);
                SIMPLE_WITH_SRCIDX(Extract1_1D, extract1_1);
                SIMPLE_WITH_SRCIDX(Extract2_1D, extract2_1);
                SIMPLE_WITH_SRCIDX(Extract1_2D, extract1_2);
                SIMPLE_WITH_SRCIDX(Extract2_2D, extract2_2);
                SIMPLE_WITH_SRCIDX(Subassign1_1D, subassign1);
#undef SIMPLE_WITH_SRCIDX

            case Tag::CallImplicit: {
                auto call = CallImplicit::Cast(instr);
                std::vector<BC::FunIdx> args;
                call->eachArg([&](Promise* p) {
                    args.push_back(cs.addPromise(getPromise(ctx, p)));
                });

                if (call->names.empty())
                    cs << BC::callImplicit(args, Pool::get(call->srcIdx));
                else
                    cs << BC::callImplicit(args, call->names,
                                           Pool::get(call->srcIdx));
                break;
            }
            case Tag::Call: {
                auto call = Call::Cast(instr);
                cs << BC::call(call->nCallArgs(), Pool::get(call->srcIdx));
                break;
            }
            case Tag::NamedCall: {
                auto call = NamedCall::Cast(instr);
                cs << BC::call(call->nCallArgs(), call->names,
                               Pool::get(call->srcIdx));
                break;
            }
            case Tag::StaticCall: {
                auto call = StaticCall::Cast(instr);
                // TODO: Unfortunately we cannot compile the target. We might
                // have optimized this function under assumptions (and we don't
                // even have access to the assumptions here). To be able to not
                // waste that work and actually compile the optimized version,
                // we need to put it in a specific slot and then have a
                // staticCall that dispatches to that slot.
                compiler.compile(call->cls(), call->origin(), dryRun);
                cs << BC::staticCall(call->nCallArgs(), Pool::get(call->srcIdx),
                                     call->origin());
                break;
            }
            case Tag::CallBuiltin: {
                // TODO(mhyee): all args have to be values, optimize here?
                auto blt = CallBuiltin::Cast(instr);
                cs << BC::staticCall(blt->nCallArgs(), Pool::get(blt->srcIdx),
                                     blt->blt);
                break;
            }
            case Tag::CallSafeBuiltin: {
                // TODO(mhyee): all args have to be values, optimize here?
                auto blt = CallSafeBuiltin::Cast(instr);
                cs << BC::staticCall(blt->nargs(), Pool::get(blt->srcIdx),
                                     blt->blt);
                break;
            }
            case Tag::MkEnv: {
                auto mkenv = MkEnv::Cast(instr);

                cs << BC::makeEnv();

                if (mkenv->nLocals() > 0) {
                    cs << BC::setEnv();
                    currentEnv = instr;

                    mkenv->eachLocalVarRev(
                        [&](SEXP name, Value* val) { cs << BC::stvar(name); });

                    cs << BC::getEnv();
                }
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
            case Tag::Deopt:
            case Tag::Assume:
            case Tag::Checkpoint: {
                assert(false && "Deopt instructions must be lowered into "
                                "standard branches and scheduled deopt, "
                                "before pir_2_rir");
                break;
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

                cs << BC::deopt(store);
                return;
            }
            case Tag::FrameState: {
                assert(false && "FrameState must be folded into scheduled "
                                "deopt, before pir_2_rir");
                break;
            }
            // values, not instructions
            case Tag::Tombstone:
            case Tag::Missing:
            case Tag::Env:
            case Tag::Nil:
                break;
            // dummy sentinel enum item
            case Tag::_UNUSED_:
                break;
            }

            // Store the result
            if (hasResult) {
                if (!alloc.hasSlot(instr))
                    cs << BC::pop();
                else if (!alloc.onStack(instr))
                    cs << BC::stloc(alloc[instr]);
            };
        }

        // this BB has exactly one successor, trueBranch()
        // jump through empty blocks
        assert(bb->trueBranch());
        auto next = bb->trueBranch();
        while (next->isEmpty())
            next = next->trueBranch();
        cs << BC::br(bbLabels[next]);
    });

    return alloc.slots();
} // namespace

template <typename CallType>
static bool allLazy(CallType* call, std::vector<Promise*>& args) {
    bool allLazy = true;
    call->eachCallArg([&](Value* v) {
        if (!allLazy)
            return;
        if (auto arg = MkArg::Cast(v)) {
            if (arg->eagerArg() != Missing::instance()) {
                allLazy = false;
            } else {
                args.push_back(arg->prom());
            }
        } else {
            allLazy = false;
        }
    });
    return allLazy;
}

void Pir2Rir::lower(Code* code) {
    Visitor::runPostChange(
        code->entry, [&](BB* bb) {
            auto it = bb->begin();
            while (it != bb->end()) {
                auto next = it + 1;
                if (auto call = CallInstruction::CastCall(*it))
                    call->clearFrameState();
                if (auto ldfun = LdFun::Cast(*it)) {
                    // the guessed binding in ldfun is just used as a temporary
                    // store. If we did not manage to resolve ldfun by now, we
                    // have to remove the guess again, since apparently we
                    // where not sure it is correct.
                    if (ldfun->guessedBinding())
                        ldfun->clearGuessedBinding();
                } else if (auto deopt = Deopt::Cast(*it)) {
                    // Lower Deopt instructions + their FrameStates to a
                    // ScheduledDeopt.
                    auto newDeopt = new ScheduledDeopt();
                    newDeopt->consumeFrameStates(deopt);
                    bb->replace(it, newDeopt);
                } else if (auto expect = Assume::Cast(*it)) {
                    BBTransform::lowerExpect(
                        code, bb, it, expect->condition(), expect->assumeTrue,
                        expect->checkpoint()->bb()->falseBranch());
                    // lowerExpect splits the bb from current position. There
                    // remains nothing to process. Breaking seems more robust
                    // than trusting the modified iterator.
                    break;
                } else if (auto call = Call::Cast(*it)) {
                    // Lower calls to call implicit
                    std::vector<Promise*> args;
                    if (allLazy(call, args))
                        call->replaceUsesAndSwapWith(
                            new CallImplicit(call->callerEnv(), call->cls(),
                                             std::move(args), {}, call->srcIdx),
                            it);
                } else if (auto call = NamedCall::Cast(*it)) {
                    // Lower named calls to call implicit
                    std::vector<Promise*> args;
                    if (allLazy(call, args))
                        call->replaceUsesAndSwapWith(
                            new CallImplicit(call->callerEnv(), call->cls(),
                                             std::move(args), call->names,
                                             call->srcIdx),
                            it);
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
}

rir::Code* Pir2Rir::getPromise(Context& ctx, Promise* p) {
    if (!promises.count(p)) {
        ctx.push(src_pool_at(globalContext(), p->srcPoolIdx()));
        size_t localsCnt = compileCode(ctx, p);
        promises[p] = ctx.finalizeCode(localsCnt);
    }
    return promises.at(p);
}

rir::Function* Pir2Rir::finalize() {
    // TODO: keep track of source ast indices in the source pool
    // (for now, calls, promises and operators do)
    // + how to deal with inlined stuff?

    FunctionWriter function;
    Context ctx(function);

    FunctionSignature signature(FunctionSignature::CalleeCreatedEnv,
                                FunctionSignature::OptimizedVersion,
                                cls->assumptions);

    // PIR does not support default args currently.
    for (size_t i = 0; i < cls->nargs(); ++i) {
        function.addArgWithoutDefault();
        signature.pushDefaultArgument();
    }

    assert(signature.nargs() == cls->nargs());
    ctx.push(R_NilValue);
    size_t localsCnt = compileCode(ctx, cls);
    log.finalPIR(cls);
    auto body = ctx.finalizeCode(localsCnt);
    function.finalize(body, signature);
#ifdef ENABLE_SLOWASSERT
    CodeVerifier::verifyFunctionLayout(function.function()->container(),
                                       globalContext());
#endif
    log.finalRIR(function.function());
    return function.function();
}

} // namespace

void Pir2RirCompiler::compile(Closure* cls, SEXP origin, bool dryRun) {
    if (done.count(cls))
        return;
    // Avoid recursivly compiling the same closure
    done.insert(cls);

    auto table = DispatchTable::unpack(BODY(origin));
    if (table->contains(cls->assumptions))
        return;

    auto& log = logger.get(cls);
    Pir2Rir pir2rir(*this, cls, origin, dryRun, logger.get(cls));
    auto fun = pir2rir.finalize();

    if (dryRun)
        return;

    Protect p(fun->container());

    auto oldFun = table->baseline();
    fun->body()->funInvocationCount = oldFun->body()->funInvocationCount;

    table->insert(fun);

    log.flush();
}

} // namespace pir
} // namespace rir
