#include "verifier.h"
#include "../pir/pir_impl.h"
#include "../util/cfg.h"
#include "../util/visitor.h"

namespace {
using namespace rir::pir;

class TheVerifier {
  public:
    ClosureVersion* f;

    explicit TheVerifier(ClosureVersion* f, bool slow) : f(f), slow(slow) {}

    bool ok = true;
    bool slow = false;
    std::unordered_map<Code*, DominanceGraph> doms;
    std::unordered_map<Code*, CFG> cfgs;

    void operator()() {
        Visitor::run(f->entry, [&](BB* bb) { return verify(bb, false); });

        if (!ok) {
            std::cerr << "Verification of function " << *f << " failed\n";
            f->print(std::cerr, true);
            assert(false);
            return;
        }

        f->eachPromise([&](Promise* p) {
            if (p) {
                if (p != f->promise(p->id)) {
                    std::cerr << "Promise with id " << p->id
                              << " is in the wrong slot\n";
                    ok = false;
                }
                verify(p);
            }
            if (!ok) {
                std::cerr << "Verification of promise failed\n";
                p->print(std::cerr, true);
                return;
            }
        });
    }

    const CFG& cfg(Code* c) {
        if (!cfgs.count(c))
            cfgs.emplace(c, c);
        return cfgs.at(c);
    }

    const DominanceGraph& dom(Code* c) {
        if (!doms.count(c))
            doms.emplace(c, c);
        return doms.at(c);
    }

    void verify(BB* bb, bool inPromise) {
        for (auto i : *bb) {
            if (FrameState::Cast(i) && inPromise) {
                std::cerr << "Framestate in promise!\n";
                ok = false;
            }
            verify(i, bb);
        }
        /* This check verifies that our graph is in edge-split format.
            Currently we do not rely on this property, however we should
            make it a conscious decision if we want to violate it.
            This basically rules out graphs of the following form:

                A       B
                \   /   \
                    C       D

            or
                _
            | / \
            A __/
            |

            The nice property about edge-split graphs is, that merge-points
            are always dominated by *both* inputs, therefore local code
            motion can push instructions to both input blocks.

            In the above example, we can't push an instruction from C to A
            and B, without worrying about D.
        */
        if (slow && cfg(bb->owner).isMergeBlock(bb)) {
            for (auto in : cfg(bb->owner).immediatePredecessors(bb)) {
                if (in->falseBranch()) {
                    std::cerr << "BB " << in->id << " merges into " << bb->id
                              << " and branches into " << in->falseBranch()->id
                              << " at the same time.\n";
                    ok = false;
                }
            }
        }

        if (bb->isEmpty()) {
            if (bb->isExit()) {
                std::cerr << "bb" << bb->id << " has no successor\n";
                ok = false;
            }
        } else {
            Instruction* last = bb->last();
            if (last->branches()) {
                if (!bb->falseBranch() || !bb->trueBranch()) {
                    std::cerr << "split bb" << bb->id
                              << " must end in branch\n";
                    ok = false;
                }
            } else if (last->exits()) {
                if (bb->trueBranch() || bb->falseBranch()) {
                    std::cerr << "exit bb" << bb->id << " must end in return\n";
                    ok = false;
                }
            } else {
                assert(!last->branchOrExit());
                if (bb->falseBranch()) {
                    std::cerr << "bb" << bb->id
                              << " has false branch but no branch instr\n";
                    ok = false;
                }
                if (!bb->trueBranch()) {
                    std::cerr << "bb" << bb->id << " has no successor\n";
                    ok = false;
                }
            }
        }
    }

    void verify(Promise* p) {
        Visitor::run(p->entry, [&](BB* bb) { verify(bb, true); });
    }

    void verify(Instruction* i, BB* bb) {
        if (i->bb() != bb) {
            std::cerr << "Error: instruction '";
            i->print(std::cerr);
            std::cerr << "' is supposed to point to BB " << bb
                      << " but points to " << i->bb() << "\n";
            ok = false;
        }

        if (auto call = StaticCall::Cast(i)) {
            if (call->hint && call->tryDispatch() &&
                call->hint->owner() != call->tryDispatch()->owner()) {
                std::cerr << "Error: instruction '";
                i->print(std::cerr);
                std::cerr << "' has broken hint (hint must be a version of the "
                             "same closure)\n";
                ok = false;
            }
        }

        if (auto mk = MkArg::Cast(i)) {
            auto p = mk->prom();
            assert(p->owner->promise(p->id) == p);
            if (p->owner != f) {
                mk->printRef(std::cerr);
                std::cerr << " is referencing a promise from another function "
                          << p->owner->name() << "\n";
                ok = false;
            }
        }

        if (i->branchOrExit())
            assert(i == bb->last() &&
                   "Only last instruction of BB can have controlflow");

        Phi* phi = Phi::Cast(i);
        i->eachArg([&](const InstrArg& a) -> void {
            auto v = a.val();
            auto t = a.type();
            if (auto iv = Instruction::Cast(v)) {
                if (phi) {
                    if (slow &&
                        !cfg(bb->owner).isPredecessor(iv->bb(), i->bb())) {
                        std::cerr << "Error at instruction '";
                        i->print(std::cerr);
                        std::cerr << "': input '";
                        iv->printRef(std::cerr);
                        std::cerr << "' does not come from a predecessor.\n";
                        ok = false;
                    }
                } else if ((iv->bb() == i->bb() &&
                            bb->indexOf(iv) > bb->indexOf(i)) ||
                           (iv->bb() != i->bb() && slow &&
                            !dom(bb->owner).dominates(iv->bb(), bb))) {
                    std::cerr << "Error at instruction '";
                    i->print(std::cerr);
                    std::cerr << "': input '";
                    iv->printRef(std::cerr);
                    std::cerr << "' used before definition.\n";
                    ok = false;
                }

                if (iv->bb()->owner != i->bb()->owner) {
                    std::cerr << "Error at instruction '";
                    i->print(std::cerr);
                    std::cerr << "': input '";
                    iv->printRef(std::cerr);
                    std::cerr << "' from a different function ("
                              << iv->bb()->owner << ")\n";
                    ok = false;
                }
            }

            if (!t.isSuper(v->type)) {
                std::cerr << "Error at instruction '";
                i->print(std::cerr);
                std::cerr << "': Value ";
                v->printRef(std::cerr);
                std::cerr << " has type " << v->type
                          << " which is not a subtype of " << t << "\n";
                ok = false;
            }
        });
    }
};
} // namespace

namespace rir {
namespace pir {

bool Verify::apply(ClosureVersion* f, bool slow) {
    TheVerifier v(f, slow);
    v();
    return v.ok;
}
} // namespace pir
} // namespace rir
