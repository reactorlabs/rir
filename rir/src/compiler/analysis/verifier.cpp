#include "verifier.h"
#include "../pir/pir_impl.h"
#include "../util/cfg.h"
#include "../util/visitor.h"

namespace {
using namespace rir::pir;

class TheVerifier {
  public:
    Closure* f;

    TheVerifier(Closure* f) : f(f) {}

    bool ok = true;

    void operator()() {
        DominanceGraph dom(f);
        CFG cfg(f);
        Visitor::run(f->entry, [&](BB* bb) { return verify(bb, dom, cfg); });

        if (!ok) {
            std::cerr << "Verification of function " << *f << " failed\n";
            f->print(std::cerr);
            assert(false);
            return;
        }

        for (auto p : f->promises) {
            if (p)
                verify(p);
            if (!ok) {
                std::cerr << "Verification of promise failed\n";
                p->print(std::cerr);
                return;
            }
        }
        for (auto p : f->defaultArgs) {
            if (p)
                verify(p);
            if (!ok) {
                std::cerr << "Verification of argument failed\n";
                p->print(std::cerr);
                return;
            }
        }
    }

    void verify(BB* bb, const DominanceGraph& dom, const CFG& cfg) {
        for (auto i : *bb)
            verify(i, bb, dom, cfg);
        if (bb->isEmpty()) {
            if (!bb->next0 && !bb->next1) {
                std::cerr << "bb" << bb->id << " has no successor\n";
                ok = false;
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
            if (cfg.isMergeBlock(bb)) {
                for (auto in : cfg.immediatePredecessors(bb)) {
                    if (in->next1) {
                        std::cerr << "BB " << in->id << " merges into "
                                  << bb->id << " and branches into "
                                  << in->next1->id << " at the same time.\n";
                        ok = false;
                    }
                }
            }
        } else {
            Instruction* last = bb->last();
            if ((Branch::Cast(last))) {
                if (!bb->next0 || !bb->next1) {
                    std::cerr << "split bb" << bb->id
                              << " must end in branch\n";
                    ok = false;
                }
            } else if ((Deopt::Cast(last)) || (Return::Cast(last))) {
                if (bb->next0 || bb->next1) {
                    std::cerr << "exit bb" << bb->id << " must end in return\n";
                    ok = false;
                }
            } else {
                if (!bb->next0 || bb->next1) {
                    std::cerr << "bb" << bb->id << " has next1 but no next0\n";
                    ok = false;
                }
            }
        }
    }

    void verify(Promise* p) {
        DominanceGraph dom(p);
        CFG cfg(p);
        Visitor::run(p->entry, [&](BB* bb) { verify(bb, dom, cfg); });
    }

    void verify(Instruction* i, BB* bb, const DominanceGraph& dom,
                const CFG& cfg) {
        if (i->bb() != bb) {
            std::cerr << "Error: instruction '";
            i->print(std::cerr);
            std::cerr << "' is supposed to point to BB " << bb
                      << " but points to " << i->bb() << "\n";
            ok = false;
        }

        Phi* phi = Phi::Cast(i);
        i->eachArg([&](const InstrArg& a) -> void {
            auto v = a.val();
            auto t = a.type();
            Instruction* iv = Instruction::Cast(v);
            if (iv) {
                if (phi) {
                    if (!cfg.isPredecessor(iv->bb(), i->bb())) {
                        std::cerr << "Error at instruction '";
                        i->print(std::cerr);
                        std::cerr << "': input '";
                        iv->printRef(std::cerr);
                        std::cerr << "' does not come from a predecessor.\n";
                        ok = false;
                    }
                } else if ((iv->bb() == i->bb() &&
                            bb->indexOf(iv) > bb->indexOf(i)) ||
                           (iv->bb() != i->bb() &&
                            !dom.dominates(iv->bb(), bb))) {
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
                    std::cerr << "' from a different function.\n";
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
}

namespace rir {
namespace pir {

bool Verify::apply(Closure* f) {
    TheVerifier v(f);
    v();
    return v.ok;
}
}
}
