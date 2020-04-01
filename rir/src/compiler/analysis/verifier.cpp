#include "verifier.h"
#include "../pir/pir_impl.h"
#include "../util/cfg.h"
#include "../util/visitor.h"

/*
 * When does the verifier run, and is it the fast or slow version?
 *
 *                        FULLVERIFIER   ENABLE_SLOWASSERT   Release   NDEBUG
 * After RIR to PIR          slow             fast            fast       x
 * After each PIR pass       slow             fast            x          x
 * After all PIR opts        [1]              slow            fast       x
 * Before PIR to RIR         slow             fast            x          x
 *
 * [1] Not run, subsumed by running the full verifier after each PIR pass.
 */

namespace {
using namespace rir::pir;

class TheVerifier {
  public:
    const std::string& msg;
    ClosureVersion* f;

    explicit TheVerifier(ClosureVersion* f, const std::string& msg, bool slow)
        : msg(msg), f(f), slow(slow) {}

    bool ok = true;
    bool slow = false;
    std::unordered_map<Code*, DominanceGraph> doms;
    std::unordered_map<Code*, CFG> cfgs;
    std::unordered_set<BB*> seenPreds;

    void operator()() {
        Visitor::run(f->entry, [&](BB* bb) { return verify(bb, false); });
        Visitor::run(f->entry, [&](BB* bb) { seenPreds.erase(bb); });
        if (!seenPreds.empty()) {
            std::cerr << "The following preds are not reachable from entry: ";
            for (auto p : seenPreds)
                std::cerr << p->id << " ";
            std::cerr << "\n";
            ok = false;
        }

        if (!ok) {
            std::cerr << "Verification of function " << *f << " failed\n";
            f->print(std::cerr, false);
            Rf_error(msg.c_str());
        }

        f->eachPromise([&](Promise* p) {
            if (p) {
                if (p != f->promise(p->id)) {
                    std::cerr << "Promise with id " << p->id
                              << " is in the wrong slot\n";
                    ok = false;
                }
                verify(p);
                Visitor::run(p->entry, [&](BB* bb) { seenPreds.erase(bb); });
                if (!seenPreds.empty()) {
                    std::cerr
                        << "The following preds are not reachable from entry: ";
                    for (auto p : seenPreds)
                        std::cerr << p->id << " ";
                    std::cerr << "\n";
                    ok = false;
                }
            }
            if (!ok) {
                std::cerr << "Verification of promise failed\n";
                p->printCode(std::cerr, false, false);
                Rf_error("");
            }
        });

        if (!ok) {
            Rf_error("");
        }
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
        for (auto suc : bb->succsessors()) {
            if (!suc->predecessors().count(bb)) {
                std::cout << "BB" << bb->id << " points to BB" << suc->id
                          << " but that one is not pointing back\n";
                ok = false;
            }
        }
        for (auto p : bb->predecessors()) {
            seenPreds.insert(p);
            if (!p->succsessors().any([&](BB* suc) { return suc == bb; })) {
                std::cout << "BB" << bb->id << " points back to BB" << p->id
                          << " but that one does not have us as a succsessor\n";
                ok = false;
            }
        }

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

               A      B
                \   /   \
                  C      D

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
        if (bb->isMerge()) {
            for (auto in : bb->predecessors()) {
                if (in->isBranch()) {
                    unsigned other = (in->trueBranch()->id == bb->id)
                                         ? in->falseBranch()->id
                                         : in->trueBranch()->id;
                    std::cerr << "BB" << bb->id << " is a merge node, but "
                              << "predecessor BB" << in->id << " has two "
                              << "successors (BB" << in->trueBranch()->id
                              << " and BB" << in->falseBranch()->id << "):\n"
                              << " n      " << in->id << "\n"
                              << "  \\   /   \\\n"
                              << "    " << bb->id << "      " << other << "\n";
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
                if (bb->succsessors().size() == 1) {
                    std::cerr << "split bb" << bb->id
                              << " must end in branch\n";
                    ok = false;
                }
            } else if (last->exits()) {
                if (bb->succsessors().size() > 0) {
                    std::cerr << "exit bb" << bb->id << " must end in return\n";
                    ok = false;
                }
            } else {
                if (bb->succsessors().size() > 1) {
                    std::cerr << "bb" << bb->id
                              << " has false branch but no branch instr\n";
                    ok = false;
                }
                if (bb->succsessors().size() == 0) {
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

        if (auto call = CallSafeBuiltin::Cast(i)) {
            call->eachCallArg([&](Value* v) {
                if (v->type.maybePromiseWrapped()) {
                    std::cerr << "Error: instruction '";
                    i->print(std::cerr);
                    std::cerr << "' has prom wrapped arg\n";
                    ok = false;
                }
            });
        }

        if (auto call = CallBuiltin::Cast(i)) {
            call->eachCallArg([&](Value* v) {
                if (v->type.maybePromiseWrapped()) {
                    std::cerr << "Error: instruction '";
                    i->print(std::cerr);
                    std::cerr << "' has prom wrapped arg\n";
                    ok = false;
                }
            });
        }

        if (auto assume = Assume::Cast(i)) {
            if (IsType::Cast(assume->arg(0).val())) {
                if (assume->feedbackOrigin.empty()) {
                    std::cerr << "Error: instruction '";
                    i->print(std::cerr);
                    std::cerr << "' typecheck without origin information\n";
                    ok = false;
                }
            }
        }

        if (auto mk = MkArg::Cast(i)) {
            auto p = mk->prom();
            if (p->owner->promise(p->id) != p) {
                std::cerr
                    << "PIR Verifier: Promise code out of current closure";
                ok = false;
            }
            if (p->owner != f) {
                mk->printRef(std::cerr);
                std::cerr << " is referencing a promise from another function "
                          << p->owner->name() << "\n";
                ok = false;
            }
        }

        if (i->branchOrExit())
            if (i != bb->last()) {
                std::cerr
                    << "PIR Verifier: Only last instruction of BB can have "
                       "controlflow";
                ok = false;
            }

        if (auto phi = Phi::Cast(i)) {
            phi->eachArg([&](BB* input, Value* v) {
                if (auto iv = Instruction::Cast(v)) {
                    if (input == phi->bb()) {
                        // Note: can happen in a one-block loop, but only if it
                        // is not edge-split
                        std::cerr << "Error at instruction '";
                        i->print(std::cerr);
                        std::cerr << "': input '";
                        iv->printRef(std::cerr);
                        std::cerr << "' one of the phi inputs is equal to the "
                                  << "BB this phi is located at. This is not "
                                  << "possible and makes no sense!\n";
                        ok = false;
                    }

                    if (slow) {
                        if ((!cfg(bb->owner).isPredecessor(iv->bb(), i->bb()) ||
                             // A block can be it's own predecessor (loop). But
                             // then the input must come after the phi!
                             (iv->bb() == i->bb() &&
                              i->bb()->indexOf(iv) < i->bb()->indexOf(i)))) {
                            std::cerr << "Error at instruction '";
                            i->print(std::cerr);
                            std::cerr << "': input '";
                            iv->printRef(std::cerr);
                            std::cerr
                                << "' does not come from a predecessor.\n";
                            ok = false;
                        }
                    }
                }
            });
                std::unordered_set<BB*> inp;
                for (auto in : bb->predecessors())
                    inp.insert(in);
                phi->eachArg([&](BB* bb, Value*) {
                    auto pos = inp.find(bb);
                    if (pos == inp.end()) {
                        std::cerr << "Error at instruction '";
                        i->print(std::cerr);
                        std::cerr << " input BB" << bb->id
                                  << " is not a predecessor\n";
                        ok = false;
                    } else {
                        inp.erase(pos);
                    }
                });
                if (!inp.empty()) {
                    std::cerr << "Error at instruction '";
                    i->print(std::cerr);
                    std::cerr << " the following predecessor blocks are not "
                                 "handled in phi: ";
                    for (auto& in : inp)
                        std::cerr << in->id << " ";
                    std::cerr << "\n";
                    ok = false;
                }
        }

        if (auto fs = FrameState::Cast(i)) {
            if (fs->env() == Env::elided()) {
                std::cerr << "Error at instruction '";
                i->print(std::cerr);
                std::cerr << " framestate env cannot be elided\n";
                ok = false;
            }
        }

        static std::unordered_set<Tag> allowStub{
            Tag::LdVar,     Tag::Force,          Tag::PushContext,
            Tag::StVar,     Tag::StVarSuper,     Tag::FrameState,
            Tag::IsEnvStub, Tag::MaterializeEnv, Tag::CallBuiltin,
            Tag::Call, Tag::StaticCall};
        if (i->hasEnv() && !allowStub.count(i->tag)) {
            auto env = MkEnv::Cast(i->env());
            if (env && env->stub) {
                std::cerr << "Error at instruction '";
                i->print(std::cerr);
                std::cerr << " that uses a stub environment\n";
                ok = false;
            }
        }

        i->eachArg([&](const InstrArg& a) -> void {
            auto v = a.val();
            auto t = a.type();

            if (auto iv = Instruction::Cast(v)) {
                if (!Phi::Cast(i)) {
                    if ((iv->bb() == i->bb() &&
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

void Verify::apply(ClosureVersion* f, const std::string& msg, bool slow) {
    TheVerifier v(f, msg, slow);
    v();
}
} // namespace pir
} // namespace rir
