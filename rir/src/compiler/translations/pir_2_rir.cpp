
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

namespace rir {
namespace pir {

namespace {

class Alloc {
  public:
    typedef size_t LocalSlotIdx;

    LocalSlotIdx allocateLocal(Value* val) {
        assert(alloc.count(val) == 0);
        alloc[val] = maxLocalIdx;
        return maxLocalIdx++;
    }

    void allocateLocal(Value* val, LocalSlotIdx i) {
        assert(alloc.count(val) == 0);
        assert(i < maxLocalIdx);
        alloc[val] = i;
    }

    LocalSlotIdx slots() const { return maxLocalIdx; }

    std::unordered_map<Value*, LocalSlotIdx> alloc;

  private:
    LocalSlotIdx maxLocalIdx = 0;
};

class Pir2Rir {
  public:
    Pir2Rir(Closure* cls, Alloc& a) : cls(cls), a(a) {}
    rir::Function* finalize();

  private:
    Closure* cls;
    Alloc& a;
};

rir::Function* Pir2Rir::finalize() {
    FunctionWriter funWrt = FunctionWriter::create();
    std::vector<CodeStream*> codeStreams;

    // for now, ignore formals

    // TODO: ast is NIL for now, how to deal with that? after inlining
    // functions and asts don't correspond anymore
    codeStreams.push_back(new CodeStream(funWrt, R_NilValue));

    BreadthFirstVisitor::run(cls->entry, [&](BB* bb) {
        CodeStream& cs = *codeStreams.back();
        for (auto instr : *bb) {
            switch (instr->tag) {
            case Tag::LdConst:
                cs << BC::push(LdConst::Cast(instr)->c)
                   << BC::stloc(a.alloc[instr]);
                break;
            case Tag::LdArg:
                cs << BC::ldarg(LdArg::Cast(instr)->id)
                   << BC::stloc(a.alloc[instr]);
                break;
            case Tag::Add: {
                auto add = Add::Cast(instr);
                auto lhs = a.alloc[add->arg(0).val()];
                auto rhs = a.alloc[add->arg(1).val()];
                auto res = a.alloc[add];
                cs << BC::ldloc(lhs) << BC::ldloc(rhs) << BC::add()
                   << BC::stloc(res);
                break;
            }
            case Tag::Force: {
                auto force = Force::Cast(instr);
                auto slot = a.alloc[force];
                auto argslot = a.alloc[force->arg(0).val()];
                cs << BC::ldloc(argslot) << BC::force() << BC::stloc(slot);
                break;
            }
            case Tag::Return: {
                Return* ret = Return::Cast(instr);
                auto slot = a.alloc[ret->arg(0).val()];
                cs << BC::ldloc(slot) << BC::ret();
                break;
            }
            default:
                assert(false && "PIR to RIR translation of this instruction "
                                "not yet implemented.");
            }
        }
    });

    for (auto cs : codeStreams) {
        cs->finalize(false);
        delete cs;
    }

    CodeEditor code(funWrt.function->body());

    for (size_t i = 0; i < code.numPromises(); ++i)
        if (code.promise(i))
            Optimizer::optimize(*code.promise(i));
    Optimizer::optimize(code);

    auto opt = code.finalize();

#ifdef ENABLE_SLOWASSERT
    CodeVerifier::verifyFunctionLayout(opt->container(), globalContext());
#endif

    opt->body()->localsCount = a.slots();
    opt->isPirCompiled = true;

    return opt;
}

} // namespace

rir::Function* Pir2RirCompiler::operator()(Module* m) {

    // TODO: what about multiple functions??
    std::vector<rir::Function*> results;

    m->eachPirFunction([&](Closure* cls) {
        // For each Phi, insert copies
        BreadthFirstVisitor::run(cls->entry, [&](BB* bb) {
            std::vector<Instruction*> phiCopies;
            for (auto instr : *bb) {
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
                    phiCopies.push_back(phiCopy);
                    phi->replaceUsesWith(phiCopy);
                }
            }
            // find last phi in bb, insert all copies after it
            auto lastPhi = bb->end();
            for (auto it = bb->begin(); it != bb->end(); ++it)
                if (Phi::Cast(*it))
                    lastPhi = it;
            ++lastPhi;
            bb->insert(lastPhi, phiCopies);
        });

        // std::cout << "--- phi copies inserted ---\n";
        // cls->print(std::cout);

        Alloc a;
        BreadthFirstVisitor::run(cls->entry, [&](Instruction* instr) {
            Phi* phi = Phi::Cast(instr);
            if (phi) {
                auto slot = a.allocateLocal(phi);
                phi->eachArg([&](Value* arg) { a.allocateLocal(arg, slot); });
            }
        });
        BreadthFirstVisitor::run(cls->entry, [&](Instruction* instr) {
            if (instr->type != PirType::voyd() && a.alloc.count(instr) == 0)
                a.allocateLocal(instr);
        });

        // std::cout << "--- locals allocation ---\n";
        // for (auto x : a.alloc) {
        //     std::cout << "  ";
        //     x.first->printRef(std::cout);
        //     std::cout << " -> " << x.second << "\n";
        // }
        // std::cout << "total slots needed: " << a.slots() << "\n";

        Pir2Rir cmp(cls, a);
        results.push_back(cmp.finalize());
    });

    // for now, assert there is only one
    assert(results.size() == 1);

    return results.back();
}

} // namespace pir
} // namespace rir
