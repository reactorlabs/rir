#include "rir_2_pir.h"
#include "../analysis/query.h"
#include "../analysis/verifier.h"
#include "../opt/cleanup.h"
#include "../opt/delay_env.h"
#include "../opt/delay_instr.h"
#include "../opt/elide_env.h"
#include "../opt/force_dominance.h"
#include "../opt/inline.h"
#include "../opt/scope_resolution.h"
#include "../pir/pir_impl.h"
#include "../transform/insert_cast.h"
#include "../util/builder.h"
#include "../util/cfg.h"
#include "../util/visitor.h"
#include "R/RList.h"
#include "ir/BC.h"
#include "ir/Compiler.h"

#include <deque>
#include <vector>

namespace {

using namespace rir::pir;
typedef rir::Function Function;
typedef rir::Opcode Opcode;
typedef rir::BC BC;
typedef rir::RList RList;

typedef std::pair<BB*, Value*> ReturnSite;

template <size_t SIZE>
struct Matcher {
    const std::array<Opcode, SIZE> seq;

    typedef std::function<void(Opcode*)> MatcherMaybe;

    bool operator()(Opcode* pc, Opcode* end, MatcherMaybe m) const {
        for (size_t i = 0; i < SIZE; ++i) {
            if (*pc != seq[i])
                return false;
            BC::advance(&pc);
            if (pc == end)
                return false;
        }
        m(pc);
        return true;
    }
};
}

namespace rir {
namespace pir {

Closure* Rir2PirCompiler::compileClosure(SEXP closure) {
    assert(TYPEOF(closure) == CLOSXP);

    if (!isValidClosureSEXP(closure)) {
        Protect p;
        SEXP rirClosure =
            p(rir::Compiler::compileClosure(BODY(closure), FORMALS(closure)));
        return compileClosure(rirClosure);
    }

    DispatchTable* tbl = DispatchTable::unpack(BODY(closure));
    auto formals = RList(FORMALS(closure));

    std::vector<SEXP> fmls;
    for (auto it = formals.begin(); it != formals.end(); ++it)
        fmls.push_back(it.tag());

    rir::Function* srcFunction = tbl->first();
    return compileClosure(srcFunction, fmls, module->getEnv(CLOENV(closure)));
}

Closure* Rir2PirCompiler::compileFunction(rir::Function* srcFunction,
                                          const std::vector<SEXP>& args) {
    return compileClosure(srcFunction, args, Env::notClosed());
}

Closure* Rir2PirCompiler::compileClosure(rir::Function* srcFunction,
                                         const std::vector<SEXP>& args,
                                         Env* closureEnv) {
    Closure* pirFunction = module->declare(srcFunction, args, closureEnv);

    Builder builder(pirFunction, closureEnv);

    {
        Rir2Pir rir2pir(*this, builder, srcFunction, srcFunction->body());
        rir2pir.translate();
    }

    assert(Verify::apply(pirFunction));

    return pirFunction;
}

Value* Rir2Pir::translate() {
    assert(!done);
    done = true;

    recoverCFG(srcCode);

    std::deque<StackMachine> worklist;

    StackMachine state(srcFunction, srcCode);

    auto popFromWorklist = [&]() {
        assert(!worklist.empty());
        state = worklist.back();
        worklist.pop_back();
        insert.bb = state.getEntry();
    };

    while (!state.atEnd() || !worklist.empty()) {
        if (state.atEnd())
            popFromWorklist();

        BC bc = state.getCurrentBC();

        if (mergepoint.count(state.getPC()) > 0) {
            StackMachine* other = &mergepoint.at(state.getPC());
            bool todo = state.doMerge(state.getPC(), insert, other);
            state = mergepoint.at(state.getPC());
            insert.next(state.getEntry());
            if (!todo) {
                if (worklist.empty()) {
                    state.clear();
                    break;
                }
                popFromWorklist();
                continue;
            }
        }

        if (bc.isJmp()) {
            auto trg = bc.jmpTarget(state.getPC());
            Opcode* fallpc = BC::next(state.getPC());
            if (bc.isUncondJmp()) {
                state.setPC(trg);
                continue;
            }

            // Conditional jump
            switch (bc.bc) {
            case Opcode::brtrue_:
            case Opcode::brfalse_: {
                Value* v = state.pop();
                insert(new Branch(v));
                break;
            }
            case Opcode::brobj_: {
                Value* v = insert(new IsObject(state.top()));
                insert(new Branch(v));
                break;
            }
            default:
                assert(false);
            }

            BB* branch = insert.createBB();
            BB* fall = insert.createBB();

            switch (bc.bc) {
            case Opcode::brtrue_:
                insert.bb->next0 = fall;
                insert.bb->next1 = branch;
                break;
            case Opcode::brfalse_:
            case Opcode::brobj_:
                insert.bb->next0 = branch;
                insert.bb->next1 = fall;
                break;
            default:
                assert(false);
            }

            switch (bc.bc) {
            case Opcode::brtrue_:
            case Opcode::brfalse_: {
                state.setPC(trg);
                state.setEntry(branch);
                worklist.push_back(state);
                break;
            }
            case Opcode::brobj_: {
                state.setPC(trg);
                state.setEntry(branch);
                insert.bb = branch;
                Value* front = state.front();
                insert(new Deopt(insert.env, state.getPC(), state.stack_size(),
                                 &front));
                break;
            }
            default:
                assert(false);
            }

            state.setPC(fallpc);
            state.setEntry(fall);
            insert.bb = fall;
            continue;
        }

        const static Matcher<4> ifFunctionLiteral(
            {{{Opcode::push_, Opcode::push_, Opcode::push_, Opcode::close_}}});

        bool matched = false;

        ifFunctionLiteral(state.getPC(), srcCode->endCode(), [&](Opcode* next) {
            Opcode* pc = state.getPC();
            BC ldfmls = BC::advance(&pc);
            BC ldcode = BC::advance(&pc);
            /* BC ldsrc = */ BC::advance(&pc);
            BC::advance(&pc); // close

            auto fmlsl = RList(ldfmls.immediateConst());
            SEXP code = ldcode.immediateConst();
            // SEXP src = ldsrc.immediateConst();

            std::vector<SEXP> fmls;
            for (auto it = fmlsl.begin(); it != fmlsl.end(); ++it) {
                fmls.push_back(it.tag());
            }

            DispatchTable* dt = DispatchTable::unpack(code);
            rir::Function* function = dt->first();

            Closure* innerF = cmp.compileFunction(function, fmls);

            state.push(insert(new MkFunCls(innerF, insert.env)));

            matched = true;
            state.setPC(next);
        });

        if (!matched) {
            int size = state.stack_size();
            state.runCurrentBC(*this, insert);
            assert(state.stack_size() == size - bc.popCount() + bc.pushCount());
            state.advancePC();
        }
    }
    assert(state.empty());

    Value* res;
    assert(results.size() > 0);
    if (results.size() == 1) {
        insert.bb = results.back().first;
        res = results.back().second;
    } else {
        BB* merge = insert.createBB();
        Phi* phi = insert(new Phi());
        for (auto r : results) {
            r.first->next0 = merge;
            phi->addInput(r.first, r.second);
        }
        phi->updateType();
        res = phi;
    }

    results.clear();
    compileReturn(res);

    // CFG cfg(insert.code->entry);

    // Remove excessive Phis
    Visitor::run(insert.code->entry, [&](BB* bb) {
        auto it = bb->begin();
        while (it != bb->end()) {
            Phi* p = Phi::Cast(*it);
            if (!p) {
                it++;
                continue;
            }
            if (p->nargs() == 1) {
                if (p == res)
                    res = p->arg(0).val();
                p->replaceUsesWith(p->arg(0).val());
                it = bb->remove(it);
                continue;
            }
            p->updateType();
            it++;
        }
    });

    InsertCast c(insert.code->entry);
    c();

    if (isVerbose()) {
        std::cout << " ========== Done compiling " << srcFunction << "\n";
        insert.function->print(std::cout);
        std::cout << " ==========\n";
    }

    return res;
}

void Rir2Pir::recoverCFG(rir::Code* srcCode) {
    std::unordered_map<Opcode*, std::vector<Opcode*>> incom;
    // Mark incoming jmps
    for (auto pc = srcCode->code(); pc != srcCode->endCode();) {
        BC bc = BC::decode(pc);
        if (bc.isJmp()) {
            incom[bc.jmpTarget(pc)].push_back(pc);
        }
        BC::advance(&pc);
    }
    // Mark falltrough to label
    for (auto pc = srcCode->code(); pc != srcCode->endCode();) {
        BC bc = BC::decode(pc);
        if (!bc.isUncondJmp()) {
            Opcode* next = BC::next(pc);
            if (incom.count(next))
                incom[next].push_back(pc);
        }
        BC::advance(&pc);
    }
    // Create mergepoints
    for (auto m : incom) {
        if (std::get<1>(m).size() > 1) {
            mergepoint.emplace(m.first,
                               StackMachine(srcFunction, srcCode, m.first));
        }
    }
}

void Rir2PirCompiler::optimizeModule() {
    size_t passnr = 0;
    bool verbose = isVerbose();

    auto print = [&](const std::string& pass, Closure* f) {
        std::cout << "============== " << pass << " == " << passnr++
                << " ======================\n";
        f->print(std::cout);
    };

    auto apply = [&](Closure* f, bool verb) {
        ForceDominance::apply(f);
        if (verb)
            print("force", f);
        ScopeResolution::apply(f);
        if (verb)
            print("scope", f);
        Cleanup::apply(f);
        if (verb)
            print("cleanup", f);
        DelayInstr::apply(f);
        if (verb)
            print("delay instr", f);
        ElideEnv::apply(f);
        if (verb)
            print("elide env", f);
        DelayEnv::apply(f);
        if (verb)
            print("delay env", f);
    };

    module->eachPirFunction([&](Module::VersionedClosure& v) {
        auto f = v.current();
        if (verbose)
            v.saveVersion();
        apply(f, verbose);
        apply(f, verbose);
    });

    for (int i = 0; i < 5; ++i) {
        module->eachPirFunction([&](Module::VersionedClosure& v) {
            auto f = v.current();
            if (verbose)
                v.saveVersion();
            Inline::apply(f);
            if (verbose)
                print("inline", f);

            apply(f, verbose);
        });
    }
}

void Rir2Pir::compileReturn(Value* res) { insert(new Return(res)); }
}
}
