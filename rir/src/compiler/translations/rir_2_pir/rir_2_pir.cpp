#include "rir_2_pir.h"
#include "../../analysis/query.h"
#include "../../analysis/verifier.h"
#include "../../pir/pir_impl.h"
#include "../../transform/insert_cast.h"
#include "../../util/builder.h"
#include "../../util/cfg.h"
#include "../../util/visitor.h"
#include "R/RList.h"
#include "ir/BC.h"
#include "pir_compiler.h"

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
} // namespace

namespace rir {
namespace pir {

void Rir2Pir::apply(IRCode input) {
    rir::Function* srcFunction = input.getRirInputFormat()->function;
    translate(srcFunction, srcFunction->body());
    // input.pirClosure
}

Value* Rir2Pir::translate(rir::Function* srcFunction, rir::Code* srcCode) {
    assert(!done);
    done = true;

    Builder* insert = cmp.getBuilder();

    recoverCFG(srcFunction, srcCode);

    std::deque<StackMachine> worklist;
    StackMachine state(srcFunction, srcCode);

    auto popFromWorklist = [&]() {
        assert(!worklist.empty());
        state = worklist.back();
        worklist.pop_back();
        insert->bb = state.getEntry();
    };

    while (!state.atEnd() || !worklist.empty()) {
        if (state.atEnd())
            popFromWorklist();

        BC bc = state.getCurrentBC();

        if (mergepoint.count(state.getPC()) > 0) {
            StackMachine* other = &mergepoint.at(state.getPC());
            bool todo = state.doMerge(state.getPC(), *insert, other);
            state = mergepoint.at(state.getPC());
            insert->next(state.getEntry());
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
                (*insert)(new Branch(v));
                break;
            }
            case Opcode::brobj_: {
                Value* v = (*insert)(new IsObject(state.top()));
                (*insert)(new Branch(v));
                break;
            }
            default:
                assert(false);
            }

            BB* branch = insert->createBB();
            BB* fall = insert->createBB();

            switch (bc.bc) {
            case Opcode::brtrue_:
                insert->bb->next0 = fall;
                insert->bb->next1 = branch;
                break;
            case Opcode::brfalse_:
            case Opcode::brobj_:
                insert->bb->next0 = branch;
                insert->bb->next1 = fall;
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
                insert->bb = branch;
                Value* front = state.front();
                (*insert)(new Deopt(insert->env, state.getPC(),
                                    state.stack_size(), &front));
                break;
            }
            default:
                assert(false);
            }

            state.setPC(fallpc);
            state.setEntry(fall);
            insert->bb = fall;
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

            RirInput input =
                PirCompiler::createRirInputFromFunction(function, fmls);
            IRCode entry{.rirInput = &input};
            PirCompiler anotherCompiler(cmp.getModule());
            anotherCompiler.setVerbose(true);
            Closure* innerF =
                (anotherCompiler.compile(entry)).getPirInputFormat();

            state.push((*insert)(new MkFunCls(innerF, insert->env)));

            matched = true;
            state.setPC(next);
        });

        if (!matched) {
            int size = state.stack_size();
            state.runCurrentBC(*this, *insert);
            assert(state.stack_size() == size - bc.popCount() + bc.pushCount());
            state.advancePC();
        }
    }
    assert(state.empty());

    Value* res;
    assert(results.size() > 0);
    if (results.size() == 1) {
        insert->bb = results.back().first;
        res = results.back().second;
    } else {
        BB* merge = insert->createBB();
        Phi* phi = (*insert)(new Phi());
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
    Visitor::run(insert->code->entry, [&](BB* bb) {
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

    InsertCast c((*insert).code->entry);
    c();

    return res;
}

void Rir2Pir::recoverCFG(rir::Function* srcFunction, rir::Code* srcCode) {
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

void Rir2Pir::compileReturn(Value* res) {
    (*cmp.getBuilder())(new Return(res));
}
} // namespace pir
} // namespace rir
