#include "rir_2_pir.h"
#include "R/RList.h"
#include "../analysis/query.h"
#include "../analysis/verifier.h"
#include "ir/BC.h"
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

#include <deque>
#include <vector>

namespace {

using namespace rir::pir;
typedef rir::pir::Function Function;
typedef rir::Opcode Opcode;
typedef rir::BC BC;
typedef rir::RList RList;

typedef std::pair<BB*, Value*> ReturnSite;

}

namespace rir {

pir::IRTransformation* Rir2Pir::declare(SEXP& function) {
    assert(isValidClosureSEXP(function));
    DispatchTable* tbl = DispatchTable::unpack(BODY(function));
    auto formals = RList(FORMALS(function));

    std::vector<SEXP> fml;
    for (auto it = formals.begin(); it != formals.end(); ++it) {
        fml.push_back(it.tag());
    }
    pir::Function* pirFunction = new pir::Function(fml);
    
    return
        new IRTransformation(tbl->first(), tbl->first()->body(), pirFunction);
}

pir::IRTransformation* Rir2Pir::declare(Function* rirFunction) {
    return Rir2Pir::declare(rirFunction, rirFunction->body());
}

pir::IRTransformation* Rir2Pir::declare(Function* rirFunction, rir::Code* srcCode) {
    // auto formals = RList(FORMALS(rirFunction->container()));

    std::vector<SEXP> fml;
    /*for (auto it = formals.begin(); it != formals.end(); ++it) {
        fml.push_back(it.tag());
    }*/
    pir::Function* pirFunction = new pir::Function(fml);
    return new IRTransformation(rirFunction, srcCode, pirFunction);
}

pir::Function* Rir2Pir::compileFunction(SEXP function2Compile) {
    pir::IRTransformation* rir2PirTranslation = declare(function2Compile);
    return this->compileFunction(rir2PirTranslation);
}

pir::Function* Rir2Pir::compileFunction(Function* function2Compile){
    //auto formals = RList(FORMALS(function2Compile->body()));

    std::vector<SEXP> fmls;
    /*for (auto it = formals.begin(); it != formals.end(); ++it) {
        fml.push_back(it.tag());
    }*/
    return this->compileFunction(function2Compile, fmls);
}

pir::Function* Rir2Pir::compileFunction(Function* function2Compile,
                                        std::vector<SEXP> fmls) {
    pir::Function* pirFunction = new pir::Function(fmls);
    IRTransformation* rir2Pir =
        new IRTransformation(function2Compile, pirFunction);
    return this->compileFunction(rir2Pir);
}

pir::Function* Rir2Pir::compileFunction(IRTransformation* rir2PirTransformation) {
    this->module->functions.push_back(rir2PirTransformation);
    this->recoverCFG(rir2PirTransformation);
    
    pir::Function* pirFunction = rir2PirTransformation->dstFunction;

    rir::Code* srcCode = rir2PirTransformation->srcCode;
    std::deque<StackMachine> worklist;
    if (builder == nullptr)
        this->builder = new Builder(pirFunction, Env::theContext());

    pir::Builder* builder = this->getBuilder();

    state.setPC(srcCode->code());
    Opcode* end = srcCode->endCode();

    std::vector<ReturnSite> results;

    while (state.getPC() != end || !worklist.empty()) {
        if (state.getPC() == end)
            popFromWorklist(&worklist, builder);

        BC bc = state.getCurrentBC();

        if (mergepoint.count(state.getPC()) > 0) {
            StackMachine* other = &mergepoint.at(state.getPC());
            bool todo = state.doMerge(state.getPC(), builder, other);
            state = mergepoint.at(state.getPC());
            builder->next(state.getEntry());
            if (!todo) {
                if (worklist.empty()) {
                    state.clear();
                    break;
                }
                popFromWorklist(&worklist, builder);
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
                (*builder)(new Branch(v));
                break;
            }
            case Opcode::brobj_: {
                Value* v = (*builder)(new IsObject(state.top()));
                (*builder)(new Branch(v));
                break;
            }
            default:
                assert(false);
            }

            BB* branch = builder->createBB();
            BB* fall = builder->createBB();

            switch (bc.bc) {
            case Opcode::brtrue_:
                builder->bb->next0 = fall;
                builder->bb->next1 = branch;
                break;
            case Opcode::brfalse_:
            case Opcode::brobj_:
                builder->bb->next0 = branch;
                builder->bb->next1 = fall;
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
                builder->bb = branch;
                Value* front = state.front();
                (*builder)(new Deopt(builder->env, state.getPC(),
                                     state.stack_size(), &front));
                break;
            }
            default:
                assert(false);
            }

            state.setPC(fallpc);
            state.setEntry(fall);
            builder->bb = fall;
            continue;
        }

        const static Matcher<4> ifFunctionLiteral(
            {{{Opcode::push_, Opcode::push_, Opcode::push_, Opcode::close_}}});

        bool matched = false;

        ifFunctionLiteral(state.getPC(), end, [&](Opcode* next) {
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

            Rir2Pir compiler;
            pir::Function* innerF = compiler.compileFunction(function);

            state.push((*builder)(new MkFunCls(innerF, builder->env)));

            matched = true;
            state.setPC(next);
        });

        if (!matched) {
            int size = state.stack_size();
            state.runCurrentBC(builder, rir2PirTransformation->srcFunction, &results);
            assert(state.stack_size() == size - bc.popCount() + bc.pushCount());
        }
    }
    assert(state.empty());

    Value* res;
    assert(results.size() > 0);
    if (results.size() == 1) {
        builder->bb = std::get<0>(results.back());
        res = std::get<1>(results.back());
    } else {
        BB* merge = builder->createBB();
        Phi* phi = (*builder)(new Phi());
        for (auto r : results) {
            r.first->next0 = merge;
            phi->addInput(r.first, r.second);
        }
        phi->updateType();
        res = phi;
    }

    results.clear();
    this->addReturn(res);

    CFG cfg(builder->code->entry);

    // Remove excessive Phis
    Visitor::run(builder->code->entry, [&](BB* bb) {
        auto it = bb->begin();
        while (it != bb->end()) {
            Phi* p = Phi::Cast(*it);
            if (!p) {
                it++;
                continue;
            }
            if (p->nargs() == 1) {
                if (p == res)
                    res = p->arg(0);
                p->replaceUsesWith(p->arg(0));
                it = bb->remove(it);
                continue;
            }
            p->updateType();
            it++;
        }
    });
    
    /*
    What is this?
    InsertCast c(builder.code->entry);
    c();
    */

    this->optimizeFunction(pirFunction);
    if (Verify::apply(pirFunction))
        return pirFunction;
    return rir2PirTransformation->dstFunction;
}

void Rir2Pir::recoverCFG(IRTransformation* rir2PirTransformation) {
    std::unordered_map<Opcode*, std::vector<Opcode*>> incom;
    // Mark incoming jmps
    rir::Code* srcCode = rir2PirTransformation->srcCode;
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
            mergepoint[std::get<0>(m)] = StackMachine();
        }
    }
}

void Rir2Pir::optimizeFunction(pir::Function* function){
    size_t passnr = 0;
    bool verbose = getVerbose();

    auto print =     [&](const std::string& pass, pir::Function* f) {
        std::cout << "============== " << pass << " == " << passnr++
                << " ======================\n";
        f->print(std::cout);
    };

    auto apply = [&](pir::Function* f, bool verb) {
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

    if (verbose)
        this->module->print(std::cout);

    apply(function, false);
    apply(function, false);

    if (verbose) {
        std::cout << "============== whole module passes ======================\n";
        this->module->print(std::cout);
    }

    IRTransformation* moduleFunction = *this->module->functions.begin();
    for (size_t iter = 0; iter < 5; ++iter) {
        Inline::apply(moduleFunction->dstFunction);
        if (verbose)
            print("inline", moduleFunction->dstFunction);

        apply(moduleFunction->dstFunction, verbose);
    }
}

Module* Rir2Pir::compileModule(SEXP f) {
    for (auto f : this->module->functions) {
        this->compileFunction(f);
    }
    return this->module;
} 

void Rir2Pir::popFromWorklist(std::deque<StackMachine>* worklist, Builder* builder){
    assert(!worklist->empty());
    state = worklist->back();
    worklist->pop_back();
    builder->bb = state.getEntry();
}

void Rir2Pir::addReturn(Value* res) { (*(this->getBuilder()))(new Return(res)); }

pir::Builder* Rir2Pir::getBuilder() {  
    return builder;    
}
}
