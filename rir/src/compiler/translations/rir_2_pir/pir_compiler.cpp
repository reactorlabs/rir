#include "pir_compiler.h"
#include "../../pir/pir_impl.h"
#include "R/RList.h"
#include "rir_2_pir.h"

#include "../../analysis/query.h"
#include "../../analysis/verifier.h"
#include "../../opt/cleanup.h"
#include "../../opt/delay_env.h"
#include "../../opt/delay_instr.h"
#include "../../opt/elide_env.h"
#include "../../opt/force_dominance.h"
#include "../../opt/inline.h"
#include "../../opt/scope_resolution.h"
#include "ir/BC.h"

namespace rir {
namespace pir {

PirCompiler::PirCompiler(Module* module) : RirCompiler(module) {
    compileToOptimizingIR = new Rir2Pir(*this);
    optimizations.push_back(new ForceDominance(*this));
    optimizations.push_back(new ScopeResolution(*this));
    optimizations.push_back(new Cleanup(*this));
    optimizations.push_back(new DelayInstr(*this));
    optimizations.push_back(new ElideEnv(*this));
    optimizations.push_back(new DelayEnv(*this));
}

RirInput PirCompiler::createRirInputFromSEXP(SEXP closure,
                                             std::vector<SEXP>& fmls,
                                             Env* env) {
    assert(isValidClosureSEXP(closure));
    DispatchTable* tbl = DispatchTable::unpack(BODY(closure));
    auto formals = RList(FORMALS(closure));

    for (auto it = formals.begin(); it != formals.end(); ++it)
        fmls.push_back(it.tag());

    rir::Function* srcFunction = tbl->first();
    return RirInput(srcFunction, &fmls, env);
    // return RirInput(srcFunction, &fmls, module->getEnv(CLOENV(closure)));
    // compileClosure(srcFunction, fmls, module->getEnv(CLOENV(closure)));
}

RirInput PirCompiler::createRirInputFromFunction(rir::Function* srcFunction,
                                                 std::vector<SEXP>& args) {
    return RirInput(srcFunction, &args, Env::notClosed());
}

IRCode PirCompiler::compile(IRCode sourceIR) {
    RirInput* rirSource = sourceIR.getRirInputFormat();
    Closure* pirFunction =
        module->getOrCreate(rirSource->function, *rirSource->args,
                            rirSource->env, [&](Closure* pirFunction) {});
    Builder builder(pirFunction, rirSource->env);
    this->setBuilder(&builder);
    if (isVerbose()) {
        std::cout << " ========== Compiling Rir Function: "
                  << rirSource->function << "\n";
    }
    this->compileToOptimizingIR->apply(sourceIR);

    assert(Verify::apply(pirFunction));

    if (isVerbose()) {
        std::cout << " ========== Pir Function Generated: "
                  << "\n";
        pirFunction->print(std::cout);
        std::cout << " ==========\n";
    }

    if (this->optimizationsEnabled()) {
        this->optimize(rirSource->function, pirFunction);
    }
    // Decompilation still here
    sourceIR.pirInput = pirFunction;
    return sourceIR;
}

void PirCompiler::optimize(rir::Function* rirFuntion, Closure* closure) {
    if (isVerbose())
        this->getModule()->getVersioned(rirFuntion)->saveVersion();
    applyOptimizations(closure, "Optimizations 1st Pass");
    applyOptimizations(closure, "Optimizations 2nd Pass");
}

void PirCompiler::optimizeModule() {
    size_t passnr = 0;
    bool verbose = isVerbose();

    for (int i = 0; i < 6; ++i) {
        module->eachPirFunction([&](Module::VersionedClosure& v) {
            auto f = v.current();
            if (verbose)
                v.saveVersion();
            Inline::apply(f);
            if (verbose)
                printAfterPass("inline", "Inlining", f, passnr++);
            applyOptimizations(f, "Optimizations After Inlining");
        });
    }
}

void PirCompiler::printAfterPass(const std::string& pass,
                                 const std::string& category, Closure* f,
                                 size_t passnr) {
    std::cout << "============== " << category << ": " << pass
              << " == " << passnr << " ======================\n";
    f->print(std::cout);
}

void PirCompiler::applyOptimizations(Closure* f, const std::string& category) {
    size_t passnr = 0;
    for (auto translation : this->optimizations) {
        IRCode input = {.pirInput = f};
        translation->apply(input);
        if (isVerbose())
            printAfterPass(translation->getName(), category, f, passnr++);
    }
}

} // namespace pir
} // namespace rir