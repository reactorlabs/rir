#ifndef PASS_DRIVER_H
#define PASS_DRIVER_H

#include "llvm/Pass.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/BasicBlock.h"
#include "ir/Pass.h"
#include "llvm.h"

namespace rjit {
namespace ir {

template <typename aPass>
struct PassDriver : public FunctionPass {
  public:
    static char ID;

    template <typename T>
    struct RegisterMe {
        RegisterMe() : X(aPass::getPassName(), "", false, false) {
            std::cout << aPass::getPassName() << "Registered\n";
        }
        RegisterPass<T> X;
    };
    static RegisterMe<PassDriver<aPass>> Registered;

    PassDriver() : FunctionPass(ID) {}

    void getAnalysisUsage(AnalysisUsage& AU) const override {}
};

template <typename aPass>
char PassDriver<aPass>::ID = 0;

template <typename aPass>
PassDriver<aPass>::RegisterMe<PassDriver<aPass>> PassDriver<aPass>::Registered;

template <typename Pass>
class ForwardDriver : public PassDriver<Pass> {
  public:
    Pass pass;

    bool runOnFunction(Function& f) override {
        if (f.isDeclaration() || f.empty())
            return false;

        return runOnFunction_(f);
    }

    virtual bool runOnFunction_(Function& f) { return dispatch_(f); }

    virtual bool dispatch_(Function& f) {
        for (auto& b : f) {
            BasicBlock::iterator i = b.begin();
            while (i != b.end()) {
                if (!pass.dispatch(i))
                    i++;
            }
        }

        return false;
    }
};
}
}

#endif
