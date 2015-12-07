#ifndef HANDLER_PASS_WRAPPER_H
#define HANDLER_PASS_WRAPPER_H

#include "llvm/Pass.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/BasicBlock.h"
#include "ir/Handler.h"
#include "llvm.h"

namespace rjit {
namespace ir {

namespace {

template <typename Handler>
struct HandlerPassWrapper : public FunctionPass {
    static char ID;

    template <typename T>
    struct RegisterMe {
        RegisterMe() : X(Handler::getPassName(), "", false, false) {
            std::cout << Handler::getPassName() << "Registered\n";
        }
        RegisterPass<T> X;
    };
    static RegisterMe<HandlerPassWrapper<Handler>> Registered;

    HandlerPassWrapper() : FunctionPass(ID) {}

    bool runOnFunction(Function& F) override {
        llvm::Module* m = F.getParent();
        for (llvm::Function& f : m->getFunctionList()) {
            if (f.isDeclaration() || f.empty())
                continue;

            for (auto& b : f) {
                BasicBlock::iterator i = b.begin();
                while (i != b.end()) {
                    handler.dispatch(i);
                }
            }
        }

        return false;
    };

    void getAnalysisUsage(AnalysisUsage& AU) const override {}

    Handler handler;
};

template <typename Handler>
char HandlerPassWrapper<Handler>::ID = 0;

template <typename Handler>
HandlerPassWrapper<Handler>::RegisterMe<HandlerPassWrapper<Handler>>
    HandlerPassWrapper<Handler>::Registered;
}
}

template <typename Handler>
FunctionPass* createHandlerPassWrapper() {
    return new ir::HandlerPassWrapper<Handler>();
}
}

#endif
