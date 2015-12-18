#ifndef HANDLER_PASS_WRAPPER_H
#define HANDLER_PASS_WRAPPER_H

#include "llvm/Pass.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/BasicBlock.h"
#include "ir/Handler.h"
#include "llvm.h"

namespace rjit {
namespace ir {

template <typename Handler>
struct HandlerPassWrapper : public FunctionPass {
  public:
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

    void getAnalysisUsage(AnalysisUsage& AU) const override {}
};

template <typename Handler>
char HandlerPassWrapper<Handler>::ID = 0;

template <typename Handler>
HandlerPassWrapper<Handler>::RegisterMe<HandlerPassWrapper<Handler>>
    HandlerPassWrapper<Handler>::Registered;
}
}

#endif
