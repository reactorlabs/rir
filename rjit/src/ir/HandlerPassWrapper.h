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

template <typename Handler> struct HandlerPassWrapper : public FunctionPass {
    static char ID;

    template <typename T> struct RegisterMe {
        RegisterMe() : X(Handler::getPassName(), "", false, false) {
            std::cout << Handler::getPassName() << "Registered\n";
        }
        RegisterPass<T> X;
    };
    static RegisterMe<HandlerPassWrapper<Handler>> Registered;

    HandlerPassWrapper() = delete;
    HandlerPassWrapper(Builder& builder)
        : FunctionPass(ID), builder(builder), handler(builder) {}

    bool runOnFunction(Function& F) override {
        for (llvm::Function& f : builder.module()->getFunctionList()) {
            if (f.isDeclaration() || f.empty())
                continue;

            for (auto& b : f) {
                BasicBlock::iterator i = b.begin();
                handler.dispatch(i);
            }
        }

        return false;
    };

    void getAnalysisUsage(AnalysisUsage& AU) const override {}

    Builder& builder;
    Handler handler;
};

template <typename Handler> char HandlerPassWrapper<Handler>::ID = 0;

template <typename Handler>
HandlerPassWrapper<Handler>::RegisterMe<HandlerPassWrapper<Handler>>
    HandlerPassWrapper<Handler>::Registered;
}
}

template <typename Handler>
FunctionPass* createHandlerPassWrapper(ir::Builder& b) {
    return new ir::HandlerPassWrapper<Handler>(b);
}
}

#endif
