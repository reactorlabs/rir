#ifndef VARIABLE_ANALYSIS_H
#define VARIABLE_ANALYSIS_H

#include "ir/Pass.h"
#include "ir/PassDriver.h"

#include "api.h"

#include "RIntlns.h"

#include <unordered_map>

namespace rjit {
namespace ir {

class VariablePass : public Pass {
  public:
    JITModule* m;
    enum Type : int { Argument, Local, Parent, Any };

    std::unordered_map<SEXP, Type> locals;

    VariablePass() : Pass() {}

    match gv(GenericGetVar* var) {
        auto s = var->symbolValue();
        if (!locals.count(s)) {
            locals[s] = Type::Parent;
        }
    }

    match sv(GenericSetVar* var) {
        auto s = var->symbolValue();
        if (!locals.count(s)) {
            locals[s] = Type::Local;
        } else {
            auto cur = locals[s];
            if (cur == Parent) {
                locals[s] = Type::Any;
            }
        }
    }

    bool dispatch(llvm::BasicBlock::iterator& i) override;
};

class VariableAnalysis : public ForwardDriver<VariablePass> {
  public:
    bool runOnFunction_(Function& f) override {
        pass.m = static_cast<JITModule*>(f.getParent());
        SEXP formals = pass.m->formals(&f);
        while (formals != R_NilValue) {
            pass.locals[TAG(formals)] = VariablePass::Type::Argument;
            formals = CDR(formals);
        }
        bool res = dispatch_(f);

        if (RJIT_DEBUG) {
            std::cout << &f << " Vars: ";
            for (auto v : pass.locals) {
                std::cout << CHAR(PRINTNAME(std::get<0>(v))) << ": ";
                switch (std::get<1>(v)) {
                case VariablePass::Type::Argument:
                    std::cout << "Arg";
                    break;
                case VariablePass::Type::Local:
                    std::cout << "Loc";
                    break;
                case VariablePass::Type::Parent:
                    std::cout << "Par";
                    break;
                case VariablePass::Type::Any:
                    std::cout << "Any";
                    break;
                }
                std::cout << ", ";
            }
            std::cout << "\n";
        }

        return res;
    }
};
}
}

#endif
