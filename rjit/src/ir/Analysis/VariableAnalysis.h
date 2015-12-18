#ifndef VARIABLE_ANALYSIS_H
#define VARIABLE_ANALYSIS_H

#include "ir/Handler.h"
#include "ir/Analysis.h"

#include "api.h"

#include "RIntlns.h"

#include <unordered_map>

namespace rjit {
namespace ir {

class VariableHandler : public Handler {
  public:
    JITModule* m;

    enum Type : int { Argument, Local, Parent, Any };

    std::unordered_map<SEXP, Type> locals;

    VariableHandler() : Handler() {}

    handler gv(GenericGetVar* var) {
        auto s = var->symbolValue();
        if (!locals.count(s)) {
            locals[s] = Type::Parent;
        }
    }

    handler sv(GenericSetVar* var) {
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

    handler defaultHandler(Instruction* ins) override {}

    bool dispatch(llvm::BasicBlock::iterator& i) override;
};

class VariableAnalysis : public ForwardAnalysis<VariableHandler> {
  public:
    bool runOnFunction_(Function& f) override {
        handler.m = static_cast<JITModule*>(f.getParent());
        SEXP formals = handler.m->formals(&f);
        while (formals != R_NilValue) {
            handler.locals[TAG(formals)] = VariableHandler::Type::Argument;
            formals = CDR(formals);
        }
        bool res = dispatch_(f);

        if (RJIT_DEBUG) {
            std::cout << &f << " Vars: ";
            for (auto v : handler.locals) {
                std::cout << CHAR(PRINTNAME(std::get<0>(v))) << ": ";
                switch (std::get<1>(v)) {
                case VariableHandler::Type::Argument:
                    std::cout << "Arg";
                    break;
                case VariableHandler::Type::Local:
                    std::cout << "Loc";
                    break;
                case VariableHandler::Type::Parent:
                    std::cout << "Par";
                    break;
                case VariableHandler::Type::Any:
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
