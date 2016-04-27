#include "Compiler.h"

#include "Function.h"
#include "BC.h"
#include "CodeStream.h"

#include "../Sexp.h"
#include "../RIntlns.h"
#include "../RList.h"

#include "Pool.h"

namespace rjit {
namespace rir {

namespace {

size_t compileExpression(Function& f, SEXP exp);
void compileExpression(Function& f, CodeStream& cs, SEXP exp);

void compileCall(Function& f, CodeStream& cs, SEXP ast, SEXP fun, SEXP args) {
    Match(fun) {
        Case(SYMSXP) { cs << BC1::getfun(fun); }
        Else(compileExpression(f, cs, fun));
    }

    for (auto arg : RList(args)) {
        size_t prom = compileExpression(f, arg);
        cs << BC1::mkprom(prom);
    }

    cs << BC::call();
}

void compileGetvar(CodeStream& cs, SEXP name) { cs << BC1::getvar(name); }

void compileConst(CodeStream& cs, SEXP constant) { cs << BC1::push(constant); }

void compileExpression(Function& f, CodeStream& cs, SEXP exp) {
    Match(exp) {
        Case(LANGSXP, fun, args) { compileCall(f, cs, exp, fun, args); }
        Case(SYMSXP) { compileGetvar(cs, exp); }
        Else(compileConst(cs, exp));
    }
}

size_t compileExpression(Function& f, SEXP exp) {
    CodeStream cs(f);
    compileExpression(f, cs, exp);
    return cs.finalize();
}
}

SEXP Compiler::finalize() {
    Function f;

    compileExpression(f, exp);

    size_t i = 0;
    std::cout << "===========\n";
    for (auto c : f.code) {
        std::cout << "== " << i++ << " ======\n";
        c->print();
    }
    std::cout << "===========\n";

    return R_NilValue;
}
}
}
