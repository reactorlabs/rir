#include "Compiler.h"

#include "Function.h"
#include "BC.h"
#include "CodeStream.h"

#include "../Sexp.h"
#include "../RIntlns.h"
#include "../RList.h"

#include "Pool.h"
#include "Interpreter.h"

namespace rjit {
namespace rir {

namespace {

fun_idx_t compileExpression(Function& f, SEXP exp);
void compileExpression(Function& f, CodeStream& cs, SEXP exp);

void compileCall(Function& f, CodeStream& cs, SEXP ast, SEXP fun, SEXP args) {
    Match(fun) {
        Case(SYMSXP) { cs << BC::getfun(fun); }
        Else(compileExpression(f, cs, fun));
    }

    size_t numArgs = 0;
    RVector names;
    bool hasNames = false;
    for (auto arg = RList(args).begin(); arg != RList::end(); ++arg) {
        numArgs++;
        size_t prom = compileExpression(f, *arg);
        if (arg.hasTag()) {
            hasNames = true;
            names.append(arg.tag());
        } else {
            names.append(R_NilValue);
        }
        cs << BC::mkprom(prom);
    }
    assert(numArgs < MAX_NUM_ARGS);

    if (hasNames) {
        cs << BC::call_name(names);
    } else {
        cs << BC::call(numArgs);
    }
}

void compileGetvar(CodeStream& cs, SEXP name) { cs << BC::getvar(name); }

void compileConst(CodeStream& cs, SEXP constant) { cs << BC::push(constant); }

void compileExpression(Function& f, CodeStream& cs, SEXP exp) {
    Match(exp) {
        Case(LANGSXP, fun, args) { compileCall(f, cs, exp, fun, args); }
        Case(SYMSXP) { compileGetvar(cs, exp); }
        Else(compileConst(cs, exp));
    }
}

fun_idx_t compileExpression(Function& f, SEXP exp) {
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

    Interpreter interp(f);
    return interp.run();
}
}
}
