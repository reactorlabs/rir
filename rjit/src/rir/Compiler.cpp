#include "Compiler.h"

#include "Function.h"
#include "BC.h"
#include "CodeStream.h"

#include "../Sexp.h"
#include "../RIntlns.h"

#include "Pool.h"

namespace rjit {
namespace rir {

namespace {

void call(CodeStream& cs, SEXP ast, SEXP fun, SEXP args) { assert(false); }

void getvar(CodeStream& cs, SEXP name) { assert(false); }

void loadConst(CodeStream& cs, SEXP constant) { cs << BC::push << constant; }
}

SEXP Compiler::finalize() {
    CodeStream cs;
    Function f;

    cs << BC::push << exp;

    if (false) {
        Match(exp) {
            Case(LANGSXP, fun, args) { call(cs, exp, fun, args); }
            Case(SYMSXP, name) { getvar(cs, name); }
            Else(loadConst(cs, exp));
        }
    }

    auto code = cs.to_code();
    f.setFun(code);
    f.getFun()->print();
    return R_NilValue;
}
}
}
