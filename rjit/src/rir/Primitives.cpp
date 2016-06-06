#include "Primitives.h"
#include "CodeStream.h"
#include "../RIntlns.h"
#include "../RList.h"

#include <iostream>
#include <unordered_map>

namespace rjit {
namespace rir {

namespace {

bool compileBuiltin(CodeStream& cs, int builtin_id) {
    const std::string name = R_FunTab[builtin_id].name;

    if (name.compare("<") == 0) {
        cs << BC::lt() << BC::ret();
        return true;
    }

    if (name.compare("+") == 0) {
        cs << BC::add() << BC::ret();
        return true;
    }

    if (name.compare("-") == 0) {
        cs << BC::sub() << BC::ret();
        return true;
    }

    if (name.compare("for") == 0) {
        // TODO
    }

    std::cout << "Unknow builtin '" << R_FunTab[builtin_id].name << "'\n";

    // cs << BC::force_all() << BC::call_builtin(builtin_id);

    return false;
}

bool compileSpecial(CodeStream& cs, int special_id) {
    const std::string name = R_FunTab[special_id].name;

    if (name.compare("function") == 0) {
        cs << BC::load_arg(0) << BC::get_ast() << BC::load_arg(1)
           << BC::get_ast() << BC::mkclosure() << BC::ret();
        return true;
    }

    if (name.compare("{") == 0) {
        Label doneL = cs.mkLabel();
        Label nextL = cs.mkLabel();
        Label beginL = cs.mkLabel();

        cs << BC::numargi() << BC::pushi(0) << BC::eqi()
           << BC::jmp_false(beginL) << BC::push(R_NilValue) << BC::ret()

           << beginL

           << BC::pushi(0)

           << nextL

           << BC::dupi() << BC::inci() << BC::numargi() << BC::eqi()
           << BC::jmp_true(doneL)

           << BC::dupi() << BC::load_argi() << BC::force() << BC::pop()
           << BC::inci()

           << BC::jmp(nextL)

           << doneL

           << BC::load_argi() << BC::force();

        return true;
    }

    if (name.compare("<-") == 0) {
        // TODO check numargs
        cs << BC::load_arg(0) << BC::get_ast() << BC::load_arg(1) << BC::force()
           << BC::setvar();
        return true;
    }

    if (name.compare("substitute") == 0) {
        cs << BC::load_arg(0) << BC::get_ast();
        return true;
    }

    if (name.compare("if") == 0) {
        Label trueBranch = cs.mkLabel();
        Label falseBranch = cs.mkLabel();
        Label nextBranch = cs.mkLabel();

        cs << BC::load_arg(0) << BC::force() << BC::to_bool()
           << BC::jmp_true(trueBranch)

           << BC::numargi() << BC::pushi(3) << BC::lti()
           << BC::jmp_false(falseBranch)

           << BC::push(R_NilValue) << BC::jmp(nextBranch)

           << falseBranch << BC::load_arg(2) << BC::force()

           << BC::jmp(nextBranch) << trueBranch << BC::load_arg(1)
           << BC::force() << nextBranch;

        return true;
    }

    std::cout << "Unknow special '" << R_FunTab[special_id].name << "'\n";

    // cs << BC::call_special(special_id);
    return false;
}

} // namespace

SEXP Primitives::doCompilePrimitive(SEXP fun) {
    Function* f = new Function;
    CodeStream cs(f, fun);

    int idx = Rinternals::primoffset(fun);
    bool success = false;

    switch (TYPEOF(fun)) {
    case SPECIALSXP:
        success = compileSpecial(cs, idx);
        break;
    case BUILTINSXP:
        success = compileBuiltin(cs, idx);
        break;
    }

    SEXP cls = nullptr;

    if (success) {
        cs << BC::ret();
        cs.finalize();
        cls = mkBCCls(f, FORMALS(fun), VARIADIC_ARGS,
                      TYPEOF(fun) == BUILTINSXP ? BCClosure::CC::stackEager
                                                : BCClosure::CC::stackLazy,
                      nullptr);
        PrimitivesCache[idx] = cls;
    }

    PrimitivesCacheOccupied[idx] = true;

    return cls;
}

} // rir
} // rjit
