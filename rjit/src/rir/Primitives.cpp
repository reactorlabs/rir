#include "Primitives.h"
#include "CodeStream.h"
#include "RIntlns.h"

#include <iostream>

namespace rjit {
namespace rir {

namespace {

void compileBuiltin(CodeStream& cs, int builtin_id, num_args_t nargs) {
    const std::string name = R_FunTab[builtin_id].name;

    if (name.compare("for") == 0) {
        // TODO
    }

    std::cout << "Unknow builtin '" << R_FunTab[builtin_id].name << "'\n";

    cs << BC::force_all() << BC::call_builtin(builtin_id);
}

void compileSpecial(CodeStream& cs, int special_id, num_args_t nargs) {
    const std::string name = R_FunTab[special_id].name;

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

        return;
    }

    if (name.compare("<-") == 0) {
        // TODO check numargs
        cs << BC::load_arg(0) << BC::get_ast() << BC::load_arg(1) << BC::force()
           << BC::setvar();
        return;
    }

    if (name.compare("substitute") == 0) {
        cs << BC::load_arg(0) << BC::get_ast();
        return;
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

        return;
    }

    std::cout << "Unknow special '" << R_FunTab[special_id].name << "'\n";

    cs << BC::call_special(special_id);
}

static std::map<unsigned, BCClosure*> PrimitivesCache;

} // namespace

BCClosure* Primitives::compilePrimitive(SEXP fun, num_args_t nargs) {
    int idx;

    switch (TYPEOF(fun)) {
    case SPECIALSXP:
    case BUILTINSXP:
        idx = fun->u.primsxp.offset;
        if (PrimitivesCache.count(idx))
            return PrimitivesCache.at(idx);
        break;
    default:
        assert(false);
    }

    Function* f = new Function;
    CodeStream cs(*f, fun);

    switch (TYPEOF(fun)) {
    case SPECIALSXP:
        compileSpecial(cs, idx, nargs);
        break;
    case BUILTINSXP:
        compileBuiltin(cs, idx, nargs);
        break;
    }

    cs << BC::ret();
    cs.finalize(fun);
    BCClosure* cls = new BCClosure;
    cls->env = nullptr;
    cls->fun = f;
    cls->formals = FORMALS(fun);

    PrimitivesCache[idx] = cls;

    return cls;
}

} // rir
} // rjit
