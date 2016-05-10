#include "Primitives.h"
#include "CodeStream.h"

namespace rjit {
namespace rir {

namespace {

void compileSpecial(CodeStream& cs, long special_id, num_args_t nargs) {
    if (special_id == Primitives::do_begin_id) {
        if (nargs == 0) {
            cs << BC::push(R_NilValue);
            return;
        }

        for (num_args_t i = 0; i < nargs; ++i) {
            cs << BC::load_arg(i) << BC::force();
            // leave the last arg on the stack to return
            if (i != nargs - 1)
                cs << BC::pop();
        }
        return;
    }

    if (special_id == Primitives::do_set_id) {
        cs << BC::check_numarg(2) << BC::load_arg(0) << BC::get_ast()
           << BC::load_arg(1) << BC::force() << BC::setvar();
        return;
    }

    if (special_id == Primitives::do_substitute_id) {
        cs << BC::check_numarg(1) << BC::load_arg(0) << BC::get_ast();
        return;
    }

    if (special_id == Primitives::do_if_id) {
        Label trueBranch = cs.mkLabel();
        Label nextBranch = cs.mkLabel();

        cs << BC::load_arg(0) << BC::force() << BC::to_bool()
           << BC::jmp_true(trueBranch) << BC::load_arg(2) << BC::force()
           << BC::jmp(nextBranch) << trueBranch << BC::load_arg(1)
           << BC::force() << nextBranch;

        return;
    }

    assert(false);
}

} // namespace

BCClosure* Primitives::compilePrimitive(SEXP fun, num_args_t nargs) {
    Function* f = new Function;
    CodeStream cs(*f);

    switch (TYPEOF(fun)) {
    case SPECIALSXP: {
        long idx = (long)CAR(fun);
        compileSpecial(cs, idx, nargs);
        break;
    }
    default:
        assert(false);
    }

    cs << BC::ret();
    cs.finalize(fun);
    BCClosure* cls = new BCClosure;
    cls->env = CLOENV(fun);
    cls->fun = f;
    cls->formals = FORMALS(fun);
    return cls;
}

} // rir
} // rjit
