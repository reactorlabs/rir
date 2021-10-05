#include "errors.h"
#include "interpreter/instance.h"

namespace rir {

void Errors::makeCall(const char* msg, Signature signature) {
// TODO: localization
#ifndef _
#define _(x) x
#endif

    switch (signature) {
    case Signature::NoArgs: {
        Rf_error(_(msg));
        break;
    }
    case Signature::Str: {
        auto arg = ostack_at(globalContext(), 0);
        assert(TYPEOF(arg) == CHARSXP);
        Rf_error(_(msg), CHAR(arg));
        break;
    }
    case Signature::Int: {
        auto arg = ostack_at(globalContext(), 0);
        assert(TYPEOF(arg) == INTSXP);
        Rf_error(_(msg), INTEGER(arg)[0]);
        break;
    }
    case Signature::IntIntInt: {
        auto arg1 = ostack_at(globalContext(), 2);
        assert(TYPEOF(arg1) == INTSXP);
        auto arg2 = ostack_at(globalContext(), 1);
        assert(TYPEOF(arg2) == INTSXP);
        auto arg3 = ostack_at(globalContext(), 0);
        assert(TYPEOF(arg3) == INTSXP);
        Rf_error(_(msg), INTEGER(arg1)[0], INTEGER(arg2)[0], INTEGER(arg3)[0]);
        break;
    }
    case Signature::StrIntStr: {
        auto arg1 = ostack_at(globalContext(), 2);
        assert(TYPEOF(arg1) == CHARSXP);
        auto arg2 = ostack_at(globalContext(), 1);
        assert(TYPEOF(arg2) == INTSXP);
        auto arg3 = ostack_at(globalContext(), 0);
        assert(TYPEOF(arg3) == CHARSXP);
        Rf_error(_(msg), CHAR(arg1), INTEGER(arg2)[0], CHAR(arg3));
        break;
    }
    }
}

} // namespace rir
