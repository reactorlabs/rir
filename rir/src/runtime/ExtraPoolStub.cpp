//
// Created by Jakob Hain on 10/9/23.
//

#include "ExtraPoolStub.h"
#include "runtime/Code.h"

namespace rir {

static const char* STUB_PREFIX = "\x02extraPoolStub_\x03";

/// From https://stackoverflow.com/a/4770992
bool isPrefix(const char* prefix, const char* str) {
    return strncmp(prefix, str, strlen(prefix)) == 0;
}

bool ExtraPoolStub::check(SEXP sexp) {
    return TYPEOF(sexp) == SYMSXP && isPrefix(STUB_PREFIX, CHAR(PRINTNAME(sexp)));
}

size_t ExtraPoolStub::unpack(SEXP sexp) {
    assert(check(sexp) && "not an extra pool stub");
    auto numStr = CHAR(PRINTNAME(sexp)) + strlen(STUB_PREFIX);
    char* endptr;
    auto num = strtol(numStr, &endptr, 10);
    assert(*endptr == '\0' &&
           "extra pool stub corrupt, has the right prefix but it's not "
           "followed by a number");
    return (size_t)num;
}

SEXP ExtraPoolStub::create(size_t index) {
    char stubName[100];
    snprintf(stubName, sizeof(stubName), "%s%zu", STUB_PREFIX, index);
    return Rf_install(stubName);
}

void ExtraPoolStub::pad(Code* codeWithPool, size_t size) {
    for (auto i = (size_t)codeWithPool->extraPoolSize; i < size; i++) {
        codeWithPool->addExtraPoolEntry(create(i));
    }
}

} // namespace rir