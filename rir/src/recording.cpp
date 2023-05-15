#include "recording.h"
#include <recording.h>
#include <sstream>
#include <string>
#include <unordered_map>
#include <utility>

extern "C" {
SEXP Rf_deparse1(SEXP, Rboolean, int);
}

namespace rir {
namespace recording {

static std::unordered_map<std::string, FunRecorder> recordings_;

std::string sexp_address(SEXP s) {
    char* caddress;
    if (asprintf(&caddress, "%p", (void*)s) == -1) {
        Rf_error("Getting address of SEXP failed");
    }

    return caddress;
}

std::string deparse_r_code(SEXP s) {
    // TODO: are the opts = 0 OK?
    auto r_str = Rf_deparse1(s, FALSE, 0);
    std::ostringstream res;

    for (auto i = 0; i < XLENGTH(r_str); i++) {
        res << CHAR(STRING_ELT(r_str, i));
        res << std::endl;
    }

    return res.str();
}

void record_compile(SEXP cls, const std::string& name) {
    auto address = sexp_address(cls);
    auto r = recordings_.insert({address, FunRecorder{}});
    auto& v = r.first->second;
    if (r.second) {
        v.name = name;
        v.r_code = deparse_r_code(cls);
    }
    v.events.push_back(CompilationEvent{});

    Rprintf("Recording %s %s\n%s\n", v.name.c_str(), address.c_str(),
            v.r_code.c_str());
}

void record_deopt(SEXP cls) {
    auto address = sexp_address(cls);
    auto r = recordings_.find(address);
    if (r == recordings_.end()) {
        return;
    }

    auto v = r->second;
    v.events.push_back(DeoptEvent{});
    Rprintf("Deopt from %s %s\n", v.name.c_str(), address.c_str());
}

} // namespace recording
} // namespace rir
