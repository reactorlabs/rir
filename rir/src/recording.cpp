#include "recording.h"
#include "compiler/pir/module.h"
#include "compiler/pir/pir.h"
#include <memory>
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

std::string sexp_address(const SEXP s) {
    char* caddress;
    if (asprintf(&caddress, "%p", (void*)s) == -1) {
        Rf_error("Getting address of SEXP failed");
    }

    return caddress;
}

std::string deparse_r_code(const SEXP s) {
    // TODO: are the opts = 0 OK?
    auto r_str = Rf_deparse1(s, FALSE, 0);
    std::ostringstream res;

    for (auto i = 0; i < XLENGTH(r_str); i++) {
        res << CHAR(STRING_ELT(r_str, i));
        res << std::endl;
    }

    return res.str();
}

void record_compile(const SEXP cls, const std::string& name,
                    pir::Module* module) {
    auto address = sexp_address(cls);
    auto r = recordings_.insert({address, FunRecorder{}});
    auto& v = r.first->second;

    if (r.second) {
        v.name = name;
        v.r_code = deparse_r_code(cls);
    }

    CompilationEvent event;

    module->eachPirClosureVersion(
        [&](pir::ClosureVersion* c) { event.add_pir_closure_version(c); });

    v.events.push_back(std::make_unique<CompilationEvent>(std::move(event)));

    std::cerr << "Compilation " << address << std::endl << v;
}

void record_deopt(const SEXP cls) {
    auto address = sexp_address(cls);
    auto r = recordings_.find(address);
    if (r == recordings_.end()) {
        return;
    }

    auto& v = r->second;
    DeoptEvent event;

    v.events.push_back(std::make_unique<DeoptEvent>(std::move(event)));

    std::cerr << "Deopt " << address << std::endl;
}

void CompilationEvent::add_pir_closure_version(
    const pir::ClosureVersion* version) {
    std::ostringstream code;
    version->print(code, false);
    versions[{version->name(), version->context().toI()}] = code.str();
}

void CompilationEvent::print(std::ostream& out) const {
    out << "Compilation" << std::endl;
    for (auto& e : versions) {
        out << "fun: " << e.first.first << " : " << e.first.second << std::endl;
        out << e.second << std::endl;
        out << "----" << std::endl;
    }
}

std::ostream& operator<<(std::ostream& out, const FunRecorder& fr) {
    out << "Recording of: " << fr.name << std::endl;
    out << fr.r_code << std::endl;
    out << "Log:" << std::endl;
    for (const auto& e : fr.events) {
        out << *e << std::endl;
    }
    return out;
}

std::ostream& operator<<(std::ostream& out, const Event& e) {
    e.print(out);
    return out;
}
} // namespace recording
} // namespace rir
