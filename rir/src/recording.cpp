#include "recording.h"
#include "Rdefines.h"
#include "Rinternals.h"
#include "api.h"
#include "compiler/pir/module.h"
#include "compiler/pir/pir.h"
#include "recording_serialization.h"
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

void record_compile(SEXP const cls, const std::string& name,
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

SEXP CompilationEvent::to_sexp() const {
    const char* fields[] = {"versions", ""};
    auto sexp = PROTECT(Rf_mkNamed(VECSXP, fields));
    SET_CLASS(sexp, serialization::shared_class_name_event_compile);
    auto versions_sexp = Rf_allocVector(VECSXP, (int)this->versions.size());
    SET_VECTOR_ELT(sexp, 0, versions_sexp);

    int i = 0;
    for (auto& version : this->versions) {
        const char* version_fields[] = {"name", "context", "code", ""};
        auto version_sexp = Rf_mkNamed(VECSXP, version_fields);
        SET_VECTOR_ELT(versions_sexp, i++, version_sexp);
        SET_VECTOR_ELT(version_sexp, 0,
                       serialization::to_sexp(version.first.first));
        SET_VECTOR_ELT(version_sexp, 1,
                       serialization::to_sexp(version.first.second));
        SET_VECTOR_ELT(version_sexp, 2, serialization::to_sexp(version.second));
    }

    UNPROTECT(1);
    return sexp;
}

void CompilationEvent::init_from_sexp(SEXP sexp) {
    assert(Rf_length(sexp) == 1);
    auto versions_sexp = VECTOR_ELT(sexp, 0);

    for (auto i = 0; i < Rf_length(versions_sexp); i++) {
        auto version_sexp = VECTOR_ELT(versions_sexp, i);
        assert(Rf_length(version_sexp) == 3);
        auto name_sexp = VECTOR_ELT(version_sexp, 0);
        auto context_sexp = VECTOR_ELT(version_sexp, 1);
        auto code_sexp = VECTOR_ELT(version_sexp, 2);

        std::string name = serialization::string_from_sexp(name_sexp);
        unsigned long context = serialization::uint64_t_from_sexp(context_sexp);
        std::string code = serialization::string_from_sexp(code_sexp);

        this->versions.insert({{name, context}, code});
    }
}

void CompilationEvent::print(std::ostream& out) const {
    out << "Compilation" << std::endl;
    for (auto& e : versions) {
        out << "fun: " << e.first.first << " : " << e.first.second << std::endl;
        out << e.second << std::endl;
        out << "----" << std::endl;
    }
}

SEXP DeoptEvent::to_sexp() const {
    auto sexp = PROTECT(Rf_allocVector(VECSXP, 0));
    SET_CLASS(sexp, serialization::shared_class_name_event_deopt);
    UNPROTECT(1);
    return sexp;
}

void DeoptEvent::init_from_sexp(SEXP sexp) { assert(Rf_length(sexp) == 0); }

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

size_t saveTo(FILE* file) {
    auto sexp = serialization::to_sexp(recordings_);
    PROTECT(sexp);
    R_SaveToFile(sexp, file, 3);
    UNPROTECT(1);
    return recordings_.size();
}

size_t replayFrom(FILE* file) {
    SEXP sexp = R_LoadFromFile(file, 3);
    assert(Rf_isVector(sexp));
    SEXP names = Rf_getAttrib(sexp, R_NamesSymbol);
    assert(Rf_isVector(names));

    decltype(recordings_) new_recordings;

    for (auto i = 0; i < Rf_length(sexp); i++) {
        auto recorder_name_sexp = STRING_ELT(names, i);
        auto recorder_sexp = VECTOR_ELT(sexp, i);
        FunRecorder recorder =
            serialization::fun_recorder_from_sexp(recorder_sexp);
        std::string recorder_name =
            serialization::string_from_sexp(recorder_name_sexp);
        new_recordings.insert({std::move(recorder_name), std::move(recorder)});
    }

    return new_recordings.size();
}

} // namespace recording
} // namespace rir
