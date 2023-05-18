#include "recording.h"
#include "Rinternals.h"
#include "api.h"
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

void record_compile(SEXP const cls, const std::string& name,
                    pir::Module* module, const Context& assumptions) {
    auto address = sexp_address(cls);
    auto r = recordings_.insert({address, FunRecorder{}});
    auto& v = r.first->second;

    if (r.second) {
        v.name = name;
        v.r_code = deparse_r_code(cls);
    }

    CompilationEvent event;
    event.assumptions = assumptions;

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

std::unique_ptr<Event> Event::read_any(char* line) {
    char* first_comma = strchr(line, ',');
    *first_comma = 0;

    char* event_name = line;
    std::unique_ptr<Event> event;
    if (strcmp(event_name, "compile") == 0) {
        event = std::make_unique<CompilationEvent>();
    } else if (strcmp(event_name, "deopt") == 0) {
        event = std::make_unique<DeoptEvent>();
    } else {
        std::abort();
    }

    event->read(first_comma + 1);
    return event;
}

void CompilationEvent::add_pir_closure_version(
    const pir::ClosureVersion* version) {
    std::ostringstream code;
    version->print(code, false);
    versions[{version->name(), version->context().toI()}] = code.str();
}

void CompilationEvent::write(FILE* file) const {
    fprintf(file, "compile,%ld", this->assumptions.toI());
}

void CompilationEvent::read(char* args) {
    this->assumptions = Context(atoll(args));
}

void CompilationEvent::replay(SEXP cls, char* cls_name) const {
    rirCompile(cls, R_GlobalEnv);
    pirCompile(cls, this->assumptions, cls_name,
               pir::DebugOptions::DefaultDebugOptions);
}

void CompilationEvent::print(std::ostream& out) const {
    out << "Compilation" << std::endl;
    for (auto& e : versions) {
        out << "fun: " << e.first.first << " : " << e.first.second << std::endl;
        out << e.second << std::endl;
        out << "----" << std::endl;
    }
}

void DeoptEvent::write(FILE* file) const { fprintf(file, "deopt,"); }

void DeoptEvent::read(char* file) {}

void DeoptEvent::replay(SEXP cls, char* cls_name) const {
    // TODO
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

size_t saveTo(FILE* file) {
    for (auto& kv : recordings_) {
        for (auto& ev : kv.second.events) {
            fprintf(file, "%s,", kv.second.name.c_str());
            ev->write(file);
            fprintf(file, "\n");
        }
    }

    return recordings_.size();
}

size_t replayFrom(FILE* file) {
    char* line = nullptr;
    size_t len;
    unsigned total_events = 0;
    while (getline(&line, &len, file) != -1) {
        char* first_comma = strchr(line, ',');
        *first_comma = 0;
        auto symbol = Rf_install(line);
        auto event = Event::read_any(first_comma + 1);
        auto cls = PROTECT(Rf_findFun(symbol, R_GlobalEnv));
        assert(cls);
        event->replay(cls, line);
        free(line);
        line = nullptr;
        UNPROTECT(1);
        total_events++;
    }
    return total_events;
}

} // namespace recording
} // namespace rir
