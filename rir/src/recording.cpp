#include "recording.h"
#include "R/Serialize.h"
#include "compiler/pir/module.h"
#include "compiler/pir/pir.h"
#include "runtime/Context.h"
#include "runtime/DispatchTable.h"
#include "runtime/Function.h"
#include "utils/Pool.h"
#include <cstdio>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <memory>
#include <ostream>
#include <sstream>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

extern "C" {
SEXP Rf_deparse1(SEXP, Rboolean, int);
}

namespace rir {
namespace recording {

const char* get_env(const char* name, const char* default_value) {
    const char* value = getenv(name);
    return value ? value : default_value;
}

static std::unordered_map<std::string, FunRecorder> recordings_;
static bool hook_registered_ = false;
static bool hook_ran_ = false;
static bool recording_ = std::getenv("RSH_REC") != NULL;
static const char* recording_output_ = get_env("RSH_REC", "recordings.rds");

static void exit_hook() {
    if (hook_ran_) {
        return;
    }
    hook_ran_ = true;

    SEXP result = PROTECT(Rf_allocVector(VECSXP, recordings_.size()));
    Rf_setAttrib(result, R_ClassSymbol, Rf_mkString("rsh_rec"));

    int entry_idx = 0;
    for (auto& entry : recordings_) {
        auto& rec = entry.second;

        SEXP r = PROTECT(Rf_allocVector(VECSXP, 3));
        Rf_setAttrib(r, R_ClassSymbol, Rf_mkString("rsh_rec_entry"));

        SET_VECTOR_ELT(r, 0, Rf_mkString(rec.name.c_str()));
        SET_VECTOR_ELT(r, 1, rec.closure);

        if (!rec.events.empty()) {
            SEXP s_events = PROTECT(Rf_allocVector(VECSXP, rec.events.size()));

            int event_idx = 0;
            for (auto& event : rec.events) {
                SET_VECTOR_ELT(s_events, event_idx++, event->to_sexp());
            }

            SET_VECTOR_ELT(r, 2, s_events);
            UNPROTECT(1);
        }

        UNPROTECT(1);
        SET_VECTOR_ELT(result, entry_idx++, r);
    }

    FILE* f = std::fopen(recording_output_, "w");
    R_SaveToFile(result, f, 0);
    std::fclose(f);
    UNPROTECT(1);
}

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

void record_closure_speculative_context(SEXP, std::vector<SpeculativeContext>&);
void record_closure_speculative_context(const Code*,
                                        std::vector<SpeculativeContext>&);

void record_closure_speculative_context(SEXP cls,
                                        std::vector<SpeculativeContext>& ctx) {
    auto dt = DispatchTable::unpack(BODY(cls));
    auto fun = dt->baseline();
    auto code = fun->body();
    record_closure_speculative_context(code, ctx);
}

void record_closure_speculative_context(const Code* code,
                                        std::vector<SpeculativeContext>& ctx) {
    auto end = code->endCode();
    auto pc = code->code();
    Opcode* prev = NULL;
    Opcode* pprev = NULL;

    while (pc < end) {
        switch (*pc) {
        case Opcode::mk_promise_:
        case Opcode::mk_eager_promise_: {
            Immediate id = BC::readImmediate(&pc);
            auto promise = code->getPromise(id);
            record_closure_speculative_context(promise, ctx);
            break;
        }
        case Opcode::close_: {
            // prev is the push_ of srcref
            // pprev is the push_ of body
            auto cp_idx = BC::readImmediate(&pprev);
            SEXP cls = Pool::get(cp_idx);
            record_closure_speculative_context(cls, ctx);
            break;
        }
        case Opcode::record_call_: {
            ObservedCallees* feedback = (ObservedCallees*)pc;
            ctx.push_back(*feedback);
            pc += sizeof(ObservedCallees);
            break;
        }
        case Opcode::record_test_: {
            ObservedTest* feedback = (ObservedTest*)pc;
            ctx.push_back(*feedback);
            pc += sizeof(ObservedTest);
            break;
        }
        case Opcode::record_type_: {
            ObservedValues* feedback = (ObservedValues*)pc;
            ctx.push_back(*feedback);
            pc += sizeof(ObservedValues);
            break;
        }
        default: {
            pc = BC::next(pc);
            break;
        }
        }
        pprev = prev;
        prev = pc;
    }
}

void record_compile(const SEXP cls, const std::string& name,
                    const Context& assumptions) {
    if (!recording_) {
        return;
    }

    auto address = sexp_address(cls);
    auto r = recordings_.insert({address, FunRecorder{}});
    auto& v = r.first->second;

    if (r.second) {
        // we are seeing it for the first time
        v.name = name;
        v.closure = PROTECT(
            R_serialize(cls, R_NilValue, R_NilValue, R_NilValue, R_NilValue));
    }

    std::vector<SpeculativeContext> sc;
    record_closure_speculative_context(cls, sc);

    unsigned long dispatch_context = assumptions.toI();
    auto event = CompilationEvent(dispatch_context, std::move(sc));

    v.events.push_back(std::make_unique<CompilationEvent>(std::move(event)));

    std::cerr << "Compilation " << address << std::endl << v;

    if (!hook_registered_) {
        // TODO: there must be a better way to add onexit hook
        std::atexit(&exit_hook);
    }
}

void record_deopt(const SEXP cls) {
    if (!recording_) {
        return;
    }

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

void CompilationEvent::print(std::ostream& out) const {
    out << "Compilation" << std::endl;
    out << "context: " << dispatch_context << std::endl;
    for (auto& ctx : speculative_contexts) {
        out << "speculative contexts: " << ctx << std::endl;
    }
}

std::ostream& operator<<(std::ostream& out, const FunRecorder& fr) {
    out << "Recording of: " << fr.name << std::endl;
    /* out << fr.cls << std::endl; */
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

std::ostream& operator<<(std::ostream& out, const SpeculativeContext& ctx) {
    switch (ctx.type) {
    case SpeculativeContextType::Callees:
        out << "callees";
        break;
    case SpeculativeContextType::Test:
        out << "Tests";
        break;
    case SpeculativeContextType::Values:
        out << "values";
        break;
    }
    return out;
}

SEXP CompilationEvent::to_sexp() const {
    SEXP result = PROTECT(Rf_allocVector(VECSXP, 2));
    Rf_setAttrib(result, R_ClassSymbol, Rf_mkString("rsh_rec_compile"));
    SEXP dispatch_context = PROTECT(Rf_allocVector(INTSXP, 2));
    SET_INTEGER_ELT(dispatch_context, 0, (int)(this->dispatch_context >> 32));
    SET_INTEGER_ELT(dispatch_context, 1, (int)this->dispatch_context);
    SET_VECTOR_ELT(result, 0, dispatch_context);
    // TODO: speculative context
    UNPROTECT(2);
    return result;
}

SEXP DeoptEvent::to_sexp() const { return R_NilValue; }

} // namespace recording
} // namespace rir
