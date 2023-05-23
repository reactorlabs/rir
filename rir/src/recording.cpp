#include "recording.h"
#include "R/Serialize.h"
#include "Rdefines.h"
#include "Rinternals.h"
#include "api.h"
#include "compiler/pir/module.h"
#include "compiler/pir/pir.h"
#include "recording_serialization.h"
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

// TODO: convert this to an R API so it could be called from
// reg.finalizer(
//  e=loadNamespace("base"),
//  onexit=TRUE,
//  f=function(x) {
//    rir.save_recordings("/tmp/X")
//  }
// )
// which itself could be run from R_PROFILE script
static void exit_hook() {
    if (hook_ran_) {
        return;
    }
    hook_ran_ = true;

    Rprintf("running exit hook, saving recordings\n");
    FILE* f = std::fopen(recording_output_, "w");
    saveTo(f);
    std::fclose(f);
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

void replay_closure_speculative_context(
    SEXP, std::vector<SpeculativeContext>::iterator&);
void replay_closure_speculative_context(
    const Code*, std::vector<SpeculativeContext>::iterator&);

void replay_closure_speculative_context(
    SEXP cls, std::vector<SpeculativeContext>::iterator& ctx) {
    auto dt = DispatchTable::unpack(BODY(cls));
    auto fun = dt->baseline();
    auto code = fun->body();
    replay_closure_speculative_context(code, ctx);
}

void replay_closure_speculative_context(
    const Code* code, std::vector<SpeculativeContext>::iterator& ctx) {
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
            replay_closure_speculative_context(promise, ctx);
            break;
        }
        case Opcode::close_: {
            // prev is the push_ of srcref
            // pprev is the push_ of body
            auto cp_idx = BC::readImmediate(&pprev);
            SEXP cls = Pool::get(cp_idx);
            replay_closure_speculative_context(cls, ctx);
            break;
        }
        case Opcode::record_call_: {
            ObservedCallees* feedback = (ObservedCallees*)pc;
            *feedback = (*ctx++).value.callees;
            pc += sizeof(ObservedCallees);
            break;
        }
        case Opcode::record_test_: {
            ObservedTest* feedback = (ObservedTest*)pc;
            *feedback = (*ctx++).value.test;
            pc += sizeof(ObservedTest);
            break;
        }
        case Opcode::record_type_: {
            ObservedValues* feedback = (ObservedValues*)pc;
            *feedback = (*ctx++).value.values;
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

void record_compile(SEXP const cls, const std::string& name,
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

void record_deopt(SEXP const cls) {
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

// TODO: create an R API for this
REXPORT SEXP replay(SEXP recording, SEXP rho) {
    PROTECT(recording);

    // TODO: deserialize the function
    // TODO bind it into rho
    // TODO: rir compile it
    // TODO: for each compilation entry
    // - replay the speculative context
    // - pirCompile it with the dispatch context

    UNPROTECT(1);

    return R_NilValue;
}

SEXP CompilationEvent::to_sexp() const {
    const char* fields[] = {"dispatch_context", "speculative_contexts", ""};
    auto sexp = PROTECT(Rf_mkNamed(VECSXP, fields));
    SET_CLASS(sexp, serialization::shared_class_name_event_compile);
    SET_VECTOR_ELT(sexp, 0, serialization::to_sexp(this->dispatch_context));
    auto speculative_contexts_sexp =
        Rf_allocVector(VECSXP, (int)this->speculative_contexts.size());
    SET_VECTOR_ELT(sexp, 1, speculative_contexts_sexp);

    int i = 0;
    for (auto& speculative_context : this->speculative_contexts) {
        SET_VECTOR_ELT(speculative_contexts_sexp, i++,
                       serialization::to_sexp(speculative_context));
    }

    UNPROTECT(1);
    return sexp;
}

void CompilationEvent::init_from_sexp(SEXP sexp) {
    assert(Rf_length(sexp) == 2);
    this->dispatch_context =
        serialization::uint64_t_from_sexp(VECTOR_ELT(sexp, 0));

    auto speculative_contexts_sexp = VECTOR_ELT(sexp, 1);
    assert(Rf_isVector(speculative_contexts_sexp));
    for (auto i = 0; i < Rf_length(speculative_contexts_sexp); i++) {
        auto speculative_context = serialization::speculative_context_from_sexp(
            VECTOR_ELT(speculative_contexts_sexp, i));
        this->speculative_contexts.push_back(speculative_context);
    }
}

void CompilationEvent::print(std::ostream& out) const {
    out << "Compilation" << std::endl;
    out << "context: " << dispatch_context << std::endl;
    for (auto& ctx : speculative_contexts) {
        out << "speculative contexts: " << ctx << std::endl;
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
