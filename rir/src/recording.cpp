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

namespace rir {
namespace recording {

static std::unordered_map<std::string, FunRecorder> recordings_;
static bool recording_ = false;

// TODO: convert this to an R API so it could be called from
// reg.finalizer(
//  e=loadNamespace("base"),
//  onexit=TRUE,
//  f=function(x) {
//    rir.save_recordings("/tmp/X")
//  }
// )

std::string sexp_address(const SEXP s) {
    char* caddress;
    if (asprintf(&caddress, "%p", (void*)s) == -1) {
        Rf_error("Getting address of SEXP failed");
    }

    return caddress;
}

void replay_closure_speculative_context(
    SEXP, std::vector<SpeculativeContext>::const_iterator&);
void replay_closure_speculative_context(
    const Code*, std::vector<SpeculativeContext>::const_iterator&);

void replay_closure_speculative_context(
    SEXP cls, std::vector<SpeculativeContext>::const_iterator& ctx) {
    auto dt = DispatchTable::unpack(BODY(cls));
    auto fun = dt->baseline();
    auto code = fun->body();
    replay_closure_speculative_context(code, ctx);
}

void replay_closure_speculative_context(
    const Code* code, std::vector<SpeculativeContext>::const_iterator& ctx) {
    auto end = code->endCode();
    auto pc = code->code();
    Opcode* prev = NULL;
    Opcode* pprev = NULL;

    while (pc < end) {
        // TODO: assert ctx != ctx->end
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

SEXP DeoptEvent::to_sexp() const {
    auto sexp = PROTECT(Rf_allocVector(VECSXP, 0));
    SET_CLASS(sexp, serialization::shared_class_name_event_deopt);
    UNPROTECT(1);
    return sexp;
}

void DeoptEvent::init_from_sexp(SEXP sexp) { assert(Rf_length(sexp) == 0); }

size_t saveTo(FILE* file) {
    auto sexp = serialization::to_sexp(recordings_);
    PROTECT(sexp);
    R_SaveToFile(sexp, file, 3);
    UNPROTECT(1);
    return recordings_.size();
}

size_t replayFrom(FILE* file, SEXP rho) {
    replay(R_LoadFromFile(file, 3), rho);
    return 0;
}

REXPORT SEXP replay(SEXP recording, SEXP rho) {
    PROTECT(recording);
    assert(Rf_isVector(recording));
    SEXP names = Rf_getAttrib(recording, R_NamesSymbol);
    assert(Rf_isVector(names));

    // TODO: The temporary map may not be necessary, we can probably replay the
    //       events on the go, from within the for loop.
    decltype(recordings_) new_recordings;

    for (auto i = 0; i < Rf_length(recording); i++) {
        auto recorder_name_sexp = STRING_ELT(names, i);
        auto recorder_sexp = VECTOR_ELT(recording, i);
        FunRecorder recorder =
            serialization::fun_recorder_from_sexp(recorder_sexp);

        SEXP name = Rf_install(recorder.name.c_str());
        SEXP closure = PROTECT(R_unserialize(recorder.closure, R_NilValue));
        closure = rirCompile(closure, rho);

        for (auto& event : recorder.events) {
            std::cerr << "event" << std::endl;
            event->replay(closure);
        }

        Rf_defineVar(name, closure, rho);
        UNPROTECT(1);

        std::string recorder_name =
            serialization::string_from_sexp(recorder_name_sexp);
        new_recordings.insert({std::move(recorder_name), std::move(recorder)});
    }

    UNPROTECT(1);
    return R_NilValue;
}

void CompilationEvent::replay(SEXP closure) const {
    auto i = speculative_contexts.begin();
    replay_closure_speculative_context(closure, i);
    // TODO: pirCompile
}

void DeoptEvent::replay(SEXP closure) const {
    // TODO: replay deopt
}

} // namespace recording
} // namespace rir

REXPORT SEXP start_recording() {
    rir::recording::recording_ = true;
    return R_NilValue;
}

REXPORT SEXP stop_recording() {
    rir::recording::recording_ = false;
    return R_NilValue;
}

REXPORT SEXP is_recording() {
    return Rf_ScalarLogical(rir::recording::recording_);
}
