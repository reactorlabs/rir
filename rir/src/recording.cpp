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
#include "runtime/TypeFeedback.h"
#include "utils/Pool.h"
#include <cstdint>
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

// TODO: encapsulate into a Recording class
static std::unordered_map<std::string, Idx> recordings_index_;
static std::vector<FunRecorder> fun_recordings_;
static bool is_recording_ = false;

void Replay::replaySpeculativeContext(
    DispatchTable* dt, std::vector<SpeculativeContext>::const_iterator& ctx) {

    auto fun = dt->baseline();
    auto code = fun->body();
    this->replaySpeculativeContext(code, ctx);
}

void Replay::replaySpeculativeContext(
    Code* code, std::vector<SpeculativeContext>::const_iterator& ctx) {

    Opcode* end = code->endCode();
    Opcode* pc = code->code();
    Opcode* prev = NULL;
    Opcode* pprev = NULL;

    while (pc < end) {
        auto bc = BC::decode(pc, code);
        // TODO: assert ctx != ctx->end
        switch (bc.bc) {
        case Opcode::mk_promise_:
        case Opcode::mk_eager_promise_: {
            auto promise = code->getPromise(bc.immediate.fun);
            this->replaySpeculativeContext(promise, ctx);
            break;
        }
        case Opcode::close_: {
            // prev is the push_ of srcref
            // pprev is the push_ of body
            auto body = BC::decodeShallow(pprev).immediateConst();
            auto dt = DispatchTable::unpack(body);
            this->replaySpeculativeContext(dt, ctx);
            break;
        }
        case Opcode::record_call_: {
            ObservedCallees* feedback = (ObservedCallees*)(pc + 1);
            auto callees_idx = (*ctx++).value.callees;

            for (auto callee_idx : callees_idx) {
                if (callee_idx == NO_INDEX) {
                    continue;
                }

                SEXP callee = replayClosure(callee_idx);
                feedback->record(code, callee);
            }

            break;
        }
        case Opcode::record_test_: {
            ObservedTest* feedback = (ObservedTest*)(pc + 1);
            *feedback = (*ctx++).value.test;
            break;
        }
        case Opcode::record_type_: {
            ObservedValues* feedback = (ObservedValues*)(pc + 1);
            *feedback = (*ctx++).value.values;
            break;
        }
        default: {
        }
        }
        pprev = prev;
        prev = pc;
        pc = bc.next(pc);
    }
}

Idx initOrGetRecording(const SEXP cls, std::string name = "");
void record_closure_speculative_context(SEXP, std::vector<SpeculativeContext>&);
void record_closure_speculative_context(const Code*,
                                        std::vector<SpeculativeContext>&);

void record_closure_speculative_context(DispatchTable* dt,
                                        std::vector<SpeculativeContext>& ctx) {
    auto fun = dt->baseline();
    auto code = fun->body();
    record_closure_speculative_context(code, ctx);
}

void record_closure_speculative_context(const Code* code,
                                        std::vector<SpeculativeContext>& ctx) {
    Opcode* end = code->endCode();
    Opcode* pc = code->code();
    Opcode* prev = NULL;
    Opcode* pprev = NULL;

    while (pc < end) {
        auto bc = BC::decode(pc, code);
        switch (bc.bc) {
        case Opcode::mk_promise_:
        case Opcode::mk_eager_promise_: {
            auto promise = code->getPromise(bc.immediate.fun);
            record_closure_speculative_context(promise, ctx);
            break;
        }
        case Opcode::close_: {
            // prev is the push_ of srcref
            // pprev is the push_ of body
            auto body = BC::decodeShallow(pprev).immediateConst();
            auto dt = DispatchTable::unpack(body);
            record_closure_speculative_context(dt, ctx);
            break;
        }
        case Opcode::record_call_: {
            auto observed = bc.immediate.callFeedback;
            decltype(SpeculativeContext::value.callees) callees;

            for (unsigned int i = 0; i < ObservedCallees::MaxTargets; i++) {
                Idx idx;
                if (i < observed.numTargets) {
                    auto target = observed.getTarget(code, i);
                    idx = initOrGetRecording(target);
                } else {
                    idx = NO_INDEX;
                }
                callees[i] = idx;
            }

            ctx.push_back(std::move(callees));
            break;
        }
        case Opcode::record_test_: {
            ctx.push_back(bc.immediate.testFeedback);
            break;
        }
        case Opcode::record_type_: {
            ctx.push_back(bc.immediate.typeFeedback);
            break;
        }
        default: {
        }
        }
        pprev = prev;
        prev = pc;
        pc = bc.next(pc);
    }
}

Idx initOrGetRecording(const SEXP cls, std::string name) {
    auto address = sexpAddress(cls);
    auto r = recordings_index_.insert({address, fun_recordings_.size()});
    if (r.second) {
        // we are seeing it for the first time
        fun_recordings_.push_back(FunRecorder{});
        auto& v = fun_recordings_.back();
        v.name = name;
        v.closure = PROTECT(
            R_serialize(cls, R_NilValue, R_NilValue, R_NilValue, R_NilValue));
    }

    return r.first->second;
}

void recordCompile(SEXP const cls, const std::string& name,
                   const Context& assumptions) {
    if (!is_recording_) {
        return;
    }

    initOrGetRecording(cls, name);

    std::vector<SpeculativeContext> sc;
    auto dt = DispatchTable::unpack(BODY(cls));
    record_closure_speculative_context(dt, sc);

    auto dispatch_context = assumptions.toI();
    auto event = CompilationEvent(dispatch_context, std::move(sc));

    auto& rec = fun_recordings_[initOrGetRecording(cls, name)];
    rec.events.push_back(std::make_unique<CompilationEvent>(std::move(event)));
    std::cout << "A";
}

void recordDeopt(SEXP const cls) {
    if (!is_recording_) {
        return;
    }

    DeoptEvent event;

    FunRecorder& rec = fun_recordings_[initOrGetRecording(cls, "")];
    rec.events.push_back(std::make_unique<DeoptEvent>(std::move(event)));
}

SEXP CompilationEvent::to_sexp() const {
    const char* fields[] = {"dispatch_context", "speculative_contexts", ""};
    auto sexp = PROTECT(Rf_mkNamed(VECSXP, fields));
    setClassName(sexp, R_CLASS_COMPILE_EVENT);
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
    setClassName(sexp, R_CLASS_DEOPT_EVENT);
    UNPROTECT(1);
    return sexp;
}

void DeoptEvent::init_from_sexp(SEXP sexp) { assert(Rf_length(sexp) == 0); }

size_t saveTo(FILE* file) {
    auto sexp = PROTECT(serialization::to_sexp(fun_recordings_));
    R_SaveToFile(sexp, file, 3);
    UNPROTECT(1);
    return fun_recordings_.size();
}

size_t replayFrom(FILE* file, SEXP rho) {
    replayRecording(R_LoadFromFile(file, 3), rho);
    return 0;
}

SEXP Replay::replayClosure(Idx idx) {
    SEXP closure = closures_.at(idx);
    if (closure != R_NilValue) {
        return closure;
    }

    SEXP rawRecording = VECTOR_ELT(recordings_, idx);
    FunRecorder recording = serialization::fun_recorder_from_sexp(rawRecording);

    SEXP name = R_NilValue;
    if (!recording.name.empty()) {
        name = Rf_install(recording.name.c_str());
    }

    closure = PROTECT(R_unserialize(recording.closure, R_NilValue));
    closure = rirCompile(closure, rho_);
    closures_[idx] = closure;

    for (auto& event : recording.events) {
        event->replay(*this, closure);
    }

    if (name != R_NilValue) {
        Rf_defineVar(name, closure, rho_);
    }

    UNPROTECT(1);

    return closure;
}

void Replay::replay() {
    for (auto i = 0; i < Rf_length(recordings_); i++) {
        replayClosure(i);
    }
}

void CompilationEvent::replay(Replay& replay, SEXP closure) const {
    auto ctx = speculative_contexts.begin();
    auto dt = DispatchTable::unpack(BODY(closure));
    replay.replaySpeculativeContext(dt, ctx);

    // TODO: pirCompile
}

void DeoptEvent::replay(Replay& replay, SEXP closure) const {
    // TODO: replay deopt
}

SEXP setClassName(SEXP s, const char* className) {
    SEXP t = Rf_mkString(className);
    Rf_setAttrib(s, R_ClassSymbol, t);
    return s;
}

std::string sexpAddress(const SEXP s) {
    char* caddress;
    if (asprintf(&caddress, "%p", (void*)s) == -1) {
        Rf_error("Getting address of SEXP failed");
    }

    return caddress;
}

} // namespace recording
} // namespace rir

REXPORT SEXP startRecording() {
    rir::recording::is_recording_ = true;
    return R_NilValue;
}

REXPORT SEXP stopRecording() {
    rir::recording::is_recording_ = false;
    return R_NilValue;
}

REXPORT SEXP isRecording() {
    return Rf_ScalarLogical(rir::recording::is_recording_);
}

REXPORT SEXP replayRecording(SEXP recordings, SEXP rho) {
    rir::recording::Replay replay(recordings, rho);
    replay.replay();

    return R_NilValue;
}

REXPORT SEXP replayRecordingFromFile(SEXP filename, SEXP rho) {
    if (TYPEOF(filename) != STRSXP)
        Rf_error("must provide a string path");

    FILE* file = fopen(CHAR(Rf_asChar(filename)), "r");
    if (!file)
        Rf_error("couldn't open file at path");

    auto replayed_count = rir::recording::replayFrom(file, rho);
    fclose(file);

    return Rf_ScalarInteger((int)replayed_count);
}

REXPORT SEXP saveRecording(SEXP filename) {
    if (TYPEOF(filename) != STRSXP)
        Rf_error("must provide a string path");

    FILE* file = fopen(CHAR(Rf_asChar(filename)), "w");
    if (!file)
        Rf_error("couldn't open file at path");

    auto saved_count = rir::recording::saveTo(file);
    fclose(file);

    return Rf_ScalarInteger((int)saved_count);
}
