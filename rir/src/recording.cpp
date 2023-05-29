#include "recording.h"
#include "R/Serialize.h"
#include "R/r.h"
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

// global state
// a flag indicating whether the recording is active or not
static bool is_recording_ = false;
// the main recorder
static Record recorder_;

void Record::recordSpeculativeContext(DispatchTable* dt,
                                      std::vector<SpeculativeContext>& ctx) {
    auto fun = dt->baseline();
    auto code = fun->body();
    recordSpeculativeContext(code, ctx);
}

void Record::recordSpeculativeContext(const Code* code,
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
            recordSpeculativeContext(promise, ctx);
            break;
        }
        case Opcode::close_: {
            // prev is the push_ of srcref
            // pprev is the push_ of body
            auto body = BC::decodeShallow(pprev).immediateConst();
            auto dt = DispatchTable::unpack(body);
            recordSpeculativeContext(dt, ctx);
            break;
        }
        case Opcode::record_call_: {
            auto observed = bc.immediate.callFeedback;
            decltype(SpeculativeContext::value.callees) callees;

            for (unsigned int i = 0; i < ObservedCallees::MaxTargets; i++) {
                size_t idx;
                if (i < observed.numTargets) {
                    auto target = observed.getTarget(code, i);
                    auto rec = initOrGetRecording(target);
                    idx = rec.first;
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

std::pair<size_t, FunRecording&> Record::initOrGetRecording(const SEXP cls,
                                                            std::string name) {
    auto address = sexpAddress(cls);
    auto r = recordings_index_.insert({address, fun_recordings_.size()});
    FunRecording* v;

    if (r.second) {
        // we are seeing it for the first time
        fun_recordings_.push_back(FunRecording{});
        v = &fun_recordings_.back();
        v->name = name;
        v->env = getEnvironmentName(CLOENV(cls));
        v->closure = PROTECT(
            R_serialize(cls, R_NilValue, R_NilValue, R_NilValue, R_NilValue));
        R_PreserveObject(v->closure);
        UNPROTECT(1);
        // TODO: who should clear this? saveToFile?
    } else {
        v = &fun_recordings_[r.first->second];
    }

    return {r.first->second, *v};
}

size_t Record::saveToFile(FILE* file) {
    auto sexp = PROTECT(serialization::to_sexp(fun_recordings_));
    R_SaveToFile(sexp, file, 3);
    UNPROTECT(1);
    return fun_recordings_.size();
}

Replay::Replay(SEXP recordings, SEXP rho) : recordings_(recordings), rho_(rho) {
    PROTECT(recordings_);
    PROTECT(rho_);

    assert(Rf_isVector(recordings_));
    assert(Rf_isEnvironment(rho_));

    auto n = Rf_length(recordings_);
    closures_.reserve(n);
    for (auto i = 0; i < n; i++) {
        closures_.push_back(R_NilValue);
    }
}
Replay::~Replay() {
    UNPROTECT_PTR(recordings_);
    UNPROTECT_PTR(rho_);
}

SEXP Replay::replayClosure(size_t idx) {
    SEXP closure = closures_.at(idx);
    if (closure != R_NilValue) {
        return closure;
    }

    SEXP rawRecording = VECTOR_ELT(recordings_, idx);
    FunRecording recording =
        serialization::fun_recorder_from_sexp(rawRecording);

    SEXP name = R_NilValue;
    if (!recording.name.empty()) {
        name = Rf_install(recording.name.c_str());
    }

    closure = PROTECT(R_unserialize(recording.closure, R_NilValue));
    closure = rirCompile(closure, rho_);
    closures_[idx] = closure;

    for (auto& event : recording.events) {
        event->replay(*this, closure, recording.name);
    }

    std::cerr << "Replayed: " << recording.name << " from " << recording.env
              << std::endl;

    if (name != R_NilValue) {
        SEXP env = getEnvironment(recording.env);

        if (env == R_GlobalEnv) {
            Rf_defineVar(name, closure, rho_);
        } else if (env != R_UnboundValue) {
            SEXP existing_closure = Rf_findFun(name, env);
            if (existing_closure != R_UnboundValue) {
                BODY(existing_closure) = BODY(closure);
            }
        }
    }

    UNPROTECT(1);

    return closure;
}

size_t Replay::replay() {
    auto n = Rf_length(recordings_);

    for (auto i = 0; i < n; i++) {
        replayClosure(i);
    }

    return n;
}

void Replay::replaySpeculativeContext(
    DispatchTable* dt,
    std::vector<SpeculativeContext>::const_iterator& ctxStart,
    std::vector<SpeculativeContext>::const_iterator& ctxEnd) {

    auto fun = dt->baseline();
    auto code = fun->body();
    this->replaySpeculativeContext(code, ctxStart, ctxEnd);
}

void Replay::replaySpeculativeContext(
    Code* code, std::vector<SpeculativeContext>::const_iterator& ctxStart,
    std::vector<SpeculativeContext>::const_iterator& ctxEnd) {

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
            this->replaySpeculativeContext(promise, ctxStart, ctxEnd);
            break;
        }
        case Opcode::close_: {
            // prev is the push_ of srcref
            // pprev is the push_ of body
            auto body = BC::decodeShallow(pprev).immediateConst();
            auto dt = DispatchTable::unpack(body);
            this->replaySpeculativeContext(dt, ctxStart, ctxEnd);
            break;
        }
        case Opcode::record_call_: {
            assert(ctxStart != ctxEnd);

            ObservedCallees* feedback = (ObservedCallees*)(pc + 1);
            auto callees_idx = (*ctxStart++).value.callees;

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
            assert(ctxStart != ctxEnd);

            ObservedTest* feedback = (ObservedTest*)(pc + 1);
            *feedback = (*ctxStart++).value.test;
            break;
        }
        case Opcode::record_type_: {
            assert(ctxStart != ctxEnd);

            ObservedValues* feedback = (ObservedValues*)(pc + 1);
            *feedback = (*ctxStart++).value.values;
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

void CompilationEvent::replay(Replay& replay, SEXP closure,
                              std::string& closure_name) const {
    auto start = speculative_contexts.begin();
    auto end = speculative_contexts.end();
    auto dt = DispatchTable::unpack(BODY(closure));
    replay.replaySpeculativeContext(dt, start, end);

    // TODO: pirCompile
}

SEXP CompilationEvent::toSEXP() const {
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

void CompilationEvent::fromSEXP(SEXP sexp) {
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

void DeoptEvent::replay(Replay& replay, SEXP closure,
                        std::string& closure_name) const {
    // TODO: replay deopt
}

SEXP DeoptEvent::toSEXP() const {
    auto sexp = PROTECT(Rf_allocVector(VECSXP, 0));
    setClassName(sexp, R_CLASS_DEOPT_EVENT);
    UNPROTECT(1);
    return sexp;
}

void DeoptEvent::fromSEXP(SEXP sexp) { assert(Rf_length(sexp) == 0); }

void recordCompile(SEXP const cls, const std::string& name,
                   const Context& assumptions) {
    if (!is_recording_) {
        return;
    }

    recorder_.initOrGetRecording(cls, name);

    std::vector<SpeculativeContext> sc;
    auto dt = DispatchTable::unpack(BODY(cls));
    recorder_.recordSpeculativeContext(dt, sc);

    auto dispatch_context = assumptions.toI();
    auto event = CompilationEvent(dispatch_context, std::move(sc));

    auto rec = recorder_.initOrGetRecording(cls, name);
    auto& v = rec.second;
    v.events.push_back(std::make_unique<CompilationEvent>(std::move(event)));
    std::cout << "A";
}

void recordDeopt(SEXP const cls) {
    if (!is_recording_) {
        return;
    }

    DeoptEvent event;

    auto rec = recorder_.initOrGetRecording(cls);
    auto& v = rec.second;
    v.events.push_back(std::make_unique<DeoptEvent>(std::move(event)));
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

std::string getEnvironmentName(SEXP env) {
    if (env == R_GlobalEnv) {
        return GLOBAL_ENV_NAME;
    } else if (R_IsPackageEnv(env) == TRUE) {
        // cf. builtin.c:432 do_envirName
        return CHAR(STRING_ELT(R_PackageEnvName(env), 0));
    } else if (R_IsNamespaceEnv(env) == TRUE) {
        // cf. builtin.c:434 do_envirName
        return CHAR(STRING_ELT(R_NamespaceEnvSpec(env), 0));
    } else {
        return "";
    }
}

bool stringStartsWith(const std::string& s, const std::string& prefix) {
    return s.substr(0, prefix.length()) == prefix;
}

SEXP getEnvironment(const std::string& name) {
    if (name.empty()) {
        return R_UnboundValue;
    }

    // try global
    if (name == GLOBAL_ENV_NAME) {
        return R_GlobalEnv;
    }

    SEXP env_sxp_name = PROTECT(Rf_mkString(name.c_str()));

    // try package environment
    if (stringStartsWith(name, "package:")) {
        SEXP env = R_FindPackageEnv(env_sxp_name);
        UNPROTECT(1);
        if (env != R_GlobalEnv) {
            return env;
        } else {
            return R_UnboundValue;
        }
    }

    // try a namespace
    SEXP env = R_FindNamespace(env_sxp_name);

    UNPROTECT(1);
    return env;
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
    auto res = replay.replay();

    return Rf_ScalarInteger(res);
}

REXPORT SEXP replayRecordingFromFile(SEXP filename, SEXP rho) {
    auto rec = loadRecording(filename);
    auto res = replayRecording(rec, rho);

    return res;
}

REXPORT SEXP saveRecording(SEXP filename) {
    if (TYPEOF(filename) != STRSXP)
        Rf_error("must provide a string path");

    FILE* file = fopen(CHAR(Rf_asChar(filename)), "w");
    if (!file)
        Rf_error("couldn't open file at path");

    auto saved_count = rir::recording::recorder_.saveToFile(file);
    fclose(file);

    return Rf_ScalarInteger((int)saved_count);
}

REXPORT SEXP loadRecording(SEXP filename) {
    if (TYPEOF(filename) != STRSXP)
        Rf_error("must provide a string path");

    FILE* file = fopen(CHAR(Rf_asChar(filename)), "r");
    if (!file)
        Rf_error("couldn't open file at path");

    auto res = R_LoadFromFile(file, 3);

    fclose(file);

    return res;
}
