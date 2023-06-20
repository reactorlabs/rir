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
                    if (Rf_isFunction(target)) {
                        auto rec = initOrGetRecording(target);
                        idx = rec.first;
                    } else {
                        idx = NO_INDEX;
                    }
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
    assert(Rf_isFunction(cls));
    auto clsAddress = stringAddressOf(cls);
    auto r = recordings_index_.insert({clsAddress, fun_recordings_.size()});
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
    } else {
        v = &fun_recordings_[r.first->second];
        // If closure is undefined, that means this FunRecorder was created from
        // a DispatchTable, so we init it
        if (Rf_isNull(v->closure)) {
            v->env = getEnvironmentName(CLOENV(cls));
            v->closure = PROTECT(R_serialize(cls, R_NilValue, R_NilValue,
                                             R_NilValue, R_NilValue));
            R_PreserveObject(v->closure);
            UNPROTECT(1);
        }
    }

    // special functions don't have dispatch tables, let's not error here
    if (TYPEOF(BODY(cls)) == EXTERNALSXP) {
        auto dt = DispatchTable::unpack(BODY(cls));
        dt_to_recording_index_.insert({dt, r.first->second});
    }

    return {r.first->second, *v};
}

std::pair<size_t, FunRecording&>
Record::initOrGetRecording(const DispatchTable* dt, std::string name) {
    // First, we check if we can find the dt in the appropriate mapping
    auto dt_index = dt_to_recording_index_.find(dt);
    if (dt_index != dt_to_recording_index_.end()) {
        return {dt_index->second, fun_recordings_[dt_index->second]};
    } else {
        FunRecording dummyRecorder = {std::move(name), {}, R_NilValue, {}};

        auto insertion_index = fun_recordings_.size();
        fun_recordings_.push_back(std::move(dummyRecorder));
        dt_to_recording_index_.insert({dt, insertion_index});
        return {insertion_index, fun_recordings_[insertion_index]};
    }
}

Record::~Record() {
    for (auto& v : fun_recordings_) {
        R_ReleaseObject(v.closure);
    }
}

SEXP Record::save() { return serialization::to_sexp(fun_recordings_); }

Replay::Replay(SEXP recordings) : recordings_(recordings) {
    PROTECT(recordings_);

    assert(Rf_isVector(recordings_));

    auto n = Rf_length(recordings_);
    closures_.reserve(n);
    for (auto i = 0; i < n; i++) {
        closures_.push_back(R_NilValue);
    }
}
Replay::~Replay() { UNPROTECT_PTR(recordings_); }

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
    // TODO: the env parameter is likely not correct
    closure = rirCompile(closure, R_GlobalEnv);
    closures_[idx] = closure;

    for (auto& event : recording.events) {
        event->replay(*this, closure, recording.name);
    }

    std::cerr << "Replayed: " << recording.name << " from " << recording.env
              << std::endl;

    if (name != R_NilValue) {
        SEXP env = getEnvironment(recording.env);

        if (env == R_GlobalEnv) {
            Rf_defineVar(name, closure, R_GlobalEnv);
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

                SEXP callee = PROTECT(replayClosure(callee_idx));
                feedback->record(code, callee);
                UNPROTECT(1);
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
    pirCompile(closure, Context(this->dispatch_context), closure_name,
               pir::DebugOptions::DefaultDebugOptions);
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

DeoptEvent::DeoptEvent(size_t functionIdx, DeoptReason::Reason reason,
                       std::pair<size_t, size_t> reasonCodeIdx,
                       uint32_t reasonCodeOff, SEXP trigger)
    : functionIdx_(functionIdx), reason_(reason),
      reasonCodeIdx_(std::move(reasonCodeIdx)), reasonCodeOff_(reasonCodeOff),
      trigger_(trigger) {
    if (this->trigger_) {
        R_PreserveObject(trigger_);
    }
}

DeoptEvent::~DeoptEvent() {
    if (this->trigger_) {
        R_ReleaseObject(this->trigger_);
    }
}

Code* retrieveCodeFromIndex(const std::vector<SEXP>& closures,
                            const std::pair<ssize_t, ssize_t> index) {
    assert(index.first != -1);
    SEXP closure = closures[index.first];
    auto* dt = DispatchTable::unpack(BODY(closure));
    Code* code = dt->baseline()->body();

    if (index.second == -1) {
        return code;
    } else {
        return code->getPromise(index.second);
    }
}

void DeoptEvent::replay(Replay& replay, SEXP closure,
                        std::string& closure_name) const {
    // A deopt normally occurs _while executing_ a function, and partly consists
    // in converting the native stack frame into an interpreted stack frame,
    // while keeping note of the broken invariants that led to the function
    // being deoptimized in the first place. To replay a deoptimization, we only
    // have to store the broken invariants. Nothing else.

    auto dt = DispatchTable::unpack(BODY(closure));
    auto fun = dt->get(functionIdx_);
    Code* code = fun->body();

    // Copy and set current closure's code
    Code* deoptSrcCode =
        retrieveCodeFromIndex(replay.closures_, this->reasonCodeIdx_);
    Opcode* deoptPc = (Opcode*)((uintptr_t)deoptSrcCode + reasonCodeOff_);
    FeedbackOrigin origin(deoptSrcCode, deoptPc);
    DeoptReason reason(origin, this->reason_);

    reason.record(this->trigger_);
    fun->registerDeopt();
    recordDeopt(code, closure, reason, this->trigger_);
}

SEXP DeoptEvent::toSEXP() const {
    const char* fields[] = {"functionIdx",     "reason",  "reason_code_idx",
                            "reason_code_off", "trigger", ""};
    auto sexp = PROTECT(Rf_mkNamed(VECSXP, fields));
    setClassName(sexp, R_CLASS_DEOPT_EVENT);

    SET_VECTOR_ELT(sexp, 0, serialization::to_sexp(this->functionIdx_));
    SET_VECTOR_ELT(sexp, 1, serialization::to_sexp(this->reason_));
    SET_VECTOR_ELT(sexp, 2, serialization::to_sexp(this->reasonCodeOff_));
    SET_VECTOR_ELT(sexp, 3, serialization::to_sexp(this->reasonCodeIdx_));
    SET_VECTOR_ELT(sexp, 4, this->trigger_);
    UNPROTECT(1);
    return sexp;
}

void DeoptEvent::fromSEXP(SEXP sexp) {
    assert(Rf_isVector(sexp));
    assert(Rf_length(sexp) == 5);

    this->functionIdx_ = serialization::uint64_t_from_sexp(VECTOR_ELT(sexp, 0));
    this->reason_ = serialization::deopt_reason_from_sexp(VECTOR_ELT(sexp, 1));
    this->reasonCodeOff_ =
        serialization::uint32_t_from_sexp(VECTOR_ELT(sexp, 2));
    this->reasonCodeIdx_ = serialization::pair_from_sexp(VECTOR_ELT(sexp, 3));
    this->trigger_ = VECTOR_ELT(sexp, 4);
    R_PreserveObject(this->trigger_);
}

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
}

size_t Record::indexOfBaseline(const rir::Code* code) const {
    auto entry = dt_to_recording_index_.find(code->function()->dispatchTable());
    if (entry != dt_to_recording_index_.end()) {
        return entry->second;
    }

    Rf_error(
        "baseline code %p not found in current DispatchTable->FunRecorder map",
        code);
}

std::pair<ssize_t, ssize_t> Record::findIndex(rir::Code* code,
                                              rir::Code* needle) {
    if (code == needle) {
        return {indexOfBaseline(code), -1};
    }

    // find the index of the reason source
    // 1. try promises
    for (size_t i = 0; i < code->extraPoolSize; i++) {
        auto extraEntry = code->getExtraPoolEntry(i);
        auto prom = (Code*)STDVEC_DATAPTR(extraEntry);
        if (prom->info.magic == CODE_MAGIC && prom == needle) {
            return {indexOfBaseline(code), i};
        }
    }

    // 2. search globally
    return {indexOfBaseline(needle), -1};
}

void recordDeopt(rir::Code* c, const SEXP cls, DeoptReason& reason,
                 SEXP trigger) {
    if (!is_recording_) {
        return;
    }

    // find the affected version
    int funIdx = 0;
    auto dt = DispatchTable::unpack(BODY(cls));
    // it deopts from native so it cannot be the baseline RIR
    for (size_t i = 1; i < dt->size(); i++) {
        if (dt->get(i)->body() == c) {
            funIdx = i;
            break;
        }
    }

    assert(funIdx != 0 && "Could not find deopted function index in its "
                          "closure's dispatch table");

    auto reasonCodeIdx =
        recorder_.findIndex(dt->baseline()->body(), reason.srcCode());

    assert(reasonCodeIdx.first >= 0 &&
           "Could not locate deopt reason location");

    // TODD
    DeoptEvent event(funIdx, reason.reason, reasonCodeIdx,
                     reason.origin.offset(), trigger);

    auto rec = recorder_.initOrGetRecording(cls);
    auto& v = rec.second;
    v.events.push_back(std::make_unique<DeoptEvent>(std::move(event)));
}

SEXP setClassName(SEXP s, const char* className) {
    SEXP t = PROTECT(Rf_mkString(className));
    Rf_setAttrib(s, R_ClassSymbol, t);
    UNPROTECT(1);
    return s;
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

REXPORT SEXP startRecordings() {
    rir::recording::is_recording_ = true;
    return R_NilValue;
}

REXPORT SEXP stopRecordings() {
    rir::recording::is_recording_ = false;
    return R_NilValue;
}

REXPORT SEXP resetRecordings() {
    rir::recording::recorder_.reset();
    return R_NilValue;
}

REXPORT SEXP isRecordings() {
    return Rf_ScalarLogical(rir::recording::is_recording_);
}

REXPORT SEXP replayRecordings(SEXP recordings) {
    rir::recording::Replay replay(recordings);
    auto res = replay.replay();

    return Rf_ScalarInteger(res);
}

REXPORT SEXP replayRecordingsFromFile(SEXP filename) {
    auto recordings = loadRecordings(filename);
    auto res = replayRecordings(recordings);

    return res;
}

REXPORT SEXP saveRecordings(SEXP filename) {
    if (TYPEOF(filename) != STRSXP)
        Rf_error("must provide a string path");

    FILE* file = fopen(CHAR(Rf_asChar(filename)), "w");
    if (!file)
        Rf_error("couldn't open file at path");

    auto recordings = PROTECT(getRecordings());
    auto size = Rf_length(recordings);
    R_SaveToFile(recordings, file, 3);
    fclose(file);
    UNPROTECT(1);

    return Rf_ScalarInteger(size);
}

REXPORT SEXP loadRecordings(SEXP filename) {
    if (TYPEOF(filename) != STRSXP)
        Rf_error("must provide a string path");

    FILE* file = fopen(CHAR(Rf_asChar(filename)), "r");
    if (!file)
        Rf_error("couldn't open file at path");

    auto res = R_LoadFromFile(file, 3);

    fclose(file);

    return res;
}

REXPORT SEXP getRecordings() { return rir::recording::recorder_.save(); }
