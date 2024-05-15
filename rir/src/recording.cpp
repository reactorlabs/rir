#include "recording.h"
#include "R/Serialize.h"
#include "R/r.h"
#include "Rdefines.h"
#include "Rinternals.h"
#include "api.h"
#include "compiler/compiler.h"
#include "compiler/pir/module.h"
#include "compiler/pir/pir.h"
#include "recording_serialization.h"
#include "runtime/Context.h"
#include "runtime/DispatchTable.h"
#include "runtime/Function.h"
#include "runtime/TypeFeedback.h"
#include "utils/Pool.h"

#include "llvm/Support/raw_os_ostream.h"

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

#include "prefixer.cpp"

namespace rir {
namespace recording {

// global state
// a flag indicating whether the recording is active or not
static bool is_recording_ = false;

static InvocationEvent::SourceSet invocation_source_
    = InvocationEvent::SourceSet::None();

// the main recorder
static Record recorder_;

/**
 * Bitmask filter of events to record
 */
struct {
    bool compile : 1;
    bool deopt : 1;
    bool typeFeedback : 1;
    bool invoke : 1;
} filter_ = {
    .compile = true,
    .deopt = true,
    .typeFeedback = false,
    .invoke = false,
};

// Don't record invocations while replaying compile events
static bool isPlayingCompile = false;

CompileReasons compileReasons_;
std::stack<std::pair<CompilationEvent::Time, CompilationEvent>> compilation_stack_;

SEXP InvocationCountTimeReason::toSEXP() const {
    auto vec = PROTECT(this->CompileReasonImpl::toSEXP());

    size_t i = 0;
    SET_VECTOR_ELT(vec, i++, serialization::to_sexp(count));
    SET_VECTOR_ELT(vec, i++, serialization::to_sexp(minimalCount));
    SET_VECTOR_ELT(vec, i++, serialization::to_sexp(time));
    SET_VECTOR_ELT(vec, i++, serialization::to_sexp(minimalTime));

    UNPROTECT(1);
    return vec;
}

void InvocationCountTimeReason::fromSEXP(SEXP sexp){
    this->CompileReasonImpl::fromSEXP(sexp);

    size_t i = 0;
    this->count = serialization::uint64_t_from_sexp(VECTOR_ELT(sexp, i++));
    this->minimalCount = serialization::uint64_t_from_sexp(VECTOR_ELT(sexp, i++));
    this->time = serialization::uint64_t_from_sexp(VECTOR_ELT(sexp, i++));
    this->minimalTime = serialization::uint64_t_from_sexp(VECTOR_ELT(sexp, i++));
}

SEXP PirWarmupReason::toSEXP() const {
    auto vec = PROTECT(this->CompileReasonImpl::toSEXP());

    SET_VECTOR_ELT(vec, 0, serialization::to_sexp(invocationCount));

    UNPROTECT(1);
    return vec;
}

void PirWarmupReason::fromSEXP(SEXP sexp){
    this->CompileReasonImpl::fromSEXP(sexp);

    this->invocationCount = serialization::uint64_t_from_sexp(VECTOR_ELT(sexp, 0));
}

SEXP OSRLoopReason::toSEXP() const {
    auto vec = PROTECT(this->CompileReasonImpl::toSEXP());

    SET_VECTOR_ELT(vec, 0, serialization::to_sexp(loopCount));

    UNPROTECT(1);
    return vec;
}

void OSRLoopReason::fromSEXP(SEXP sexp){
    this->CompileReasonImpl::fromSEXP(sexp);

    this->loopCount = serialization::uint64_t_from_sexp(VECTOR_ELT(sexp, 0));
}

bool Record::contains(const DispatchTable* dt) {
    return dt_to_recording_index_.count(dt);
}

void Record::recordSpeculativeContext(DispatchTable* dt,
                                      std::vector<SpeculativeContext>& ctx) {
    auto fun = dt->baseline();
    auto code = fun->body();
    recordSpeculativeContext(code, ctx);
}

void Record::recordSpeculativeContext(const Code* code,
                                      std::vector<SpeculativeContext>& ctx) {
    auto feedback = code->function()->typeFeedback();

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
            auto observed = feedback->callees(bc.immediate.i);
            decltype(SpeculativeContext::value.callees) callees;

            for (unsigned int i = 0; i < ObservedCallees::MaxTargets; i++) {
                size_t idx;
                if (i < observed.numTargets) {
                    auto target = observed.getTarget(code->function(), i);
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

            ctx.emplace_back(std::move(callees));
            break;
        }
        case Opcode::record_test_: {
            ctx.emplace_back(feedback->test(bc.immediate.i));
            break;
        }
        case Opcode::record_type_: {
            ctx.emplace_back(feedback->types(bc.immediate.i));
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
                                                            const std::string& name) {
    assert(Rf_isFunction(cls));
    auto& body = *BODY(cls);

    // Primitives are stored as a special case
    if (TYPEOF(cls) == SPECIALSXP || TYPEOF(cls) == BUILTINSXP) {
        auto primIdx = cls->u.primsxp.offset;

        // Do we already have it stored?
        auto primEntry = primitive_to_body_index.find(primIdx);
        if (primEntry != primitive_to_body_index.end()) {
            return {primEntry->second, functions[primEntry->second]};
        }

        size_t idx = functions.size();
        functions.emplace_back(primIdx);
        auto& body = functions.back();
        body.closure = PROTECT(
            R_serialize(cls, R_NilValue, R_NilValue, R_NilValue, R_NilValue));
        R_PreserveObject(body.closure);
        UNPROTECT(1);
        return {idx, body};
    }

    assert(TYPEOF(cls) == CLOSXP);
    auto dt = DispatchTable::check(&body);
    bool inserted;
    size_t index;
    if (dt) {
        // Leaking for the moment
        R_PreserveObject(dt->container());
        auto r = dt_to_recording_index_.emplace(dt, functions.size());
        inserted = r.second;
        index = r.first->second;
    } else {
        R_PreserveObject(&body);
        auto r = bcode_to_body_index.emplace(&body, functions.size());
        inserted = r.second;
        index = r.first->second;
    }

    auto r = dt_to_recording_index_.emplace(dt, functions.size());
    FunRecording* v;

    if (inserted) {
        // we are seeing it for the first time
        functions.emplace_back();
        v = &functions.back();
        v->env = getEnvironmentName(CLOENV(cls));
        v->closure = PROTECT(
            R_serialize(cls, R_NilValue, R_NilValue, R_NilValue, R_NilValue));
        R_PreserveObject(v->closure);
        UNPROTECT(1);
    } else {
        v = &functions[index];
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

    if (v->name.empty() && !name.empty()) {
        v->name = name;
    }

    if (r.second && false) {
        assert(dt->size() == 1);
        auto* base = dt->baseline();
        log.emplace_back(std::make_unique<DtInitEvent>(
            r.first->second, base->invocationCount(), base->deoptCount()));
    }

    return {r.first->second, *v};
}

std::pair<size_t, FunRecording&>
Record::initOrGetRecording(const DispatchTable* dt, const std::string& name) {
    // First, we check if we can find the dt in the appropriate mapping
    auto dt_index = dt_to_recording_index_.find(dt);
    if (dt_index != dt_to_recording_index_.end()) {
        return {dt_index->second, functions[dt_index->second]};
    } else {
        auto insertion_index = functions.size();
        functions.emplace_back();
        functions.back().name = name;
        dt_to_recording_index_.emplace(dt, insertion_index);
        return {insertion_index, functions[insertion_index]};
    }
}

Record::~Record() {
    for (auto& v : functions) {
        R_ReleaseObject(v.closure);
    }
}

SEXP Record::save() {
    // Check if we have dispatch tables without an associated SEXP and remove
    // related events. Might be avoidable eventually.
    size_t recIdx = 0;
    for (auto& recording : functions) {
        if (Rf_isNull(recording.closure)) {
            auto refersToRecording =
                [recIdx](std::unique_ptr<rir::recording::Event>& event) {
                    return event->containsReference(recIdx);
                };

            log.erase(std::remove_if(log.begin(), log.end(), refersToRecording),
                      log.end());
        }
        recIdx++;
    }

    const char* fields[] = {"functions", "events", ""};
    auto recordSexp = PROTECT(Rf_mkNamed(VECSXP, fields));
    auto bodiesSexp = PROTECT(serialization::to_sexp(functions));
    auto eventsSexp = PROTECT(serialization::to_sexp(log));

    SET_VECTOR_ELT(recordSexp, 0, bodiesSexp);
    SET_VECTOR_ELT(recordSexp, 1, eventsSexp);

    UNPROTECT(3);
    return recordSexp;
}

// Plays along nicer with diff tools
#define HIDE_UNKNOWN_CLOSURE_POINTER true

std::ostream& operator<<(std::ostream& out, const FunRecording& that) {
    if (that.primIdx >= 0) {
        // Weird condition coming from names.c:R_Primitive
        bool isInternal = (R_FunTab[that.primIdx].eval % 100) / 10;
        if (isInternal) {
            out << ".Internal(" << that.name << ")";
        } else {
            out << ".Primitive(" << that.name << ")";
        }
    } else if (that.name.length()) {
        out << that.name;
    } else if (HIDE_UNKNOWN_CLOSURE_POINTER) {
        out << "<?>";
    } else {
        out << (void*)that.closure;
    }

    return out;
}

const char* ClosureEvent::targetName(const std::vector<FunRecording>& mapping) const {
    return mapping[closureIndex].name.c_str();
}

const char* DtEvent::targetName(const std::vector<FunRecording>& mapping) const {
    return mapping[dispatchTableIndex].name.c_str();
}

void SpeculativeContext::print(const std::vector<FunRecording>& mapping,
                               std::ostream& out) const {
    switch (type) {
    case SpeculativeContextType::Callees: {
        out << "Callees[";
        bool first = true;
        for (auto c : value.callees) {
            if (c == NO_INDEX)
                break;
            if (first)
                first = false;
            else
                out << ',';
            out << mapping[c];
        }
        out << "]";
        return;
    }
    case SpeculativeContextType::Test:
        out << "Test{";
        switch (value.test.seen) {
        case ObservedTest::None:
            out << "None";
            break;
        case ObservedTest::OnlyTrue:
            out << "OnlyTrue";
            break;
        case ObservedTest::OnlyFalse:
            out << "OnlyFalse";
            break;
        case ObservedTest::Both:
            out << "Both";
            break;
        }
        out << "}";
        return;
    case SpeculativeContextType::Values:
        out << "Values{";
        value.values.print(out);
        out << "}";
        return;
    }
}

SEXP SpeculativeContextEvent::toSEXP() const {
    SEXP sexp = PROTECT(Rf_allocVector(VECSXP, 4));
    setClassName(sexp, R_CLASS_SC_EVENT);
    size_t i = 0;
    assert(codeIndex != -2);
    SET_VECTOR_ELT(sexp, i++, serialization::to_sexp(dispatchTableIndex));
    SET_VECTOR_ELT(sexp, i++, serialization::to_sexp(codeIndex));
    SET_VECTOR_ELT(sexp, i++, serialization::to_sexp(offset));
    SET_VECTOR_ELT(sexp, i++, serialization::to_sexp(sc));
    UNPROTECT(1);
    return sexp;
}

void SpeculativeContextEvent::fromSEXP(SEXP sexp) {
    assert(Rf_isVector(sexp));
    assert(Rf_length(sexp) == 4);
    size_t i = 0;
    dispatchTableIndex =
        serialization::uint64_t_from_sexp(VECTOR_ELT(sexp, i++));
    codeIndex = serialization::int64_t_from_sexp(VECTOR_ELT(sexp, i++));
    assert(codeIndex >= -1);
    offset = serialization::uint64_t_from_sexp(VECTOR_ELT(sexp, i++));
    sc = serialization::speculative_context_from_sexp(VECTOR_ELT(sexp, i++));
}

bool SpeculativeContextEvent::containsReference(size_t recordingIdx) const {
    if (sc.type == SpeculativeContextType::Callees) {
        const auto& callees = sc.value.callees;
        const size_t* found =
            std::find(callees.begin(), callees.end(), recordingIdx);

        if (found != callees.end())
            return true;
    }

    return DtEvent::containsReference(recordingIdx);
}

void SpeculativeContextEvent::print(const std::vector<FunRecording>& mapping,
                                    std::ostream& out) const {
    out << "SpeculativeContextEvent{\n        code=";

    if (codeIndex == -1) {
        out << "<body>";
    } else if (codeIndex >= 0) {
        out << "<promise #" << codeIndex << ">";
    }

    out << "\n        offset=" << offset << "\n        sc=";
    sc.print(mapping, out);
    out << "\n    }";
}

bool CompilationEvent::containsReference(size_t recordingIdx) const {
    for (auto& sc : speculative_contexts) {
        if (sc.type == SpeculativeContextType::Callees) {
            const auto& callees = sc.value.callees;
            const size_t* found =
                std::find(callees.begin(), callees.end(), recordingIdx);

            if (found != callees.end())
                return true;
        }
    }

    return ClosureEvent::containsReference(recordingIdx);
}

void CompilationEvent::print(const std::vector<FunRecording>& mapping,
                             std::ostream& out) const {
    out << "CompilationEvent{\n        dispatch_context="
        << Context(this->dispatch_context) << ",\n        name=" << compileName
        << ",\n        speculative_contexts=[\n";
    for (auto& spec : this->speculative_contexts) {
        out << "            ";
        spec.print(mapping, out);
        out << "\n";
    }
    out << "        ],\n        opt_reasons=[\n";
    if(this->compile_reasons.heuristic){
        out << "            heuristic=";
        this->compile_reasons.heuristic->print(out);
        out << "\n";
    }

    if(this->compile_reasons.condition){
        out << "            condition=";
        this->compile_reasons.condition->print(out);
        out << "\n";
    }

    if(this->compile_reasons.osr){
        out << "            osr_reason=";
        this->compile_reasons.osr->print(out);
        out << "\n";
    }

    out << "        ]\n    }";
}

SEXP CompilationEvent::toSEXP() const {
    const char* fields[] = {"closure",
                            "dispatch_context",
                            "name",
                            "speculative_contexts",
                            "compile_reason_heuristic",
                            "compile_reason_condition",
                            "compile_reason_osr",
                            "time",
                            "subevents",
                            "bitcode",
                            "succesful",
                            ""};
    auto sexp = PROTECT(Rf_mkNamed(VECSXP, fields));
    setClassName(sexp, R_CLASS_COMPILE_EVENT);

    size_t i = 0;
    SET_VECTOR_ELT(sexp, i++, serialization::to_sexp(closureIndex));
    SET_VECTOR_ELT(sexp, i++, serialization::to_sexp(this->dispatch_context));
    SET_VECTOR_ELT(sexp, i++, serialization::to_sexp(this->compileName));

    auto speculative_contexts_sexp = Rf_allocVector(VECSXP, this->speculative_contexts.size());
    SET_VECTOR_ELT(sexp, i++, speculative_contexts_sexp);

    size_t scI = 0;
    for (auto& speculative_context : this->speculative_contexts) {
        SET_VECTOR_ELT(speculative_contexts_sexp, scI++,
                       serialization::to_sexp(speculative_context));
    }

    if ( compile_reasons.heuristic ){
        SET_VECTOR_ELT(sexp, i, compile_reasons.heuristic->toSEXP());
    }
    i++;

    if ( compile_reasons.condition ){
        SET_VECTOR_ELT(sexp, i, compile_reasons.condition->toSEXP());
    }
    i++;

    if( compile_reasons.osr ){
        SET_VECTOR_ELT(sexp, i, compile_reasons.osr->toSEXP());
    }
    i++;

    SET_VECTOR_ELT( sexp, i++, serialization::to_sexp(this->time_length) );
    SET_VECTOR_ELT( sexp, i++, serialization::to_sexp(this->subevents) );
    SET_VECTOR_ELT(sexp, i++, serialization::to_sexp(this->bitcode));
    SET_VECTOR_ELT(sexp, i++, serialization::to_sexp(this->succesful));

    UNPROTECT(1);
    return sexp;
}

void CompilationEvent::fromSEXP(SEXP sexp) {
    assert(Rf_length(sexp) == 11);
    this->closureIndex = serialization::uint64_t_from_sexp(VECTOR_ELT(sexp, 0));
    this->dispatch_context =
        serialization::uint64_t_from_sexp(VECTOR_ELT(sexp, 1));
    this->compileName = serialization::string_from_sexp(VECTOR_ELT(sexp, 2));
    auto speculative_contexts_sexp = VECTOR_ELT(sexp, 3);
    assert(Rf_isVector(speculative_contexts_sexp));
    for (auto i = 0; i < Rf_length(speculative_contexts_sexp); i++) {
        auto speculative_context = serialization::speculative_context_from_sexp(
            VECTOR_ELT(speculative_contexts_sexp, i));
        this->speculative_contexts.push_back(std::move(speculative_context));
    }

    this->compile_reasons.heuristic = serialization::compile_reason_from_sexp(VECTOR_ELT(sexp, 4));
    this->compile_reasons.condition = serialization::compile_reason_from_sexp(VECTOR_ELT(sexp, 5));
    this->compile_reasons.osr = serialization::compile_reason_from_sexp(VECTOR_ELT(sexp, 6));

    this->time_length = serialization::time_from_sexp(VECTOR_ELT(sexp, 7));
    this->subevents = serialization::vector_from_sexp<size_t, serialization::uint64_t_from_sexp>(VECTOR_ELT(sexp, 8));
    this->bitcode = serialization::string_from_sexp(VECTOR_ELT(sexp, 9));
    this->succesful = serialization::bool_from_sexp(VECTOR_ELT(sexp, 10));
}

DeoptEvent::DeoptEvent(size_t dispatchTableIndex, Context version,
                       DeoptReason::Reason reason,
                       std::pair<ssize_t, ssize_t> reasonCodeIdx,
                       uint32_t reasonCodeOff, SEXP trigger)
    : VersionEvent(dispatchTableIndex, version), reason_(reason),
      reasonCodeIdx_(std::move(reasonCodeIdx)), reasonCodeOff_(reasonCodeOff) {
    setTrigger(trigger);
}

DeoptEvent::~DeoptEvent() {
    if (trigger_) {
        setTrigger(nullptr);
    }
}

void DeoptEvent::setTrigger(SEXP newTrigger) {
    if (trigger_) {
        R_ReleaseObject(trigger_);
    }

    trigger_ = nullptr;
    triggerClosure_ = -1;

    if (newTrigger == nullptr) {
        return;
    }

    if (TYPEOF(newTrigger) == CLOSXP) {
        auto rec = recorder_.initOrGetRecording(newTrigger);
        triggerClosure_ = (ssize_t)rec.first;
        return;
    }

    if (newTrigger) {
        R_PreserveObject(newTrigger);
    }

    trigger_ = newTrigger;
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

bool DeoptEvent::containsReference(size_t recordingIdx) const {
    return (size_t)reasonCodeIdx_.first == recordingIdx ||
           (size_t)triggerClosure_ == recordingIdx ||
           VersionEvent::containsReference(recordingIdx);
}

void DeoptEvent::print(const std::vector<FunRecording>& mapping,
                       std::ostream& out) const {
    const auto& reasonRec = mapping[(size_t)this->reasonCodeIdx_.first];

    out << "DeoptEvent{ [version=" << this->version;
    out << "]\n        reason=" << this->reason_;
    out << ",\n        reasonCodeIdx=(" << reasonRec << ","
        << this->reasonCodeIdx_.second << ")";
    out << ",\n        reasonCodeOff=" << this->reasonCodeOff_ << "\n    }";
}

SEXP DeoptEvent::toSEXP() const {
    const char* fields[] = {
        "dispatchTable",   "version", "reason",         "reason_code_idx",
        "reason_code_off", "trigger", "triggerClosure", ""};
    auto sexp = PROTECT(Rf_mkNamed(VECSXP, fields));
    setClassName(sexp, R_CLASS_DEOPT_EVENT);

    int i = 0;
    SET_VECTOR_ELT(sexp, i++, serialization::to_sexp(dispatchTableIndex));
    SET_VECTOR_ELT(sexp, i++, serialization::to_sexp(this->version));
    SET_VECTOR_ELT(sexp, i++, serialization::to_sexp(this->reason_));
    SET_VECTOR_ELT(sexp, i++, serialization::to_sexp(this->reasonCodeOff_));
    SET_VECTOR_ELT(sexp, i++, serialization::to_sexp(this->reasonCodeIdx_));
    SET_VECTOR_ELT(sexp, i++, this->trigger_ ? this->trigger_ : R_NilValue);
    SET_VECTOR_ELT(sexp, i++, serialization::to_sexp(this->triggerClosure_));
    UNPROTECT(1);
    return sexp;
}

void DeoptEvent::fromSEXP(SEXP sexp) {
    assert(Rf_isVector(sexp));
    assert(Rf_length(sexp) == 7);

    int i = 0;
    this->dispatchTableIndex =
        serialization::uint64_t_from_sexp(VECTOR_ELT(sexp, i++));
    this->version = serialization::context_from_sexp(VECTOR_ELT(sexp, i++));
    this->reason_ =
        serialization::deopt_reason_from_sexp(VECTOR_ELT(sexp, i++));
    this->reasonCodeOff_ =
        serialization::uint32_t_from_sexp(VECTOR_ELT(sexp, i++));
    this->reasonCodeIdx_ =
        serialization::pair_from_sexp<int64_t, int64_t,
                                      serialization::int64_t_from_sexp,
                                      serialization::int64_t_from_sexp>(
            VECTOR_ELT(sexp, i++));
    SEXP trigger = VECTOR_ELT(sexp, i++);
    ssize_t triggerClosure =
        serialization::int64_t_from_sexp(VECTOR_ELT(sexp, i++));
    if (triggerClosure >= 0) {
        triggerClosure_ = triggerClosure;
    } else {
        assert(trigger);
        setTrigger(trigger);
    }
}

void DtInitEvent::print(const std::vector<FunRecording>& mapping,
                        std::ostream& out) const {
    out << "DtInitEvent{\n        invocations=" << this->invocations
        << "\n        deopts=" << this->deopts << "\n    }";
}

SEXP DtInitEvent::toSEXP() const {
    const char* fields[] = {"dispatchTable", "invocations", "deopts", ""};
    auto sexp = PROTECT(Rf_mkNamed(VECSXP, fields));
    setClassName(sexp, R_CLASS_DT_INIT_EVENT);

    int i = 0;
    SET_VECTOR_ELT(sexp, i++, serialization::to_sexp(dispatchTableIndex));
    SET_VECTOR_ELT(sexp, i++, serialization::to_sexp(this->invocations));
    SET_VECTOR_ELT(sexp, i++, serialization::to_sexp(this->deopts));
    UNPROTECT(1);
    return sexp;
}

void DtInitEvent::fromSEXP(SEXP sexp) {
    assert(Rf_isVector(sexp));
    assert(Rf_length(sexp) == 3);

    int i = 0;
    this->dispatchTableIndex =
        serialization::uint64_t_from_sexp(VECTOR_ELT(sexp, i++));
    this->invocations =
        serialization::uint64_t_from_sexp(VECTOR_ELT(sexp, i++));
    this->deopts = serialization::uint64_t_from_sexp(VECTOR_ELT(sexp, i++));
}

SEXP InvocationEvent::toSEXP() const {
    const char* fields[] = {"dispatchTable", "context", "deltaCount",
                            "deltaDeopt", "source", ""};
    auto sexp = PROTECT(Rf_mkNamed(VECSXP, fields));
    setClassName(sexp, R_CLASS_INVOCATION_EVENT);
    int i = 0;
    SET_VECTOR_ELT(sexp, i++,
                   PROTECT(serialization::to_sexp(dispatchTableIndex)));
    SET_VECTOR_ELT(sexp, i++, PROTECT(serialization::to_sexp(version)));
    SET_VECTOR_ELT(sexp, i++, PROTECT(serialization::to_sexp(deltaCount)));
    SET_VECTOR_ELT(sexp, i++, PROTECT(serialization::to_sexp(deltaDeopt)));
    SET_VECTOR_ELT(sexp, i++, PROTECT(serialization::to_sexp(source)));
    UNPROTECT(6);

    return sexp;
}

void InvocationEvent::fromSEXP(SEXP sexp) {
    assert(TYPEOF(sexp) == VECSXP);
    assert(Rf_length(sexp) == 5);
    int i = 0;
    dispatchTableIndex =
        serialization::uint64_t_from_sexp(VECTOR_ELT(sexp, i++));
    version = serialization::context_from_sexp(VECTOR_ELT(sexp, i++));
    deltaCount = serialization::int64_t_from_sexp(VECTOR_ELT(sexp, i++));
    deltaDeopt = serialization::uint64_t_from_sexp(VECTOR_ELT(sexp, i++));
    source = serialization::invocation_source_set_from_sexp(VECTOR_ELT(sexp, i++));
}

void InvocationEvent::print(const std::vector<FunRecording>& mapping,
                            std::ostream& out) const {
    out << std::dec << "Invocation{ [version=" << version << "] ";
    if (deltaCount > 0) {
        out << "invocations += " << deltaCount;
    } else if (deltaCount < 0) {
        out << "invocations -= " << -deltaCount;
    } else {
        out << "deoptCount += " << deltaDeopt;
    }
    out << " }";
}

#define RECORDER_FILTER_GUARD(field_name)                                      \
    if (!is_recording_ || !filter_.field_name)                        \
        return;

void recordCompile(SEXP cls, const std::string& name,
                   const Context& assumptions) {
    RECORDER_FILTER_GUARD(compile);

    auto rec = recorder_.initOrGetRecording(cls, name);
    if (rec.second.name == "" ){
        rec.second.name = name;
    }

    std::vector<SpeculativeContext> sc;
    auto dt = DispatchTable::unpack(BODY(cls));
    recorder_.recordSpeculativeContext(dt, sc);

    auto dispatch_context = assumptions.toI();

    compilation_stack_.emplace(
        std::piecewise_construct,
        std::forward_as_tuple(CompilationEvent::Clock::now()),
        std::forward_as_tuple(rec.first, dispatch_context, name, std::move(sc), std::move(compileReasons_))
    );
    recordReasonsClear();
}

void recordCompileFinish(bool succesful){
    RECORDER_FILTER_GUARD(compile);

    auto end_time = CompilationEvent::Clock::now();

    assert(!compilation_stack_.empty());
    auto start_time = compilation_stack_.top().first;
    auto event = std::make_unique<CompilationEvent>( std::move(compilation_stack_.top().second) );
    compilation_stack_.pop();

    auto duration = std::chrono::duration_cast<CompilationEvent::Duration>(end_time - start_time);
    event->set_time(duration);
    event->set_success(succesful);

    size_t idx = recorder_.push_event( std::move(event) );

    if ( !compilation_stack_.empty() ){
        compilation_stack_.top().second.add_subcompilation(idx);
    }
}

void recordOsrCompile(const SEXP cls) {
    RECORDER_FILTER_GUARD(compile);

    auto env = PROTECT(CLOENV(cls));
    auto symbols = PROTECT(R_lsInternal(env, TRUE));

    std::string name = "";

    auto size = Rf_length(symbols);
    for( int i = 0; i < size; i++ ){
        const char * symbol_char = CHAR( VECTOR_ELT(symbols, i) );
        auto symbol = PROTECT( Rf_install( symbol_char ) );

        auto value = PROTECT(Rf_findVarInFrame( env, symbol ));

        if ( value == cls ){
            name = symbol_char;
            UNPROTECT(2);
            break;
        }

        UNPROTECT(2);
    }

    UNPROTECT(2);

    recordCompile( cls, name, pir::Compiler::defaultContext );
}


void recordLLVMBitcode( llvm::Function* fun ) {
    RECORDER_FILTER_GUARD(compile);

    std::stringstream ss {};
    llvm::raw_os_ostream os {ss};

    fun->print( os );

    assert(!compilation_stack_.empty());
    compilation_stack_.top().second.set_bitcode( ss.str() );
}

size_t Record::indexOfBaseline(const rir::Code* code) {
    DispatchTable* dt = code->function()->dispatchTable();
    return initOrGetRecording(const_cast<DispatchTable*>(dt)).first;
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

void recordDeopt(rir::Code* c, const DispatchTable* dt, DeoptReason& reason,
                 SEXP trigger) {
    RECORDER_FILTER_GUARD(deopt);

    // find the affected version
    Context version;
    bool found = false;
    // it deopts from native so it cannot be the baseline RIR
    for (size_t i = 1; i < dt->size(); i++) {
        auto* fi = dt->get(i);
        if (fi->body() == c) {
            version = fi->context();
            found = true;
            break;
        }
    }

    if (!found) {
        version = c->function()->context();
    }

    auto reasonCodeIdx =
        recorder_.findIndex(dt->baseline()->body(), reason.origin.function()->body());

    assert(reasonCodeIdx.first >= 0 &&
           "Could not locate deopt reason location");

    recorder_.record<DeoptEvent>(dt, version, reason.reason, reasonCodeIdx,
                                 reason.origin.idx(), trigger);
}

void recordDtOverwrite(const DispatchTable* dt, size_t funIdx,
                       size_t oldDeoptCount) {
    if (!is_recording_) {
        return;
    }

    recorder_.record<DtInitEvent>(dt, funIdx, oldDeoptCount);
}

void recordInvocation(Function* f, ssize_t deltaCount,
                      size_t previousCount) {
    RECORDER_FILTER_GUARD(invoke);
    if (!is_recording_ || isPlayingCompile)
        return;

    Context version = f->context();
    auto* dt = f->dispatchTable();
    if (!dt) {
        return;
    }

    if (!recorder_.contains(dt)) {
        return;
    }

    recorder_.record<InvocationEvent>(dt, version, deltaCount, previousCount, invocation_source_);
    invocation_source_ = InvocationEvent::SourceSet::None();
}

void recordInvocationDoCall(){
    RECORDER_FILTER_GUARD(invoke);
    std::cout << "DoCall\n";
    invocation_source_.set(InvocationEvent::DoCall);
}

void recordInvocationNativeCallTrampoline(){
    RECORDER_FILTER_GUARD(invoke);
    std::cout << "Trampoline\n";
    invocation_source_.set(InvocationEvent::NativeCallTrampoline);
}

#define RECORD_SC_GUARD()                                                      \
    if (!is_recording_ || isPlayingCompile)                                    \
        return;

const Code* nextSCcode = nullptr;

int test = 0;
void prepareRecordSC(const Code* container) {
    RECORDER_FILTER_GUARD(typeFeedback);
    RECORD_SC_GUARD();
    test++;
    if (nextSCcode) {
        std::cerr << "prepared speculative context record twice" << std::endl;
        abort();
    } else {
        nextSCcode = container;
    }
}

inline const Code* getSCCode() {
    assert(nextSCcode && "recordSC wasn't prepared");
    auto* next = nextSCcode;
    nextSCcode = nullptr;
    return next;
}

void recordSC(const Opcode* immediate, const SpeculativeContext& sc) {
    RECORDER_FILTER_GUARD(typeFeedback);
    auto* c = getSCCode();
    ptrdiff_t offset = immediate - c->code();
    assert(offset > 0 && offset < (ptrdiff_t)c->size());

    auto* dt = c->function()->dispatchTable();
    auto* baseline = dt->baseline()->body();
    ssize_t codeIndex = -2;
    if (baseline == c) {
        codeIndex = -1;
    } else {
        for (size_t i = 0; i < baseline->extraPoolSize; i++) {
            SEXP epe = baseline->getExtraPoolEntry(i);
            if (Code::check(epe) == c) {
                codeIndex = i;
                break;
            }
        }
    }

    if (codeIndex == -2) {
        std::cerr << "[rec speculative ctx] code not found" << std::endl;
        return;
    }

    recorder_.record<SpeculativeContextEvent>(dt, codeIndex, (size_t &&) offset,
                                              sc);
}

void recordSC(const ObservedCallees& callees) {
    RECORDER_FILTER_GUARD(typeFeedback);
    RECORD_SC_GUARD();
    decltype(SpeculativeContext::Value::callees) targets;
    targets.fill(-1);

    auto* code = getSCCode();

    for (size_t i = 0; i < callees.numTargets; i++) {
        targets[i] =
            recorder_.initOrGetRecording(callees.getTarget(code->function(), i)).first;
    }

    nextSCcode = code;

    recordSC((Opcode*)&callees, SpeculativeContext(targets));
}

void recordSC(const ObservedTest& test) {
    RECORDER_FILTER_GUARD(typeFeedback);
    RECORD_SC_GUARD();
    recordSC((Opcode*)&test, SpeculativeContext(test));
}

void recordSC(const ObservedValues& type) {
    RECORDER_FILTER_GUARD(typeFeedback);
    RECORD_SC_GUARD();
    recordSC((Opcode*)&type, SpeculativeContext(type));
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

void printRecordings(std::ostream& out,
                     const std::vector<std::unique_ptr<rir::recording::Event>>& events,
                     const std::vector<FunRecording>& functions){
    for (auto& eventEntry : events) {
        const char* name = eventEntry->targetName(functions);

        // If name is empty (unknown), use a different display strategy
        if (*name == 0) {
            name = "<?>";
        }

        Prefixer prefixed(out, name);
        prefixed << "    ";
        eventEntry->print(functions, prefixed);
        prefixed << std::endl;
    }
}

// Compile heuristics
void recordMarkOptReasonHeuristic(){
    RECORDER_FILTER_GUARD(compile)
    compileReasons_.set_heuristic<MarkOptReason>();
}

void recordInvocationCountTimeReason( size_t count, size_t minimalCount, unsigned long time, unsigned long minimalTime ){
    RECORDER_FILTER_GUARD(compile)
    compileReasons_.set_heuristic<InvocationCountTimeReason>(count, minimalCount, time, minimalTime);
}

void recordPirWarmupReason( size_t invocation_count ){
    RECORDER_FILTER_GUARD(compile)
    compileReasons_.set_heuristic<PirWarmupReason>(invocation_count);
}

// Compile condition
void recordMarkOptReasonCondition(){
    RECORDER_FILTER_GUARD(compile)
    compileReasons_.set_condition<MarkOptReason>();
}

void recordNotOptimizedReason(){
    RECORDER_FILTER_GUARD(compile)
    compileReasons_.set_condition<NotOptimizedReason>();
}

void recordIsImprovingReason(){
    RECORDER_FILTER_GUARD(compile)
    compileReasons_.set_condition<IsImprovingReason>();
}

void recordReoptimizeFlagReason(){
    RECORDER_FILTER_GUARD(compile)
    compileReasons_.set_condition<ReoptimizeFlagReason>();
}

// OSR reason
void recordOsrTriggerCallerCalle(){
    RECORDER_FILTER_GUARD(compile)
    compileReasons_.set_osr<OSRCallerCalleeReason>();
}

void recordOsrTriggerLoop(size_t loopCount){
    RECORDER_FILTER_GUARD(compile)
    compileReasons_.set_osr<OSRLoopReason>( loopCount );
}

void recordReasonsClear(){
    compileReasons_.heuristic = nullptr;
    compileReasons_.condition = nullptr;
    compileReasons_.osr = nullptr;
}

const char * finalizerPath = nullptr;

void recordFinalizer(SEXP) {
    std::cerr << "Saving recording to \"" << finalizerPath << "\"\n";
    stopRecordings();

    SEXP filepath = PROTECT(Rf_mkString(finalizerPath));
    saveRecordings(filepath);
    UNPROTECT(1);
}

void recordExecution( const char* filePath, const char* filterArg ){
    if ( filterArg != nullptr ) {
        filter_ = {
            .compile = false,
            .deopt = false,
            .typeFeedback = false,
            .invoke = false
        };

        std::istringstream is ( filterArg );
        std::string str;

        while( std::getline( is, str, ',' ) ){
            if (str == "Compile") {
                filter_.compile = true;
            } else if (str == "Deopt") {
                filter_.deopt = true;
            } else if (str == "TypeFeedback") {
                filter_.typeFeedback = true;
            } else if (str == "Invoke") {
                filter_.invoke = true;
            } else {
                std::cerr
                    << "Unknown recording filter type: "
                    << str
                    << "\nValid flags are:\n- Compile\n- Deopt\n- TypeFeedback\n- Invoke\n";
                exit(1);
            }
        }
    }

    std::cerr << "Recording to \"" << filePath << "\" (environment variable";
    if ( filterArg != nullptr ){
        std::cerr << ": " << filterArg << ")\n";
    } else {
        std::cerr << ")\n";
    }
    startRecordings();

    finalizerPath = filePath;

    // Call `loadNamespace("Base")`
    SEXP baseStr = PROTECT(Rf_mkString("base"));
    SEXP expr = PROTECT(Rf_lang2(Rf_install("loadNamespace"), baseStr));
    SEXP namespaceRes = PROTECT(Rf_eval( expr, R_GlobalEnv ));

    if ( namespaceRes == R_NilValue ){
        std::cerr << "Failed to load namespace base\n";
        UNPROTECT(3);
        return;
    }

    R_RegisterCFinalizerEx(namespaceRes, &recordFinalizer, TRUE);
    UNPROTECT(3);
}

} // namespace recording
} // namespace rir

REXPORT SEXP filterRecordings(SEXP compile, SEXP deoptimize, SEXP typeFeedback,
                              SEXP invocation) {
    rir::recording::filter_ = {
        .compile = (bool)Rf_asLogical(compile),
        .deopt = (bool)Rf_asLogical(deoptimize),
        .typeFeedback = (bool)Rf_asLogical(typeFeedback),
        .invoke = (bool)Rf_asLogical(invocation),
    };

    return R_NilValue;
}

REXPORT SEXP startRecordings() {
    rir::recording::is_recording_ = true;
    rir::recording::recorder_.reset();
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
    // Incorrectly identified by cppcheck
    // cppcheck-suppress constArgument
    return Rf_ScalarLogical(rir::recording::is_recording_);
}

REXPORT SEXP saveRecordings(SEXP filename) {
    if (TYPEOF(filename) != STRSXP)
        Rf_error("must provide a string path");

    FILE* file = fopen(CHAR(Rf_asChar(filename)), "w");
    if (!file)
        Rf_error("couldn't open file at path");

    auto recordings = PROTECT(getRecordings());
    R_SaveToFile(recordings, file, 3);
    fclose(file);
    UNPROTECT(1);

    return R_NilValue;
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

REXPORT SEXP printRecordings(SEXP from) {
    auto& out = std::cout;
    out << "Recordings:" << std::endl;

    if (Rf_isNull(from)) {
        rir::recording::printRecordings(out,
                                        rir::recording::recorder_.log,
                                        rir::recording::recorder_.functions);
    } else {
        SEXP expr;
        if (Rf_isString(from)) {
            expr = PROTECT(loadRecordings(from));
        } else {
            expr = from;
        }

        assert(Rf_isVector(expr));
        assert(Rf_length(expr) == 2);

        // Populate functions
        auto bodies = VECTOR_ELT(expr, 0);
        size_t bodiesLength = Rf_length(bodies);

        std::vector<rir::recording::FunRecording> functions;
        functions.reserve(bodiesLength);

        for (size_t i = 0; i < bodiesLength; i++){
            functions.push_back(
                rir::recording::serialization::fun_recorder_from_sexp(
                    VECTOR_ELT(bodies, i)
                )
            );
        }

        // Populate events
        auto log = VECTOR_ELT( expr, 1 );
        size_t logLength = Rf_length(log);

        std::vector<std::unique_ptr<rir::recording::Event>> logVector;
        logVector.reserve(logLength);

        for (size_t i = 0; i < logLength; i++){
            logVector.push_back(
                rir::recording::serialization::event_from_sexp(
                    VECTOR_ELT( log, i )
                )
            );
        }

        rir::recording::printRecordings( out, logVector, functions );

        if (expr != from){
            UNPROTECT(1);
        }
    }

    return R_NilValue;
}

REXPORT SEXP printEventPart(SEXP obj, SEXP type, SEXP functions) {
    if (Rf_isNull(obj)) {
        return Rf_mkString("");
    }

    if (!Rf_isString(type)) {
        std::cerr << "type parameter is not a string" << std::endl;
        return R_NilValue;
    }

    auto type_str = std::string(CHAR(STRING_ELT(type, 0)));

    std::ostringstream ss;

    if (type_str == "context") { // version or dispatch context
        auto disp = rir::recording::serialization::context_from_sexp(obj);
        ss << disp;
    } else if (type_str == "speculative") {
        auto sc =
             rir::recording::serialization::speculative_context_from_sexp(obj);

        switch (sc.type) {
        case rir::recording::SpeculativeContextType::Callees: {
            ss << "Callees[";
            bool first = true;
            for (auto c : sc.value.callees) {
                if (c == NO_INDEX)
                    break;
                if (first){
                    first = false;
                }
                else {
                    ss << ',';
                }

                auto fun =
                        rir::recording::serialization::fun_recorder_from_sexp(VECTOR_ELT( functions, c ));
                ss << fun;
            }
            ss << "]";
            break;
        }

        case rir::recording::SpeculativeContextType::Test:
            ss << "Test{";
            switch (sc.value.test.seen) {
            case rir::ObservedTest::None:
                ss << "None";
                break;
            case rir::ObservedTest::OnlyTrue:
                ss << "OnlyTrue";
                break;
            case rir::ObservedTest::OnlyFalse:
                ss << "OnlyFalse";
                break;
            case rir::ObservedTest::Both:
                ss << "Both";
                break;
            }
            ss << "}";
            break;

        case rir::recording::SpeculativeContextType::Values:
            ss << "Values{";
            sc.value.values.print(ss);
            ss << "}";
            break;
        }
    } else if (type_str == "reason") {
        auto ev = rir::recording::serialization::compile_reason_from_sexp(obj);
        ev->print(ss);
    } else if ( type_str == "invocation_source" ){
        auto src = rir::recording::serialization::invocation_source_set_from_sexp(obj);

        if (src.contains(rir::recording::InvocationEvent::DoCall) && src.contains(rir::recording::InvocationEvent::NativeCallTrampoline)){
            ss << "DoCall,NativeCallTrampoline";
        } else if (src.contains(rir::recording::InvocationEvent::DoCall)){
            ss << "DoCall";
        } else if (src.contains(rir::recording::InvocationEvent::NativeCallTrampoline)){
            ss << "NativeCallTrampoline";
        }

    } else if (type_str == "deopt_reason") {
        auto reason = rir::recording::serialization::deopt_reason_from_sexp(obj);

        switch (reason) {
        case rir::DeoptReason::Reason::Typecheck:
            ss << "Typecheck";
            break;

        case rir::DeoptReason::Reason::DeadCall:
            ss << "DeadCall";
            break;

        case rir::DeoptReason::Reason::CallTarget:
            ss << "CallTarget";
            break;

        case rir::DeoptReason::Reason::ForceAndCall:
            ss << "ForceAndCall";
            break;

        case rir::DeoptReason::Reason::EnvStubMaterialized:
            ss << "EnvStubMaterialized";
            break;

        case rir::DeoptReason::Reason::DeadBranchReached:
            ss << "DeadBranchReached";
            break;

        case rir::DeoptReason::Reason::Unknown:
            ss << "Unknown";
            break;
        }
    } else {
        std::cerr << "type parameter '" << type_str
                  << "' is not a known type (context,speculative,reason,deopt_reason)"
                  << std::endl;
        return R_NilValue;
    }

    return Rf_mkString(ss.str().c_str());
}
