#include "recording.h"
#include "R/Serialize.h"
#include "R/r.h"
#include "Rinternals.h"
#include "compiler/compiler.h"
#include "recording_serialization.h"
#include "runtime/Context.h"
#include "runtime/DispatchTable.h"
#include "runtime/Function.h"
#include "runtime/TypeFeedback.h"

#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <memory>
#include <ostream>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

namespace rir {
namespace recording {


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

extern Record recorder_;

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

