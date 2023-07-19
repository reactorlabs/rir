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

#include "prefixer.cpp"

namespace rir {
namespace recording {

// global state
// a flag indicating whether the recording is active or not
static bool is_recording_ = false;
// the main recorder
static Record recorder_;

static int isReplayingSC = 0;

// Don't record invocations while replaying compile events
static bool isPlayingCompile = false;

void Record::record(const DispatchTable* dt, std::unique_ptr<Event> event) {
    auto entry = initOrGetRecording(dt);
    log.push_back({entry.first, std::move(event)});
}

void Record::record(const SEXP cls, std::string name,
                    std::unique_ptr<Event> event) {
    auto entry = initOrGetRecording(cls, name);
    log.push_back({entry.first, std::move(event)});
}

void Record::record(const SEXP cls, std::unique_ptr<Event> event) {
    record(cls, "", std::move(event));
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

    if (v->name.empty() && !name.empty()) {
        v->name = name;
    }

    // special functions don't have dispatch tables, let's not error here
    if (TYPEOF(BODY(cls)) == EXTERNALSXP) {
        auto dt = DispatchTable::unpack(BODY(cls));
        if (r.second) {
            assert(dt->size() == 1);
            auto* base = dt->baseline();
            log.push_back({r.first->second,
                           std::make_unique<DtInitEvent>(
                               base->invocationCount(), base->deoptCount())});
        }
        dt_to_recording_index_.insert({dt, r.first->second});
    } else if (!Rf_isNull(BODY(cls))) {
        auto type = Rf_type2char(TYPEOF(BODY(cls)));
        std::cerr << "saw special function of type " << type << std::endl;
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
        FunRecording dummyRecorder = {std::move(name), {}, R_NilValue};

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

SEXP Record::save() {
    // Check if we have dispatch tables without an associated SEXP and remove
    // related events. Might be avoidable eventually.
    size_t recIdx = 0;
    for (auto& recording : fun_recordings_) {
        if (Rf_isNull(recording.closure)) {
            auto refersToRecording =
                [recIdx](
                    std::pair<size_t, std::unique_ptr<rir::recording::Event>>&
                        entry) {
                    return entry.first == recIdx ||
                           entry.second->containsReference(recIdx);
                };

            log.erase(std::remove_if(log.begin(), log.end(), refersToRecording),
                      log.end());
        }
        recIdx++;
    }

    const char* fields[] = {"bodies", "events", ""};
    auto recordSexp = PROTECT(Rf_mkNamed(VECSXP, fields));
    auto bodiesSexp = PROTECT(serialization::to_sexp(fun_recordings_));
    auto eventsSexp = PROTECT(serialization::to_sexp(log));

    SET_VECTOR_ELT(recordSexp, 0, bodiesSexp);
    SET_VECTOR_ELT(recordSexp, 1, eventsSexp);

    UNPROTECT(3);
    return recordSexp;
}

// Plays along nicer with diff tools
#define HIDE_UNKNOWN_CLOSURE_POINTER true

void Record::printRecordings(std::ostream& out) {
    for (auto& eventEntry : log) {
        auto& body = fun_recordings_[eventEntry.first];

        const char* name = body.name.c_str();
        std::string ptr_str;

        // If name is empty (unknown), use a different display strategy
        if (*name == 0) {
            if (!HIDE_UNKNOWN_CLOSURE_POINTER) {
                std::stringstream buf;
                buf << (void*)body.closure;
                ptr_str = buf.str();
                name = ptr_str.c_str();
            } else {
                name = "<?>";
            }
        }

        Prefixer prefixed(out, name);
        prefixed << "    ";
        eventEntry.second->print(fun_recordings_, prefixed);
        prefixed << std::endl;
    }
}

std::ostream& operator<<(std::ostream& out, const FunRecording& that) {
    if (that.name.length()) {
        out << that.name;
    } else if (HIDE_UNKNOWN_CLOSURE_POINTER) {
        out << "<?>";
    } else {
        out << (void*)that.closure;
    }

    return out;
}

Replay::Replay(SEXP recordings) {
    PROTECT(recordings);
    assert(Rf_isVector(recordings));
    assert(Rf_length(recordings) == 2);

    SEXP bodiesSexp = VECTOR_ELT(recordings, 0);
    auto n = Rf_length(bodiesSexp);
    closures_.reserve(n);
    bodies.reserve(n);
    for (auto i = 0; i < n; i++) {
        closures_.push_back(R_NilValue);
        SEXP bodySexp = VECTOR_ELT(bodiesSexp, i);
        bodies.push_back(serialization::fun_recorder_from_sexp(bodySexp));
    }

    log = VECTOR_ELT(recordings, 1);
    R_PreserveObject(log);

    UNPROTECT(1);
}

Replay::~Replay() { R_ReleaseObject(log); }

size_t Replay::getEventCount() { return (size_t)Rf_length(log); }

std::pair<size_t, std::unique_ptr<Event>> Replay::getEvent(size_t idx) {
    assert(idx < getEventCount());
    SEXP rawEventEntry = VECTOR_ELT(log, idx);
    return serialization::pair_from_sexp<size_t, std::unique_ptr<Event>,
                                         serialization::uint64_t_from_sexp,
                                         serialization::event_from_sexp>(
        rawEventEntry);
}

SEXP Replay::replayClosure(size_t idx) {
    SEXP closure = closures_.at(idx);
    if (closure != R_NilValue) {
        return closure;
    }

    const FunRecording& recording = bodies.at(idx);
    assert(!Rf_isNull(recording.closure));

    SEXP name = R_NilValue;
    if (!recording.name.empty()) {
        name = Rf_install(recording.name.c_str());
    }

    closure = PROTECT(R_unserialize(recording.closure, R_NilValue));
    if (Rf_isNull(closure)) {
        UNPROTECT(1);
        return R_NilValue;
    }

    // TODO: the env parameter is likely not correct
    rirCompile(closure, R_GlobalEnv);
    closures_[idx] = closure;

    if (name != R_NilValue) {
        SEXP env = getEnvironment(recording.env);

        if (env == R_GlobalEnv) {
            Rf_defineVar(name, closure, R_GlobalEnv);
        } else if (env != R_UnboundValue) {
            SEXP existing_closure = Rf_findFun(name, env);
            if (existing_closure != R_UnboundValue) {
                BODY(existing_closure) = BODY(closure);
                assert(BODY(existing_closure));
            }
        }
    }

    UNPROTECT(1);

    return closure;
}

size_t Replay::replay() {
    auto n = getEventCount();

    for (size_t i = 0; i < n; i++) {
        auto entry = getEvent(i);
        auto cls = PROTECT(replayClosure(entry.first));
        const auto& body = bodies.at(entry.first);
        entry.second->replay(*this, cls, const_cast<std::string&>(body.name));
        UNPROTECT(1);
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

            // TODO not sure if this is correct, but it fixes an issue
            feedback->numTargets = 0;
            for (auto callee_idx : callees_idx) {
                if (callee_idx == NO_INDEX) {
                    continue;
                }

                SEXP callee = PROTECT(replayClosure(callee_idx));
                assert(Rf_isFunction(callee));
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
        out << "Test";
        break;
    case SpeculativeContextType::Values:
        out << "Values";
        break;
    }
    unsigned char rawValue[sizeof(SpeculativeContext::Value)];
    memcpy(rawValue, (void*)&value, sizeof(SpeculativeContext::Value));
    out << "{0x";
    for (size_t i = 0; i < sizeof(SpeculativeContext::Value); i++) {
        out << std::hex << ((unsigned int)(uint8_t)rawValue[i]);
    }
    out << "}";
}

SEXP SpeculativeContextEvent::toSEXP() const {
    SEXP sexp = PROTECT(Rf_allocVector(VECSXP, 3));
    setClassName(sexp, R_CLASS_SC_EVENT);
    size_t i = 0;
    assert(codeIndex != -2);
    SET_VECTOR_ELT(sexp, i++, serialization::to_sexp(codeIndex));
    SET_VECTOR_ELT(sexp, i++, serialization::to_sexp(offset));
    SET_VECTOR_ELT(sexp, i++, serialization::to_sexp(sc));
    UNPROTECT(1);
    return sexp;
}

void SpeculativeContextEvent::fromSEXP(SEXP sexp) {
    assert(Rf_isVector(sexp));
    assert(Rf_length(sexp) == 3);
    size_t i = 0;
    codeIndex = serialization::int64_t_from_sexp(VECTOR_ELT(sexp, i++));
    assert(codeIndex >= -1);
    offset = serialization::uint64_t_from_sexp(VECTOR_ELT(sexp, i++));
    sc = serialization::speculative_context_from_sexp(VECTOR_ELT(sexp, i++));
}

void SpeculativeContextEvent::replay(Replay& replay, SEXP closure,
                                     std::string& closure_name) const {
    auto& baseline = *DispatchTable::unpack(BODY(closure))->baseline();
    Code* c = nullptr;
    if (codeIndex == -1) {
        c = baseline.body();
    } else if (codeIndex >= 0) {
        c = baseline.body()->getPromise((size_t)codeIndex);
    }

    assert(offset < c->size());
    Opcode* immediate = c->code() + offset;
    Opcode* opcode = immediate - 1;
    auto bc = BC::decode(opcode, c);

    switch (sc.type) {
    case SpeculativeContextType::Callees: {
        assert(bc.is(Opcode::record_call_));
        auto& oc = *(ObservedCallees*)immediate;

        oc.numTargets = 0;
        for (auto callee : sc.value.callees) {
            if (callee == (size_t)-1)
                break;

            SEXP cls = PROTECT(replay.replayClosure(callee));
            assert(!Rf_isNull(cls));
            isReplayingSC++;
            oc.record(c, cls);
            isReplayingSC--;
            UNPROTECT(1);
        }

        prepareRecordSC(c);
        recordSC(oc);
        break;
    }
    case SpeculativeContextType::Test: {
        assert(bc.is(Opcode::record_test_));
        auto& ot = *(ObservedTest*)immediate;
        ot = sc.value.test;

        prepareRecordSC(c);
        recordSC(ot);
        break;
    }
    case SpeculativeContextType::Values: {
        assert(bc.is(Opcode::record_type_));
        auto& ov = *(ObservedValues*)immediate;
        ov = sc.value.values;

        prepareRecordSC(c);
        recordSC(ov);
        break;
    }
    }
}

bool SpeculativeContextEvent::containsReference(size_t dispatchTable) const {
    if (sc.type == SpeculativeContextType::Callees) {
        const auto& callees = sc.value.callees;
        const size_t* found =
            std::find(callees.begin(), callees.end(), dispatchTable);

        if (found != callees.end())
            return true;
    }

    return false;
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

void CompilationEvent::replay(Replay& replay, SEXP closure,
                              std::string& closure_name) const {
    isPlayingCompile = true;
    isReplayingSC++;
    auto start = speculative_contexts.begin();
    auto end = speculative_contexts.end();
    auto dt = DispatchTable::unpack(BODY(closure));
    replay.replaySpeculativeContext(dt, start, end);
    pirCompile(closure, Context(this->dispatch_context), closure_name,
               pir::DebugOptions::DefaultDebugOptions);
    isReplayingSC--;
    isPlayingCompile = false;
}

bool CompilationEvent::containsReference(size_t dispatchTable) const {
    for (auto& sc : speculative_contexts) {
        if (sc.type == SpeculativeContextType::Callees) {
            const auto& callees = sc.value.callees;
            const size_t* found =
                std::find(callees.begin(), callees.end(), dispatchTable);

            if (found != callees.end())
                return true;
        }
    }

    return false;
}

void CompilationEvent::print(const std::vector<FunRecording>& mapping,
                             std::ostream& out) const {
    out << "CompilationEvent{\n        dispatch_context="
        << this->dispatch_context << ",\n        speculative_contexts=[\n";
    for (auto& spec : this->speculative_contexts) {
        out << "            ";
        spec.print(mapping, out);
        out << "\n";
    }
    out << "        ]\n    }";
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
                       std::pair<ssize_t, ssize_t> reasonCodeIdx,
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
    Function* fun = nullptr;
    Code* code = nullptr;
    for (size_t i = 0; i < dt->size(); i++) {
        auto* fi = dt->get(i);
        if (fi->context().toI() == functionIdx_) {
            fun = fi;
        }
    }

    if (fun) {
        code = fun->body();
    } else {
        std::cerr << "didn't find code" << std::endl;
        return;
    }

    // Copy and set current closure's code
    Code* deoptSrcCode =
        retrieveCodeFromIndex(replay.closures_, this->reasonCodeIdx_);
    Opcode* deoptPc = (Opcode*)((uintptr_t)deoptSrcCode + reasonCodeOff_);
    FeedbackOrigin origin(deoptSrcCode, deoptPc);
    DeoptReason reason(origin, this->reason_);

    recordDeopt(code, closure, reason, this->trigger_);

    isReplayingSC++;
    reason.record(this->trigger_);
    isReplayingSC--;
    if (fun) {
        // We're doing fun->registerDeopt(), but without the incrementation
        assert(fun->isOptimized());
        fun->flags.set(Function::Flag::Deopt);
    }
}

bool DeoptEvent::containsReference(size_t dispatchTable) const {
    return reasonCodeIdx_.first >= 0 &&
           (size_t)reasonCodeIdx_.first == dispatchTable;
}

void DeoptEvent::print(const std::vector<FunRecording>& mapping,
                       std::ostream& out) const {
    auto& reasonRec = mapping[(size_t)this->reasonCodeIdx_.first];

    out << "DeoptEvent{ [funIdx=" << this->functionIdx_;
    out << "]\n        reason=" << this->reason_;
    out << ",\n        reasonCodeIdx=(" << reasonRec << ","
        << this->reasonCodeIdx_.second << ")";
    out << ",\n        reasonCodeOff=" << this->reasonCodeOff_ << "\n    }";
}

SEXP DeoptEvent::toSEXP() const {
    const char* fields[] = {"functionIdx",     "reason",  "reason_code_idx",
                            "reason_code_off", "trigger", ""};
    auto sexp = PROTECT(Rf_mkNamed(VECSXP, fields));
    setClassName(sexp, R_CLASS_DEOPT_EVENT);

    int i = 0;
    SET_VECTOR_ELT(sexp, i++, serialization::to_sexp(this->functionIdx_));
    SET_VECTOR_ELT(sexp, i++, serialization::to_sexp(this->reason_));
    SET_VECTOR_ELT(sexp, i++, serialization::to_sexp(this->reasonCodeOff_));
    SET_VECTOR_ELT(sexp, i++, serialization::to_sexp(this->reasonCodeIdx_));
    SET_VECTOR_ELT(sexp, i++, this->trigger_);
    UNPROTECT(1);
    return sexp;
}

void DeoptEvent::fromSEXP(SEXP sexp) {
    assert(Rf_isVector(sexp));
    assert(Rf_length(sexp) == 5);

    int i = 0;
    this->functionIdx_ =
        serialization::uint64_t_from_sexp(VECTOR_ELT(sexp, i++));
    this->reason_ =
        serialization::deopt_reason_from_sexp(VECTOR_ELT(sexp, i++));
    this->reasonCodeOff_ =
        serialization::uint32_t_from_sexp(VECTOR_ELT(sexp, i++));
    this->reasonCodeIdx_ =
        serialization::pair_from_sexp<int64_t, int64_t,
                                      serialization::int64_t_from_sexp,
                                      serialization::int64_t_from_sexp>(
            VECTOR_ELT(sexp, i++));
    this->trigger_ = VECTOR_ELT(sexp, i++);
    R_PreserveObject(this->trigger_);
}

void DtInitEvent::replay(Replay& replay, SEXP closure,
                         std::string& closure_name) const {
    DispatchTable* dt = DispatchTable::unpack(BODY(closure));
    dt->baseline()->init(invocations, deopts);
    recordDtOverwrite(dt, invocations, deopts);
}

void DtInitEvent::print(const std::vector<FunRecording>& mapping,
                        std::ostream& out) const {
    out << "DtInitEvent{\n        invocations=" << this->invocations
        << "\n        deopts=" << this->deopts << "\n    }";
}

SEXP DtInitEvent::toSEXP() const {
    const char* fields[] = {"version_index", "old_deopt_count", ""};
    auto sexp = PROTECT(Rf_mkNamed(VECSXP, fields));
    setClassName(sexp, R_CLASS_DT_INIT_EVENT);

    int i = 0;
    SET_VECTOR_ELT(sexp, i++, serialization::to_sexp(this->invocations));
    SET_VECTOR_ELT(sexp, i++, serialization::to_sexp(this->deopts));
    UNPROTECT(1);
    return sexp;
}

void DtInitEvent::fromSEXP(SEXP sexp) {
    assert(Rf_isVector(sexp));
    assert(Rf_length(sexp) == 2);

    int i = 0;
    this->invocations =
        serialization::uint64_t_from_sexp(VECTOR_ELT(sexp, i++));
    this->deopts = serialization::uint64_t_from_sexp(VECTOR_ELT(sexp, i++));
}

SEXP InvocationEvent::toSEXP() const {
    const char* fields[] = {"oldDtSize", "context", "deltaCount", "deltaDeopt",
                            ""};
    auto sexp = PROTECT(Rf_mkNamed(VECSXP, fields));
    setClassName(sexp, R_CLASS_INVOCATION_EVENT);
    int i = 0;
    SET_VECTOR_ELT(sexp, i++, PROTECT(serialization::to_sexp(dtSize)));
    SET_VECTOR_ELT(sexp, i++, PROTECT(serialization::to_sexp(funIdx)));
    SET_VECTOR_ELT(sexp, i++, PROTECT(serialization::to_sexp(deltaCount)));
    SET_VECTOR_ELT(sexp, i++, PROTECT(serialization::to_sexp(deltaDeopt)));
    UNPROTECT(i + 1);
    return sexp;
}

void InvocationEvent::fromSEXP(SEXP sexp) {
    assert(TYPEOF(sexp) == VECSXP);
    assert(Rf_length(sexp) == 4);
    int i = 0;
    dtSize = serialization::uint64_t_from_sexp(VECTOR_ELT(sexp, i++));
    funIdx = serialization::uint64_t_from_sexp(VECTOR_ELT(sexp, i++));
    deltaCount = serialization::int64_t_from_sexp(VECTOR_ELT(sexp, i++));
    deltaDeopt = serialization::uint64_t_from_sexp(VECTOR_ELT(sexp, i++));
}

void InvocationEvent::replay(Replay& replay, SEXP closure,
                             std::string& closure_name) const {
    auto* dt = DispatchTable::unpack(BODY(closure));

    Function* f = nullptr;
    for (size_t i = 0; i < dt->size(); i++) {
        auto fi = dt->get(i);
        if (fi->context() == Context(funIdx)) {
            f = fi;
        }
    }

    if (!f) {
        std::cerr << "couldn't find\n  " << Context(funIdx) << "  in [\n";
        for (size_t i = 0; i < dt->size(); i++) {
            std::cerr << "  " << Context(dt->get(i)->context()) << "\n";
        }
        std::cerr << "]" << std::endl;
        return;
    }

    if (deltaCount > 0) {
        for (ssize_t i = 0; i < deltaCount; i++) {
            f->registerInvocation();
            f->registerEndInvocation();
        }
    } else if (deltaCount < 0) {
        auto remaining = -deltaCount;
        for (ssize_t i = 0; i < remaining; i++) {
            f->unregisterInvocation();
        }
    } else {
        f->addDeoptCount(deltaDeopt);
    }
}

void InvocationEvent::print(const std::vector<FunRecording>& mapping,
                            std::ostream& out) const {
    out << std::dec << "Invocation{ [funIdx=" << funIdx << "] ";
    if (deltaCount > 0) {
        out << "invocations += " << deltaCount;
    } else if (deltaCount < 0) {
        out << "invocations -= " << -deltaCount;
    } else {
        out << "deoptCount += " << deltaDeopt;
    }
    out << " }";
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
    recorder_.record(cls, name,
                     std::make_unique<CompilationEvent>(std::move(event)));
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
    size_t funIdx = 0;
    bool found = false;
    auto dt = DispatchTable::unpack(BODY(cls));
    // it deopts from native so it cannot be the baseline RIR
    for (size_t i = 1; i < dt->size(); i++) {
        auto* fi = dt->get(i);
        if (fi->body() == c) {
            funIdx = fi->context().toI();
            found = true;
            break;
        } else if (fi->overridenBy && fi->overridenBy->body() == c) {
            std::cerr << "found in override" << std::endl;
            assert(fi->context() == fi->overridenBy->context());
            funIdx = fi->context().toI();
            found = true;
            break;
        }
    }

    if (!found) {
        std::cerr << "not found" << std::endl;
        funIdx = c->function()->context().toI();
    }

    auto reasonCodeIdx =
        recorder_.findIndex(dt->baseline()->body(), reason.srcCode());

    assert(reasonCodeIdx.first >= 0 &&
           "Could not locate deopt reason location");

    DeoptEvent event(funIdx, reason.reason, reasonCodeIdx,
                     reason.origin.offset(), trigger);
    recorder_.record(cls, std::make_unique<DeoptEvent>(std::move(event)));
}

void recordDtOverwrite(const DispatchTable* dt, size_t funIdx,
                       size_t oldDeoptCount) {
    if (!is_recording_) {
        return;
    }

    DtInitEvent event(funIdx, oldDeoptCount);
    recorder_.record(dt, std::make_unique<DtInitEvent>(std::move(event)));
}

void recordInvocation(const Function* f, ssize_t deltaCount,
                      size_t previousCount) {
    if (!is_recording_ || isPlayingCompile)
        return;

    size_t funIdx = f->context().toI();
    auto* dt = f->dispatchTable(false);
    if (!dt) {
        if (f->overridenBy) {
            std::cerr << "[rec invocation] function was overriden" << std::endl;
        } else {
            std::cerr << "[rec invocation] function not found" << std::endl;
        }
        return;
    }

    if (!recorder_.contains(dt)) {
        return;
    }

    InvocationEvent event(dt->size(), funIdx, deltaCount, previousCount);
    recorder_.record(dt, std::make_unique<InvocationEvent>(std::move(event)));
}

#define RECORD_SC_GUARD()                                                      \
    if (!is_recording_ || isReplayingSC || isPlayingCompile)                   \
        return;

const Code* nextSCcode = nullptr;

int test = 0;
void prepareRecordSC(const Code* container) {
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

void recordSC(const Opcode* immediate, SpeculativeContext sc) {
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

    SpeculativeContextEvent event(codeIndex, offset, sc);
    recorder_.record(
        dt, std::make_unique<SpeculativeContextEvent>(std::move(event)));
}

void recordSC(const ObservedCallees& callees) {
    RECORD_SC_GUARD();
    decltype(SpeculativeContext::Value::callees) targets;
    targets.fill(-1);

    auto* code = getSCCode();

    for (size_t i = 0; i < callees.numTargets; i++) {
        targets[i] = recorder_.initOrGetRecording(callees.getTarget(code, i)).first;
    }

    nextSCcode = code;

    recordSC((Opcode*)&callees, SpeculativeContext(targets));
}

void recordSC(const ObservedTest& test) {
    RECORD_SC_GUARD();
    recordSC((Opcode*)&test, SpeculativeContext(test));
}

void recordSC(const ObservedValues& type) {
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

void printRecordings(std::ostream& out) { recorder_.printRecordings(out); }

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

REXPORT SEXP replayRecordings(SEXP recordings, bool startRecording) {
    rir::recording::Replay replay(recordings);
    if (startRecording)
        startRecordings();
    auto res = replay.replay();

    return Rf_ScalarInteger(res);
}

REXPORT SEXP replayRecordingsFromFile(SEXP filename, bool startRecording) {
    auto recordings = loadRecordings(filename);
    auto res = replayRecordings(recordings, startRecording);

    return res;
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

REXPORT SEXP printRecordings(SEXP filename, SEXP fromFile) {
    auto& out = std::cout;
    out << "Recordings:" << std::endl;

    if (Rf_isNull(fromFile)) {
        rir::recording::printRecordings(out);
    } else {
        auto recordingsSexp = PROTECT(loadRecordings(fromFile));
        rir::recording::Replay replay(recordingsSexp);

        auto eventCount = replay.getEventCount();
        for (size_t idx = 0; idx < eventCount; idx++) {
            const auto eventEntry = replay.getEvent(idx);
            const auto& body = replay.bodies[eventEntry.first];

            const char* name = body.name.c_str();
            std::string ptr_str;

            // If name is empty (unknown), use a different display strategy
            if (*name == 0) {
                if (!HIDE_UNKNOWN_CLOSURE_POINTER) {
                    std::stringstream buf;
                    buf << (void*)body.closure;
                    ptr_str = buf.str();
                    name = ptr_str.c_str();
                } else {
                    name = "<?>";
                }
            }

            Prefixer prefixed(out, name);
            prefixed << "    ";
            eventEntry.second->print(replay.bodies, prefixed);
            prefixed << std::endl;
        }

        UNPROTECT(1);
    }

    return R_NilValue;
}
