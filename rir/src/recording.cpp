#ifdef RECORDING

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

SEXP PirWarmupReason::toSEXP() const {
    auto vec = PROTECT(this->CompileReasonImpl::toSEXP());
    SET_VECTOR_ELT(vec, 0, serialization::to_sexp(invocationCount));

    UNPROTECT(1);
    return vec;
}

void PirWarmupReason::fromSEXP(SEXP sexp) {
    this->CompileReasonImpl::fromSEXP(sexp);

    this->invocationCount =
        serialization::uint64_t_from_sexp(VECTOR_ELT(sexp, 0));
}

SEXP OSRLoopReason::toSEXP() const {
    auto vec = PROTECT(this->CompileReasonImpl::toSEXP());

    SET_VECTOR_ELT(vec, 0, serialization::to_sexp(loopCount));

    UNPROTECT(1);
    return vec;
}

void OSRLoopReason::fromSEXP(SEXP sexp) {
    this->CompileReasonImpl::fromSEXP(sexp);

    this->loopCount = serialization::uint64_t_from_sexp(VECTOR_ELT(sexp, 0));
}

size_t Record::initOrGetRecording(const SEXP cls, const std::string& name) {
    if (TYPEOF(cls) == EXTERNALSXP) {
        auto dt = DispatchTable::unpack(cls);
        size_t idx = initOrGetRecording(dt);
        auto& entry = get_recording(idx);
        if (entry.name.empty()) {
            entry.name = name;
        }

        return idx;
    }

    assert(Rf_isFunction(cls));
    auto body = BODY(cls);

    auto getClosure = [cls]() {
        auto closure = PROTECT(
            R_serialize(cls, R_NilValue, R_NilValue, R_NilValue, R_NilValue));
        R_PreserveObject(closure);
        UNPROTECT(1);
        return closure;
    };

    // Primitives are stored as a special case
    if (TYPEOF(cls) == SPECIALSXP || TYPEOF(cls) == BUILTINSXP) {
        auto primIdx = cls->u.primsxp.offset;

        auto primEntry = primitive_to_body_index.find(primIdx);
        if (primEntry != primitive_to_body_index.end()) {
            // Closure is already there, we do not add the env name
            return primEntry->second;
        }

        size_t idx = functions.size();
        functions.emplace_back(primIdx, getClosure());

        return idx;
    }

    assert(TYPEOF(cls) == CLOSXP);
    auto dt = DispatchTable::check(body);
    auto envName = getEnvironmentName(CLOENV(cls));

    // Getting the closure name can be expensive
    // -> thus it is computed lazily only when needed
    auto getName = [&name, cls]() {
        if (name.empty()) {
            return getClosureName(cls);
        }

        return name;
    };

    // If the function recording does not contain it, add to it the closure
    // and environment name
    auto fixupRecording = [&getClosure, &getName, &envName](FunRecording& rec) {
        if (Rf_isNull(rec.closure)) {
            rec.closure = getClosure();
        }

        if (rec.name.empty()) {
            rec.name = getName();
        }

        if (rec.env.empty()) {
            rec.env = envName;
        }
    };

    if (dt == nullptr) {
        auto bcodeEntry = bcode_to_body_index.find(body);
        if (bcodeEntry != bcode_to_body_index.end()) {
            fixupRecording(get_recording(bcodeEntry->second));
            return bcodeEntry->second;
        }

        R_PreserveObject(body);
        auto idx = functions.size();
        bcode_to_body_index.emplace(body, idx);

        functions.emplace_back(getName(), envName, getClosure());

        return idx;
    } else {
        auto dtEntry = dt_to_recording_index_.find(dt);
        if (dtEntry != dt_to_recording_index_.end()) {
            fixupRecording(get_recording(dtEntry->second));
            return dtEntry->second;
        }

        // Container of dispatch table
        R_PreserveObject(body);
        auto idx = functions.size();
        dt_to_recording_index_.emplace(dt, idx);

        functions.emplace_back(getName(), envName, getClosure());
        return idx;
    }
}

size_t Record::initOrGetRecording(const DispatchTable* dt) {
    assert(dt != nullptr);

    auto dt_index = dt_to_recording_index_.find(dt);
    if (dt_index != dt_to_recording_index_.end()) {
        return dt_index->second;
    }

    R_PreserveObject(dt->container());
    auto insertion_index = functions.size();
    dt_to_recording_index_.emplace(dt, insertion_index);

    functions.emplace_back("");
    return insertion_index;
}

size_t Record::initOrGetRecording(Function* fun) {
    assert(fun != nullptr);

    auto fun_entry = expr_to_body_index.find(fun);
    if (fun_entry != expr_to_body_index.end()) {
        return fun_entry->second;
    }

    R_PreserveObject(fun->container());
    auto insertion_index = functions.size();
    expr_to_body_index.emplace(fun, insertion_index);

    // Make the address the name
    std::stringstream ss;
    ss << "<" << std::hex << fun << ">";

    functions.emplace_back(ss.str());
    return insertion_index;
}

SEXP Record::save() {
    const char* fields[] = {"functions", "events", ""};
    auto recordSexp = PROTECT(Rf_mkNamed(VECSXP, fields));
    auto bodiesSexp = PROTECT(serialization::to_sexp(functions));
    auto eventsSexp = PROTECT(serialization::to_sexp(log));

    SET_VECTOR_ELT(recordSexp, 0, bodiesSexp);
    SET_VECTOR_ELT(recordSexp, 1, eventsSexp);

    UNPROTECT(3);
    return recordSexp;
}

std::pair<size_t, ssize_t> Record::findIndex(rir::Code* code,
                                             rir::Code* needle) {
    const auto toIdx = [this](Code* c) {
        return initOrGetRecording(c->function()->dispatchTable());
    };

    if (code == needle) {
        return {toIdx(code), -1};
    }

    // find the index of the reason source
    // 1. try promises
    for (size_t i = 0; i < code->extraPoolSize; i++) {
        auto extraEntry = code->getExtraPoolEntry(i);
        auto prom = Code::check(extraEntry);
        if (prom != nullptr && prom == needle) {
            return {toIdx(code), i};
        }
    }

    // 2. search globally
    return {toIdx(needle), -1};
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

const std::vector<const char*> SpeculativeContextEvent::fieldNames = {
    "funIdx", "is_promise", "index", "sc", "changed"};

SEXP SpeculativeContextEvent::toSEXP() const {
    return serialization::fields_to_sexp<SpeculativeContextEvent>(
        funRecIndex_, is_promise, index, sc, changed);
}

void SpeculativeContextEvent::fromSEXP(SEXP sexp) {
    return serialization::fields_from_sexp<SpeculativeContextEvent, uint64_t,
                                           bool, uint64_t, SpeculativeContext,
                                           bool>(
        sexp, {funRecIndex_, serialization::uint64_t_from_sexp},
        {is_promise, serialization::bool_from_sexp},
        {index, serialization::uint64_t_from_sexp},
        {sc, serialization::speculative_context_from_sexp},
        {changed, serialization::bool_from_sexp});
}

const std::vector<const char*> CompilationStartEvent::fieldNames = {
    "funIdx", "name", "compile_reason_heuristic", "compile_reason_condition",
    "compile_reason_osr"};

SEXP CompilationStartEvent::toSEXP() const {
    return serialization::fields_to_sexp<CompilationStartEvent>(
        funRecIndex_, compileName, compile_reasons.heuristic,
        compile_reasons.condition, compile_reasons.osr);
}

void CompilationStartEvent::fromSEXP(SEXP sexp) {
    serialization::fields_from_sexp<CompilationStartEvent, uint64_t,
                                    std::string, std::unique_ptr<CompileReason>,
                                    std::unique_ptr<CompileReason>,
                                    std::unique_ptr<CompileReason>>(
        sexp, {funRecIndex_, serialization::uint64_t_from_sexp},
        {compileName, serialization::string_from_sexp},
        {compile_reasons.heuristic, serialization::compile_reason_from_sexp},
        {compile_reasons.condition, serialization::compile_reason_from_sexp},
        {compile_reasons.osr, serialization::compile_reason_from_sexp});
}

const std::vector<const char*> CompilationEvent::fieldNames = {
    "funIdx", "version", "speculative_contexts", "bitcode", "pir_code"};

SEXP CompilationEvent::toSEXP() const {
    return serialization::fields_to_sexp<CompilationEvent>(
        funRecIndex_, version, speculative_contexts, bitcode, pir_code);
}

void CompilationEvent::fromSEXP(SEXP sexp) {
    serialization::fields_from_sexp<CompilationEvent, uint64_t, Context,
                                    std::vector<SpeculativeContext>,
                                    std::string, std::string>(
        sexp, {funRecIndex_, serialization::uint64_t_from_sexp},
        {version, serialization::context_from_sexp},
        {speculative_contexts,
         serialization::vector_from_sexp<
             SpeculativeContext, serialization::speculative_context_from_sexp>},
        {bitcode, serialization::string_from_sexp},
        {pir_code, serialization::string_from_sexp});
}

const std::vector<const char*> CompilationEndEvent::fieldNames = {
    "funIdx", "time_length", "succesful"};

SEXP CompilationEndEvent::toSEXP() const {
    return serialization::fields_to_sexp<CompilationEndEvent>(
        funRecIndex_, time_length, succesful);
}

void CompilationEndEvent::fromSEXP(SEXP sexp) {
    serialization::fields_from_sexp<CompilationEndEvent, uint64_t, Duration,
                                    bool>(
        sexp, {funRecIndex_, serialization::uint64_t_from_sexp},
        {time_length, serialization::time_from_sexp},
        {succesful, serialization::bool_from_sexp});
}

DeoptEvent::DeoptEvent(size_t dispatchTableIndex, Context version,
                       DeoptReason::Reason reason, size_t reasonCodeIdx,
                       ssize_t reasonPromiseIdx, uint32_t reasonCodeOff,
                       SEXP trigger)
    : VersionEvent(dispatchTableIndex, version), reason_(reason),
      reasonCodeIdx_(reasonCodeIdx), reasonPromiseIdx_(reasonPromiseIdx),
      reasonCodeOff_(reasonCodeOff) {
    setTrigger(trigger);
}

DeoptEvent::~DeoptEvent() {
    if (trigger_ != R_NilValue) {
        R_ReleaseObject(trigger_);
    }
}

extern Record recorder_;

// TODO try to maybe find some way to eliminate global
void DeoptEvent::setTrigger(SEXP newTrigger) {
    if (trigger_ != R_NilValue) {
        R_ReleaseObject(trigger_);
    }

    trigger_ = R_NilValue;
    triggerClosure_ = -1;

    if (newTrigger == nullptr) {
        return;
    }

    if (TYPEOF(newTrigger) == CLOSXP) {
        auto rec = recorder_.initOrGetRecording(newTrigger);
        triggerClosure_ = (ssize_t)rec;
        return;
    }

    if (newTrigger) {
        R_PreserveObject(newTrigger);
    }

    trigger_ = newTrigger;
}

const std::vector<const char*> DeoptEvent::fieldNames = {"funIdx",
                                                         "version",
                                                         "reason",
                                                         "reason_code_idx",
                                                         "reason_promise_idx",
                                                         "reason_code_off",
                                                         "trigger",
                                                         "triggerClosure"};

SEXP DeoptEvent::toSEXP() const {
    return serialization::fields_to_sexp<DeoptEvent>(
        funRecIndex_, version, reason_, reasonCodeIdx_, reasonPromiseIdx_,
        reasonCodeOff_, trigger_, triggerClosure_);
}

void DeoptEvent::fromSEXP(SEXP sexp) {
    SEXP trigger = nullptr;
    ssize_t triggerClosure = -1;

    serialization::fields_from_sexp<DeoptEvent, uint64_t, Context,
                                    DeoptReason::Reason, uint64_t, int64_t,
                                    uint32_t, SEXP, int64_t>(
        sexp, {funRecIndex_, serialization::uint64_t_from_sexp},
        {version, serialization::context_from_sexp},
        {reason_, serialization::deopt_reason_from_sexp},
        {reasonCodeIdx_, serialization::uint64_t_from_sexp},
        {reasonPromiseIdx_, serialization::int64_t_from_sexp},
        {reasonCodeOff_, serialization::uint32_t_from_sexp},
        {trigger, serialization::sexp_from_sexp},
        {triggerClosure, serialization::int64_t_from_sexp});

    if (triggerClosure >= 0) {
        triggerClosure_ = triggerClosure;
    } else {
        assert(trigger);
        setTrigger(trigger);
    }
}

const std::vector<const char*> InvocationEvent::fieldNames = {
    "funIdx",
    "context",
    "source",
    "callContext",
    "isNative",
    "address",
    "missing_asmpt_present",
    "missing_asmpt_recovered"};

SEXP InvocationEvent::toSEXP() const {
    return serialization::fields_to_sexp<InvocationEvent>(
        funRecIndex_, version, source, callContext, isNative, address,
        missingAsmptPresent, missingAsmptRecovered);
}

void InvocationEvent::fromSEXP(SEXP sexp) {
    serialization::fields_from_sexp<InvocationEvent, uint64_t, Context, Source,
                                    Context, bool, uint64_t, bool, bool>(
        sexp, {funRecIndex_, serialization::uint64_t_from_sexp},
        {version, serialization::context_from_sexp},
        {source, serialization::invocation_source_from_sexp},
        {callContext, serialization::context_from_sexp},
        {isNative, serialization::bool_from_sexp},
        {address, serialization::uint64_t_from_sexp},
        {missingAsmptPresent, serialization::bool_from_sexp},
        {missingAsmptRecovered, serialization::bool_from_sexp});
}

const std::vector<const char*> UnregisterInvocationEvent::fieldNames = {
    "funIdx", "context"};

SEXP UnregisterInvocationEvent::toSEXP() const {
    return serialization::fields_to_sexp<UnregisterInvocationEvent>(
        funRecIndex_, version);
}

void UnregisterInvocationEvent::fromSEXP(SEXP sexp) {
    serialization::fields_from_sexp<UnregisterInvocationEvent, uint64_t,
                                    Context>(
        sexp, {funRecIndex_, serialization::uint64_t_from_sexp},
        {version, serialization::context_from_sexp});
}

const std::vector<const char*> CustomEvent::fieldNames = {"name"};

SEXP CustomEvent::toSEXP() const {
    return serialization::fields_to_sexp<CustomEvent>(name);
}

void CustomEvent::fromSEXP(SEXP sexp) {
    serialization::fields_from_sexp<CustomEvent, std::string>(
        sexp, {name, serialization::string_from_sexp});
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

std::string getClosureName(SEXP cls) {
    std::string name = "";

    // 1. Look trhu frames
    auto frame = RList(FRAME(CLOENV(cls)));

    for (auto e = frame.begin(); e != frame.end(); ++e) {
        if (*e == cls) {
            name = CHAR(PRINTNAME(e.tag()));
            if (!name.empty()) {
                return name;
            }
        }
    }

    // 2. Try to look thru symbols
    auto env = PROTECT(CLOENV(cls));
    auto symbols = PROTECT(R_lsInternal(env, TRUE));

    auto size = Rf_length(symbols);
    for (int i = 0; i < size; i++) {
        const char* symbol_char = CHAR(VECTOR_ELT(symbols, i));
        auto symbol = PROTECT(Rf_install(symbol_char));

        auto value = PROTECT(Rf_findVarInFrame(env, symbol));

        if (value == cls) {
            name = symbol_char;
            UNPROTECT(2);
            break;
        }

        UNPROTECT(2);
    }
    UNPROTECT(2);

    return name;
}

} // namespace recording
} // namespace rir

#endif // RECORDING
