#ifdef RECORDING

#include "recording_serialization.h"
#include "Rdefines.h"
#include "Rinternals.h"
#include "recording.h"

namespace rir {
namespace recording {
namespace serialization {

/************************ Primitives **********************************/

SEXP to_sexp(const std::string& str) { return Rf_mkString(str.c_str()); }

std::string string_from_sexp(SEXP sexp) {
    if (sexp->sxpinfo.type == CHARSXP) {
        return CHAR(sexp);
    } else if (Rf_isString(sexp)) {
        return CHAR(STRING_ELT(sexp, 0));
    } else {
        Rf_error("cannot parse SEXP of type %d as string", sexp->sxpinfo.type);
    }
}

SEXP to_sexp(uint32_t i) {
    int32_t ii;
    memcpy(&ii, &i, sizeof(i)); // punning unsigned -> signed
    return Rf_ScalarInteger(ii);
}

uint32_t uint32_t_from_sexp(SEXP sexp) {
    assert(Rf_isInteger(sexp));
    int32_t ii = Rf_asInteger(sexp);
    uint32_t i;
    memcpy(&i, &ii, sizeof(i)); // punning signed -> unsigned
    return i;
}

SEXP to_sexp(uint64_t i) {
    // R doesn't have long ints, we use strings instead
    char i_str[21];
    sprintf(i_str, "%lu", i);
    return Rf_mkString(i_str);
}

uint64_t uint64_t_from_sexp(SEXP sexp) {
    assert(Rf_isString(sexp));
    return std::strtoul(CHAR(STRING_ELT(sexp, 0)), nullptr, 10);
}

SEXP to_sexp(int64_t i) {
    // R doesn't have long ints, we use strings instead
    char i_str[21];
    sprintf(i_str, "%ld", i);
    return Rf_mkString(i_str);
}

int64_t int64_t_from_sexp(SEXP sexp) {
    assert(Rf_isString(sexp));
    return std::strtoll(CHAR(STRING_ELT(sexp, 0)), nullptr, 10);
}

SEXP to_sexp(bool flag) { return flag ? R_TrueValue : R_FalseValue; }

bool bool_from_sexp(SEXP sexp) { return sexp == R_TrueValue; }

SEXP to_sexp(SEXP sexp) { return sexp; }

SEXP sexp_from_sexp(SEXP sexp) { return sexp; }

/************************ Objects **********************************/

SEXP to_sexp(const rir::Context ctx) { return to_sexp(ctx.toI()); }

Context context_from_sexp(SEXP sexp) {
    return Context(uint64_t_from_sexp(sexp));
}

SEXP to_sexp(const rir::recording::Event& obj) { return obj.toSEXP(); }

std::unique_ptr<rir::recording::Event> event_from_sexp(SEXP sexp) {
    assert(Rf_isVector(sexp));

    std::unique_ptr<rir::recording::Event> event;
    if (Rf_inherits(sexp, rir::recording::CompilationEvent::className)) {
        event = std::make_unique<rir::recording::CompilationEvent>();
    } else if (Rf_inherits(sexp, rir::recording::DeoptEvent::className)) {
        event = std::make_unique<rir::recording::DeoptEvent>();
    } else if (Rf_inherits(sexp, rir::recording::InvocationEvent::className)) {
        event = std::make_unique<rir::recording::InvocationEvent>();
    } else if (Rf_inherits(
                   sexp, rir::recording::SpeculativeContextEvent::className)) {
        event = std::make_unique<rir::recording::SpeculativeContextEvent>();
    } else {
        Rf_error("can't deserialize event of unknown class");
    }

    event->fromSEXP(sexp);
    return event;
}

SEXP to_sexp(const rir::recording::SpeculativeContext& obj) {
    SEXP sexp = PROTECT(Rf_allocVector(RAWSXP, sizeof(obj.value)));
    switch (obj.type) {
    case SpeculativeContextType::Callees:
        setClassName(sexp, R_CLASS_CTX_CALLEES);
        break;
    case SpeculativeContextType::Test:
        setClassName(sexp, R_CLASS_CTX_TEST);
        break;
    case SpeculativeContextType::Values:
        setClassName(sexp, R_CLASS_CTX_VALUES);
        break;
    }

    memcpy(RAW(sexp), &obj.value, sizeof(obj.value));

    UNPROTECT(1);
    return sexp;
}

rir::recording::SpeculativeContext speculative_context_from_sexp(SEXP sexp) {
    assert(TYPEOF(sexp) == RAWSXP);

    SpeculativeContext ctx(
        ObservedTest{}); // dummy initialization, overwritten later

    if (Rf_inherits(sexp, R_CLASS_CTX_CALLEES)) {
        ctx.type = SpeculativeContextType::Callees;
    } else if (Rf_inherits(sexp, R_CLASS_CTX_TEST)) {
        ctx.type = SpeculativeContextType::Test;
    } else if (Rf_inherits(sexp, R_CLASS_CTX_VALUES)) {
        ctx.type = SpeculativeContextType::Values;
    } else {
        Rf_error("can't deserialize speculative context of unknown class");
    }

    constexpr size_t field_len =
        sizeof(rir::recording::SpeculativeContext::value);
    assert(LENGTH(sexp) == field_len);
    memcpy(&ctx.value, RAW(sexp), field_len);
    return ctx;
}

SEXP to_sexp(DeoptReason::Reason obj) { return to_sexp((uint32_t)obj); }

DeoptReason::Reason deopt_reason_from_sexp(SEXP sexp) {
    auto value = uint32_t_from_sexp(sexp);
    assert(value <= DeoptReason::DeadBranchReached);
    return (DeoptReason::Reason)value;
}

SEXP to_sexp(const rir::recording::FunRecording& obj) {
    const char* fields[] = {"primIdx", "name", "env", "closure", ""};
    auto vec = PROTECT(Rf_mkNamed(VECSXP, fields));
    size_t i = 0;
    SET_VECTOR_ELT(vec, i++, PROTECT(to_sexp(obj.primIdx)));
    SET_VECTOR_ELT(vec, i++, PROTECT(Rf_mkString(obj.name.c_str())));
    SET_VECTOR_ELT(vec, i++, PROTECT(Rf_mkString(obj.env.c_str())));
    SET_VECTOR_ELT(vec, i++, obj.closure);
    UNPROTECT(i);
    return vec;
}

rir::recording::FunRecording fun_recorder_from_sexp(SEXP sexp) {
    assert(Rf_isVector(sexp));
    assert(Rf_length(sexp) == 4);

    rir::recording::FunRecording recorder;
    size_t i = 0;
    recorder.primIdx = serialization::uint64_t_from_sexp(VECTOR_ELT(sexp, i++));
    recorder.name = serialization::string_from_sexp(VECTOR_ELT(sexp, i++));
    recorder.env = serialization::string_from_sexp(VECTOR_ELT(sexp, i++));
    recorder.closure = VECTOR_ELT(sexp, i++);
    assert(Rf_isNull(recorder.closure) || TYPEOF(recorder.closure) == RAWSXP);

    return recorder;
}

SEXP to_sexp(const CompileReason& reason) { return reason.toSEXP(); }

std::unique_ptr<rir::recording::CompileReason>
compile_reason_from_sexp(SEXP sexp) {
    if (Rf_isNull(sexp)) {
        return nullptr;
    }

    std::unique_ptr<rir::recording::CompileReason> reason;
    if (Rf_inherits(sexp, MarkOptReason::NAME)) {
        reason = std::make_unique<MarkOptReason>();
    } else if (Rf_inherits(sexp, PirWarmupReason::NAME)) {
        reason = std::make_unique<PirWarmupReason>();
    } else if (Rf_inherits(sexp, NotOptimizedReason::NAME)) {
        reason = std::make_unique<NotOptimizedReason>();
    } else if (Rf_inherits(sexp, IsImprovingReason::NAME)) {
        reason = std::make_unique<IsImprovingReason>();
    } else if (Rf_inherits(sexp, ReoptimizeFlagReason::NAME)) {
        reason = std::make_unique<ReoptimizeFlagReason>();
    } else if (Rf_inherits(sexp, OSRLoopReason::NAME)) {
        reason = std::make_unique<OSRLoopReason>();
    } else if (Rf_inherits(sexp, OSRCallerCalleeReason::NAME)) {
        reason = std::make_unique<OSRCallerCalleeReason>();
    } else {
        Rf_error("can't deserialize speculative context of unknown class");
    }

    reason->fromSEXP(sexp);

    return reason;
}

SEXP to_sexp(CompilationEndEvent::Duration time) {
    int64_t count = time.count();
    return to_sexp(count);
}

CompilationEndEvent::Duration time_from_sexp(SEXP sexp) {
    int64_t count = int64_t_from_sexp(sexp);
    return CompilationEndEvent::Duration(count);
}

SEXP to_sexp(InvocationEvent::Source set) {
    return Rf_ScalarInteger(static_cast<int>(set));
}

InvocationEvent::Source invocation_source_from_sexp(SEXP sexp) {
    assert(Rf_isInteger(sexp));
    return static_cast<InvocationEvent::Source>(Rf_asInteger(sexp));
}

SEXP to_sexp(FeedbackIndex index){
    return to_sexp( index.asInteger() );
}

FeedbackIndex feedback_index_from_sexp(SEXP sexp){
    auto u = uint32_t_from_sexp(sexp);
    // Going thru void* to go around the compiler
    void* asv = &u;
    return *((FeedbackIndex*)(asv));
}

} // namespace serialization
} // namespace recording
} // namespace rir

#endif // RECORDING
