#include "recording_serialization.h"
#include "Rdefines.h"
#include "recording.h"

namespace rir {
namespace recording {
namespace serialization {

// TODO: fix the names

SEXP to_sexp(
    const std::unordered_map<std::string, rir::recording::FunRecording>& obj) {
    std::unique_ptr<const char*[]> keys(new const char*[obj.size() + 1]);
    auto ki = 0;
    for (auto& kv : obj) {
        keys[ki++] = kv.first.c_str();
    }
    keys[ki] = "";

    auto vec = PROTECT(Rf_mkNamed(VECSXP, (const char**)keys.get()));
    ki = 0;
    for (auto& kv : obj) {
        SET_VECTOR_ELT(vec, ki++, to_sexp(kv.second));
    }

    UNPROTECT(1);
    return vec;
}

// template <typename T>
// SEXP to_sexp(const std::vector<std::unique_ptr<T>>& obj) {
//     auto vec = PROTECT(Rf_allocVector(VECSXP, obj.size()));
//     for (unsigned long i = 0; i < obj.size(); i++) {
//         SET_VECTOR_ELT(vec, i, to_sexp(*obj[i].get()));
//     }
//     UNPROTECT(1);
//     return vec;
// }

template <typename T>
SEXP to_sexp(const std::unique_ptr<T>& ptr) {
    return to_sexp(*ptr);
}

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
    return std::stoul(CHAR(STRING_ELT(sexp, 0)));
}

SEXP to_sexp(const std::pair<int64_t, int64_t>& pair) {
    auto sexp = PROTECT(Rf_allocVector(RAWSXP, 2 * sizeof(int64_t)));
    memcpy(RAW(sexp) + 0, &pair.first, sizeof(int64_t));
    memcpy(RAW(sexp) + sizeof(int64_t), &pair.second, sizeof(int64_t));
    UNPROTECT(1);
    return sexp;
}

std::pair<int64_t, int64_t> pair_from_sexp(SEXP sexp) {
    assert(TYPEOF(sexp) == RAWSXP);
    assert(Rf_length(sexp) == 2 * sizeof(int64_t));
    std::pair<int64_t, int64_t> pair;
    memcpy((void*)&pair.first, RAW(sexp), sizeof(int64_t));
    memcpy((void*)&pair.second, RAW(sexp) + sizeof(int64_t), sizeof(int64_t));
    return pair;
}

SEXP to_sexp(const rir::recording::Event& obj) { return obj.toSEXP(); }

std::unique_ptr<rir::recording::Event> event_from_sexp(SEXP sexp) {
    assert(Rf_isVector(sexp));

    std::unique_ptr<rir::recording::Event> event;
    if (Rf_inherits(sexp, R_CLASS_COMPILE_EVENT)) {
        // dummy init, overwritten later
        event = std::make_unique<rir::recording::CompilationEvent>(
            0, std::vector<SpeculativeContext>{});
    } else if (Rf_inherits(sexp, R_CLASS_DEOPT_EVENT)) {
        // dummy init, overwritten later
        event = std::make_unique<rir::recording::DeoptEvent>(
            (size_t)-1, DeoptReason::Reason::Unknown,
            std::make_pair((size_t)-1, (size_t)-1), (uint32_t)0, nullptr);
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

    SpeculativeContextType type;
    if (Rf_inherits(sexp, R_CLASS_CTX_CALLEES)) {
        type = SpeculativeContextType::Callees;
    } else if (Rf_inherits(sexp, R_CLASS_CTX_TEST)) {
        type = SpeculativeContextType::Test;
    } else if (Rf_inherits(sexp, R_CLASS_CTX_VALUES)) {
        type = SpeculativeContextType::Values;
    } else {
        Rf_error("can't deserialize speculative context of unknown class");
    }

    SpeculativeContext ctx(
        ObservedTest{}); // dummy initialization, overwritten later
    ctx.type = type;
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
    const char* fields[] = {"name", "env", "closure", "events", ""};
    auto vec = PROTECT(Rf_mkNamed(VECSXP, fields));
    SET_VECTOR_ELT(vec, 0, Rf_mkString(obj.name.c_str()));
    SET_VECTOR_ELT(vec, 1, Rf_mkString(obj.env.c_str()));
    SET_VECTOR_ELT(vec, 2, obj.closure);
    SET_VECTOR_ELT(vec, 3, to_sexp(obj.events));
    UNPROTECT(1);
    return vec;
}

rir::recording::FunRecording fun_recorder_from_sexp(SEXP sexp) {
    assert(Rf_isVector(sexp));
    assert(Rf_length(sexp) == 4);

    rir::recording::FunRecording recorder;

    recorder.name = serialization::string_from_sexp(VECTOR_ELT(sexp, 0));
    recorder.env = serialization::string_from_sexp(VECTOR_ELT(sexp, 1));
    recorder.closure = VECTOR_ELT(sexp, 2);
    assert(TYPEOF(recorder.closure) == RAWSXP);
    recorder.events = vector_from_sexp<std::unique_ptr<Event>, event_from_sexp>(
        VECTOR_ELT(sexp, 3));

    return recorder;
}

} // namespace serialization
} // namespace recording
} // namespace rir
