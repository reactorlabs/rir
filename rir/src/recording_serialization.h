#ifndef SERIALIZER_H
#define SERIALIZER_H

#ifdef RECORDING

#include "recording.h"
#include <R/r.h>
#include <memory>
#include <vector>

constexpr const char* R_CLASS_CTX_CALLEES = "ctx_callees";
constexpr const char* R_CLASS_CTX_TEST = "ctx_test";
constexpr const char* R_CLASS_CTX_VALUES = "ctx_values";

namespace rir {
namespace recording {
namespace serialization {

/************************ Typeclass **********************************/

template <typename T>
struct Serializer;

template <typename T>
SEXP to_sexp(const T& obj) {
    return Serializer<T>::to_sexp_(obj);
}

template <typename T>
T from_sexp(SEXP sexp) {
    return Serializer<T>::from_sexp_(sexp);
}

/************************ Fields **********************************/

// Serialization

inline void fields_to_vec(SEXP vec, int i) {}

template <typename T, typename... Ts>
void fields_to_vec(SEXP vec, int i, const T& head, const Ts&... tail) {
    SET_VECTOR_ELT(vec, i, to_sexp<T>(head));
    fields_to_vec(vec, i + 1, tail...);
}

template <typename Derived, typename... Ts>
SEXP fields_to_sexp(const Ts&... fields) {
    assert(sizeof...(Ts) == Derived::fieldNames.size() &&
           "The number of serialized fields is not the same as number of field "
           "names");

    std::vector<const char*> names = Derived::fieldNames;
    names.push_back("");

    SEXP vec = PROTECT(Rf_mkNamed(VECSXP, names.data()));
    setClassName(vec, Derived::className);

    fields_to_vec(vec, 0, fields...);

    UNPROTECT(1);
    return vec;
}

// Deserialization
// Needs explicit template parameters -> a struct, not a function
template <typename... Ts>
struct fields_from_vec;

template <typename T, typename... Ts>
struct fields_from_vec<T, Ts...> {
    static void apply(SEXP vec, int i, T& head, Ts&... tail) {
        head = from_sexp<T>(VECTOR_ELT(vec, i));
        fields_from_vec<Ts...>::apply(vec, i + 1, tail...);
    }
};

template <>
struct fields_from_vec<> {
    static void apply(SEXP vec, int i) {}
};

template <typename Derived, typename... Ts>
void fields_from_sexp(SEXP sexp, Ts&... fields) {
    assert(Rf_isVector(sexp));
    assert(static_cast<size_t>(Rf_length(sexp)) == Derived::fieldNames.size());

    fields_from_vec<Ts...>::apply(sexp, 0, fields...);
}

/************************ Primitives **********************************/

template <>
struct Serializer<std::string> {
    static SEXP to_sexp_(const std::string& str) {
        return Rf_mkString(str.c_str());
    }

    static std::string from_sexp_(SEXP sexp) {
        if (sexp->sxpinfo.type == CHARSXP) {
            return CHAR(sexp);
        } else if (Rf_isString(sexp)) {
            return CHAR(STRING_ELT(sexp, 0));
        } else {
            Rf_error("cannot parse SEXP of type %d as string",
                     sexp->sxpinfo.type);
        }
    }
};

template <>
struct Serializer<uint32_t> {
    static SEXP to_sexp_(uint32_t i) {
        int32_t ii;
        memcpy(&ii, &i, sizeof(i)); // punning unsigned -> signed
        return Rf_ScalarInteger(ii);
    }

    static uint32_t from_sexp_(SEXP sexp) {
        assert(Rf_isInteger(sexp));
        int32_t ii = Rf_asInteger(sexp);
        uint32_t i;
        memcpy(&i, &ii, sizeof(i)); // punning signed -> unsigned
        return i;
    }
};

template <>
struct Serializer<uint64_t> {
    static SEXP to_sexp_(uint64_t i) {
        // R doesn't have long ints, we use strings instead
        char i_str[21];
        sprintf(i_str, "%lu", i);
        return Rf_mkString(i_str);
    }

    static uint64_t from_sexp_(SEXP sexp) {
        assert(Rf_isString(sexp));
        return std::strtoul(CHAR(STRING_ELT(sexp, 0)), nullptr, 10);
    }
};

template <>
struct Serializer<int64_t> {
    static SEXP to_sexp_(int64_t i) {
        // R doesn't have long ints, we use strings instead
        char i_str[21];
        sprintf(i_str, "%ld", i);
        return Rf_mkString(i_str);
    }

    static int64_t from_sexp_(SEXP sexp) {
        assert(Rf_isString(sexp));
        return std::strtoll(CHAR(STRING_ELT(sexp, 0)), nullptr, 10);
    }
};

template <>
struct Serializer<bool> {
    static SEXP to_sexp_(bool flag) {
        return flag ? R_TrueValue : R_FalseValue;
    }
    static bool from_sexp_(SEXP sexp) { return sexp == R_TrueValue; }
};

template <>
struct Serializer<SEXP> {
    static SEXP to_sexp_(SEXP sexp) { return sexp; }
    static SEXP from_sexp_(SEXP sexp) { return sexp; }
};

/************************ Objects **********************************/

template <>
struct Serializer<Context> {
    static SEXP to_sexp_(const Context ctx) {
        return to_sexp<unsigned long>(ctx.toI());
    }
    static Context from_sexp_(SEXP sexp) {
        return Context(from_sexp<unsigned long>(sexp));
    }
};

template <>
struct Serializer<std::unique_ptr<Event>> {
    // The reference is weird, but this is just to make sure we don't need
    // to call an std::move in the to_sexp helper function
    static SEXP to_sexp_(const std::unique_ptr<Event>& obj) {
        if (obj) {
            return obj->toSEXP();
        } else {
            return R_NilValue;
        }
    }

    static std::unique_ptr<Event> from_sexp_(SEXP sexp) {
        assert(Rf_isVector(sexp));

        std::unique_ptr<rir::recording::Event> event;
        if (Rf_inherits(sexp, rir::recording::CompilationEvent::className)) {
            event = std::make_unique<rir::recording::CompilationEvent>();
        } else if (Rf_inherits(sexp, rir::recording::DeoptEvent::className)) {
            event = std::make_unique<rir::recording::DeoptEvent>();
        } else if (Rf_inherits(sexp,
                               rir::recording::InvocationEvent::className)) {
            event = std::make_unique<rir::recording::InvocationEvent>();
        } else if (Rf_inherits(
                       sexp,
                       rir::recording::SpeculativeContextEvent::className)) {
            event = std::make_unique<rir::recording::SpeculativeContextEvent>();
        } else {
            Rf_error("can't deserialize event of unknown class");
        }

        event->fromSEXP(sexp);
        return event;
    }
};

template <>
struct Serializer<SpeculativeContext> {
    static SEXP to_sexp_(const SpeculativeContext& obj) {
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
    static SpeculativeContext from_sexp_(SEXP sexp) {
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
};

template <>
struct Serializer<DeoptReason::Reason> {
    static SEXP to_sexp_(DeoptReason::Reason obj) {
        return to_sexp((uint32_t)obj);
    }
    static DeoptReason::Reason from_sexp_(SEXP sexp) {
        auto value = from_sexp<uint32_t>(sexp);
        assert(value <= DeoptReason::DeadBranchReached);
        return (DeoptReason::Reason)value;
    }
};

template <>
struct Serializer<FunRecording> {
    static SEXP to_sexp_(const FunRecording& obj) {
        return fields_to_sexp<FunRecording>(obj.primIdx, obj.name, obj.env,
                                            obj.closure, obj.address);
    }

    static FunRecording from_sexp_(SEXP sexp) {
        FunRecording obj;
        fields_from_sexp<FunRecording>(sexp, obj.primIdx, obj.name, obj.env,
                                       obj.closure, obj.address);
        return obj;
    }
};

template <>
struct Serializer<std::unique_ptr<CompileReason>> {
    // Reference for the same reason as in Event
    static SEXP to_sexp_(const std::unique_ptr<CompileReason>& reason) {
        if (reason) {
            return reason->toSEXP();
        } else {
            return R_NilValue;
        }
    }

    static std::unique_ptr<CompileReason> from_sexp_(SEXP sexp) {
        if (Rf_isNull(sexp)) {
            return nullptr;
        }

        std::unique_ptr<rir::recording::CompileReason> reason;
        if (Rf_inherits(sexp, MarkOptReason::className)) {
            reason = std::make_unique<MarkOptReason>();
        } else if (Rf_inherits(sexp, PirWarmupReason::className)) {
            reason = std::make_unique<PirWarmupReason>();
        } else if (Rf_inherits(sexp, NotOptimizedReason::className)) {
            reason = std::make_unique<NotOptimizedReason>();
        } else if (Rf_inherits(sexp, IsImprovingReason::className)) {
            reason = std::make_unique<IsImprovingReason>();
        } else if (Rf_inherits(sexp, ReoptimizeFlagReason::className)) {
            reason = std::make_unique<ReoptimizeFlagReason>();
        } else if (Rf_inherits(sexp, OSRLoopReason::className)) {
            reason = std::make_unique<OSRLoopReason>();
        } else if (Rf_inherits(sexp, OSRCallerCalleeReason::className)) {
            reason = std::make_unique<OSRCallerCalleeReason>();
        } else {
            Rf_error("can't deserialize speculative context of unknown class");
        }

        reason->fromSEXP(sexp);

        return reason;
    }
};

template <>
struct Serializer<CompilationEndEvent::Duration> {
    static SEXP to_sexp_(CompilationEndEvent::Duration time) {
        return to_sexp<int64_t>(time.count());
    }

    static CompilationEndEvent::Duration from_sexp_(SEXP sexp) {
        return CompilationEndEvent::Duration(from_sexp<int64_t>(sexp));
    }
};

template <>
struct Serializer<InvocationEvent::Source> {
    static SEXP to_sexp_(InvocationEvent::Source set) {
        return Rf_ScalarInteger(static_cast<int>(set));
    }

    static InvocationEvent::Source from_sexp_(SEXP sexp) {
        assert(Rf_isInteger(sexp));
        return static_cast<InvocationEvent::Source>(Rf_asInteger(sexp));
    }
};

template <>
struct Serializer<FeedbackIndex> {
    static SEXP to_sexp_(FeedbackIndex index) {
        return to_sexp<uint32_t>(index.asInteger());
    }
    static FeedbackIndex from_sexp_(SEXP sexp) {
        auto u = from_sexp<uint32_t>(sexp);
        // Going thru void* to go around the compiler
        void* asv = &u;
        return *((FeedbackIndex*)(asv));
    }
};

/************************ Generics **********************************/

template <typename T, typename U>
struct Serializer<std::pair<T, U>> {
    static SEXP to_sexp_(const std::pair<T, U>& obj) {
        SEXP pair = PROTECT(Rf_allocVector(VECSXP, 2));
        SET_VECTOR_ELT(pair, 0, to_sexp<T>(obj.first));
        SET_VECTOR_ELT(pair, 1, to_sexp<U>(obj.second));
        UNPROTECT(1);
        return pair;
    }

    static std::pair<T, U> from_sexp_(SEXP sexp) {
        assert(TYPEOF(sexp) == VECSXP);
        assert(Rf_length(sexp) == 2);
        return {
            from_sexp<T>(VECTOR_ELT(sexp, 0)),
            from_sexp<U>(VECTOR_ELT(sexp, 1)),
        };
    }
};

template <typename T>
struct Serializer<std::vector<T>> {
    static SEXP to_sexp_(const std::vector<T>& obj) {
        auto vec = PROTECT(Rf_allocVector(VECSXP, obj.size()));
        for (unsigned long i = 0; i < obj.size(); i++) {
            SET_VECTOR_ELT(vec, i, to_sexp<T>(obj[i]));
        }
        UNPROTECT(1);
        return vec;
    }

    static std::vector<T> from_sexp_(SEXP sexp) {
        assert(TYPEOF(sexp) == VECSXP);
        const size_t length = Rf_length(sexp);

        std::vector<T> vec;
        vec.reserve(length);

        for (unsigned long i = 0; i < length; i++) {
            vec.emplace_back(from_sexp<T>(VECTOR_ELT(sexp, i)));
        }

        return vec;
    }
};

} // namespace serialization
} // namespace recording
} // namespace rir

#endif // RECORDING
#endif
