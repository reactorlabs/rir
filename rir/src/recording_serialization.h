#ifndef SERIALIZER_H
#define SERIALIZER_H

#ifdef RECORDING

#include "recording.h"
#include <R/r.h>
#include <map>
#include <memory>
#include <vector>

constexpr const char* R_CLASS_CTX_CALLEES = "ctx_callees";
constexpr const char* R_CLASS_CTX_TEST = "ctx_test";
constexpr const char* R_CLASS_CTX_VALUES = "ctx_values";

namespace rir {
namespace recording {
namespace serialization {

/************************ Primitives **********************************/

SEXP to_sexp(const std::string&);

std::string string_from_sexp(SEXP sexp);

SEXP to_sexp(uint32_t i);

uint32_t uint32_t_from_sexp(SEXP sexp);

SEXP to_sexp(uint64_t i);

uint64_t uint64_t_from_sexp(SEXP sexp);

SEXP to_sexp(int64_t i);

int64_t int64_t_from_sexp(SEXP sexp);

SEXP to_sexp(bool flag);

bool bool_from_sexp(SEXP sexp);

SEXP to_sexp(SEXP sexp);

SEXP sexp_from_sexp(SEXP sexp);

/************************ Objects **********************************/

SEXP to_sexp(
    const std::unordered_map<std::string, rir::recording::FunRecording>& obj);

SEXP to_sexp(const rir::Context);

Context context_from_sexp(SEXP sexp);

SEXP to_sexp(const rir::recording::Event& obj);

std::unique_ptr<rir::recording::Event> event_from_sexp(SEXP sexp);

SEXP to_sexp(const rir::recording::SpeculativeContext&);

rir::recording::SpeculativeContext speculative_context_from_sexp(SEXP sexp);

SEXP to_sexp(DeoptReason obj);

DeoptReason::Reason deopt_reason_from_sexp(SEXP sexp);

SEXP to_sexp(const rir::recording::FunRecording& obj);

rir::recording::FunRecording fun_recorder_from_sexp(SEXP sexp);

SEXP to_sexp(const CompileReason& reason);

std::unique_ptr<rir::recording::CompileReason>
compile_reason_from_sexp(SEXP sexp);

SEXP to_sexp(CompilationEndEvent::Duration time);

CompilationEndEvent::Duration time_from_sexp(SEXP sexp);

SEXP to_sexp(InvocationEvent::Source set);

InvocationEvent::Source invocation_source_from_sexp(SEXP sexp);

SEXP to_sexp(FeedbackIndex index);

FeedbackIndex feedback_index_from_sexp(SEXP sexp);

/************************ Generics **********************************/
template <typename T>
using from_sexp_t = T (*)(SEXP);

template <typename T>
SEXP to_sexp(const std::unique_ptr<T>& ptr) {
    if (ptr != nullptr) {
        return to_sexp(*ptr);
    } else {
        return R_NilValue;
    }
}

template <typename T, typename U>
SEXP to_sexp(const std::pair<T, U>& obj) {
    SEXP pair = PROTECT(Rf_allocVector(VECSXP, 2));
    SET_VECTOR_ELT(pair, 0, to_sexp(obj.first));
    SET_VECTOR_ELT(pair, 1, to_sexp(obj.second));
    UNPROTECT(1);
    return pair;
}

template <typename T, typename U, from_sexp_t<T> first_from_sexp,
          from_sexp_t<U> second_from_sexp>
std::pair<T, U> pair_from_sexp(SEXP sexp) {
    assert(TYPEOF(sexp) == VECSXP);
    assert(Rf_length(sexp) == 2);
    return {
        std::move(first_from_sexp(VECTOR_ELT(sexp, 0))),
        std::move(second_from_sexp(VECTOR_ELT(sexp, 1))),
    };
}

template <typename T>
SEXP to_sexp(const std::vector<T>& obj) {
    auto vec = PROTECT(Rf_allocVector(VECSXP, obj.size()));
    for (unsigned long i = 0; i < obj.size(); i++) {
        SET_VECTOR_ELT(vec, i, to_sexp(obj[i]));
    }
    UNPROTECT(1);
    return vec;
}

template <typename T, from_sexp_t<T> element_from_sexp>
std::vector<T> vector_from_sexp(SEXP sexp) {
    assert(TYPEOF(sexp) == VECSXP);
    const size_t length = Rf_length(sexp);

    std::vector<T> vec;
    vec.reserve(length);

    for (unsigned long i = 0; i < length; i++) {
        vec.emplace_back(std::move(element_from_sexp(VECTOR_ELT(sexp, i))));
    }

    return vec;
}

/************************ Fields **********************************/

// Serialization

inline void fields_to_vec(SEXP vec, int i) {}

template <typename T, typename... Ts>
void fields_to_vec(SEXP vec, int i, const T& head, const Ts&... tail) {
    SET_VECTOR_ELT(vec, i, serialization::to_sexp(head));
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
    static void apply(SEXP vec, int i, std::pair<T&, from_sexp_t<T>> head,
                      std::pair<Ts&, from_sexp_t<Ts>>... tail) {
        head.first = head.second(VECTOR_ELT(vec, i));
        fields_from_vec<Ts...>::apply(vec, i + 1, tail...);
    }
};

template <>
struct fields_from_vec<> {
    static void apply(SEXP vec, int i) {}
};

template <typename Derived, typename... Ts>
void fields_from_sexp(SEXP sexp, std::pair<Ts&, from_sexp_t<Ts>>... pairs) {
    assert(Rf_isVector(sexp));
    assert(static_cast<size_t>(Rf_length(sexp)) == Derived::fieldNames.size());
    fields_from_vec<Ts...>::apply(sexp, 0, pairs...);
}

} // namespace serialization
} // namespace recording
} // namespace rir

#endif // RECORDING
#endif
