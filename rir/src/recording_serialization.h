#ifndef SERIALIZER_H
#define SERIALIZER_H

#include "recording.h"
#include <R/r.h>
#include <map>
#include <memory>
#include <vector>

#define R_CLASS_COMPILE_EVENT "event_compile"
#define R_CLASS_DEOPT_EVENT "event_deopt"
#define R_CLASS_CTX_CALLEES "ctx_callees"
#define R_CLASS_CTX_TEST "ctx_test"
#define R_CLASS_CTX_VALUES "ctx_values"

namespace rir {
namespace recording {
namespace serialization {

SEXP to_sexp(
    const std::unordered_map<std::string, rir::recording::FunRecording>& obj);

template <typename T>
SEXP to_sexp(const std::unique_ptr<T>& ptr);

SEXP to_sexp(const std::string&);

std::string string_from_sexp(SEXP sexp);

SEXP to_sexp(uint64_t i);

uint64_t uint64_t_from_sexp(SEXP sexp);

SEXP to_sexp(const rir::recording::Event& obj);

std::unique_ptr<rir::recording::Event> event_from_sexp(SEXP sexp);

SEXP to_sexp(const rir::recording::SpeculativeContext&);

rir::recording::SpeculativeContext speculative_context_from_sexp(SEXP sexp);

SEXP to_sexp(const DeoptReason& obj);

DeoptReason deopt_reason_from_sexp(SEXP sexp);

SEXP to_sexp(const rir::recording::FunRecording& obj);

rir::recording::FunRecording fun_recorder_from_sexp(SEXP sexp);

template <typename T>
SEXP to_sexp(const std::vector<T>& obj) {
    auto vec = PROTECT(Rf_allocVector(VECSXP, obj.size()));
    for (unsigned long i = 0; i < obj.size(); i++) {
        SET_VECTOR_ELT(vec, i, to_sexp(obj[i]));
    }
    UNPROTECT(1);
    return vec;
}

} // namespace serialization
} // namespace recording
} // namespace rir

#endif
