#ifndef SERIALIZER_H
#define SERIALIZER_H

#include "recording.h"
#include <R/r.h>
#include <map>
#include <memory>
#include <vector>

namespace serializer {

extern SEXP shared_class_name_event_compile;
extern SEXP shared_class_name_event_deopt;

void init_shared_class_names();

// template <typename V>
// SEXP to_sexp(const std::unordered_map<std::string, V>& obj);

SEXP to_sexp(
    const std::unordered_map<std::string, rir::recording::FunRecorder>& obj);

template <typename T>
SEXP to_sexp(const std::vector<std::unique_ptr<T>>& obj);

SEXP to_sexp(const std::string&);

std::string string_from_sexp(SEXP sexp);

SEXP to_sexp(uint64_t i);

uint64_t uint64_t_from_sexp(SEXP sexp);

SEXP to_sexp(const rir::recording::Event& obj);

std::unique_ptr<rir::recording::Event> event_from_sexp(SEXP sexp);

SEXP to_sexp(const rir::recording::FunRecorder& obj);

rir::recording::FunRecorder fun_recorder_from_sexp(SEXP sexp);

} // namespace serializer

#endif
