#include "serializer.h"

namespace rir {
namespace recording {
namespace serialization {

SEXP shared_class_name_event_compile = nullptr;
SEXP shared_class_name_event_deopt = nullptr;

void init_shared_class_names() {
    if (shared_class_name_event_compile == nullptr) {
        shared_class_name_event_compile = Rf_mkString("compile_event");
        R_PreserveObject(shared_class_name_event_compile);
        shared_class_name_event_deopt = Rf_mkString("deopt_event");
        R_PreserveObject(shared_class_name_event_deopt);
    }
}

SEXP to_sexp(
    const std::unordered_map<std::string, rir::recording::FunRecorder>& obj) {
    init_shared_class_names();
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

template <typename T>
SEXP to_sexp(const std::vector<std::unique_ptr<T>>& obj) {
    auto vec = PROTECT(Rf_allocVector(VECSXP, obj.size()));
    for (unsigned long i = 0; i < obj.size(); i++) {
        SET_VECTOR_ELT(vec, i, to_sexp(*obj[i].get()));
    }
    UNPROTECT(1);
    return vec;
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

SEXP to_sexp(uint64_t i) {
    // R doesn't have long ints, we use strings instead
    char i_str[21];
    sprintf(i_str, "%lu", i);
    return Rf_mkString(i_str);
}

uint64_t uint64_t_from_sexp(SEXP sexp) {
    return std::stoul(CHAR(STRING_ELT(sexp, 0)));
}

SEXP to_sexp(const rir::recording::Event& obj) { return obj.to_sexp(); }

std::unique_ptr<rir::recording::Event> event_from_sexp(SEXP sexp) {
    assert(Rf_isVector(sexp));

    std::unique_ptr<rir::recording::Event> event;
    if (Rf_inherits(sexp, "compile_event")) {
        event = std::make_unique<rir::recording::CompilationEvent>();
    } else if (Rf_inherits(sexp, "deopt_event")) {
        event = std::make_unique<rir::recording::DeoptEvent>();
    } else {
        Rf_error("can't deserialize event of unknown class");
    }

    event->init_from_sexp(sexp);
    return event;
}

SEXP to_sexp(const rir::recording::FunRecorder& obj) {
    const char* fields[] = {"name", "function", "events", ""};
    auto vec = PROTECT(Rf_mkNamed(VECSXP, fields));
    SET_VECTOR_ELT(vec, 0, Rf_mkString(obj.name.c_str()));
    SET_VECTOR_ELT(vec, 1, R_NilValue); // TODO
    SET_VECTOR_ELT(vec, 2, to_sexp(obj.events));
    UNPROTECT(1);
    return vec;
}

rir::recording::FunRecorder fun_recorder_from_sexp(SEXP sexp) {
    assert(Rf_isVector(sexp));
    assert(Rf_length(sexp) == 3);

    rir::recording::FunRecorder recorder;
    recorder.name = serialization::string_from_sexp(VECTOR_ELT(sexp, 0));

    auto events_sexp = VECTOR_ELT(sexp, 2);
    for (auto i = 0; i < Rf_length(events_sexp); i++) {
        auto event_sexp = VECTOR_ELT(events_sexp, i);
        recorder.events.push_back(event_from_sexp(event_sexp));
    }

    return recorder;
}

} // namespace serialization
} // namespace recording
} // namespace rir
