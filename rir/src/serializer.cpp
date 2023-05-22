#include "serializer.h"

namespace serializer {

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

SEXP to_sexp(uint64_t i) {
    // R doesn't have long ints, we use strings instead
    char i_str[21];
    sprintf(i_str, "%lu", i);
    return Rf_mkString(i_str);
}

SEXP to_sexp(const rir::recording::Event& obj) { return obj.to_sexp(); }

SEXP to_sexp(const rir::recording::FunRecorder& obj) {
    const char* fields[] = {"name", "function", "events", ""};
    auto vec = PROTECT(Rf_mkNamed(VECSXP, fields));
    SET_VECTOR_ELT(vec, 0, Rf_mkString(obj.name.c_str()));
    // SET_VECTOR_ELT(vec, 1, obj.to_sexp()); TODO
    SET_VECTOR_ELT(vec, 1, R_NilValue);
    SET_VECTOR_ELT(vec, 2, to_sexp(obj.events));
    UNPROTECT(1);
    return vec;
}

} // namespace serializer
