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

const std::vector<const char*> PirWarmupReason::fieldNames = {
    "invocationCount"};

SEXP PirWarmupReason::toSEXP() const {
    return serialization::fields_to_sexp<PirWarmupReason>(invocationCount);
}

void PirWarmupReason::fromSEXP(SEXP sexp) {
    serialization::fields_from_sexp<PirWarmupReason>(sexp, invocationCount);
}

const std::vector<const char*> OSRLoopReason::fieldNames = {"loopCount"};

SEXP OSRLoopReason::toSEXP() const {
    return serialization::fields_to_sexp<OSRLoopReason>(loopCount);
}

void OSRLoopReason::fromSEXP(SEXP sexp) {
    serialization::fields_from_sexp<OSRLoopReason>(sexp, loopCount);
}

size_t Record::initOrGetRecording(const SEXP cls, const std::string& name) {
    auto address = reinterpret_cast<uintptr_t>(cls);

    if (TYPEOF(cls) == EXTERNALSXP) {
        auto dt = DispatchTable::unpack(cls);
        size_t idx = initOrGetRecording(dt);

        auto& entry = get_recording(idx);
        if (entry.name.empty()) {
            entry.name = name;
        }
        entry.address = address;

        return idx;
    }

    assert(Rf_isFunction(cls));
    auto body = BODY(cls);

    auto getClosure = [cls]() {
        if (SERIALIZE_SEXP) {
            auto closure = R_serialize(cls, R_NilValue, R_NilValue, R_NilValue,
                                       R_NilValue);
            R_PreserveObject(closure);
            return closure;
        } else {
            return R_NilValue;
        }
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
        if (SERIALIZE_SEXP && Rf_isNull(rec.closure)) {
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

        functions.emplace_back(getName(), envName, getClosure(), address);

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

        functions.emplace_back(getName(), envName, getClosure(), address);
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

    functions.emplace_back("", reinterpret_cast<uintptr_t>(dt));
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

    functions.emplace_back(ss.str(), reinterpret_cast<uintptr_t>(fun));
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

const std::vector<const char*> FunRecording::fieldNames = {
    "primIdx", "name", "env", "closure", "address"};

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
    "funIdx", "is_promise", "index", "sc", "changed", "deopt"};

SEXP SpeculativeContextEvent::toSEXP() const {
    return serialization::fields_to_sexp<SpeculativeContextEvent>(
        funRecIndex_, is_promise, index, sc, changed, deopt);
}

void SpeculativeContextEvent::fromSEXP(SEXP sexp) {
    return serialization::fields_from_sexp<SpeculativeContextEvent>(
        sexp, funRecIndex_, is_promise, index, sc, changed, deopt);
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
    serialization::fields_from_sexp<CompilationStartEvent>(
        sexp, funRecIndex_, compileName, compile_reasons.heuristic,
        compile_reasons.condition, compile_reasons.osr);
}

const std::vector<const char*> CompilationEvent::fieldNames = {
    "funIdx",  "version",  "speculative_contexts",
    "bitcode", "pir_code", "deopt_count"};

SEXP CompilationEvent::toSEXP() const {
    return serialization::fields_to_sexp<CompilationEvent>(
        funRecIndex_, version, speculative_contexts, bitcode, pir_code,
        deopt_count);
}

void CompilationEvent::fromSEXP(SEXP sexp) {
    serialization::fields_from_sexp<CompilationEvent>(
        sexp, funRecIndex_, version, speculative_contexts, bitcode, pir_code,
        deopt_count);
}

const std::vector<const char*> CompilationEndEvent::fieldNames = {
    "funIdx", "time_length", "succesful"};

SEXP CompilationEndEvent::toSEXP() const {
    return serialization::fields_to_sexp<CompilationEndEvent>(
        funRecIndex_, time_length, succesful);
}

void CompilationEndEvent::fromSEXP(SEXP sexp) {
    serialization::fields_from_sexp<CompilationEndEvent>(
        sexp, funRecIndex_, time_length, succesful);
}

const std::vector<const char*> DeoptEvent::fieldNames = {
    "funIdx", "version", "reason",       "origin_function",
    "index",  "trigger", "trigger_index"};

SEXP DeoptEvent::toSEXP() const {
    return serialization::fields_to_sexp<DeoptEvent>(
        funRecIndex_, version, reason, origin_function, index, trigger,
        trigger_index);
}

void DeoptEvent::fromSEXP(SEXP sexp) {
    serialization::fields_from_sexp<DeoptEvent>(sexp, funRecIndex_, version,
                                                reason, origin_function, index,
                                                trigger, trigger_index);

    if (trigger != R_NilValue) {
        R_PreserveObject(trigger);
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
    serialization::fields_from_sexp<InvocationEvent>(
        sexp, funRecIndex_, version, source, callContext, isNative, address,
        missingAsmptPresent, missingAsmptRecovered);
}

const std::vector<const char*> UnregisterInvocationEvent::fieldNames = {
    "funIdx", "context"};

SEXP UnregisterInvocationEvent::toSEXP() const {
    return serialization::fields_to_sexp<UnregisterInvocationEvent>(
        funRecIndex_, version);
}

void UnregisterInvocationEvent::fromSEXP(SEXP sexp) {
    serialization::fields_from_sexp<UnregisterInvocationEvent>(
        sexp, funRecIndex_, version);
}

const std::vector<const char*> CustomEvent::fieldNames = {"name"};

SEXP CustomEvent::toSEXP() const {
    return serialization::fields_to_sexp<CustomEvent>(name);
}

void CustomEvent::fromSEXP(SEXP sexp) {
    serialization::fields_from_sexp<CustomEvent>(sexp, name);
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
    auto symbols = PROTECT(R_lsInternal3(env, TRUE, FALSE));

    auto size = Rf_length(symbols);
    for (int i = 0; i < size; i++) {
        const char* symbol_char = CHAR(VECTOR_ELT(symbols, i));

        // TODO: check parity with R_findVarInFrame
        auto symbol = PROTECT(Rf_install(symbol_char));
        R_varloc_t loc = R_findVarLocInFrame(env, symbol);
        UNPROTECT(1);

        if (loc.cell == cls) {
            name = symbol_char;
            break;
        }
    }

    UNPROTECT(2);
    return name;
}

} // namespace recording
} // namespace rir

#endif // RECORDING
