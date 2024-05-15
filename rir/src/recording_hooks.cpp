#include "recording_hooks.h"
#include "R/Serialize.h"
#include "R/r.h"
#include "Rinternals.h"
#include "api.h"
#include "compiler/compiler.h"
#include "recording_serialization.h"
#include "runtime/Context.h"
#include "runtime/DispatchTable.h"
#include "runtime/Function.h"
#include "runtime/TypeFeedback.h"

#include "llvm/Support/raw_os_ostream.h"

#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <memory>
#include <ostream>
#include <sstream>
#include <string>
#include <utility>
#include <vector>

#include "prefixer.cpp"

namespace rir {
namespace recording {

/************************ Globals **********************************/

// global state
// a flag indicating whether the recording is active or not
bool is_recording_ = false;

InvocationEvent::SourceSet invocation_source_ =
    InvocationEvent::SourceSet::None();

// the main recorder
Record recorder_;

/**
 * Bitmask filter of events to record
 */
struct {
    bool compile : 1;
    bool deopt : 1;
    bool typeFeedback : 1;
    bool invoke : 1;
} filter_ = {
    .compile = true,
    .deopt = true,
    .typeFeedback = false,
    .invoke = false,
};

// Don't record invocations while replaying compile events
static bool isPlayingCompile = false;

CompileReasons compileReasons_;
std::stack<std::pair<CompilationEvent::Time, CompilationEvent>>
    compilation_stack_;

const Code* nextSCcode = nullptr;

int test = 0;

const char* finalizerPath = nullptr;

/************************ Hooks **********************************/

#define RECORDER_FILTER_GUARD(field_name)                                      \
    if (!is_recording_ || !filter_.field_name)                                 \
        return;

void recordCompile(SEXP cls, const std::string& name,
                   const Context& assumptions) {
    RECORDER_FILTER_GUARD(compile);

    auto rec = recorder_.initOrGetRecording(cls, name);
    if (rec.second.name == "") {
        rec.second.name = name;
    }

    std::vector<SpeculativeContext> sc;
    auto dt = DispatchTable::unpack(BODY(cls));
    recorder_.recordSpeculativeContext(dt, sc);

    auto dispatch_context = assumptions.toI();

    compilation_stack_.emplace(
        std::piecewise_construct,
        std::forward_as_tuple(CompilationEvent::Clock::now()),
        std::forward_as_tuple(rec.first, dispatch_context, name, std::move(sc),
                              std::move(compileReasons_)));
    recordReasonsClear();
}

void recordCompileFinish(bool succesful) {
    RECORDER_FILTER_GUARD(compile);

    auto end_time = CompilationEvent::Clock::now();

    assert(!compilation_stack_.empty());
    auto start_time = compilation_stack_.top().first;
    auto event = std::make_unique<CompilationEvent>(
        std::move(compilation_stack_.top().second));
    compilation_stack_.pop();

    auto duration = std::chrono::duration_cast<CompilationEvent::Duration>(
        end_time - start_time);
    event->set_time(duration);
    event->set_success(succesful);

    size_t idx = recorder_.push_event(std::move(event));

    if (!compilation_stack_.empty()) {
        compilation_stack_.top().second.add_subcompilation(idx);
    }
}

void recordOsrCompile(const SEXP cls) {
    RECORDER_FILTER_GUARD(compile);

    auto env = PROTECT(CLOENV(cls));
    auto symbols = PROTECT(R_lsInternal(env, TRUE));

    std::string name = "";

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

    recordCompile(cls, name, pir::Compiler::defaultContext);
}

void recordLLVMBitcode(llvm::Function* fun) {
    RECORDER_FILTER_GUARD(compile);

    std::stringstream ss{};
    llvm::raw_os_ostream os{ss};

    fun->print(os);

    assert(!compilation_stack_.empty());
    compilation_stack_.top().second.set_bitcode(ss.str());
}

size_t Record::indexOfBaseline(const rir::Code* code) {
    DispatchTable* dt = code->function()->dispatchTable();
    return initOrGetRecording(const_cast<DispatchTable*>(dt)).first;
}

void recordDeopt(rir::Code* c, const DispatchTable* dt, DeoptReason& reason,
                 SEXP trigger) {
    RECORDER_FILTER_GUARD(deopt);

    // find the affected version
    Context version;
    bool found = false;
    // it deopts from native so it cannot be the baseline RIR
    for (size_t i = 1; i < dt->size(); i++) {
        auto* fi = dt->get(i);
        if (fi->body() == c) {
            version = fi->context();
            found = true;
            break;
        }
    }

    if (!found) {
        version = c->function()->context();
    }

    auto reasonCodeIdx = recorder_.findIndex(dt->baseline()->body(),
                                             reason.origin.function()->body());

    assert(reasonCodeIdx.first >= 0 &&
           "Could not locate deopt reason location");

    recorder_.record<DeoptEvent>(dt, version, reason.reason, reasonCodeIdx,
                                 reason.origin.idx(), trigger);
}

void recordDtOverwrite(const DispatchTable* dt, size_t funIdx,
                       size_t oldDeoptCount) {
    if (!is_recording_) {
        return;
    }

    recorder_.record<DtInitEvent>(dt, funIdx, oldDeoptCount);
}

void recordInvocation(Function* f, ssize_t deltaCount, size_t previousCount) {
    RECORDER_FILTER_GUARD(invoke);
    if (!is_recording_ || isPlayingCompile)
        return;

    Context version = f->context();
    auto* dt = f->dispatchTable();
    if (!dt) {
        return;
    }

    if (!recorder_.contains(dt)) {
        return;
    }

    recorder_.record<InvocationEvent>(dt, version, deltaCount, previousCount,
                                      invocation_source_);
    invocation_source_ = InvocationEvent::SourceSet::None();
}

void recordInvocationDoCall() {
    RECORDER_FILTER_GUARD(invoke);
    invocation_source_.set(InvocationEvent::DoCall);
}

void recordInvocationNativeCallTrampoline() {
    RECORDER_FILTER_GUARD(invoke);
    invocation_source_.set(InvocationEvent::NativeCallTrampoline);
}

#define RECORD_SC_GUARD()                                                      \
    if (!is_recording_ || isPlayingCompile)                                    \
        return;

void prepareRecordSC(const Code* container) {
    RECORDER_FILTER_GUARD(typeFeedback);
    RECORD_SC_GUARD();
    test++;
    if (nextSCcode) {
        std::cerr << "prepared speculative context record twice" << std::endl;
        abort();
    } else {
        nextSCcode = container;
    }
}

inline const Code* getSCCode() {
    assert(nextSCcode && "recordSC wasn't prepared");
    auto* next = nextSCcode;
    nextSCcode = nullptr;
    return next;
}

void recordSC(const Opcode* immediate, const SpeculativeContext& sc) {
    RECORDER_FILTER_GUARD(typeFeedback);
    auto* c = getSCCode();
    ptrdiff_t offset = immediate - c->code();
    assert(offset > 0 && offset < (ptrdiff_t)c->size());

    auto* dt = c->function()->dispatchTable();
    auto* baseline = dt->baseline()->body();
    ssize_t codeIndex = -2;
    if (baseline == c) {
        codeIndex = -1;
    } else {
        for (size_t i = 0; i < baseline->extraPoolSize; i++) {
            SEXP epe = baseline->getExtraPoolEntry(i);
            if (Code::check(epe) == c) {
                codeIndex = i;
                break;
            }
        }
    }

    if (codeIndex == -2) {
        std::cerr << "[rec speculative ctx] code not found" << std::endl;
        return;
    }

    recorder_.record<SpeculativeContextEvent>(dt, codeIndex, (size_t &&) offset,
                                              sc);
}

void recordSC(const ObservedCallees& callees) {
    RECORDER_FILTER_GUARD(typeFeedback);
    RECORD_SC_GUARD();
    decltype(SpeculativeContext::Value::callees) targets;
    targets.fill(-1);

    auto* code = getSCCode();

    for (size_t i = 0; i < callees.numTargets; i++) {
        targets[i] =
            recorder_.initOrGetRecording(callees.getTarget(code->function(), i))
                .first;
    }

    nextSCcode = code;

    recordSC((Opcode*)&callees, SpeculativeContext(targets));
}

void recordSC(const ObservedTest& test) {
    RECORDER_FILTER_GUARD(typeFeedback);
    RECORD_SC_GUARD();
    recordSC((Opcode*)&test, SpeculativeContext(test));
}

void recordSC(const ObservedValues& type) {
    RECORDER_FILTER_GUARD(typeFeedback);
    RECORD_SC_GUARD();
    recordSC((Opcode*)&type, SpeculativeContext(type));
}

SEXP setClassName(SEXP s, const char* className) {
    SEXP t = PROTECT(Rf_mkString(className));
    Rf_setAttrib(s, R_ClassSymbol, t);
    UNPROTECT(1);
    return s;
}

void printRecordings(
    std::ostream& out,
    const std::vector<std::unique_ptr<rir::recording::Event>>& events,
    const std::vector<FunRecording>& functions) {
    for (auto& eventEntry : events) {
        const char* name = eventEntry->targetName(functions);

        // If name is empty (unknown), use a different display strategy
        if (*name == 0) {
            name = "<?>";
        }

        Prefixer prefixed(out, name);
        prefixed << "    ";
        eventEntry->print(functions, prefixed);
        prefixed << std::endl;
    }
}

// Compile heuristics
void recordMarkOptReasonHeuristic() {
    RECORDER_FILTER_GUARD(compile)
    compileReasons_.set_heuristic<MarkOptReason>();
}

void recordInvocationCountTimeReason(size_t count, size_t minimalCount,
                                     unsigned long time,
                                     unsigned long minimalTime) {
    RECORDER_FILTER_GUARD(compile)
    compileReasons_.set_heuristic<InvocationCountTimeReason>(
        count, minimalCount, time, minimalTime);
}

void recordPirWarmupReason(size_t invocation_count) {
    RECORDER_FILTER_GUARD(compile)
    compileReasons_.set_heuristic<PirWarmupReason>(invocation_count);
}

// Compile condition
void recordMarkOptReasonCondition() {
    RECORDER_FILTER_GUARD(compile)
    compileReasons_.set_condition<MarkOptReason>();
}

void recordNotOptimizedReason() {
    RECORDER_FILTER_GUARD(compile)
    compileReasons_.set_condition<NotOptimizedReason>();
}

void recordIsImprovingReason() {
    RECORDER_FILTER_GUARD(compile)
    compileReasons_.set_condition<IsImprovingReason>();
}

void recordReoptimizeFlagReason() {
    RECORDER_FILTER_GUARD(compile)
    compileReasons_.set_condition<ReoptimizeFlagReason>();
}

// OSR reason
void recordOsrTriggerCallerCalle() {
    RECORDER_FILTER_GUARD(compile)
    compileReasons_.set_osr<OSRCallerCalleeReason>();
}

void recordOsrTriggerLoop(size_t loopCount) {
    RECORDER_FILTER_GUARD(compile)
    compileReasons_.set_osr<OSRLoopReason>(loopCount);
}

void recordReasonsClear() {
    compileReasons_.heuristic = nullptr;
    compileReasons_.condition = nullptr;
    compileReasons_.osr = nullptr;
}

void recordFinalizer(SEXP) {
    std::cerr << "Saving recording to \"" << finalizerPath << "\"\n";
    stopRecordings();

    SEXP filepath = PROTECT(Rf_mkString(finalizerPath));
    saveRecordings(filepath);
    UNPROTECT(1);
}

void recordExecution(const char* filePath, const char* filterArg) {
    if (filterArg != nullptr) {
        filter_ = {.compile = false,
                   .deopt = false,
                   .typeFeedback = false,
                   .invoke = false};

        std::istringstream is(filterArg);
        std::string str;

        while (std::getline(is, str, ',')) {
            if (str == "Compile") {
                filter_.compile = true;
            } else if (str == "Deopt") {
                filter_.deopt = true;
            } else if (str == "TypeFeedback") {
                filter_.typeFeedback = true;
            } else if (str == "Invoke") {
                filter_.invoke = true;
            } else {
                std::cerr << "Unknown recording filter type: " << str
                          << "\nValid flags are:\n- Compile\n- Deopt\n- "
                             "TypeFeedback\n- Invoke\n";
                exit(1);
            }
        }
    }

    std::cerr << "Recording to \"" << filePath << "\" (environment variable";
    if (filterArg != nullptr) {
        std::cerr << ": " << filterArg << ")\n";
    } else {
        std::cerr << ")\n";
    }
    startRecordings();

    finalizerPath = filePath;

    // Call `loadNamespace("Base")`
    SEXP baseStr = PROTECT(Rf_mkString("base"));
    SEXP expr = PROTECT(Rf_lang2(Rf_install("loadNamespace"), baseStr));
    SEXP namespaceRes = PROTECT(Rf_eval(expr, R_GlobalEnv));

    if (namespaceRes == R_NilValue) {
        std::cerr << "Failed to load namespace base\n";
        UNPROTECT(3);
        return;
    }

    R_RegisterCFinalizerEx(namespaceRes, &recordFinalizer, TRUE);
    UNPROTECT(3);
}
} // namespace recording
} // namespace rir

REXPORT SEXP filterRecordings(SEXP compile, SEXP deoptimize, SEXP typeFeedback,
                              SEXP invocation) {
    rir::recording::filter_ = {
        .compile = (bool)Rf_asLogical(compile),
        .deopt = (bool)Rf_asLogical(deoptimize),
        .typeFeedback = (bool)Rf_asLogical(typeFeedback),
        .invoke = (bool)Rf_asLogical(invocation),
    };

    return R_NilValue;
}

REXPORT SEXP startRecordings() {
    rir::recording::is_recording_ = true;
    rir::recording::recorder_.reset();
    return R_NilValue;
}

REXPORT SEXP stopRecordings() {
    rir::recording::is_recording_ = false;
    return R_NilValue;
}

REXPORT SEXP resetRecordings() {
    rir::recording::recorder_.reset();
    return R_NilValue;
}
REXPORT SEXP isRecordings() {
    // Incorrectly identified by cppcheck
    // cppcheck-suppress constArgument
    return Rf_ScalarLogical(rir::recording::is_recording_);
}

REXPORT SEXP saveRecordings(SEXP filename) {
    if (TYPEOF(filename) != STRSXP)
        Rf_error("must provide a string path");

    FILE* file = fopen(CHAR(Rf_asChar(filename)), "w");
    if (!file)
        Rf_error("couldn't open file at path");

    auto recordings = PROTECT(getRecordings());
    R_SaveToFile(recordings, file, 3);
    fclose(file);
    UNPROTECT(1);

    return R_NilValue;
}

REXPORT SEXP loadRecordings(SEXP filename) {
    if (TYPEOF(filename) != STRSXP)
        Rf_error("must provide a string path");

    FILE* file = fopen(CHAR(Rf_asChar(filename)), "r");
    if (!file)
        Rf_error("couldn't open file at path");

    auto res = R_LoadFromFile(file, 3);

    fclose(file);

    return res;
}

REXPORT SEXP getRecordings() { return rir::recording::recorder_.save(); }

REXPORT SEXP printRecordings(SEXP from) {
    auto& out = std::cout;
    out << "Recordings:" << std::endl;

    if (Rf_isNull(from)) {
        rir::recording::printRecordings(out, rir::recording::recorder_.log,
                                        rir::recording::recorder_.functions);
    } else {
        SEXP expr;
        if (Rf_isString(from)) {
            expr = PROTECT(loadRecordings(from));
        } else {
            expr = from;
        }

        assert(Rf_isVector(expr));
        assert(Rf_length(expr) == 2);

        // Populate functions
        auto bodies = VECTOR_ELT(expr, 0);
        size_t bodiesLength = Rf_length(bodies);

        std::vector<rir::recording::FunRecording> functions;
        functions.reserve(bodiesLength);

        for (size_t i = 0; i < bodiesLength; i++) {
            functions.push_back(
                rir::recording::serialization::fun_recorder_from_sexp(
                    VECTOR_ELT(bodies, i)));
        }

        // Populate events
        auto log = VECTOR_ELT(expr, 1);
        size_t logLength = Rf_length(log);

        std::vector<std::unique_ptr<rir::recording::Event>> logVector;
        logVector.reserve(logLength);

        for (size_t i = 0; i < logLength; i++) {
            logVector.push_back(rir::recording::serialization::event_from_sexp(
                VECTOR_ELT(log, i)));
        }

        rir::recording::printRecordings(out, logVector, functions);

        if (expr != from) {
            UNPROTECT(1);
        }
    }

    return R_NilValue;
}

REXPORT SEXP printEventPart(SEXP obj, SEXP type, SEXP functions) {
    if (Rf_isNull(obj)) {
        return Rf_mkString("");
    }

    if (!Rf_isString(type)) {
        std::cerr << "type parameter is not a string" << std::endl;
        return R_NilValue;
    }

    auto type_str = std::string(CHAR(STRING_ELT(type, 0)));

    std::ostringstream ss;

    if (type_str == "context") { // version or dispatch context
        auto disp = rir::recording::serialization::context_from_sexp(obj);
        ss << disp;
    } else if (type_str == "speculative") {
        auto sc =
            rir::recording::serialization::speculative_context_from_sexp(obj);

        switch (sc.type) {
        case rir::recording::SpeculativeContextType::Callees: {
            ss << "Callees[";
            bool first = true;
            for (auto c : sc.value.callees) {
                if (c == NO_INDEX)
                    break;
                if (first) {
                    first = false;
                } else {
                    ss << ',';
                }

                auto fun =
                    rir::recording::serialization::fun_recorder_from_sexp(
                        VECTOR_ELT(functions, c));
                ss << fun;
            }
            ss << "]";
            break;
        }

        case rir::recording::SpeculativeContextType::Test:
            ss << "Test{";
            switch (sc.value.test.seen) {
            case rir::ObservedTest::None:
                ss << "None";
                break;
            case rir::ObservedTest::OnlyTrue:
                ss << "OnlyTrue";
                break;
            case rir::ObservedTest::OnlyFalse:
                ss << "OnlyFalse";
                break;
            case rir::ObservedTest::Both:
                ss << "Both";
                break;
            }
            ss << "}";
            break;

        case rir::recording::SpeculativeContextType::Values:
            ss << "Values{";
            sc.value.values.print(ss);
            ss << "}";
            break;
        }
    } else if (type_str == "reason") {
        auto ev = rir::recording::serialization::compile_reason_from_sexp(obj);
        ev->print(ss);
    } else if (type_str == "invocation_source") {
        auto src =
            rir::recording::serialization::invocation_source_set_from_sexp(obj);

        if (src.contains(rir::recording::InvocationEvent::DoCall) &&
            src.contains(
                rir::recording::InvocationEvent::NativeCallTrampoline)) {
            ss << "DoCall,NativeCallTrampoline";
        } else if (src.contains(rir::recording::InvocationEvent::DoCall)) {
            ss << "DoCall";
        } else if (src.contains(
                       rir::recording::InvocationEvent::NativeCallTrampoline)) {
            ss << "NativeCallTrampoline";
        }

    } else if (type_str == "deopt_reason") {
        auto reason =
            rir::recording::serialization::deopt_reason_from_sexp(obj);

        switch (reason) {
        case rir::DeoptReason::Reason::Typecheck:
            ss << "Typecheck";
            break;

        case rir::DeoptReason::Reason::DeadCall:
            ss << "DeadCall";
            break;

        case rir::DeoptReason::Reason::CallTarget:
            ss << "CallTarget";
            break;

        case rir::DeoptReason::Reason::ForceAndCall:
            ss << "ForceAndCall";
            break;

        case rir::DeoptReason::Reason::EnvStubMaterialized:
            ss << "EnvStubMaterialized";
            break;

        case rir::DeoptReason::Reason::DeadBranchReached:
            ss << "DeadBranchReached";
            break;

        case rir::DeoptReason::Reason::Unknown:
            ss << "Unknown";
            break;
        }
    } else {
        std::cerr
            << "type parameter '" << type_str
            << "' is not a known type (context,speculative,reason,deopt_reason)"
            << std::endl;
        return R_NilValue;
    }

    return Rf_mkString(ss.str().c_str());
}
