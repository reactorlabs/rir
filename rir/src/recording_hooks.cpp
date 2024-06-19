#include "recording_hooks.h"

#ifdef RECORDING

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

namespace rir {
namespace recording {

/************************ Globals **********************************/

// global state
// a flag indicating whether the recording is active or not
bool is_recording_ = false;

InvocationEvent::SourceSet invocation_source_ =
    InvocationEvent::SourceSet::None();

Context call_context_ = Context();

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

CompileReasons compileReasons_;
std::stack<std::pair<CompilationEvent::Time, CompilationEvent>>
    compilation_stack_;

const char* finalizerPath = nullptr;

bool sc_changed_;

/************************ Hooks **********************************/

#define RECORDER_FILTER_GUARD(field_name)                                      \
    if (!is_recording_ || !filter_.field_name)                                 \
        return;

std::vector<SpeculativeContext> getSpeculativeContext(TypeFeedback* feedback,
                                                      Function* baseline) {
    std::vector<SpeculativeContext> result;

    for (size_t i = 0; i < feedback->callees_size(); i++) {
        auto observed = feedback->callees(i);
        SpeculativeContext::ObservedCalleesArr callees;
        callees.fill(NO_INDEX);

        for (size_t j = 0; j < observed.numTargets; j++) {
            auto target = observed.getTarget(baseline, j);
            if (Rf_isFunction(target)) {
                callees[j] = recorder_.initOrGetRecording(target);
            }
        }

        result.emplace_back(callees);
    }

    for (size_t i = 0; i < feedback->tests_size(); i++) {
        result.emplace_back(feedback->test(i));
    }

    for (size_t i = 0; i < feedback->types_size(); i++) {
        result.emplace_back(feedback->types(i));
    }

    return result;
}

void recordCompile(SEXP cls, const std::string& name,
                   const Context& assumptions) {
    RECORDER_FILTER_GUARD(compile);

    auto rec_idx = recorder_.initOrGetRecording(cls, name);
    auto dt = DispatchTable::unpack(BODY(cls));

    auto dispatch_context = assumptions.toI();

    auto baseline = dt->baseline();
    auto sc = getSpeculativeContext(baseline->typeFeedback(), baseline);

    compilation_stack_.emplace(
        std::piecewise_construct,
        std::forward_as_tuple(CompilationEvent::Clock::now()),
        std::forward_as_tuple(rec_idx, dispatch_context, name, std::move(sc),
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
    recordCompile(cls, "", pir::Compiler::defaultContext);
}

void recordLLVMBitcode(llvm::Function* fun) {
    RECORDER_FILTER_GUARD(compile);

    std::stringstream ss{};
    llvm::raw_os_ostream os{ss};

    fun->print(os);

    assert(!compilation_stack_.empty());
    compilation_stack_.top().second.set_bitcode(ss.str());
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

    recorder_.record<DeoptEvent>(dt, version, reason.reason,
                                 reasonCodeIdx.first, reasonCodeIdx.second,
                                 reason.origin.idx(), trigger);
}

void recordInvocation(Function* f) {
    RECORDER_FILTER_GUARD(invoke);

    Context version = f->context();
    auto* dt = f->dispatchTable();

    bool isNative = f->body()->kind == Code::Kind::Native;
    SEXP cls = dt->container();

    recorder_.record<InvocationEvent>(dt, version, invocation_source_,
                                      call_context_, isNative,
                                      reinterpret_cast<uintptr_t>(cls));

    invocation_source_ = InvocationEvent::SourceSet::None();
    call_context_ = Context();
}

void recordUnregisterInvocation(Function* f) {
    RECORDER_FILTER_GUARD(invoke);
    Context version = f->context();
    auto* dt = f->dispatchTable();

    recorder_.record<UnregisterInvocationEvent>(dt, version);
}

void recordInvocationDoCall(Context callContext) {
    RECORDER_FILTER_GUARD(invoke);
    invocation_source_.set(InvocationEvent::DoCall);
    call_context_ = callContext;
}

void recordInvocationNativeCallTrampoline(Context callContext) {
    RECORDER_FILTER_GUARD(invoke);
    invocation_source_.set(InvocationEvent::NativeCallTrampoline);
    call_context_ = callContext;
}

void recordSC(const SpeculativeContext& sc, size_t idx, Function* fun) {
    auto dt = fun->dispatchTable();
    bool isPromise = fun != dt->baseline();

    recorder_.record<SpeculativeContextEvent>(dt, isPromise, idx, sc,
                                              sc_changed_);
}

void recordSC(const ObservedCallees& callees, size_t idx, Function* fun) {
    RECORDER_FILTER_GUARD(typeFeedback);
    SpeculativeContext::ObservedCalleesArr targets;
    targets.fill(-1);

    for (size_t i = 0; i < callees.numTargets; i++) {
        targets[i] = recorder_.initOrGetRecording(callees.getTarget(fun, i));
    }

    recordSC(SpeculativeContext(targets), idx, fun);
}

void recordSC(const ObservedTest& test, size_t idx, Function* fun) {
    RECORDER_FILTER_GUARD(typeFeedback);
    recordSC(SpeculativeContext(test), idx, fun);
}

void recordSC(const ObservedValues& type, size_t idx, Function* fun) {
    RECORDER_FILTER_GUARD(typeFeedback);
    recordSC(SpeculativeContext(type), idx, fun);
}

void recordSCChanged(bool changed) {
    RECORDER_FILTER_GUARD(typeFeedback);
    sc_changed_ = changed;
}

/**
 * Output stream transformer that prefixes each line with a string
 */
class Prefixer : private std::streambuf, public std::ostream {
  public:
    Prefixer(std::ostream& base, const char* prefix)
        : std::ostream(this), base(base), prefix(prefix) {
        base.flush();
    }

    inline virtual int sync(void) override {
        base.flush();
        return 0;
    }

  private:
    std::ostream& base;
    const char* prefix;

    bool needsPrefix = true;

    int overflow(int c) override {
        if (needsPrefix) {
            base << prefix;
            needsPrefix = false;
        }

        if (c == '\n') {
            needsPrefix = true;
        }

        base.put(c);
        return 0;
    }
};

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
void recordOsrTriggerCallerCallee() {
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
        std::cerr << ": " << filterArg;
    }

    std::cerr << ")\n";
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

        if (!Rf_isVector(expr) || Rf_length(expr) != 2) {
            Rf_error("Expression is not a vector");
        }

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
            ss << "[";
            bool first = true;
            for (auto c : sc.value.callees) {
                if (c == rir::recording::NO_INDEX)
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
            ss << "(";
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
            ss << ")";
            break;

        case rir::recording::SpeculativeContextType::Values:
            ss << "(";
            sc.value.values.print(ss);
            ss << ")";
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

#endif // RECORDING

REXPORT SEXP isRecordingsDefined() {
#ifdef RECORDING
    return Rf_ScalarLogical(TRUE);
#else
    return Rf_ScalarLogical(FALSE);
#endif
}
