#include "recording_hooks.h"

#ifdef RECORDING

#include "R/Serialize.h"
#include "R/r.h"
#include "Rinternals.h"
#include "api.h"
#include "compiler/compiler.h"
#include "compiler/pir/closure.h"
#include "compiler/pir/module.h"
#include "compiler/util/visitor.h"

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
CompilationEndEvent::Time compilation_time_start_;

std::unordered_map<pir::ClosureVersion*, std::vector<SpeculativeContext>>
    inner_compilations_sc_;
std::unordered_map<pir::ClosureVersion*, std::string>
    inner_compilations_bitcode_;

size_t compilation_idx_ = -1;

const char* finalizerPath = nullptr;

bool sc_changed_;
bool in_deopt_ = false;

/************************ Hooks **********************************/

#define RECORDER_FILTER_GUARD(field_name)                                      \
    if (!is_recording_ || !filter_.field_name)                                 \
        return;

SpeculativeContext::ObservedCalleesArr
getCallees(const ObservedCallees& callees, Function* baseline) {
    SpeculativeContext::ObservedCalleesArr res;
    res.fill(NO_INDEX);

    for (size_t j = 0; j < callees.numTargets; j++) {
        auto target = callees.getTarget(baseline, j);
        if (Rf_isFunction(target)) {
            res[j] = recorder_.initOrGetRecording(target);
        } else if (TYPEOF(target) == PROMSXP) {
            res[j] = PROMISE_INDEX;
        }
    }

    return res;
}

std::vector<SpeculativeContext> getSpeculativeContext(TypeFeedback* feedback,
                                                      Function* baseline) {
    std::vector<SpeculativeContext> result;

    for (size_t i = 0; i < feedback->callees_size(); i++) {
        auto observed = feedback->callees(i);
        result.emplace_back(getCallees(observed, baseline));
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
    compilation_time_start_ = CompilationEndEvent::Clock::now();

    auto rec_idx = recorder_.initOrGetRecording(cls, name);

    recorder_.push_event(std::make_unique<CompilationStartEvent>(
        rec_idx, name, std::move(compileReasons_)));

    compilation_idx_ = rec_idx;

    recordReasonsClear();
}

void recordOsrCompile(const SEXP cls) {
    recordCompile(cls, "", pir::Compiler::defaultContext);
}

void recordCompileFinish(bool succesful, pir::Module* module) {
    RECORDER_FILTER_GUARD(compile);

    // Inner complations
    module->eachPirClosure([&](pir::Closure* clos) {
        clos->eachVersion([&](pir::ClosureVersion* ver) {
            size_t rec_idx;
            if (clos->hasOriginClosure()) {
                rec_idx = recorder_.initOrGetRecording(clos->rirClosure(),
                                                       clos->name());
            } else {
                rec_idx = recorder_.initOrGetRecording(
                    clos->rirFunction()->dispatchTable());

                auto& entry = recorder_.get_recording(rec_idx);
                if (entry.name.empty()) {
                    entry.name = clos->name();
                }
            }

            Context version = ver->context();

            std::stringstream pir_code;
            ver->printCode(pir_code, false, false);

            auto sc_entry = inner_compilations_sc_.find(ver);
            assert(sc_entry != inner_compilations_sc_.end());

            auto bitcode_entry = inner_compilations_bitcode_.find(ver);
            std::string bitcode;

            if (bitcode_entry != inner_compilations_bitcode_.end()) {
                bitcode = bitcode_entry->second;
            }

            size_t deopt_count = 0;
            pir::Visitor::run(ver->entry, [&deopt_count](pir::Instruction* i) {
                if (pir::Deopt::Cast(i)) {
                    deopt_count++;
                }
            });

            recorder_.push_event(std::make_unique<CompilationEvent>(
                rec_idx, version, std::move(sc_entry->second), bitcode,
                pir_code.str(), deopt_count));
        });
    });

    inner_compilations_bitcode_.clear();
    inner_compilations_sc_.clear();

    // Compilation end
    auto end_time = CompilationEndEvent::Clock::now();
    auto duration = std::chrono::duration_cast<CompilationEndEvent::Duration>(
        end_time - compilation_time_start_);

    recorder_.push_event(std::make_unique<CompilationEndEvent>(
        compilation_idx_, duration, succesful));
}

void addCompilationLLVMBitcode(pir::ClosureVersion* version,
                               llvm::Function* fun) {
    RECORDER_FILTER_GUARD(compile);

    std::stringstream ss{};
    llvm::raw_os_ostream os{ss};

    fun->print(os);

    inner_compilations_bitcode_.emplace(version, ss.str());
}

void addCompilationSC(pir::ClosureVersion* version,
                      TypeFeedback* typeFeedback) {
    RECORDER_FILTER_GUARD(compile);

    auto baseline = version->owner()->rirFunction();
    auto sc = getSpeculativeContext(typeFeedback, baseline);

    inner_compilations_sc_.emplace(version, std::move(sc));
}

void addCompilationSCCloned(pir::ClosureVersion* newVersion,
                            pir::ClosureVersion* prevVersion) {
    RECORDER_FILTER_GUARD(compile);

    auto sc_entry = inner_compilations_sc_.find(prevVersion);
    assert(sc_entry != inner_compilations_sc_.end());

    inner_compilations_sc_.emplace(newVersion, sc_entry->second);
}

void recordDeopt(rir::Code* c, const DispatchTable* dt, DeoptReason& reason,
                 SEXP trigger) {
    assert(!in_deopt_);
    in_deopt_ = true;

    RECORDER_FILTER_GUARD(deopt);

    // find the affected version
    Context version = c->function()->context();

    auto origin_function =
        recorder_.initOrGetRecording(reason.origin.function());

    ssize_t trigger_index = -1;

    if (auto dt = DispatchTable::check(trigger)) {
        trigger_index = recorder_.initOrGetRecording(dt);
        trigger = R_NilValue;
    } else if (Rf_isFunction(trigger)) {
        trigger_index = recorder_.initOrGetRecording(trigger);
        trigger = R_NilValue;
    } else if (!SERIALIZE_SEXP) {
        trigger = R_NilValue;
    }

    recorder_.record<DeoptEvent>(dt, version, reason.reason, origin_function,
                                 reason.origin.index(), trigger, trigger_index);
}

void recordSCDeoptFinish() {
    assert(in_deopt_);
    in_deopt_ = false;
}

void recordInvocation(SEXP cls, Function* f, Context callContext,
                      InvocationEvent::Source source, bool missingAsmptPresent,
                      bool missingAsmptRecovered) {
    RECORDER_FILTER_GUARD(invoke);

    Context version = f->context();

    bool isNative = f->body()->kind == Code::Kind::Native;

    size_t funIdx;
    uintptr_t address;

    if (cls != nullptr) {
        funIdx = recorder_.initOrGetRecording(cls);
        address = reinterpret_cast<uintptr_t>(cls);
    } else {
        address = 0;

        auto* dt = f->dispatchTable();
        if (dt == nullptr) {
            funIdx = recorder_.initOrGetRecording(f);
        } else {
            funIdx = recorder_.initOrGetRecording(dt);
        }
    }

    recorder_.push_event(std::make_unique<InvocationEvent>(
        funIdx, version, source, callContext, isNative, address,
        missingAsmptPresent, missingAsmptRecovered));
}

void recordInvocationDoCall(SEXP cls, Function* f, Context callContext) {
    recordInvocation(cls, f, callContext, InvocationEvent::DoCall, false,
                     false);
}

void recordInvocationNativeCallTrampoline(SEXP cls, Function* f,
                                          Context callContext,
                                          bool missingAsmptPresent,
                                          bool missingAsmptRecovered) {
    recordInvocation(cls, f, callContext, InvocationEvent::NativeCallTrampoline,
                     missingAsmptPresent, missingAsmptRecovered);
}

void recordInvocationRirEval(Function* f) {
    recordInvocation(nullptr, f, Context(), InvocationEvent::RirEval, false,
                     false);
}

void recordUnregisterInvocation(SEXP cls, Function* f) {
    RECORDER_FILTER_GUARD(invoke);
    Context version = f->context();

    recorder_.record<UnregisterInvocationEvent>(cls, version);
}

#define RECORDER_SC_GUARD()                                                    \
    RECORDER_FILTER_GUARD(typeFeedback);                                       \
    if (!sc_changed_)                                                          \
        return;

void recordSC(const SpeculativeContext& sc, size_t idx, Function* fun) {
    auto dt = fun->dispatchTable();

    if (dt == nullptr) {
        recorder_.record<SpeculativeContextEvent>(fun, false, idx, sc,
                                                  sc_changed_, in_deopt_);
        return;
    }

    bool isPromise = fun != dt->baseline();
    recorder_.record<SpeculativeContextEvent>(dt, isPromise, idx, sc,
                                              sc_changed_, in_deopt_);
}

void recordSC(const ObservedCallees& callees, size_t idx, Function* fun) {
    RECORDER_SC_GUARD();
    auto targets = getCallees(callees, fun);
    recordSC(SpeculativeContext(targets), idx, fun);
}

void recordSC(const ObservedTest& test, size_t idx, Function* fun) {
    RECORDER_SC_GUARD();
    recordSC(SpeculativeContext(test), idx, fun);
}

void recordSC(const ObservedValues& type, size_t idx, Function* fun) {
    RECORDER_SC_GUARD();
    recordSC(SpeculativeContext(type), idx, fun);
}

void recordSCChanged(bool changed) {
    RECORDER_FILTER_GUARD(typeFeedback);
    sc_changed_ = changed;
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

        std::cerr << "Recording filter: " << filterArg
                  << "(environment variable)\n";
    }

    if (filePath != nullptr) {
        std::cerr << "Recording to \"" << filePath
                  << "\" (environment variable)\n";
        startRecordings();

        finalizerPath = filePath;

        // Call `loadNamespace("Base")`
        SEXP baseStr = PROTECT(Rf_mkString("base"));
        SEXP expr = PROTECT(Rf_lang2(Rf_install("loadNamespace"), baseStr));
        SEXP namespaceRes = PROTECT(Rf_eval(expr, R_GlobalEnv));

        if (namespaceRes == R_NilValue) {
            std::cerr << "Failed to load namespace base\n";
        } else {
            R_RegisterCFinalizerEx(namespaceRes, &recordFinalizer, TRUE);
        }

        UNPROTECT(3);
    }
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

REXPORT SEXP printEventPart(SEXP obj, SEXP type, SEXP functions) {
    using namespace rir::recording::serialization;

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
        auto disp = from_sexp<rir::Context>(obj);
        ss << disp;
    } else if (type_str == "speculative") {
        auto sc = from_sexp<rir::recording::SpeculativeContext>(obj);

        ss << "[ ";
        switch (sc.type) {
        case rir::recording::SpeculativeContextType::Callees: {
            bool first = true;
            for (auto c : sc.value.callees) {
                if (c == rir::recording::NO_INDEX)
                    break;
                if (first) {
                    first = false;
                } else {
                    ss << ',';
                }

                if (c == rir::recording::PROMISE_INDEX) {
                    ss << "<promise>";
                    continue;
                }

                auto fun = from_sexp<rir::recording::FunRecording>(
                    VECTOR_ELT(functions, c));
                ss << fun;
            }
            ss << " ] Call";
            break;
        }

        case rir::recording::SpeculativeContextType::Test:
            sc.value.test.print(ss);
            ss << " ] Test";
            break;

        case rir::recording::SpeculativeContextType::Values:
            sc.value.values.print(ss);
            ss << " ] Type";
            break;
        }

    } else if (type_str == "reason") {
        auto ev =
            from_sexp<std::unique_ptr<rir::recording::CompileReason>>(obj);
        ev->print(ss);
    } else if (type_str == "invocation_source") {
        auto src = from_sexp<rir::recording::InvocationEvent::Source>(obj);

        switch (src) {
        case rir::recording::InvocationEvent::Unknown:
            ss << "Unknown";
            break;
        case rir::recording::InvocationEvent::DoCall:
            ss << "DoCall";
            break;
        case rir::recording::InvocationEvent::NativeCallTrampoline:
            ss << "NativeCallTrampoline";
            break;
        case rir::recording::InvocationEvent::RirEval:
            ss << "RirEval";
            break;
        }
    } else if (type_str == "deopt_reason") {
        auto reason = from_sexp<rir::DeoptReason::Reason>(obj);

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
    } else if (type_str == "feedback_index") {
        ss << from_sexp<rir::FeedbackIndex>(obj);
    } else if (type_str == "address") {
        ss << "0x" << std::hex << from_sexp<uintptr_t>(obj);
    } else {
        std::cerr << "type parameter '" << type_str
                  << "' is not a known type "
                     "(context,speculative,reason,deopt_reason,address)"
                  << std::endl;
        return R_NilValue;
    }

    return Rf_mkString(ss.str().c_str());
}

REXPORT SEXP recordCustomEvent(SEXP message) {
    if (!Rf_isString(message)) {
        std::cerr << "message is not a string" << std::endl;
        return R_NilValue;
    }

    auto message_str = std::string(CHAR(STRING_ELT(message, 0)));

    rir::recording::recorder_.push_event(
        std::make_unique<rir::recording::CustomEvent>(message_str));

    return R_NilValue;
}

#endif // RECORDING

REXPORT SEXP isRecordingsDefined() {
#ifdef RECORDING
    return Rf_ScalarLogical(TRUE);
#else
    return Rf_ScalarLogical(FALSE);
#endif
}
