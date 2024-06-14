#include "TypeFeedback.h"

#include "R/Serialize.h"
#include "R/Symbols.h"
#include "R/r.h"
#include "interpreter/call_context.h"
#include "runtime/Code.h"
#include "runtime/Function.h"

#include <cassert>
#include <ostream>
#include <vector>

namespace rir {

void ObservedCallees::addCallee(Function* function, SEXP callee) {
    int i = 0;
    // Because of recording type feedback even before creating function
    // and saving callees inside function body
    // all callees are stored inside the baseline function
    auto caller = function->baseline()->body();
    for (; i < numTargets; ++i)
        if (caller->getExtraPoolEntry(targets[i]) == callee)
            break;
    if (i == numTargets) {
        unsigned int idx;
        for (idx = caller->promEnd; idx < caller->extraPoolSize; ++idx)
            if (caller->getExtraPoolEntry(idx) == callee)
                break;
        if (idx == caller->extraPoolSize)
            idx = caller->addExtraPoolEntry(callee);
        targets[numTargets++] = idx;
    }
}

void ObservedCallees::record(Function* function, SEXP callee,
                             bool invalidateWhenFull) {
    if (taken < CounterOverflow)
        taken++;

    if (numTargets < MaxTargets)
        addCallee(function, callee);
    else if (invalidateWhenFull)
        invalid = true;
}

void ObservedCallees::mergeWith(const ObservedCallees& callees,
                                Function* function) {
    if (taken <= CounterOverflow - callees.taken)
        taken += callees.taken;
    else
        taken = CounterOverflow;
    auto caller = function->baseline()->body();
    for (unsigned i = 0; i < callees.numTargets; ++i) {
        if (numTargets == MaxTargets)
            return;
        addCallee(function, caller->getExtraPoolEntry(callees.targets[i]));
    }
}

SEXP ObservedCallees::getTarget(const Function* function, size_t pos) const {
    assert(pos < numTargets);
    return function->baseline()->body()->getExtraPoolEntry(targets[pos]);
}

FeedbackOrigin::FeedbackOrigin(rir::Function* function, FeedbackIndex index)
    : index_(index), function_(function) {
    assert(function->typeFeedback()->isValid(index));
}

DeoptReason::DeoptReason(const FeedbackOrigin& origin,
                         DeoptReason::Reason reason)
    : reason(reason), origin(origin) {}

void DeoptReason::record(SEXP val, const Context& context) const {
    origin.function()->registerDeoptReason(reason);
    assert(origin.function()->dispatchTable());
    auto baselineFeedback =
        origin.function()->dispatchTable()->baselineFeedback();
    auto tf = origin.function()->typeFeedback(context);

    switch (reason) {
    case DeoptReason::Unknown:
        break;
    case DeoptReason::DeadBranchReached: {
        auto& feedback = tf->test(origin.idx());
        feedback.seen = ObservedTest::Both;
        REC_HOOK(recording::recordSC(feedback, origin.function()));
        auto& bf = baselineFeedback->test(origin.idx());
        bf.seen = ObservedTest::Both;
        REC_HOOK(recording::recordSC(bf, origin.function()));
        break;
    }
    case DeoptReason::Typecheck: {
        if (val == symbol::UnknownDeoptTrigger)
            break;

        // FIXME: (cf. #1260) very similar code is in the recordTypeFeedbackImpl
        // IMHO the one there is more correct. Would it make sense
        // to pull this into the TypeFeedback::record_type()?
        // and get rid of the overload that takes lambda?
        tf->record_typeInc(baselineFeedback, origin.idx(), val);
        tf->record_typeInc(baselineFeedback, origin.idx(), [&](auto& slot) {
            if (TYPEOF(val) == PROMSXP) {
                if (PRVALUE(val) == R_UnboundValue &&
                    slot.stateBeforeLastForce < ObservedValues::promise)
                    slot.stateBeforeLastForce = ObservedValues::promise;
                else if (slot.stateBeforeLastForce <
                         ObservedValues::evaluatedPromise)
                    slot.stateBeforeLastForce =
                        ObservedValues::evaluatedPromise;
            }
        });
        break;
    }
    case DeoptReason::DeadCall:
    case DeoptReason::ForceAndCall:
    case DeoptReason::CallTarget: {
        if (val == symbol::UnknownDeoptTrigger)
            break;
        tf->record_calleeInc(baselineFeedback, origin.idx(), origin.function(),
                             val, true);
        break;
    }
    case DeoptReason::EnvStubMaterialized: {
        break;
    }
    }
}

void ObservedCallees::print(std::ostream& out, const Function* function) const {
    if (taken == ObservedCallees::CounterOverflow)
        out << "*, <";
    else
        out << taken << ", <";
    if (numTargets == ObservedCallees::MaxTargets)
        out << "*>, ";
    else
        out << numTargets << ">, ";

    out << (invalid ? "invalid" : "valid");
    out << (numTargets ? ", " : " ");

    for (unsigned i = 0; i < numTargets; ++i) {
        auto target = getTarget(function, i);
        out << target << "(" << Rf_type2char(TYPEOF(target)) << ") ";
    }
}

void TypeFeedback::serialize(SEXP refTable, R_outpstream_t out) const {
    OutInteger(out, recordingCount_);
    OutInteger(out, callees_size_);
    for (size_t i = 0; i < callees_size_; i++) {
        OutBytes(out, callees_ + i, sizeof(ObservedCallees));
    }

    OutInteger(out, tests_size_);
    for (size_t i = 0; i < tests_size_; i++) {
        OutBytes(out, tests_ + i, sizeof(ObservedTest));
    }

    OutInteger(out, types_size_);
    for (size_t i = 0; i < types_size_; i++) {
        OutBytes(out, types_ + i, sizeof(ObservedValues));
    }
}

TypeFeedback* TypeFeedback::deserialize(SEXP refTable, R_inpstream_t inp) {
    auto recordingCount = InInteger(inp);
    auto size = InInteger(inp);
    std::vector<ObservedCallees> callees;
    callees.reserve(size);
    for (auto i = 0; i < size; ++i) {
        ObservedCallees tmp;
        InBytes(inp, &tmp, sizeof(ObservedCallees));
        callees.push_back(std::move(tmp));
    }

    size = InInteger(inp);
    std::vector<ObservedTest> tests;
    tests.reserve(size);
    for (auto i = 0; i < size; ++i) {
        ObservedTest tmp;
        InBytes(inp, &tmp, sizeof(ObservedTest));
        tests.push_back(std::move(tmp));
    }

    size = InInteger(inp);
    std::vector<ObservedValues> types;
    types.reserve(size);
    for (auto i = 0; i < size; ++i) {
        ObservedValues tmp;
        InBytes(inp, &tmp, sizeof(ObservedValues));
        types.push_back(std::move(tmp));
    }

    auto res = TypeFeedback::create(callees, tests, types);
    res->recordingCount_ = recordingCount;

    return res;
}

ObservedCallees& TypeFeedback::callees(uint32_t idx) {
    return this->callees_[idx];
}

ObservedTest& TypeFeedback::test(uint32_t idx) { return this->tests_[idx]; }

ObservedValues& TypeFeedback::types(uint32_t idx) { return this->types_[idx]; }

void ObservedTest::print(std::ostream& out) const {
    switch (seen) {
    case ObservedTest::None:
        out << "_";
        break;
    case ObservedTest::OnlyTrue:
        out << "T";
        break;
    case ObservedTest::OnlyFalse:
        out << "F";
        break;
    case ObservedTest::Both:
        out << "?";
        break;
    }
}

void ObservedTest::mergeWith(const ObservedTest& test) {
    switch (seen) {
    case ObservedTest::Both:
        break;
    case ObservedTest::None:
        seen = test.seen;
        break;
    case ObservedTest::OnlyFalse:
    case ObservedTest::OnlyTrue:
        if (test.seen == ObservedTest::Both ||
            (test.seen != ObservedTest::None && test.seen != seen))
            seen = ObservedTest::Both;
        break;
    }
}

void ObservedValues::print(std::ostream& out) const {
    if (numTypes) {
        for (size_t i = 0; i < numTypes; ++i) {
            out << Rf_type2char(seen[i]);
            if (i != (unsigned)numTypes - 1)
                out << ", ";
        }
        out << " (" << (object ? "o" : "") << (attribs ? "a" : "")
            << (notFastVecelt ? "v" : "") << (!notScalar ? "s" : "") << ")";
        if (stateBeforeLastForce !=
            ObservedValues::StateBeforeLastForce::unknown) {
            out << " | "
                << ((stateBeforeLastForce ==
                     ObservedValues::StateBeforeLastForce::value)
                        ? "value"
                        : (stateBeforeLastForce ==
                           ObservedValues::StateBeforeLastForce::
                               evaluatedPromise)
                              ? "evaluatedPromise"
                              : "promise");
        }
    } else {
        out << "<?>";
    }
}

bool FeedbackOrigin::hasSlot() const { return !index_.isUndefined(); }

uint32_t TypeFeedback::Builder::addCallee() { return ncallees_++; }

uint32_t TypeFeedback::Builder::addTest() { return ntests_++; }

uint32_t TypeFeedback::Builder::addType() { return ntypes_++; }

TypeFeedback* TypeFeedback::Builder::build() {
    std::vector<ObservedCallees> callees(ncallees_, ObservedCallees{});
    std::vector<ObservedTest> tests(ntests_, ObservedTest{});
    std::vector<ObservedValues> types(ntypes_, ObservedValues{});

    return TypeFeedback::create(callees, tests, types);
}

TypeFeedback* TypeFeedback::empty() { return TypeFeedback::create({}, {}, {}); }

void FeedbackOrigin::function(Function* fun) {
    assert(!hasSlot() || fun->typeFeedback()->isValid(index_));
    function_ = fun;
}
bool TypeFeedback::isValid(const FeedbackIndex& index) const {
    switch (index.kind) {
    case FeedbackKind::Call:
        return index.idx < callees_size_;
    case FeedbackKind::Test:
        return index.idx < tests_size_;
    case FeedbackKind::Type:
        return index.idx < types_size_;
    default:
        return false;
    }
}

TypeFeedback* TypeFeedback::create(const std::vector<ObservedCallees>& callees,
                                   const std::vector<ObservedTest>& tests,
                                   const std::vector<ObservedValues>& types) {
    size_t dataSize = callees.size() * sizeof(ObservedCallees) +
                      tests.size() * sizeof(ObservedTest) +
                      types.size() * sizeof(ObservedValues);

    size_t objSize = sizeof(TypeFeedback) + dataSize;

    SEXP store = Rf_allocVector(EXTERNALSXP, objSize);

    TypeFeedback* res =
        new (INTEGER(store)) TypeFeedback(callees, tests, types);

    return res;
}

TypeFeedback::TypeFeedback(const std::vector<ObservedCallees>& callees,
                           const std::vector<ObservedTest>& tests,
                           const std::vector<ObservedValues>& types)
    : RirRuntimeObject(0, 0), callees_size_(callees.size()),
      tests_size_(tests.size()), types_size_(types.size()) {

    size_t callees_mem_size = callees_size_ * sizeof(ObservedCallees);
    size_t tests_mem_size = tests_size_ * sizeof(ObservedTest);
    size_t types_mem_size = types_size_ * sizeof(ObservedValues);

    callees_ = (ObservedCallees*)slots_;
    tests_ = (ObservedTest*)(slots_ + callees_mem_size);
    types_ = (ObservedValues*)(slots_ + callees_mem_size + tests_mem_size);

    if (callees_size_) {
        memcpy(callees_, callees.data(), callees_mem_size);
    }

    if (tests_size_) {
        memcpy(tests_, tests.data(), tests_mem_size);
    }

    if (types_size_) {
        memcpy(types_, types.data(), types_mem_size);
    }
}

void TypeFeedback::record_calleeInc(TypeFeedback* inclusive, uint32_t idx,
                                    Function* function, SEXP callee,
                                    bool invalidateWhenFull) {
    record_callee(idx, function, callee, invalidateWhenFull);
    if (inclusive && inclusive != this)
        inclusive->record_callee(idx, function, callee, invalidateWhenFull);
}

void TypeFeedback::record_testInc(TypeFeedback* inclusive, uint32_t idx,
                                  const SEXP e) {
    record_test(idx, e);
    if (inclusive && inclusive != this)
        inclusive->record_test(idx, e);
}

void TypeFeedback::record_typeInc(TypeFeedback* inclusive, uint32_t idx,
                                  const SEXP e) {
    record_type(idx, e);
    if (inclusive && inclusive != this)
        inclusive->record_type(idx, e);
}

void TypeFeedback::record_typeInc(TypeFeedback* inclusive, uint32_t idx,
                                  std::function<void(ObservedValues&)> f) {
    record_type(idx, f);
    if (inclusive && inclusive != this)
        inclusive->record_type(idx, f);
}

TypeFeedback* TypeFeedback::emptyCopy() const {
    std::vector<ObservedCallees> callees(callees_size_, ObservedCallees{});
    std::vector<ObservedTest> tests(tests_size_, ObservedTest{});
    std::vector<ObservedValues> types(types_size_, ObservedValues{});

    return TypeFeedback::create(callees, tests, types);
}

TypeFeedback* TypeFeedback::copy() const {
    std::vector<ObservedCallees> callees(callees_, callees_ + callees_size_);
    std::vector<ObservedTest> tests(tests_, tests_ + tests_size_);
    std::vector<ObservedValues> types(types_, types_ + types_size_);

    auto tf = TypeFeedback::create(callees, tests, types);
    tf->recordingCount_ = recordingCount_;
    return tf;
}

void TypeFeedback::mergeWith(const TypeFeedback* tf, Function* function) {
    if (recordingCount_ <= UINT_MAX - tf->recordingCount_)
        recordingCount_ += tf->recordingCount_;
    else
        recordingCount_ = UINT_MAX;
    for (size_t i = 0; i < callees_size_; i++)
        callees(i).mergeWith(tf->callees_[i], function);

    for (size_t i = 0; i < tests_size_; i++)
        test(i).mergeWith(tf->tests_[i]);

    for (size_t i = 0; i < types_size_; i++)
        types(i).mergeWith(tf->types_[i]);
}

void TypeFeedback::fillWith(const TypeFeedback* tf) {
    for (size_t i = 0; i < tests_size_; i++)
        if (test(i).isEmpty())
            memcpy(tests_ + i, tf->tests_ + i, sizeof(ObservedTest));

    for (size_t i = 0; i < types_size_; i++)
        if (types(i).isEmpty())
            memcpy(types_ + i, tf->types_ + i, sizeof(ObservedValues));
}

const char* FeedbackIndex::name() const {
    switch (kind) {
    case FeedbackKind::Call:
        return "Call";
        break;
    case FeedbackKind::Test:
        return "Test";
        break;
    case FeedbackKind::Type:
        return "Type";
        break;
    default:
        assert(false);
    }
}
} // namespace rir
