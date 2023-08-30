#include "TypeFeedback.h"

#include "R/Symbols.h"
#include "R/r.h"
#include "runtime/Code.h"
#include "runtime/Function.h"

#include <cassert>
#include <ostream>
#include <vector>

namespace rir {

void ObservedCallees::record(Function* function, SEXP callee,
                             bool invalidateWhenFull) {
    if (taken < CounterOverflow)
        taken++;

    if (numTargets < MaxTargets) {
        int i = 0;
        auto caller = function->body();
        for (; i < numTargets; ++i)
            if (caller->getExtraPoolEntry(targets[i]) == callee)
                break;
        if (i == numTargets) {
            auto idx = caller->addExtraPoolEntry(callee);
            targets[numTargets++] = idx;
        }
    } else {
        if (invalidateWhenFull)
            invalid = true;
    }
}

SEXP ObservedCallees::getTarget(const Function* function, size_t pos) const {
    assert(pos < numTargets);
    return function->body()->getExtraPoolEntry(targets[pos]);
}

FeedbackPosition::FeedbackPosition(rir::Function* function, FeedbackIndex index)
    : index_(index), function_(function) {
    assert(function->typeFeedback()->isValid(index));
}

DeoptReason::DeoptReason(const FeedbackPosition& origin,
                         DeoptReason::Reason reason)
    : reason(reason), origin(origin) {}

void DeoptReason::record(SEXP val) const {
    if (origin.function()->body()->kind == Code::Kind::Deserializing) {
        // TODO: Is there still a way to record? We probably already have
        //  function in some cases, if so maybe we could set it earlier...
        //  Regardless, the only issue here is we just deopt again
        return;
    }
    origin.function()->registerDeoptReason(reason);

    switch (reason) {
    case DeoptReason::Unknown:
        break;
    case DeoptReason::DeadBranchReached: {
        auto& feedback = origin.function()->typeFeedback()->test(origin.idx());
        feedback.seen = ObservedTest::Both;
        break;
    }
    case DeoptReason::Typecheck: {
        if (val == symbol::UnknownDeoptTrigger)
            break;
        auto feedback = origin.function()->typeFeedback()->types(origin.idx());
        feedback.record(val);
        if (TYPEOF(val) == PROMSXP) {
            if (PRVALUE(val) == R_UnboundValue &&
                feedback.stateBeforeLastForce < ObservedValues::promise)
                feedback.stateBeforeLastForce = ObservedValues::promise;
            else if (feedback.stateBeforeLastForce <
                     ObservedValues::evaluatedPromise)
                feedback.stateBeforeLastForce =
                    ObservedValues::evaluatedPromise;
        }
        break;
    }
    case DeoptReason::DeadCall:
    case DeoptReason::ForceAndCall:
    case DeoptReason::CallTarget: {
        if (val == symbol::UnknownDeoptTrigger)
            break;
        auto feedback =
            origin.function()->typeFeedback()->callees(origin.idx());
        feedback.record(origin.function(), val, true);
        assert(feedback.taken > 0);
        break;
    }
    case DeoptReason::EnvStubMaterialized: {
        break;
    }
    }
}

TypeFeedback* TypeFeedback::deserialize(AbstractDeserializer& deserializer) {
    auto size = deserializer.readBytesOf<size_t>();
    std::vector<ObservedCallees> callees(size);
    for (size_t i = 0; i < size; ++i) {
        deserializer.readBytes(&callees[i], sizeof(ObservedCallees));
    }

    size = deserializer.readBytesOf<size_t>();
    std::vector<ObservedTest> tests(size);
    for (size_t i = 0; i < size; ++i) {
        deserializer.readBytes(&tests[i], sizeof(ObservedTest));
    }

    size = deserializer.readBytesOf<size_t>();
    std::vector<ObservedValues> types(size);
    for (size_t i = 0; i < size; ++i) {
        deserializer.readBytes(&types[i], sizeof(ObservedValues));
    }

    auto feedback = TypeFeedback::create(callees, tests, types);
    // TypeFeedback doesn't need addRef
    return feedback;
}

void TypeFeedback::serialize(AbstractSerializer& serializer) const {
    serializer.writeBytesOf(callees_size_);
    for (size_t i = 0; i < callees_size_; i++) {
        serializer.writeBytes(callees_ + i, sizeof(ObservedCallees));
    }

    serializer.writeBytesOf(tests_size_);
    for (size_t i = 0; i < tests_size_; i++) {
        serializer.writeBytes(tests_ + i, sizeof(ObservedTest));
    }

    serializer.writeBytesOf(types_size_);
    for (size_t i = 0; i < types_size_; i++) {
        serializer.writeBytes(types_ + i, sizeof(ObservedValues));
    }
}

void TypeFeedback::hash(__attribute__((unused)) HasherOld& hasher) const { // NOLINT(*-convert-member-functions-to-static)
    assert(false && "Feedback should never be hashed");
}

void TypeFeedback::addConnected(
    __attribute__((unused)) ConnectedCollectorOld& collector) const { // NOLINT(*-convert-member-functions-to-static)
    assert(false && "Feedback should never be hashed (don't call addConnected)");
}

ObservedCallees& TypeFeedback::callees(uint32_t idx) {
    return this->callees_[idx];
}

ObservedTest& TypeFeedback::test(uint32_t idx) { return this->tests_[idx]; }

ObservedValues& TypeFeedback::types(uint32_t idx) { return this->types_[idx]; }

void TypeFeedback::print(std::ostream& out) const {
    out << "TypeFeedback";
    if (!owner_) {
        out << " (owner not set)";
    }
    out << ":\n";

    out << "  " << callees_size_ << " callees:\n";
    for (size_t i = 0; i < callees_size_; ++i) {
        out << "    " << i << ": ";
        callees_[i].print(out, owner_);
        out << "\n";
    }

    out << "  " << tests_size_ << " tests:\n";
    for (size_t i = 0; i < tests_size_; ++i) {
        out << "    " << i << ": ";
        tests_[i].print(out);
        out << "\n";
    }

    out << "  " << types_size_ << " types:\n";
    for (size_t i = 0; i < types_size_; ++i) {
        out << "    " << i << ": ";
        types_[i].print(out);
        out << "\n";
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
        if (function) {
            auto target = getTarget(function, i);
            out << target << "(" << Rf_type2char(TYPEOF(target)) << ") ";
        } else {
            out << "<entry " << targets[i] << ">";
        }
    }
}

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
                       ObservedValues::StateBeforeLastForce::evaluatedPromise)
                        ? "evaluatedPromise"
                        : "promise");
        }
    } else {
        out << "<?>";
    }
}

bool FeedbackPosition::hasSlot() const { return !index_.isUndefined(); }

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

void FeedbackPosition::function(Function* fun) {
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
    : RirRuntimeObject(0, 0), owner_(nullptr), callees_size_(callees.size()),
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

const char* FeedbackIndex::name() const {
    switch (kind) {
    case FeedbackKind::Call:
        return "Call";
    case FeedbackKind::Test:
        return "Test";
    case FeedbackKind::Type:
        return "Type";
    default:
        assert(false);
    }
}
} // namespace rir
