#include "TypeFeedback.h"

#include "R/Serialize.h"
#include "R/Symbols.h"
#include "R/r.h"
#include "runtime/Code.h"
#include "runtime/Function.h"

#include <cassert>
#include <ostream>
#include <vector>

namespace rir {

void ObservedCallees::record(Code* caller, SEXP callee,
                             bool invalidateWhenFull) {
    if (taken < CounterOverflow)
        taken++;

    if (numTargets < MaxTargets) {
        int i = 0;
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

FeedbackOrigin::FeedbackOrigin(rir::Function* function, uint32_t idx)
    : idx_(idx), function_(function) {}

DeoptReason::DeoptReason(const FeedbackOrigin& origin,
                         DeoptReason::Reason reason)
    : reason(reason), origin(origin) {}

void DeoptReason::record(SEXP val) const {
    origin.function()->registerDeoptReason(reason);

    switch (reason) {
    case DeoptReason::Unknown:
        break;
    case DeoptReason::DeadBranchReached: {
        auto& feedback = origin.function()->typeFeedback().test(origin.idx());
        feedback.seen = ObservedTest::Both;
        break;
    }
    case DeoptReason::Typecheck: {
        if (val == symbol::UnknownDeoptTrigger)
            break;
        auto feedback = origin.function()->typeFeedback().types(origin.idx());
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
        auto feedback = origin.function()->typeFeedback().callees(origin.idx());
        feedback.record(origin.function()->body(), val, true);
        assert(feedback.taken > 0);
        break;
    }
    case DeoptReason::EnvStubMaterialized: {
        break;
    }
    }
}

void ObservedCallees::print(std::ostream& out, const Function* function) const {
    out << "callees: ";
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

TypeFeedbackSlot& TypeFeedback::operator[](size_t idx) {
    assert(idx < slots_.size());
    return slots_[idx];
}

void TypeFeedback::serialize(SEXP refTable, R_outpstream_t out) const {
    // assert(sizeof(TypeFeedbackSlot) % 4 == 0);

    OutInteger(out, size());
    for (auto& slot : slots_) {
        OutBytes(out, &slot, sizeof(TypeFeedbackSlot));
    }
}

TypeFeedback TypeFeedback::deserialize(SEXP refTable, R_inpstream_t inp) {
    auto size = InInteger(inp);

    std::vector<TypeFeedbackSlot> slots;
    slots.reserve(size);
    auto data = slots.data();

    for (auto i = 0; i < size; ++i) {
        InBytes(inp, &data[i], sizeof(TypeFeedbackSlot));
    }

    return TypeFeedback(std::move(slots));
}

ObservedCallees& TypeFeedback::callees(uint32_t idx) {
    return (*this)[idx].callees();
}

ObservedTest& TypeFeedback::test(uint32_t idx) { return (*this)[idx].test(); }

ObservedValues& TypeFeedback::types(uint32_t idx) {
    return (*this)[idx].type();
}

void ObservedTest::print(std::ostream& out) const {
    out << "test: ";
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
    out << "values: ";
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

void TypeFeedbackSlot::print(std::ostream& out,
                             const Function* function) const {
    switch (kind_) {
    case TypeFeedbackKind::Call:
        feedback_.callees.print(out, function);
        break;
    case TypeFeedbackKind::Test:
        feedback_.test.print(out);
        break;
    case TypeFeedbackKind::Type:
        feedback_.type.print(out);
        break;
    }
}

void TypeFeedback::print(std::ostream& out) const {
    std::cout << "== type feedback " << this << " (fun " << owner_
              << ") ==" << std::endl;
    int i = 0;
    for (auto& slot : slots_) {
        out << "#" << i++ << ": ";
        slot.print(out, owner_);
        out << std::endl;
    }
}

TypeFeedbackSlot& TypeFeedback::record(unsigned idx, SEXP value) {
    auto& slot = slots_[idx];

    switch (slots_[idx].kind()) {
    case TypeFeedbackKind::Call:
        slot.callees().record(owner_->body(), value);
        break;
    case TypeFeedbackKind::Test:
        slot.test().record(value);
        break;
    case TypeFeedbackKind::Type:
        slot.type().record(value);
        break;
    }

    return slot;
}

TypeFeedbackSlot* FeedbackOrigin::slot() const {
    if (function_) {
        return &function_->typeFeedback()[idx_];
    } else {
        return nullptr;
    }
}

bool FeedbackOrigin::isValid() const {
    return function_ != nullptr && function_->typeFeedback().size() > idx_;
}

uint32_t TypeFeedback::Builder::addCallee() {
    slots_.emplace_back(TypeFeedbackSlot::createCallees());
    return slots_.size() - 1;
}

uint32_t TypeFeedback::Builder::addTest() {
    slots_.emplace_back(TypeFeedbackSlot::createTest());
    return slots_.size() - 1;
}

uint32_t TypeFeedback::Builder::addType() {
    slots_.emplace_back(TypeFeedbackSlot::createType());
    return slots_.size() - 1;
}

TypeFeedback TypeFeedback::Builder::build() {
    return TypeFeedback(std::move(slots_));
}

TypeFeedback TypeFeedback::empty() { return TypeFeedback{{}}; }

} // namespace rir
