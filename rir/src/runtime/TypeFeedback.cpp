#include "TypeFeedback.h"

#include "R/Serialize.h"
#include "R/Symbols.h"
#include "R/r.h"
#include "runtime/Code.h"
#include "runtime/Function.h"

#include <algorithm>
#include <cassert>
#include <ostream>
#include <vector>

namespace rir {

constexpr uint32_t TypeFeedback::NoDep;

void ObservedCallees::record(Function* function, SEXP callee,
                             bool invalidateWhenFull) {
    REC_HOOK(bool isSuccesful = false);
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
            REC_HOOK(isSuccesful = true);
        }
    } else {
        if (invalidateWhenFull) {
            invalid = true;
            REC_HOOK(isSuccesful = true);
        }
    }

    REC_HOOK(recording::recordSCChanged(isSuccesful));
}

SEXP ObservedCallees::getTarget(const Function* function, size_t pos) const {
    assert(pos < numTargets);
    return function->body()->getExtraPoolEntry(targets[pos]);
}

FeedbackOrigin::FeedbackOrigin(rir::Function* function, FeedbackIndex index)
    : index_(index), function_(function) {
    assert(function->typeFeedback()->isValid(index));
}

DeoptReason::DeoptReason(const FeedbackOrigin& origin,
                         DeoptReason::Reason reason)
    : reason(reason), origin(origin) {}

void DeoptReason::record(SEXP val) const {
    origin.function()->registerDeoptReason(reason);

    switch (reason) {
    case DeoptReason::Unknown:
        break;
    case DeoptReason::DeadBranchReached: {
        auto& feedback = origin.function()->typeFeedback()->test(origin.idx());
        REC_HOOK(
            recording::recordSCChanged(feedback.seen != ObservedTest::Both));
        feedback.seen = ObservedTest::Both;
        REC_HOOK(
            recording::recordSC(feedback, origin.idx(), origin.function()));
        break;
    }
    case DeoptReason::Typecheck: {
        if (val == symbol::UnknownDeoptTrigger)
            break;

        auto feedback = origin.function()->typeFeedback();

        // FIXME: (cf. #1260) very similar code is in the recordTypeFeedbackImpl
        // IMHO the one there is more correct. Would it make sense
        // to pull this into the TypeFeedback::record_type()?
        // and get rid of the overload that takes lambda?
        feedback->record_type(origin.idx(), val);
        feedback->record_type(origin.idx(), [&](auto& slot) {
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
        auto feedback = origin.function()->typeFeedback();
        feedback->record_callee(origin.idx(), origin.function(), val, true);
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

    return TypeFeedback::create(callees, tests, types);
}

ObservedCallees& TypeFeedback::callees(uint32_t idx) {
    return this->callees_[idx];
}

ObservedTest& TypeFeedback::test(uint32_t idx) { return this->tests_[idx]; }
// TypeFeedback::types() is defined inline in the header (hot path).

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
                           ObservedValues::StateBeforeLastForce::
                               evaluatedPromise)
                              ? "evaluatedPromise"
                              : "promise");
        }
    } else {
        out << "<?>";
    }

    out << " @ " << this;
    if (hasParent()) {
        out << " -> Type#" << parentSlot();
    }
    // Cast the uint8_t bitfields to int: streaming a uint8_t (= unsigned char)
    // prints a character glyph (0x00/0x01 control chars), not the digits 0/1.
    // leaf-vs-inner is not stored — the opcode name printed alongside this
    // already says which it is. `dirty` is whether an inner node will record
    // on its next execution.
    out << ", dirty: " << (int)dirty;
}

bool FeedbackOrigin::hasSlot() const { return !index_.isUndefined(); }

uint32_t TypeFeedback::Builder::addCallee() { return ncallees_++; }

uint32_t TypeFeedback::Builder::addTest() { return ntests_++; }

uint32_t TypeFeedback::Builder::addType() {
    typeDeps_.push_back(NoDep);
    forceBehaviorKinds_.push_back(
        static_cast<uint8_t>(ForceBehaviorKind::Always));
    return ntypes_++;
}

void TypeFeedback::Builder::resetTypesTo(unsigned n) {
    assert(n <= ntypes_);
    ntypes_ = n;
    typeDeps_.resize(n);
    forceBehaviorKinds_.resize(n);
}

void TypeFeedback::Builder::setTypeDep(uint32_t slot, uint32_t source) {
    assert(slot < typeDeps_.size());
    typeDeps_[slot] = source;
}

void TypeFeedback::Builder::setForceBehaviorKind(uint32_t slot,
                                                 ForceBehaviorKind kind) {
    assert(slot < forceBehaviorKinds_.size());
    forceBehaviorKinds_[slot] = static_cast<uint8_t>(kind);
}

TypeFeedback* TypeFeedback::Builder::build() {
    std::vector<ObservedCallees> callees(ncallees_, ObservedCallees{});
    std::vector<ObservedTest> tests(ntests_, ObservedTest{});
    std::vector<ObservedValues> types(ntypes_, ObservedValues{});

    return TypeFeedback::create(callees, tests, types, typeDeps_,
                                forceBehaviorKinds_);
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

TypeFeedback*
TypeFeedback::create(const std::vector<ObservedCallees>& callees,
                     const std::vector<ObservedTest>& tests,
                     const std::vector<ObservedValues>& types,
                     const std::vector<uint32_t>& typeDeps,
                     const std::vector<uint8_t>& forceBehaviorKinds) {
    size_t dataSize = callees.size() * sizeof(ObservedCallees) +
                      tests.size() * sizeof(ObservedTest) +
                      types.size() * sizeof(ObservedValues) +
                      types.size() * sizeof(uint32_t) +
                      types.size() * sizeof(uint8_t);

    size_t objSize = sizeof(TypeFeedback) + dataSize;

    SEXP store = Rf_allocVector(EXTERNALSXP, objSize);

    TypeFeedback* res = new (INTEGER(store))
        TypeFeedback(callees, tests, types, typeDeps, forceBehaviorKinds);

    return res;
}

TypeFeedback::TypeFeedback(const std::vector<ObservedCallees>& callees,
                           const std::vector<ObservedTest>& tests,
                           const std::vector<ObservedValues>& types,
                           const std::vector<uint32_t>& typeDeps,
                           const std::vector<uint8_t>& forceBehaviorKinds)
    : RirRuntimeObject(0, 0), owner_(nullptr), callees_size_(callees.size()),
      tests_size_(tests.size()), types_size_(types.size()) {

    size_t callees_mem_size = callees_size_ * sizeof(ObservedCallees);
    size_t tests_mem_size = tests_size_ * sizeof(ObservedTest);
    size_t types_mem_size = types_size_ * sizeof(ObservedValues);

    callees_ = (ObservedCallees*)slots_;
    tests_ = (ObservedTest*)(slots_ + callees_mem_size);
    types_ = (ObservedValues*)(slots_ + callees_mem_size + tests_mem_size);
    typeDeps_ = (uint32_t*)(slots_ + callees_mem_size + tests_mem_size +
                            types_mem_size);
    forceBehaviorKinds_ =
        (uint8_t*)(slots_ + callees_mem_size + tests_mem_size + types_mem_size +
                   types_size_ * sizeof(uint32_t));

    if (callees_size_) {
        memcpy(callees_, callees.data(), callees_mem_size);
    }

    if (tests_size_) {
        memcpy(tests_, tests.data(), tests_mem_size);
    }

    if (types_size_) {
        memcpy(types_, types.data(), types_mem_size);
        if (!typeDeps.empty()) {
            assert(typeDeps.size() == types_size_);
            memcpy(typeDeps_, typeDeps.data(), types_size_ * sizeof(uint32_t));
        } else {
            std::fill(typeDeps_, typeDeps_ + types_size_, NoDep);
        }
        if (!forceBehaviorKinds.empty()) {
            assert(forceBehaviorKinds.size() == types_size_);
            memcpy(forceBehaviorKinds_, forceBehaviorKinds.data(),
                   types_size_ * sizeof(uint8_t));
        } else {
            std::fill(forceBehaviorKinds_, forceBehaviorKinds_ + types_size_,
                      static_cast<uint8_t>(ForceBehaviorKind::Always));
        }
    }
}

// Reconstruct the feedback that recordless chose not to observe at runtime, so
// that every slot ends up holding what a record-everything build would have.
// Two independent dimensions, both handled here — see recordless-design.md
// §2A.3 (types) and §4.5/§4.6 (force behavior) for the soundness arguments.
//
// NO CALLERS YET. This is the consumer-side step; wiring it into the JIT is
// separate work. It is written to be safe to call more than once: every rule
// below is idempotent, and re-running after the interpreter has recorded some
// more simply refreshes the derived slots from their (now newer) sources.
//
// Single forward pass. Both rules read only *lower-numbered* slots — a
// dependency is always registered after its source has been allocated
// (registerNoRecordDep) — so a source is fully resolved by the time a dependent
// reads it. That in turn means a chain of copies resolves in this one pass even
// though the graph is kept flat (§2A.4) and chains should not arise.
void TypeFeedback::reconstructFeedback() {
    for (size_t i = 0; i < types_size_; ++i) {
        const uint32_t src = typeDeps_[i];
        assert((src == NoDep || src < i) &&
               "dep must reference an earlier slot; the single forward pass "
               "relies on the source already being resolved");

        // ---- type dimension -------------------------------------------
        // A NoRecord use emits no opcode, so its slot was never written. By
        // the def-site subsumption argument (§3) its value is the source's,
        // hence so are its type observations.
        if (src != NoDep)
            types_[i].copyTypeObservationsFrom(types_[src]);

        // ---- force-behavior dimension ---------------------------------
        // Slots whose FB was recorded at runtime already hold the answer;
        // the two skipped kinds are derived. Note this is keyed on the
        // compile-time kind, not on having a dep: a RecordOnce *use* can also
        // be classified FBValue and skip FB recording without being a copy of
        // anything.
        switch (forceBehaviorKind((uint32_t)i)) {
        case ForceBehaviorKind::Always:
        case ForceBehaviorKind::RecordOnce:
        case ForceBehaviorKind::EnvBit: // disabled; behaves as Always
            // Observed directly. RecordOnce recorded only the first execution
            // of each invocation, which is the maximum over that invocation
            // because forcing moves *down* the lattice (§4.5).
            break;

        case ForceBehaviorKind::FBValue:
            // Statically a value: the binding was last written by stvar_,
            // which stores an evaluated value off the stack.
            types_[i].stateBeforeLastForce = ObservedValues::value;
            break;

        case ForceBehaviorKind::Infer: {
            // Derived from the source, but NOT copied from it. The use that
            // subsumes this one dominates it and forced the binding on its way
            // through, and R leaves the PROMSXP in place when it forces, so an
            // unforced `promise` at the source is necessarily an
            // `evaluatedPromise` here. Per execution the relation is exactly
            //     c(dep) = min(c(source), evaluatedPromise)
            // and because that clamp is monotone it commutes with the max the
            // lattice accumulates — so applying it once to the stored value is
            // lossless, not conservative (§4.5).
            if (src == NoDep) {
                assert(false && "Infer without a source slot");
                types_[i].stateBeforeLastForce = ObservedValues::promise;
                break;
            }
            // The temporary is required, not stylistic: std::min takes its
            // arguments by const reference and a reference cannot bind to a
            // bitfield.
            const uint8_t s = types_[src].stateBeforeLastForce;
            types_[i].stateBeforeLastForce =
                std::min(s, (uint8_t)ObservedValues::evaluatedPromise);
            break;
        }
        }
    }
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
