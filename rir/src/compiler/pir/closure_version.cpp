#include "closure_version.h"
#include "closure.h"
#include "compiler/util/bb_transform.h"
#include "compiler/util/visitor.h"
#include "pir_impl.h"

#include <iostream>

namespace rir {
namespace pir {

void ClosureVersion::computeFeedbackStats() {

    // fill in slotsOptimizedAway .  Slots that don't appear in the code and are
    // non-empty remove from slotsReadNotUsedStaticTypeReason and
    // slotsReadCandidateNotUsedReason  slots that were optimized away

    this->scanForSpeculation();
    this->computeSlotsOptimizedAway();
}

void ClosureVersion::scanForSpeculation() {
    Visitor::run(this->entry, [&](Instruction* i) {
        if (auto assume = Assume::Cast(i)) {

            auto fo = assume->reason.origin;

            if (!assume->defaultFeedback && !fo.index().isUndefined() &&
                fo.index().kind == FeedbackKind::Type) {

                assert(this->owner()->rirFunction());

                // Slot used

                // Type test
                auto assumeArg = assume->arg<0>().val();
                auto typeTest = IsType::Cast(assumeArg);
                if (!typeTest) {
                    assert(assumeArg == pir::False::instance());
                    return; // skip the constant-folded false
                }

                // Instruction we speculated on
                pir::Instruction* speculatedOn =
                    Instruction::Cast(typeTest->arg<0>().val());
                assert(speculatedOn);

                // The cast of speculated instr to the assumed type
                pir::CastType* cast = nullptr;
                {
                    auto bb = assume->bb();
                    auto iter = bb->atPosition(assume);
                    // Move after the assume
                    ++iter;
                    // Make sure we don't fall out of BB
                    if (iter != bb->end()) {
                        cast = pir::CastType::Cast(*iter);
                        // If it is a cast (not deopted)
                        // and the cast is on the speculated on instruction
                        if (cast && cast->arg<0>().val() == speculatedOn) {
                            // sanity check
                            assert(cast->type ==
                                   (cast->type & speculatedOn->type));
                        } else {
                            cast = nullptr;
                        }
                    }
                }

                // Construct the slotUsed
                auto slotUsed = report::SlotUsed();
                auto mkT = [](const pir::PirType& type) {
                    return new pir::PirType(type);
                };

                if (cast) {
                    slotUsed.checkFor = mkT(cast->type);
                } else {
                    slotUsed.checkFor = mkT(typeTest->type);
                }

                slotUsed.staticType = mkT(speculatedOn->type);
                slotUsed.feedbackType = mkT(report::getSlotPirType(fo));

                assert(assume->required);
                slotUsed.requiredType = assume->required;

                slotUsed.speculatedOn = report::streamToString(
                    [&](std::ostream& os) { speculatedOn->print(os); });
                slotUsed.assumeInstr = report::streamToString(
                    [&](std::ostream& os) { assume->print(os); });

                auto& info = this->feedbackStatsFor(fo.function());

                // Sanity check
                if (slotUsed.exactMatch()) {
                    assert(*slotUsed.checkFor == *slotUsed.feedbackType);
                }

                info.slotsUsed[fo.index()] = slotUsed;
            }
        }
    });
}

void ClosureVersion::computeSlotsOptimizedAway() {
    Visitor::run(this->entry, [&](Instruction* i) {
        if (!i->hasTypeFeedback()) {
            return;
        }

        const auto& tf = i->typeFeedback(false);
        const auto& origin = tf.feedbackOrigin;
        if (origin.index().isUndefined()) {
            return;
        }

        auto& info = this->feedbackStatsFor(origin.function());

        auto slotPresent = report::SlotPresent();
        slotPresent.presentInstr =
            report::streamToString([&](std::ostream& os) { i->print(os); });

        info.slotPresent[origin.index()] = slotPresent;
    });
}

void ClosureVersion::print(std::ostream& out, bool tty) const {
    print(DebugStyle::Standard, out, tty, false);
}

void ClosureVersion::print(DebugStyle style, std::ostream& out, bool tty,
                           bool omitDeoptBranches) const {
    switch (style) {
    case DebugStyle::Standard:
        printStandard(out, tty, omitDeoptBranches);
        break;
    case DebugStyle::GraphViz:
        printGraph(out, omitDeoptBranches);
        break;
    case DebugStyle::GraphVizBB:
        printBBGraph(out, omitDeoptBranches);
        break;
    default:
        assert(false);
    }
}

void ClosureVersion::printStandard(std::ostream& out, bool tty,
                                   bool omitDeoptBranches) const {
    out << *this << "\n";
    printCode(out, tty, omitDeoptBranches);
    for (auto p : promises_) {
        if (p) {
            out << "Prom " << p->id << ":\n";
            p->printCode(out, tty, omitDeoptBranches);
        }
    }
}

void ClosureVersion::printGraph(std::ostream& out,
                                bool omitDeoptBranches) const {
    out << "digraph {\n";
    out << "label=\"" << *this << "\";\n";
    printGraphCode(out, omitDeoptBranches);
    for (auto p : promises_) {
        if (p) {
            out << "subgraph p" << p->id << "{\n";
            out << "label = \"Promise " << p->id << "\";\n";
            p->printGraphCode(out, omitDeoptBranches);
            out << "}\n";
        }
    }
    out << "}\n";
}

void ClosureVersion::printBBGraph(std::ostream& out,
                                  bool omitDeoptBranches) const {
    out << "digraph {\n";
    out << "label=\"" << *this << "\";\n";
    printBBGraphCode(out, omitDeoptBranches);
    for (auto p : promises_) {
        if (p) {
            out << "subgraph {\n";
            out << "label=\"Promise " << p->id << "\";\n";
            p->printBBGraphCode(out, omitDeoptBranches);
            out << "}\n";
        }
    }
    out << "}\n";
}

Promise* ClosureVersion::createProm(rir::Code* rirSrc) {
    Promise* p = new Promise(this, promises_.size(), rirSrc);
    promises_.push_back(p);
    return p;
}

ClosureVersion::~ClosureVersion() {
    for (auto p : promises_) {
        if (p)
            delete p;
    }
}

ClosureVersion* ClosureVersion::clone(const Context& newAssumptions) {
    auto ctx = optimizationContext_ | newAssumptions;
    auto c = owner_->declareVersion(ctx, false, optFunction);
    c->properties = properties;
    c->entry = BBTransform::clone(entry, c, c);
    return c;
}

void ClosureVersion::erasePromise(unsigned id) {
    assert(promises_.at(id) && "Promise already deleted");

    // If we delete a corrupt promise it get's hard to debug...
    assert(promises_.at(id)->owner == this);
    assert(promise(promises_.at(id)->id) == promises_.at(id));

    delete promises_[id];
    promises_[id] = nullptr;
}

size_t ClosureVersion::numNonDeoptInstrs() const {
    size_t s = 0;
    VisitorNoDeoptBranch::run(entry, [&](BB* bb) { s += bb->size(); });
    return s;
}

size_t ClosureVersion::nargs() const { return owner_->nargs(); }
size_t ClosureVersion::effectiveNArgs() const {
    return owner_->nargs() - optimizationContext_.numMissing();
}

ClosureVersion::ClosureVersion(Closure* closure, rir::Function* optFunction,
                               bool root, const Context& optimizationContext,
                               const Properties& properties)
    : root(root), optFunction(optFunction), owner_(closure),
      optimizationContext_(optimizationContext), properties(properties) {
    auto id = std::stringstream();
    id << closure->name() << "[" << this << "]";
    name_ = id.str();
    id.str("");
    id << this;
    nameSuffix_ = id.str();
}

std::ostream& operator<<(std::ostream& out, const ClosureVersion::Property& p) {
    switch (p) {
    case ClosureVersion::Property::IsEager:
        out << "Eager";
        break;
    case ClosureVersion::Property::NoReflection:
        out << "!Reflection";
        break;
    }
    return out;
}

std::ostream& operator<<(std::ostream& out,
                         const ClosureVersion::Properties& props) {
    for (auto p = props.begin(); p != props.end(); ++p) {
        out << *p;
        if ((p + 1) != props.end())
            out << ", ";
    }
    if (props.argumentForceOrder.size() > 0) {
        if (!props.empty())
            out << ", ";
        out << "ForceOrd: ";
        for (auto o = props.argumentForceOrder.begin();
             o != props.argumentForceOrder.end(); ++o) {
            out << *o;
            if ((o + 1) != props.argumentForceOrder.end())
                out << " ";
        }
    }
    return out;
}

rir::Code* ClosureVersion::rirSrc() const {
    return owner()->rirFunction()->body();
}

} // namespace pir
} // namespace rir
