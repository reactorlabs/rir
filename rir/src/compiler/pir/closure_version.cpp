#include "closure_version.h"
#include "closure.h"
#include "compiler/analysis/query.h"
#include "compiler/opt/type_test.h"
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
    this->scanForPreciseTypeSlots();

    this->computeSlotsPresent();
}

void ClosureVersion::scanForPreciseTypeSlots() {

    Visitor::run(this->entry, [&](Instruction* i) {
        if (!i->hasTypeFeedback()) {
            return;
        }

        const auto& tf = i->typeFeedback(false);
        const auto& origin = tf.feedbackOrigin;
        if (origin.index().isUndefined() || tf.defaultFeedback ||
            origin.index().kind != FeedbackKind::Type) {
            return;
        }

        auto& info = this->feedbackStatsFor(origin.function());

        if (i->isReturnTypePrecise()) {
            // std::cerr << "precise type: ";
            // i->print(std::cerr, true);
            // std::cerr << "\n";

            info.preciseTypeSlots.insert(origin.index());

            // assert(false);
        }
    });
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
                    // assume->print(std::cerr, true);
                    assert(assumeArg == pir::False::instance() ||
                           assumeArg == pir::True::instance());
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
                    auto assumeSeen = false;

                    Visitor::run(bb, [&](Instruction* i) {
                        if (i == assume) {
                            assumeSeen = true;
                        }

                        if (!assumeSeen) {
                            return;
                        }

                        auto mbyCast = CastType::Cast(i);
                        if (mbyCast &&
                            mbyCast->arg<0>().val() == speculatedOn) {
                            // sanity check
                            assert(mbyCast->type ==
                                   (mbyCast->type & speculatedOn->type));

                            if (!cast) {
                                cast = mbyCast;
                            }
                        }
                    });
                }

                // Construct the slotUsed
                auto slotUsed = report::SlotUsed();
                auto mkT = [](const pir::PirType& type) {
                    return new pir::PirType(type);
                };

                if (cast) {
                    slotUsed.checkFor = mkT(cast->type);
                } else {
                    slotUsed.checkFor = mkT(typeTest->typeTest);
                }

                slotUsed.staticType = mkT(speculatedOn->type);
                slotUsed.feedbackType = mkT(report::getSlotPirType(fo));

                assert(assume->required);
                slotUsed.requiredType = assume->required;

                slotUsed.speculatedOn = report::instrToString(speculatedOn);

                slotUsed.assumeInstr = report::instrToString(assume);

                slotUsed.hoistedForce = assume->hoistedForce;
                auto& info = this->feedbackStatsFor(fo.function());

                // Sanity check
                if (slotUsed.exactMatch()) {
                    assert(*slotUsed.checkFor == *slotUsed.feedbackType);
                }

                info.slotsUsed[fo.index()] = slotUsed;

                // Add preemptive slot present
                {
                    auto slotPresent = report::SlotPresent();

                    slotPresent.staticType = slotUsed.staticType;
                    slotPresent.feedbackType = slotUsed.feedbackType;
                    slotPresent.speculation = report::Emited;
                    slotPresent.inPromiseOnly = false;
                    slotPresent.presentInstr = slotUsed.speculatedOn;

                    info.slotsPresent[fo.index()] = slotPresent;
                }
            }
        }
    });
}

void ClosureVersion::computeSlotsPresent() {
    auto doCompute = [&](BB* e, bool isPromise) {
        Visitor::run(e, [&](Instruction* i) {
            if (!i->hasTypeFeedback()) {
                return;
            }

            const auto& tf = i->typeFeedback(false);
            const auto& origin = tf.feedbackOrigin;
            if (origin.index().isUndefined() || tf.defaultFeedback ||
                origin.index().kind != FeedbackKind::Type) {
                return;
            }

            auto& info = this->feedbackStatsFor(origin.function());

            // Non-promise slotPresent has higher precedence
            if (isPromise && info.slotsPresent.count(origin.index())) {
                return;
            }

            auto slotPresent = report::SlotPresent();
            slotPresent.presentInstr = report::instrToString(i);

            slotPresent.speculation = tf.speculation;

            // if (slotPresent.speculation == report::Emited) {
            //     assert(info.slotsAssumeRemoved.count(origin.index()) ||
            //            info.slotsUsed.count(origin.index()));
            // }

            slotPresent.staticType = new pir::PirType(i->type);
            slotPresent.feedbackType = new pir::PirType(tf.type);

            slotPresent.inPromiseOnly = isPromise;

            info.slotsPresent[origin.index()] = slotPresent;
        });
    };

    doCompute(this->entry, false);
    for (auto p : promises()) {
        if (p) {
            doCompute(p->entry, true);
        }
    }

    // add slots that are present but dont'have explicit feedback attached due
    // to hoised Force. These can be recovered but looking at the Assume
    // instructions example:
    //        val?^ | miss    %26.5 = LdVar              eR    lim, e2.4
    //   lgl$#-          %32.0 = IsType                   %26.5 isA real$-
    //   void                    Assume             D     %32.0, %30.3
    //   (Typecheck@0x55d9c0a56440[Type#124])
    // No need to look into promises as we don't speculate within them
    // for (auto& fs : feedbackStatsByFunction) {
    //     // auto function = fs.first;
    //     auto& info = fs.second;
    //
    //     for (auto& su : info.slotsUsed) {
    //
    //         auto index = su.first;
    //         auto& slotUsed = su.second;
    //
    //         if (slotUsed.speculatedOn.find("Force") == std::string::npos) {
    //
    //             if (info.slotsPresent.find(index) == info.slotsPresent.end())
    //             {
    //                 auto slotPresent = report::SlotPresent();
    //
    //                 slotPresent.staticType = slotUsed.staticType;
    //                 slotPresent.feedbackType = slotUsed.feedbackType;
    //                 slotPresent.speculation = report::Emited;
    //                 slotPresent.inPromiseOnly = false;
    //                 slotPresent.presentInstr = slotUsed.speculatedOn;
    //
    //                 info.slotsPresent[index] = slotPresent;
    //             }
    //         }
    //     }
    // }
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
