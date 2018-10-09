#include "bb.h"
#include "../pir/pir_impl.h"
#include "../util/visitor.h"

#include <unordered_map>

namespace rir {
namespace pir {

BB* BBTransform::clone(BB* src, Code* target) {
    std::vector<BB*> bbs;

    // Copy instructions and remember old -> new instruction map.
    std::unordered_map<Value*, Instruction*> relocation_table;
    Visitor::run(src, [&](BB* bb) {
        BB* theClone = BB::cloneInstrs(bb, target->nextBBId++, target);
        assert(bb->size() == theClone->size());
        if (bb->id >= bbs.size())
            bbs.resize(bb->id + 5);
        bbs[bb->id] = theClone;
        for (size_t i = 0; i < bb->size(); ++i)
            relocation_table[bb->at(i)] = theClone->at(i);
    });

    // Fixup CFG: next pointers of copied BB's need to be filled in.
    Visitor::run(src, [&](BB* bb) {
        bbs[bb->id]->next0 = bbs[bb->id]->next1 = nullptr;
        if (bb->next0)
            bbs[bb->id]->next0 = bbs[bb->next0->id];
        if (bb->next1)
            bbs[bb->id]->next1 = bbs[bb->next1->id];
    });

    // Relocate argument pointers using old -> new map
    BB* newEntry = bbs[src->id];
    Visitor::run(newEntry, [&](Instruction* i) {
        auto phi = Phi::Cast(i);
        if (phi) {
            for (size_t j = 0; j < phi->input.size(); ++j)
                phi->input[j] = bbs[phi->input[j]->id];
        }
        i->eachArg([&](InstrArg& arg) {
            if (arg.val()->isInstruction()) {
                assert(relocation_table.count(arg.val()));
                arg.val() = relocation_table.at(arg.val());
            }
        });
    });

    return newEntry;
}

BB* BBTransform::split(size_t next_id, BB* src, BB::Instrs::iterator it,
                       Code* target) {
    BB* split = new BB(target, next_id);
    split->next0 = src->next0;
    split->next1 = src->next1;
    while (it != src->end()) {
        it = src->moveToEnd(it, split);
    }
    src->next0 = split;
    src->next1 = nullptr;
    Visitor::run(split, [&](Instruction* i) {
        auto phi = Phi::Cast(i);
        if (phi) {
            for (size_t j = 0; j < phi->input.size(); ++j)
                if (phi->input[j] == src)
                    phi->input[j] = split;
        }
    });
    return split;
}

Value* BBTransform::forInline(BB* inlinee, BB* splice) {
    Value* found = nullptr;
    Visitor::run(inlinee, [&](BB* bb) {
        if (bb->next0 != nullptr)
            return;

        assert(bb->next1 == nullptr);
        if (Deopt::Cast(bb->last()))
            return;

        Return* ret = Return::Cast(bb->last());
        assert(ret);

        // This transformation assumes that we have just one reachable return.
        // Assert that we do not find a second one.
        assert(!found);

        found = ret->arg<0>().val();
        bb->next0 = splice;
        bb->remove(bb->end() - 1);
    });
    assert(found);
    return found;
}

BB* BBTransform::addConditionalDeopt(Closure* closure, BB* src,
                                     BB::Instrs::iterator position,
                                     Instruction* condition,
                                     FrameState* frameState) {
    auto split =
        BBTransform::split(closure->nextBBId++, src, position, closure);
    src->append(condition);
    src->append(new Branch(condition));
    auto deoptBlock = new BB(closure, closure->nextBBId++);

    src->next1 = split;
    src->next0 = deoptBlock;
    FrameState* fsClone = FrameState::Cast(frameState->clone());
    deoptBlock->append(fsClone);
    deoptBlock->append(new Deopt(fsClone));
    return split;
}

BB* BBTransform::addCheckpoint(Closure* closure, BB* src,
                               BB::Instrs::iterator position) {
    FrameState* framestate = FrameState::Cast(*(position - 1));
    auto split =
        BBTransform::split(closure->nextBBId++, src, position, closure);
    src->append(new Checkpoint());
    auto deoptBlock = new BB(closure, closure->nextBBId++);
    src->next0 = split;
    src->next1 = deoptBlock;
    deoptBlock->append(new Deopt(framestate));
    return split;
}
} // namespace pir
} // namespace rir
