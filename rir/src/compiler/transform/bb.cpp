#include "bb.h"
#include "../pir/pir_impl.h"
#include "../util/visitor.h"

#include <unordered_map>

namespace rir {
namespace pir {

BB* BBTransform::clone(size_t* id_counter, BB* src, Code* target) {
    std::vector<BB*> bbs;

    // Copy instructions
    std::unordered_map<Value*, Value*> relocation_table;
    Visitor::run(src, [&](BB* bb) {
        *id_counter = *id_counter + 1;
        BB* theClone = BB::cloneInstrs(bb, *id_counter, target);
        assert(bb->size() == theClone->size());
        if (bb->id >= bbs.size())
            bbs.resize(bb->id + 5);
        bbs[bb->id] = theClone;
        for (size_t i = 0; i < bb->size(); ++i)
            relocation_table[bb->at(i)] = theClone->at(i);
    });

    // Fixup CFG
    Visitor::run(src, [&](BB* bb) {
        bbs[bb->id]->next0 = bbs[bb->id]->next1 = nullptr;
        if (bb->next0)
            bbs[bb->id]->next0 = bbs[bb->next0->id];
        if (bb->next1)
            bbs[bb->id]->next1 = bbs[bb->next1->id];
    });

    // Relocate arg pointers
    BB* newEntry = bbs[src->id];
    Visitor::run(newEntry, [&](BB* bb) {
        for (auto i : *bb) {
            auto phi = Phi::Cast(i);
            if (phi) {
                for (size_t j = 0; j < phi->input.size(); ++j)
                    phi->input[j] = bbs[phi->input[j]->id];
            }
            i->map_arg([&](Value** v) {
                if ((*v)->isInstruction()) {
                    assert(relocation_table.count(*v));
                    *v = relocation_table.at(*v);
                }
            });
        }
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
    Visitor::run(split, [&](BB* bb) {
        for (auto i : *bb) {
            auto phi = Phi::Cast(i);
            if (phi) {
                for (size_t j = 0; j < phi->input.size(); ++j)
                    if (phi->input[j] == src)
                        phi->input[j] = split;
            }
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
        assert(!found);
        found = ret->arg<0>();
        bb->next0 = splice;
        bb->remove(bb->end() - 1);
    });
    assert(found);
    return found;
}
}
}
