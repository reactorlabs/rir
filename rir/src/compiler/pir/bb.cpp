#include "pir_impl.h"

#include <iostream>
#include <unordered_set>

namespace rir {
namespace pir {

BB::BB(Code* owner, unsigned id) : id(id), owner(owner) {
    assert(id < owner->nextBBId);
}

void BB::print(std::ostream& out) {
    out << "BB" << id << "\n";
    for (auto i : instrs) {
        out << "  ";
        i->print(out);
        out << "\n";
    }
    if (isJmp()) {
        out << "  goto BB" << next0->id << "\n";
    }
}

BB::~BB() {
    gc();
    for (auto* i : instrs)
        delete i;
}

Instruction* BB::last() {
    assert(!instrs.empty());
    return instrs.back();
}

void BB::append(Instruction* i) {
    instrs.push_back(i);
    i->bb_ = this;
}

BB::Instrs::iterator BB::remove(Instrs::iterator it) {
    deleted.push_back(*it);
    return instrs.erase(it);
}

BB::Instrs::iterator BB::moveToEnd(Instrs::iterator it, BB* other) {
    other->append(*it);
    return instrs.erase(it);
}

BB::Instrs::iterator BB::moveToBegin(Instrs::iterator it, BB* other) {
    other->insert(other->instrs.begin(), *it);
    return instrs.erase(it);
}

void BB::swapWithNext(Instrs::iterator it) {
    Instruction* i = *it;
    *it = *(it + 1);
    *(it + 1) = i;
}

BB* BB::cloneInstrs(BB* src, unsigned id, Code* target) {
    BB* c = new BB(target, id);
    for (auto i : src->instrs) {
        Instruction* ic = i->clone();
        ic->bb_ = c;
        c->instrs.push_back(ic);
    }
    c->next0 = c->next1 = nullptr;
    return c;
}

void BB::replace(Instrs::iterator it, Instruction* i) {
    deleted.push_back(*it);
    *it = i;
    i->bb_ = this;
}

BB::Instrs::iterator BB::insert(Instrs::iterator it, Instruction* i) {
    auto itup = instrs.insert(it, i);
    i->bb_ = this;
    return itup;
}

void BB::gc() {
    // Catch double deletes
    std::unordered_set<Instruction*> dup;
    dup.insert(deleted.begin(), deleted.end());
    assert(dup.size() == deleted.size());

    for (auto i : deleted)
        delete i;
    deleted.clear();
}
}
}
