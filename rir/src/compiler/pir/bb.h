#ifndef COMPILER_BB_H
#define COMPILER_BB_H

#include "pir.h"

#include <unordered_set>

namespace rir {
namespace pir {

class DominanceGraph;
class Code;

/*
 * Basic Block, has a list of instructions.
 *
 * BB's have two successors, "next0" and "next1", both are BB*.
 *
 * There are 3 valid cases:
 *
 * - only next0 is not null:    This BB does implicitly goto next0
 * - next0, next1 are not null: This BB must end with a Branch instruction
 * - next0, next1 are null:     This BB must end in an exit point
 *                              (Return, or Deopt)
 *
 * A BB has an id, which must remain stable (since visitors and analysis use
 * the BB id as array indices).
 *
 */
class BB {
  public:
    // The visitor relies on stable ids, do not renumber inside a visitor!!!
    const unsigned id;
    Code* owner;

    BB(const BB&) = delete;
    BB& operator=(const BB&) = delete;

    BB(Code* fun, unsigned id);
    ~BB();

    static BB* cloneInstrs(BB* src, unsigned id, Code* target);

    void unsafeSetId(unsigned newId) { *const_cast<unsigned*>(&id) = newId; }

    unsigned indexOf(Instruction* i) {
        unsigned p = 0;
        for (auto j : instrs) {
            if (i == j)
                return p;
            p++;
        }
        assert(false);
        return 0;
    }

    Instruction* last() const;

    bool isJmp() const { return next0 && !next1; }
    bool isEmpty() const { return instrs.size() == 0; }

    typedef std::vector<Instruction*> Instrs;

    void append(Instruction* i);

    Instrs::iterator insert(Instrs::iterator it, Instruction* i);

    void replace(Instrs::iterator it, Instruction* i);

    void remove(Instruction* i);

    Instrs::iterator remove(Instrs::iterator it);
    Instrs::iterator moveToEnd(Instrs::iterator it, BB* other);
    Instrs::iterator moveToBegin(Instrs::iterator it, BB* other);

    void swapWithNext(Instrs::iterator);

    void print(std::ostream& = std::cout, bool tty = false);

    Instrs::iterator begin() { return instrs.begin(); }
    Instrs::iterator end() { return instrs.end(); }

    Instrs::reverse_iterator rbegin() { return instrs.rbegin(); }
    Instrs::reverse_iterator rend() { return instrs.rend(); }

    size_t size() const { return instrs.size(); }
    Instruction* at(size_t i) { return instrs[i]; }

    void gc();

    bool isExit() const { return !next0 && !next1; }

    void setBranch(BB* trueBranch, BB* falseBranch) {
        assert(!next0 && !next1);
        this->next0 = trueBranch;
        this->next1 = falseBranch;
    }
    BB* trueBranch() { return next0; }
    BB* falseBranch() { return next1; }

    void overrideNext(BB* bb) {
        assert(next0 && !next1);
        next0 = bb;
    }
    void setNext(BB* bb) {
        assert(!next0 && !next1);
        next0 = bb;
    }
    BB* next() {
        assert(next0 && !next1);
        return next0;
    }

    // don't use them directly unless you know what you are doing
    // We don't want to make them private, since we are all adults. But there
    // are probably not many reasons to use them outside the cleanup pass and
    // BBTransformer.
    BB* next0 = nullptr;
    BB* next1 = nullptr;

    void collectDominated(std::unordered_set<BB*>& subs, DominanceGraph& dom);

  private:
    Instrs instrs;
    // Keeps around deleted instructions to be able to remove them later
    Instrs deleted;
};

} // namespace pir
} // namespace rir

#endif
