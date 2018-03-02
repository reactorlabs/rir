#ifndef COMPILER_BB_H
#define COMPILER_BB_H

#include "pir.h"

#include <set>

namespace rir {
namespace pir {

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
    Code* fun;

    BB(const BB&) = delete;
    void operator=(const BB&) = delete;

    BB(Code* fun, unsigned id);
    ~BB();

    static BB* cloneInstrs(BB* src, unsigned id, Code* target);

    void unsafeSetId(unsigned newId) { *const_cast<unsigned*>(&id) = newId; }

    unsigned indexOf(Instruction* i) {
        unsigned p = 0;
        for (auto j : instr) {
            if (i == j)
                return p;
            p++;
        }
        assert(false);
        return 0;
    }

    Instruction* last() {
        assert(instr.size() > 0);
        return instr.back();
    }

    BB* next0 = nullptr;
    BB* next1 = nullptr;

    bool jmp() { return next0 && !next1; }
    bool empty() { return instr.size() == 0; }

    typedef std::vector<Instruction*> Instrs;

    void append(Instruction* i);

    Instrs::iterator insert(Instrs::iterator it, Instruction* i);
    void replace(Instrs::iterator it, Instruction* i);

    Instrs::iterator remove(Instrs::iterator it);
    Instrs::iterator moveToEnd(Instrs::iterator it, BB* other);
    Instrs::iterator moveToBegin(Instrs::iterator it, BB* other);

    void swap(Instrs::iterator);

    void print(std::ostream& = std::cout);

    Instrs::iterator begin() { return instr.begin(); }
    Instrs::iterator end() { return instr.end(); }
    size_t size() { return instr.size(); }
    Instruction* at(size_t i) { return instr[i]; }

    void gc();

  private:
    Instrs instr;
    Instrs deleted;
};

}
}

#endif
