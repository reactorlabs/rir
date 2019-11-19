#ifndef COMPILER_BB_H
#define COMPILER_BB_H

#include "pir.h"

#include "utils/Set.h"
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

    unsigned indexOf(const Instruction* i) {
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

    Instrs::iterator atPosition(Instruction* i);
    Instrs::iterator remove(Instrs::iterator it);
    Instrs::iterator moveToLast(Instrs::iterator it, BB* other);
    Instrs::iterator moveToEnd(Instrs::iterator it, BB* other);
    Instrs::iterator moveToBegin(Instrs::iterator it, BB* other);

    void swapWithNext(Instrs::iterator);

    bool before(Instruction*, Instruction*) const;

    void print(std::ostream&, bool tty);
    void printGraph(std::ostream&, bool omitDeoptBranches);
    void printBBGraph(std::ostream&, bool omitDeoptBranches);

    Instrs::iterator begin() { return instrs.begin(); }
    Instrs::iterator end() { return instrs.end(); }

    Instrs::reverse_iterator rbegin() { return instrs.rbegin(); }
    Instrs::reverse_iterator rend() { return instrs.rend(); }

    size_t size() const { return instrs.size(); }
    Instruction* at(size_t i) { return instrs[i]; }

    void gc();

    bool isExit() const { return !next0 && !next1; }
    bool isDeopt() const;
    bool isBranch() const { return next0 && next1; }
    bool isCheckpoint() const;
    bool isMerge() const { return predecessors().size() > 1; }

    void setTrueBranch(BB* trueBranch) {
        assert(!next0);
        this->next0 = trueBranch;
        trueBranch->prev.insert(this);
    }
    void setFalseBranch(BB* falseBranch) {
        assert(!next1);
        this->next1 = falseBranch;
        falseBranch->prev.insert(this);
    }
    void setBranch(BB* trueBranch, BB* falseBranch) {
        assert(!next0 && !next1);
        setTrueBranch(trueBranch);
        setFalseBranch(falseBranch);
    }

    BB* trueBranch() { return next0; }
    BB* falseBranch() { return next1; }

    BB* mainBranch() {
        SLOWASSERT(isCheckpoint());
        return trueBranch();
    }
    BB* deoptBranch() {
        SLOWASSERT(isCheckpoint());
        return falseBranch();
    }

    void overrideTrueBranch(BB* bb) {
        assert(next0 && next1);
        next0->prev.erase(this);
        next0 = nullptr;
        setTrueBranch(bb);
    }
    void overrideFalseBranch(BB* bb) {
        assert(next0 && next1);
        next1->prev.erase(this);
        next1 = nullptr;
        setFalseBranch(this);
    }

    void convertBranchToJmp(bool condition) {
        assert(next0 && next1);
        if (condition) {
            next1->prev.erase(this);
            next1 = nullptr;
        } else {
            next0->prev.erase(this);
            next0 = next1;
            next1 = nullptr;
        }
    }

    struct Successors {
      private:
        typedef std::array<BB*, 2> BBs;
        BBs next;
        friend class BB;

      public:
        Successors(BB* a, BB* b) : next{a, b} {}
        bool all(const std::function<bool(BB*)>& apply) const {
            bool ok = true;
            for (int i = 0; i < 2 && ok; ++i)
                if (next[i])
                    ok = ok && apply(next[i]);
            return ok;
        };
        bool any(const std::function<bool(BB*)>& apply) const {
            bool ok = false;
            for (int i = 0; i < 2 && !ok; ++i)
                if (next[i])
                    ok = ok || apply(next[i]);
            return ok;
        };

        const Successors map(const std::function<BB*(BB*)>& m) const {
            auto res = *this;
            for (int i = 0; i < 2; ++i)
                if (res[i])
                    res.next[i] = m(res[i]);
            return res;
        };

        BB* operator[](size_t i) const {
            assert(i == 0 || i == 1);
            // cppcheck-suppress CastIntegerToAddressAtReturn
            return next[i];
        }
        BBs::const_iterator begin() const { return next.cbegin(); }
        BBs::const_iterator end() const {
            if (!next[0]) {
                assert(!next[1]);
                return next.cbegin();
            }
            if (!next[1])
                return next.cend() - 1;
            return next.cend();
        }
    };
    const Successors nonDeoptSuccsessors() {
        auto res = succsessors();
        if (isCheckpoint())
            res.next[1] = nullptr;
        for (auto i = 0; i < 2; ++i)
            if (res[i] && res[i]->isDeopt())
                res.next[i] = nullptr;
        // this invariant is needed for the iterator to properly work
        if (res.next[1] && !res.next[0])
            return {res.next[1], nullptr};
        return res;
    }
    const Successors succsessors() { return {next0, next1}; }
    void setSuccessors(const Successors& succ) {
        if (next0)
            next0->prev.erase(this);
        next0 = succ.next[0];
        if (next0)
            next0->prev.insert(this);
        if (next1)
            next1->prev.erase(this);
        next1 = succ.next[1];
        if (next1)
            next1->prev.insert(this);
    }
    void deleteSuccessors() {
        if (next0)
            next0->prev.erase(this);
        if (next1)
            next1->prev.erase(this);
        next0 = next1 = nullptr;
    }

    void setNext(BB* bb) {
        assert(!next0 && !next1);
        next0 = bb;
        next0->prev.insert(this);
    }
    const BB* next() const {
        assert(next0 && !next1);
        return next0;
    }
    BB* next() {
        assert(next0 && !next1);
        return next0;
    }
    void overrideNext(BB* bb) {
        assert(next0 && !next1);
        next0->prev.erase(this);
        next0 = nullptr;
        setNext(bb);
    }
    void replaceSuccessor(BB* old, BB* suc) {
        if (next0 && next0 == old) {
            next0->prev.erase(this);
            next0 = suc;
        } else if (next1 && next1 == old) {
            next1->prev.erase(this);
            next1 = suc;
        } else {
            assert(false && "cannot replace, is not a successor");
        }
        suc->prev.insert(this);
    }

    BB* nextOrTrueBranch() { return next0; }
    void overrideNextOrTrueBranch(BB* bb) {
        assert(next0);
        next0->prev.erase(this);
        next0 = nullptr;
        next0 = bb;
        next0->prev.insert(this);
    }

    size_t uid() { return (size_t)this; }

    const SmallSet<BB*>& predecessors() const { return prev; }

    bool hasSinglePred() const { return prev.size() == 1; }

  private:
    // don't use them directly unless you know what you are doing
    // We don't want to make them private, since we are all adults. But there
    // are probably not many reasons to use them outside the cleanup pass and
    // BBTransformer.
    BB* next0 = nullptr;
    BB* next1 = nullptr;
    SmallSet<BB*> prev;

    Instrs instrs;
    // Keeps around deleted instructions to be able to remove them later
    Instrs deleted;
};

} // namespace pir
} // namespace rir

#endif
