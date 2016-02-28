#ifndef IR_VERIFIER_H
#define IR_VERIFIER_H

#include "llvm.h"
#include "Ir.h"
#include "IrScalars.h"

namespace rjit {
namespace ir {

/** IR Verifier.

 Makes sure that IR of given llvm function conforms to rjit's IR rules. These
 rules are:

 1) each pattern has to be consecutive
 2) any pattern used must correctly return its first and last instructions
 3) all instructions between first and last instruction of a given pattern must
 belong to that pattern
 4) pattern's length must be correct
 5) only the result instruction of the pattern may be used by instructions
 outside of the pattern

 TODO are there more?

 */
class Verifier {
  public:
    /** Checks that the bitcode of given llvm function corresponds to the ir
     * rules.
     */
    static bool check(llvm::Function* f) {
        Verifier v(f);
        return v.check();
    }

  private:
    Verifier(llvm::Function* f) : f(f) {}

    /** Checks the function. Returns true if all patterns are correctly formed.
     */
    bool check() {
        for (auto b = f->begin(), e = f->end(); b != e; ++b)
            for (llvm::BasicBlock::iterator i = b->begin(), be = b->end();
                 i != be;)
                if (not checkPattern(i))
                    return false;
        return true;
    }

    /** Checks that a pattern beginning at given iterator position is correctly
      formed and advances the iterator past it.

      If the iterator does not point to a pattern, advances it by single
      instruction and always returns true.
     */
    bool checkPattern(llvm::BasicBlock::iterator& i) {
        Pattern* p = Pattern::get(i);
        // if the instruction is not part of any pattern, all is fine
        if (p == nullptr) {
            ++i; // advance to next instruction
            return true;
        }
        llvm::Instruction* first = p->first();
        llvm::Instruction* last = p->last();
        llvm::Instruction* result = p->ins_;

        if (first != i) {
            std::cout << "Pattern reports wrong first instruction:"
                      << std::endl;
            return false;
        }
        if (Pattern::get(result) != p) {
            std::cout << "Pattern's result instruction does not belong to the "
                         "pattern:"
                      << std::endl;
            return false;
        }
        if (Pattern::get(last) != p) {
            std::cout
                << "Pattern's last instruction does not belong to the pattern:"
                << std::endl;
        }

        size_t length = 0;
        while (true) {
            if (Pattern::get(i) != p) {
                std::cout << "Instruction inside pattern not attached:"
                          << std::endl;
                return false;
            }
            if (result != i) {
                for (llvm::User* u : i->users()) {
                    llvm::Instruction* ui = dyn_cast<llvm::Instruction>(u);
                    if (Pattern::get(ui) != p) {
                        std::cout << "Non result instruction of a pattern is "
                                     "used outside of it:"
                                  << std::endl;
                        return false;
                    }
                }
            }
            ++length;
            if (last == i++)
                break;
        }

        if (length != p->length()) {
            std::cout << "Pattern reports incorrect length:" << std::endl;
            return false;
        }
        return true;
    }

    llvm::Function* const f;
};

} // namespace ir
} // namespace rjit

#endif // IR_VERIFIER_H
