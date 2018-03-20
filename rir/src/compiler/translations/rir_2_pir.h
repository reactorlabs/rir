#ifndef RIR_2_PIR_H
#define RIR_2_PIR_H

#include "pir_translator.h"
#include "../pir/stack_machine.h"
#include <unordered_map>

namespace rir {

template <size_t SIZE>
struct Matcher {
    const std::array<Opcode, SIZE> seq;

    typedef std::function<void(Opcode*)> MatcherMaybe;

    bool operator()(Opcode* pc, Opcode* end, MatcherMaybe m) const {
        for (size_t i = 0; i < SIZE; ++i) {
            if (*pc != seq[i])
                return false;
            BC::advance(&pc);
            if (pc == end)
                return false;
        }
        m(pc);
        return true;
    }
};

class Rir2Pir : public PirTranslator {
  public:
    Rir2Pir() : PirTranslator(), mergepoint() , state() {}
    pir::Function* compileFunction(SEXP);
    pir::Function* compileFunction(Function*);
    pir::Function* compileFunction(pir::IRTransformation*);
    pir::Function* compileFunction(pir::IRTransformation*, bool);
    void optimizeFunction(pir::Function*);
    pir::Module* compileModule(SEXP f, bool);
    void operator()(SEXP in);

    static pir::IRTransformation* declare(SEXP& function);
    static pir::IRTransformation* declare(rir::Function* rir);
  private:
    std::unordered_map<Opcode*, pir::StackMachine> mergepoint;
    pir::StackMachine state;
    void recoverCFG(pir::IRTransformation* rir2PirTransformation);
    bool doMerge(Opcode* trg, pir::Builder*);
    void popFromWorklist(std::deque<pir::StackMachine>*, pir::Builder*);
};

}

#endif
