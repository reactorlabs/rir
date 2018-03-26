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
    Rir2Pir()
        : PirTranslator(), mergepoint(), state(), builder(nullptr) {}
    Rir2Pir(pir::Module* module)
        : PirTranslator(module), mergepoint(), state(), builder(nullptr) {}
    Rir2Pir(pir::Builder* builder)
        : PirTranslator(), mergepoint(), state(), builder(builder) {}
    pir::Function* compileFunction(SEXP);
    pir::Function* compileFunction(Function*, std::vector<SEXP>);
    pir::Value* translateCode(rir::Function*, rir::Code*);
    void optimizeFunction(pir::Function*);
    pir::Module* compileModule(SEXP f);
    
    /*static pir::IRTransformation* declare(SEXP&);
    static pir::IRTransformation* declare(rir::Function*);
    static pir::IRTransformation* declare(rir::Function*, rir::Code*);*/
  private:
    std::unordered_map<Opcode*, pir::StackMachine> mergepoint;
    pir::StackMachine state;
    pir::Builder* builder;
    void recoverCFG(rir::Code*);
    bool doMerge(Opcode* trg, pir::Builder*);
    void popFromWorklist(std::deque<pir::StackMachine>*, pir::Builder*);
    void addReturn(pir::Value*);
    pir::Function* compileInnerFunction(Function*, std::vector<SEXP>);
    pir::Builder* getBuilder();
};
}
#endif
