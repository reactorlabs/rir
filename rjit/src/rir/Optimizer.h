#ifndef RIR_OPTIMIZER_H
#define RIR_OPTIMIZER_H

#include "BC.h"
#include "Function.h"
#include "../Symbols.h"
#include "CodeStream.h"
#include <cassert>

namespace rjit {
namespace rir {

namespace {

void optimize_(Function* fun, fun_idx_t idx);
void optimize(CodeStream& cs, Function* fun, Code* cur);

BC_t* doInlineIf(CodeStream& cs, Function* fun, Code* cur, BC_t* pc,
                 BC_t* end) {

    cs << BC::check_special(symbol::If);

    std::vector<fun_idx_t> args;

    while (pc != end) {
        BC bc = BC::advance(&pc);

        switch (bc.bc) {
        case BC_t::push_arg:
            args.push_back(bc.immediate.fun);
            break;

        case BC_t::call: {
            Label trueBranch = cs.mkLabel();
            Label nextBranch = cs.mkLabel();

            optimize(cs, fun, fun->code[args[0]]);
            cs << BC::to_bool() << BC::jmp_true(trueBranch);

            if (args.size() < 3) {
                cs << BC::push(R_NilValue);
            } else {
                optimize(cs, fun, fun->code[args[2]]);
            }
            cs << BC::jmp(nextBranch);

            cs << trueBranch;
            optimize(cs, fun, fun->code[args[1]]);

            cs << nextBranch;
            goto done;
        }

        default:
            bc.print();
            std::cout << "Unexpected bc seq in call to if function\n";
            assert(false);
        }
    }
done:
    return pc;
}

void optimize(CodeStream& cs, Function* fun, Code* cur) {
    BC_t* pc = cur->bc;
    BC_t* end = (BC_t*)(uintptr_t)pc + cur->size;

    while (pc != end) {
        BC bc = BC::advance(&pc);

        switch (bc.bc) {
        case BC_t::getfun:
            if (bc.immediateConst() == symbol::If) {
                pc = doInlineIf(cs, fun, cur, pc, end);
                continue;
            }
            break;

        case BC_t::ret:
            continue;

        case BC_t::push_arg:
            optimize_(fun, bc.immediate.fun);
            break;

        default:
            break;
        }
        cs << bc;
    }
}

void optimize_(Function* fun, fun_idx_t idx) {
    Code* c = fun->code[idx];

    CodeStream opt(c->ast);
    optimize(opt, fun, c);
    opt << BC::ret();

    Code* optCode = opt.toCode();
    fun->code[idx] = optCode;
}
}

class Optimizer {
  public:
    static void optimize(Function* fun) { optimize_(fun, 0); }
};

} // rir
} // rjit

#endif
