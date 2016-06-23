#include <map>
#include <stack>

#include <cassert>

#include "StackVerifier.h"
#include "BC_inc.h"


namespace rjit {
namespace rir {

namespace {

struct State {
    unsigned pc;
    int ostack;
    int istack;

    State(unsigned pc = 0, int ostack = 0, int istack = 0):
        pc(pc),
        ostack(ostack),
        istack(istack) {
    }

    bool operator != (State const & other) const {
        assert (pc == other.pc and "It is meaningless to compare different states");
        return pc != other.pc or ostack != other.ostack or istack != other.istack;
    }

    State & operator = (State const & from) = default;

    void updateMax(State const & other) {
        if (other.ostack > ostack)
            ostack = other.ostack;
        if (other.istack > istack)
            istack = other.istack;
    }

    void check() const {
        assert(ostack > 0 and "Too many pops");
        assert(istack > 0 and "Too many i pops");
    }

    void advance(BC_t * code, unsigned & pc) {
        ostack -= BC::popCount(code[pc]);
        istack -= BC::iPopCount(code[pc]);
        check();
        ostack += BC::pushCount(code[pc]);
        istack += BC::iPushCount(code[pc]);
        pc += rjit::rir::BC::size(code[pc]);
    }
};

} // unnamed namespace

void StackVerifier::verify(::Code * c) {
    State max; // max state
    std::map<unsigned, State> state;
    std::stack<State> q;
    q.push(State());
    BC_t * cptr = reinterpret_cast<BC_t*>(code(c));
    while (not q.empty()) {
        State i = q.top();
        q.pop();
        if (state.find(i.pc) != state.end()) {
            State current = state[i.pc];
            if (current != i)
                    assert(false and "Stack imbalance detected");
            continue;
        }
        while (true) {
            state[i.pc] = i;
            if (cptr[i.pc] == BC_t::ret_)
                break;



        }
    }


    c->stackLength = max.ostack;
    c->iStackLength = max.istack;





}


} // namespace rir
} // namespace rjit
