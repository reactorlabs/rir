#include <map>
#include <stack>

#include <cassert>

#include "utils/FunctionHandle.h"
#include "CodeVerifier.h"
#include "BC.h"

namespace rir {

namespace {

/** State for verifying the stack layout and calculating max ostack and istack
 * size.
 */
class State {
  public:
    static_assert(sizeof(SEXP) == 8, "Invalid ptr size");
    static_assert(sizeof(unsigned) == 4, "Invalid unsigned size");

    static_assert(sizeof(::Code) == 7 * 4, "Invalid ::Code size");
    static_assert(sizeof(::Function) == 6 * 4, "Invalid ::Function size");

    BC_t* pc;
    int ostack;
    int istack;

    State(BC_t* pc = nullptr, int ostack = 0, int istack = 0)
        : pc(pc), ostack(ostack), istack(istack) {}

    State(State const& from, BC_t* pc)
        : pc(pc), ostack(from.ostack), istack(from.istack) {}

    bool operator!=(State const& other) const {
        assert(pc == other.pc and
               "It is meaningless to compare different states");
        return pc != other.pc or ostack != other.ostack or
               istack != other.istack;
    }

    State& operator=(State const& from) = default;

    /** Updates own ostack and istack if the other stack has greater
     * requirements.
     */
    void updateMax(State const& other) {
        if (other.ostack > ostack)
            ostack = other.ostack;
        if (other.istack > istack)
            istack = other.istack;
    }

    void check() const {
        assert(ostack >= 0 and "Too many pops");
        assert(istack >= 0 and "Too many i pops");
    }

    void advance() {
        BC bc = BC::advance(&pc);

        ostack -= bc.popCount();
        istack -= bc.iPopCount();
        check();
        ostack += bc.pushCount();
        istack += bc.iPushCount();
    }

    BC cur() { return BC::decode(pc); }

    void checkClear() const {
        assert(ostack == 0 and istack == 0 and
               "Stack imbalance when exitting the function");
    }
};

} // unnamed namespace

void CodeVerifier::calculateAndVerifyStack(CodeHandle code) {
    State max; // max state
    std::map<BC_t*, State> state;
    std::stack<State> q;

    BC_t* cptr = code.bc();
    q.push(State(cptr));

    while (not q.empty()) {
        State i = q.top();
        q.pop();
        if (state.find(i.pc) != state.end()) {
            assert(i.pc >= code.bc() && i.pc < code.endBc());
            State current = state[i.pc];
            if (current != i)
                assert(false and "Stack imbalance detected");
            continue;
        }
        while (true) {
            state[i.pc] = i;
            BC_t* pc = i.pc;
            assert(pc >= code.bc() && pc < code.endBc());
            BC cur = BC::decode(pc);
            i.advance();
            max.updateMax(i);
            if (cur.bc == BC_t::ret_) {
                assert(!cur.isJmp());
                i.checkClear();
                break;
            } else if (cur.bc == BC_t::br_) {
                q.push(State(i, BC::jmpTarget(pc)));
                break;
            } else if (cur.bc == BC_t::brtrue_ or cur.bc == BC_t::brfalse_) {
                q.push(State(i, BC::jmpTarget(pc)));
                // no break because we want to continue verification in current
                // sequence as well
            }
        }
    }
    code.code->stackLength = max.ostack;
    code.code->iStackLength = max.istack;
}

void CodeVerifier::vefifyFunctionLayout(SEXP sexp, ::Context* ctx) {
    assert(TYPEOF(sexp) == INTSXP and "Invalid SEXPTYPE");
    FunctionHandle fun(sexp);

    // Rprintf("Checking function object at %u\n", f);
    // get the code objects
    std::vector<Code*> objs;
    for (auto c : fun) {
        // Rprintf("Checking code object at %u\n", c);
        // Rprintf("End: %u\n", e);
        objs.push_back(c);
    }

    assert(fun.function->size <= sizeof(int) *static_cast<unsigned>(Rf_length(sexp)) and
           "Reported size must be smaller than the size of the vector");
    assert(fun.function->size < 1.5 * sizeof(int) * static_cast<unsigned>(Rf_length(sexp)) and
           "Unexpectedly large Vector");

    Function* f = fun.function;
    if (f->origin) {
        assert(TYPEOF(f->origin) == INTSXP and "Invalid origin type");
        assert(static_cast<unsigned>(INTEGER(f->origin)[0]) ==
                   FUNCTION_MAGIC and
               "Origin does not seem to be function bytecode");
    }
    assert(f->codeLength == objs.size() and "Invalid number of code objects");

    // add the end sentinel
    objs.push_back(end(f));
    // check the code headers
    for (size_t i = 0, e = objs.size() - 1; i != e; ++i) {
        Code* c = objs[i];
        assert(c->magic == CODE_MAGIC and "Invalid code magic number");
        assert(function(c) == f and "Invalid code offset");
        assert(c->src != 0 and "Code must have AST");
        unsigned oldo = c->stackLength;
        unsigned oldi = c->iStackLength;
        calculateAndVerifyStack(c);
        assert(oldo == c->stackLength and "Invalid stack layout reported");
        assert(oldi == c->iStackLength and
               "Invalid integer stack layout reported");

        assert((uintptr_t)(c + 1) + pad4(c->codeSize) +
                       c->srcLength * sizeof(unsigned) ==
                   (uintptr_t)objs[i + 1] and
               "Invalid code length reported");
    }

    // remove the sentinel
    objs.pop_back();

    // check that the call instruction has proper arguments and number of
    // instructions is valid
    for (auto c : objs) {
        BC_t* cptr = reinterpret_cast<BC_t*>(code(c));
        BC_t* start = cptr;
        unsigned ninsns = 0;
        while (true) {
            ++ninsns;
            if (ninsns == c->srcLength) {
                assert((uintptr_t)cptr - (uintptr_t)(start) == 0 and
                       "Invalid code size");
                break;
            }
            assert((uintptr_t)cptr - (uintptr_t)(start) <= c->codeSize and
                   "Invalid size");
            if (*cptr == BC_t::call_) {
                unsigned* argsIndex = reinterpret_cast<ArgT*>(cptr + 1);
                assert(*argsIndex < cp_pool_length(ctx) and "Invalid arglist index");
                SEXP argsVec = cp_pool_at(ctx, *argsIndex);
                assert(TYPEOF(argsVec) == INTSXP and
                       "Invalid type of arguents vector");
                // check that the promise offsets are valid offsets within the
                // function
                for (size_t i = 0, e = Rf_length(argsVec); i != e; ++i) {
                    unsigned offset = INTEGER(argsVec)[i];
                    bool ok = false;
                    for (Code* c : objs)
                        if (c->header == offset) {
                            ok = true;
                            break;
                        }
                    assert(ok and "Invalid promise offset detected");
                }
                // check the names vector
                if (argsIndex[1] != 0) {
                    assert(argsIndex[1] < cp_pool_length(ctx) and
                           "Invalid type of argument names index");
                    SEXP namesVec = cp_pool_at(ctx, argsIndex[1]);
                    assert(TYPEOF(namesVec) == VECSXP and
                           "Invalid type of argument names vector");
                    assert(Rf_length(namesVec) == Rf_length(argsVec) and
                           "Names and args have different length");
                }
                // check the call has an ast attached
                assert(src(c)[ninsns-1]);
            }
        }

        // check that the astmap indices are within bounds
        for (auto c : objs) {
            unsigned* srcIndices = src(c);
            for (size_t i = 0; i != c->srcLength; ++i)
                assert(srcIndices[i] < src_pool_length(ctx) and
                       "Source index for instruction out of bounds");
        }
    }
}

} // namespace rir
