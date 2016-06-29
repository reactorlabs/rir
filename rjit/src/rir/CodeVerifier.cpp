#include <map>
#include <stack>

#include <cassert>

#include "CodeVerifier.h"
#include "BC_inc.h"

namespace rjit {
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

    unsigned pc;
    int ostack;
    int istack;

    State(unsigned pc = 0, int ostack = 0, int istack = 0)
        : pc(pc), ostack(ostack), istack(istack) {}

    State(State const& from, unsigned pc)
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

    void advance(BC_t* code, unsigned& pc) {
        ostack -= BC::popCount(code[pc]);
        istack -= BC::iPopCount(code[pc]);
        check();
        ostack += BC::pushCount(code[pc]);
        istack += BC::iPushCount(code[pc]);
        pc += rjit::rir::BC::size(code[pc]);
    }

    void checkClear() const {
        assert(ostack == 0 and istack == 0 and
               "Stack imbalance when exitting the function");
    }
};

} // unnamed namespace

void CodeVerifier::calculateAndVerifyStack(::Code* c) {
    State max; // max state
    std::map<unsigned, State> state;
    std::stack<State> q;
    q.push(State());
    BC_t* cptr = reinterpret_cast<BC_t*>(code(c));
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
            unsigned oldpc = i.pc;
            i.advance(cptr, i.pc);
            max.updateMax(i);
            if (cptr[oldpc] == BC_t::ret_) {
                i.checkClear();
                break;
            } else if (cptr[oldpc] == BC_t::br_) {
                unsigned target = *reinterpret_cast<ArgT*>(oldpc + 1);
                q.push(State(i, target));
                break;
            } else if (cptr[oldpc] == BC_t::brtrue_ or
                       cptr[oldpc] == BC_t::brfalse_) {
                unsigned target = *reinterpret_cast<ArgT*>(oldpc + 1);
                q.push(State(i, target));
                // no break because we want to continue verification in current
                // sequence as well
            }
        }
    }
    c->stackLength = max.ostack;
    c->iStackLength = max.istack;
}

void CodeVerifier::vefifyFunctionLayout(SEXP sexp, ::Context* ctx) {
    assert(TYPEOF(sexp) == INTSXP and "Invalid SEXPTYPE");
    ::Function* f = reinterpret_cast<::Function*>(INTEGER(sexp));
    Rprintf("Checking function object at %u\n", f);
    // get the code objects
    std::vector<::Code*> objs;
    for (::Code *c = begin(f), *e = end(f); c != e; c = next(c)) {
        Rprintf("Checking code object at %u\n", c);
        Rprintf("End: %u\n", e);
        objs.push_back(c);
    }

    // check the function header
    assert(f->magic == FUNCTION_MAGIC and "Invalid function magic number");
    // TODO This has changed - Rf_length is now >= than size, the current real length is quite vasteful and we might want to conserve space better
    assert(f->size < static_cast<unsigned>(Rf_length(sexp)) and
           "Reported size must be smaller than the size of the vector");
    if (f->origin != nullptr) {
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
    for (::Code* c : objs) {
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
                assert(*argsIndex < ctx->cp.length and "Invalid arglist index");
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
                    assert(argsIndex[1] < ctx->cp.length and
                           "Invalid type of argument names index");
                    SEXP namesVec = cp_pool_at(ctx, argsIndex[1]);
                    assert(TYPEOF(namesVec) == VECSXP and
                           "Invalid type of argument names vector");
                    assert(Rf_length(namesVec) == Rf_length(argsVec) and
                           "Names and args have different length");
                }
            }
        }

        // check that the astmap indices are within bounds
        for (::Code* c : objs) {
            unsigned* srcIndices = src(c);
            for (size_t i = 0; i != c->srcLength; ++i)
                assert(srcIndices[i] < ctx->src.length and
                       "Source index for instruction out of bounds");
        }
    }
}

} // namespace rir
} // namespace rjit
