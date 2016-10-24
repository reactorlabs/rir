#include <map>
#include <stack>

#include <cassert>

#include "utils/FunctionHandle.h"
#include "CodeVerifier.h"
#include "BC.h"
#include "R/Symbols.h"

namespace rir {

namespace {

/** State for verifying the stack layout and calculating max ostack 
 * size.
 */
class State {
  public:
    static_assert(sizeof(SEXP) == 8, "Invalid ptr size");
    static_assert(sizeof(unsigned) == 4, "Invalid unsigned size");

    BC_t* pc;
    int ostack;

    State(BC_t* pc = nullptr, int ostack = 0)
        : pc(pc), ostack(ostack) {}

    State(State const& from, BC_t* pc)
        : pc(pc), ostack(from.ostack) {}

    bool operator!=(State const& other) const {
        assert(pc == other.pc and
               "It is meaningless to compare different states");
        return pc != other.pc or ostack != other.ostack;
    }

    State& operator=(State const& from) = default;

    /** Updates own ostack if the other stack has greater
     * requirements.
     */
    void updateMax(State const& other) {
        if (other.ostack > ostack)
            ostack = other.ostack;
    }

    void check() const {
        assert(ostack >= 0 and "Too many pops");
    }

    void advance() {
        BC bc = BC::advance(&pc);

        if (bc.bc == BC_t::return_)
            ostack = 0;
        else
            ostack -= bc.popCount();
        check();
        ostack += bc.pushCount();
    }

    BC cur() { return BC::decode(pc); }

    void checkClear() const {
        assert(ostack == 0 and
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
            if (cur.bc == BC_t::ret_ || cur.bc == BC_t::return_) {
                i.checkClear();
                break;
            } else if (cur.bc == BC_t::br_) {
                q.push(State(i, BC::jmpTarget(pc)));
                break;
            } else if (cur.isJmp()) {
                q.push(State(i, BC::jmpTarget(pc)));
                // no break because we want to continue verification in current
                // sequence as well
            }
        }
    }
    code.code->stackLength = max.ostack;
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
        calculateAndVerifyStack(c);
        assert(oldo == c->stackLength and "Invalid stack layout reported");

        assert((uintptr_t)(c + 1) + pad4(c->codeSize) +
                       c->srcLength * sizeof(unsigned) +
                       c->skiplistLength * 2 * sizeof(unsigned) ==
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
        while (true) {
            assert(cptr < start + c->codeSize);
            BC cur = BC::decode(cptr);
            if (*cptr == BC_t::ldvar_) {
                unsigned* argsIndex = reinterpret_cast<ArgT*>(cptr + 1);
                assert(*argsIndex < cp_pool_length(ctx) and
                       "Invalid arglist index");
                SEXP sym = cp_pool_at(ctx, *argsIndex);
                assert(TYPEOF(sym) == SYMSXP);
                assert(strlen(CHAR(PRINTNAME(sym))));
            }
            if (*cptr == BC_t::call_stack_ || *cptr == BC_t::dispatch_stack_) {
                unsigned* argsIndex = reinterpret_cast<ArgT*>(cptr + 1);
                unsigned nargs = argsIndex[0];
                // check the names vector
                assert(argsIndex[1] < cp_pool_length(ctx) and
                       "Invalid type of argument names index");
                SEXP namesVec = cp_pool_at(ctx, argsIndex[1]);
                if (namesVec != R_NilValue) {
                    assert(TYPEOF(namesVec) == VECSXP and
                           "Invalid type of argument names vector");
                    assert((unsigned)Rf_length(namesVec) == nargs and
                           "Names and args have different length");
                }
                SEXP call;
                // check the call has an ast attached
                if (*cptr == BC_t::call_stack_) {
                    call = cp_pool_at(ctx, argsIndex[2]);
                } else {
                    SEXP selector = cp_pool_at(ctx, argsIndex[2]);
                    assert(TYPEOF(selector) == SYMSXP);
                    call = cp_pool_at(ctx, argsIndex[3]);
                }
                assert(TYPEOF(call) == LANGSXP);
            }
            if (*cptr == BC_t::promise_) {
                unsigned* promidx = reinterpret_cast<ArgT*>(cptr + 1);
                bool ok = false;
                for (Code* c : objs)
                    if (c->header == *promidx) {
                        ok = true;
                        break;
                    }
                assert(ok and "Invalid promise offset detected");
            }
            if (*cptr == BC_t::call_ || *cptr == BC_t::dispatch_) {
                unsigned callIdx = *reinterpret_cast<ArgT*>(cptr + 1);
                uint32_t* cs = &c->callSites[callIdx];
                uint32_t nargs = *reinterpret_cast<ArgT*>(cptr + 5);

                for (size_t i = 0, e = nargs; i != e; ++i) {
                    uint32_t offset = CallSite_args(cs)[i];
                    if (offset == MISSING_ARG_IDX || offset == DOTS_ARG_IDX)
                        continue;
                    bool ok = false;
                    for (Code* c : objs)
                        if (c->header == offset) {
                            ok = true;
                            break;
                        }
                    assert(ok and "Invalid promise offset detected");
                }
                if (*CallSite_hasNames(cs)) {
                    for (size_t i = 0, e = nargs; i != e; ++i) {
                        uint32_t offset = CallSite_names(cs, nargs)[i];
                        if (offset) {
                            SEXP name = cp_pool_at(ctx, offset);
                            assert(TYPEOF(name) == SYMSXP ||
                                   name == R_NilValue);
                        }
                    }
                }
                if (*cptr == BC_t::dispatch_) {
                    SEXP selector =
                        cp_pool_at(ctx, *CallSite_selector(cs, nargs));
                    assert(TYPEOF(selector) == SYMSXP);
                }
            }
            cptr += cur.size();
            if (cptr == start + c->codeSize) {
                assert(cptr == start + c->codeSize);
                assert(cur.isJmp() || cur.bc == BC_t::ret_);
                break;
            }
        }

        // check that the astmap indices are within bounds
        for (auto c : objs) {
            unsigned* srcIndices = raw_src(c);
            for (size_t i = 0; i != c->srcLength; ++i)
                assert(srcIndices[i] < src_pool_length(ctx) and
                       "Source index for instruction out of bounds");
        }
    }
}

} // namespace rir
