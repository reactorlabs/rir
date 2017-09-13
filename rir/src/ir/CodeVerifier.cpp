#include <map>
#include <stack>

#include <cassert>

#include "utils/FunctionHandle.h"
#include "CodeVerifier.h"
#include "BC.h"
#include "R/Symbols.h"
#include "interpreter/deoptimizer.h"

namespace rir {

namespace {

/** State for verifying the stack layout and calculating max ostack 
 * size.
 */
class State {
  public:
    static_assert(sizeof(SEXP) == 8, "Invalid ptr size");
    static_assert(sizeof(unsigned) == 4, "Invalid unsigned size");

    Opcode* pc;
    int ostack;

    State(Opcode* pc = nullptr, int ostack = 0) : pc(pc), ostack(ostack) {}

    State(State const& from, Opcode* pc) : pc(pc), ostack(from.ostack) {}

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

        if (bc.bc == Opcode::return_)
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
    std::map<Opcode*, State> state;
    std::stack<State> q;

    Opcode* cptr = code.bc();
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
            Opcode* pc = i.pc;
            assert(pc >= code.bc() && pc < code.endBc());
            BC cur = BC::decode(pc);
            i.advance();
            max.updateMax(i);
            if (cur.bc == Opcode::ret_ || cur.bc == Opcode::return_) {
                i.checkClear();
                break;
            } else if (cur.bc == Opcode::br_) {
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
    assert(TYPEOF(sexp) == EXTERNALSXP and "Invalid SEXPTYPE");
    FunctionHandle fun(sexp);

    // get the code objects
    std::vector<Code*> objs;
    for (auto c : fun) {
        objs.push_back(c);
    }

    assert(fun.function->size <= XLENGTH(fun.store) and
           "Reported size must be smaller than the size of the vector");

    Function* f = fun.function;
    if (f->origin) {
        assert(TYPEOF(f->origin) == EXTERNALSXP and "Invalid origin type");
        assert(sexp2function(f->origin)->magic == FUNCTION_MAGIC and
               "Origin does not seem to be function bytecode");
    }
    assert(f->codeLength == objs.size() and "Invalid number of code objects");

    // add the end sentinel
    objs.push_back(end(f));
    // check the code headers
    for (size_t i = 0, e = objs.size() - 1; i != e; ++i) {
        Code* c = objs[i];
        assert(c->magic == CODE_MAGIC and "Invalid code magic number");
        assert(code2function(c) == f and "Invalid code offset");
        assert(c->src != 0 and "Code must have AST");
        unsigned oldo = c->stackLength;
        calculateAndVerifyStack(c);
        assert(oldo == c->stackLength and "Invalid stack layout reported");

        assert((uintptr_t)(c + 1) + pad4(c->codeSize) +
                       c->srcLength * sizeof(unsigned) +
                       c->skiplistLength * 2 * sizeof(unsigned) +
                       c->callSiteLength ==
                   (uintptr_t)objs[i + 1] and
               "Invalid code length reported");
    }

    // remove the sentinel
    objs.pop_back();

    auto verifyCallSite = [&ctx](CallSiteStruct* cs, uint32_t nargs) {
        SEXP call = cp_pool_at(ctx, cs->call);
        assert(TYPEOF(call) == LANGSXP || TYPEOF(call) == SYMSXP ||
               TYPEOF(call) == NILSXP);
        if (cs->hasSelector) {
            assert(!cs->hasTarget);
            SEXP selector = cp_pool_at(ctx, *CallSite_selector(cs));
            assert(TYPEOF(selector) == SYMSXP);
        } else if (cs->hasTarget) {
            assert(!cs->hasSelector);
            SEXP selector = cp_pool_at(ctx, *CallSite_target(cs));
            assert(TYPEOF(selector) == SYMSXP);
        } else {
            assert(cs->trg == 0);
        }
        assert(cs->nargs == nargs);
    };

    // check that the call instruction has proper arguments and number of
    // instructions is valid
    for (auto c : objs) {
        Opcode* cptr = reinterpret_cast<Opcode*>(code(c));
        Opcode* start = cptr;
        Opcode* end = start + c->codeSize;
        while (true) {
            assert(cptr < end);
            BC cur = BC::decode(cptr);
            if (*cptr == Opcode::br_ || *cptr == Opcode::brobj_ ||
                *cptr == Opcode::brtrue_ || *cptr == Opcode::brfalse_) {
                int off = *reinterpret_cast<int*>(cptr + 1);
                assert(cptr + off >= start && cptr + off < end);
            }
            if (*cptr == Opcode::guard_env_) {
                unsigned deoptId = *reinterpret_cast<ArgT*>(cptr + 1);
                Opcode* deoptPc = (Opcode*)Deoptimizer_pc(deoptId);
                assert(f->origin);
                FunctionHandle deoptFun = f->origin;
                CodeHandle deoptCode = deoptFun.entryPoint();
                assert(deoptPc >= deoptCode.bc() &&
                       deoptPc < deoptCode.endBc());
            }
            if (*cptr == Opcode::ldvar_) {
                unsigned* argsIndex = reinterpret_cast<ArgT*>(cptr + 1);
                assert(*argsIndex < cp_pool_length(ctx) and
                       "Invalid arglist index");
                SEXP sym = cp_pool_at(ctx, *argsIndex);
                assert(TYPEOF(sym) == SYMSXP);
                assert(strlen(CHAR(PRINTNAME(sym))));
            }
            if (*cptr == Opcode::dispatch_stack_ ||
                *cptr == Opcode::call_stack_) {
                unsigned callIdx = *reinterpret_cast<ArgT*>(cptr + 1);
                CallSiteStruct* cs = CallSite_get(c, callIdx);
                uint32_t nargs = *reinterpret_cast<ArgT*>(cptr + 5);
                verifyCallSite(cs, nargs);

                if (cs->hasNames) {
                    for (size_t i = 0, e = nargs; i != e; ++i) {
                        uint32_t offset = CallSite_names(cs)[i];
                        if (offset) {
                            SEXP name = cp_pool_at(ctx, offset);
                            assert(TYPEOF(name) == SYMSXP ||
                                   name == R_NilValue);
                        }
                    }
                }
                if (*cptr == Opcode::dispatch_stack_) {
                    SEXP selector = cp_pool_at(ctx, *CallSite_selector(cs));
                    assert(TYPEOF(selector) == SYMSXP);
                }
            }
            if (*cptr == Opcode::promise_) {
                unsigned* promidx = reinterpret_cast<ArgT*>(cptr + 1);
                bool ok = false;
                for (Code* c : objs)
                    if (c->header == *promidx) {
                        ok = true;
                        break;
                    }
                assert(ok and "Invalid promise offset detected");
            }
            if (*cptr == Opcode::call_ || *cptr == Opcode::dispatch_) {
                unsigned callIdx = *reinterpret_cast<ArgT*>(cptr + 1);
                CallSiteStruct* cs = CallSite_get(c, callIdx);
                uint32_t nargs = *reinterpret_cast<ArgT*>(cptr + 5);
                verifyCallSite(cs, nargs);
                assert(cs->hasImmediateArgs);

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
                if (cs->hasNames) {
                    for (size_t i = 0, e = nargs; i != e; ++i) {
                        uint32_t offset = CallSite_names(cs)[i];
                        if (offset) {
                            SEXP name = cp_pool_at(ctx, offset);
                            assert(TYPEOF(name) == SYMSXP ||
                                   name == R_NilValue);
                        }
                    }
                }
                if (*cptr == Opcode::dispatch_) {
                    SEXP selector = cp_pool_at(ctx, *CallSite_selector(cs));
                    assert(TYPEOF(selector) == SYMSXP);
                }
            }
            cptr += cur.size();
            if (cptr == start + c->codeSize) {
                assert(cptr == start + c->codeSize);
                assert(cur.isJmp() || cur.bc == Opcode::ret_);
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
