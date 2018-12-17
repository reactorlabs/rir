#include <map>
#include <stack>

#include <cassert>

#include "BC.h"
#include "CodeVerifier.h"
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

    void check() const { assert(ostack >= 0 and "Too many pops"); }

    void advance(Code* code) {
        BC bc = BC::advance(&pc, code);
        // Those two instructions deal with cleanly returning from the function
        // themselves, so we can ignore leftover values on the stack. Note that
        // ret_ should not be added here, as it requires the stack to have
        // *only* the result value left.
        if (bc.bc == Opcode::return_ || bc.bc == Opcode::deopt_)
            ostack = 0;
        else
            ostack -= bc.popCount();
        check();
        ostack += bc.pushCount();
    }

    BC cur(Code* code) { return BC::decode(pc, code); }

    void checkClear() const {
        assert(ostack == 0 and "Stack imbalance when exitting the function");
    }
};

enum class Sources {
    Required,
    May,
    NotNeeded,
};

static Sources hasSources(Opcode bc) {
    switch (bc) {
    case Opcode::extract1_1_:
    case Opcode::extract1_2_:
    case Opcode::extract2_1_:
    case Opcode::extract2_2_:
    case Opcode::seq_:
    case Opcode::add_:
    case Opcode::mul_:
    case Opcode::div_:
    case Opcode::idiv_:
    case Opcode::mod_:
    case Opcode::pow_:
    case Opcode::sub_:
    case Opcode::uplus_:
    case Opcode::uminus_:
    case Opcode::not_:
    case Opcode::lt_:
    case Opcode::gt_:
    case Opcode::le_:
    case Opcode::ge_:
    case Opcode::eq_:
    case Opcode::ne_:
    case Opcode::colon_:
    case Opcode::subassign1_:
    case Opcode::subassign2_:
        return Sources::Required;

    case Opcode::inc_:
    case Opcode::identical_noforce_:
    case Opcode::push_:
    case Opcode::ldfun_:
    case Opcode::ldddvar_:
    case Opcode::ldvar_:
    case Opcode::ldvar_noforce_:
    case Opcode::ldvar_super_:
    case Opcode::ldvar_noforce_super_:
    case Opcode::ldlval_:
    case Opcode::stvar_:
    case Opcode::stvar_super_:
    case Opcode::guard_fun_:
    case Opcode::call_implicit_:
    case Opcode::named_call_implicit_:
    case Opcode::call_:
    case Opcode::named_call_:
    case Opcode::static_call_:
    case Opcode::promise_:
    case Opcode::push_code_:
    case Opcode::br_:
    case Opcode::brtrue_:
    case Opcode::beginloop_:
    case Opcode::brobj_:
    case Opcode::brfalse_:
    case Opcode::pick_:
    case Opcode::pull_:
    case Opcode::is_:
    case Opcode::put_:
    case Opcode::alloc_:
    case Opcode::ldarg_:
    case Opcode::stloc_:
    case Opcode::movloc_:
    case Opcode::nop_:
    case Opcode::make_env_:
    case Opcode::get_env_:
    case Opcode::parent_env_:
    case Opcode::set_env_:
    case Opcode::ret_:
    case Opcode::length_:
    case Opcode::names_:
    case Opcode::set_names_:
    case Opcode::force_:
    case Opcode::pop_:
    case Opcode::close_:
    case Opcode::asast_:
    case Opcode::dup_:
    case Opcode::dup2_:
    case Opcode::for_seq_size_:
    case Opcode::swap_:
    case Opcode::make_unique_:
    case Opcode::set_shared_:
    case Opcode::ensure_named_:
    case Opcode::return_:
    case Opcode::isfun_:
    case Opcode::invisible_:
    case Opcode::visible_:
    case Opcode::endloop_:
    case Opcode::isobj_:
    case Opcode::check_missing_:
    case Opcode::lgl_and_:
    case Opcode::lgl_or_:
    case Opcode::record_call_:
    case Opcode::record_binop_:
    case Opcode::deopt_:
        return Sources::NotNeeded;

    case Opcode::ldloc_:
    case Opcode::aslogical_:
    case Opcode::asbool_:
    case Opcode::missing_:
    case Opcode::int3_:
        return Sources::May;

    case Opcode::invalid_:
    case Opcode::num_of: {}
    }
    assert(false);
    return Sources::NotNeeded;
}
} // unnamed namespace

void CodeVerifier::calculateAndVerifyStack(Code* code) {
    State max; // max state
    std::map<Opcode*, State> state;
    std::stack<State> q;

    Opcode* cptr = code->code();
    q.push(State(cptr));

    while (not q.empty()) {
        State i = q.top();
        q.pop();
        if (state.find(i.pc) != state.end()) {
            assert(i.pc >= code->code() && i.pc < code->endCode());
            State current = state[i.pc];
            if (current != i)
                assert(false and "Stack imbalance detected");
            continue;
        }
        while (true) {
            state[i.pc] = i;
            Opcode* pc = i.pc;
            assert(pc >= code->code() && pc < code->endCode());
            BC cur = BC::decode(pc, code);
            i.advance(code);
            max.updateMax(i);
            if (cur.isExit()) {
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
    code->stackLength = max.ostack;
}

void CodeVerifier::verifyFunctionLayout(SEXP sexp, ::Context* ctx) {
    assert(TYPEOF(sexp) == EXTERNALSXP and "Invalid SEXPTYPE");
    Function* f = Function::unpack(sexp);

    // get the code objects
    std::vector<Code*> objs;
    objs.push_back(f->body());
    for (size_t i = 0; i < f->numArgs; ++i)
        if (f->defaultArg(i))
            objs.push_back(f->defaultArg(i));

    assert(f->size <= XLENGTH(sexp) and
           "Reported size must be smaller than the size of the vector");

    // check that the call instruction has proper arguments and number of
    // instructions is valid
    bool sawReturnOrBackjump = false;
    while (!objs.empty()) {
        auto c = objs.back();
        objs.pop_back();

        assert(c->info.magic == CODE_MAGIC and "Invalid code magic number");
        assert(c->src != 0 and "Code must have AST");
        unsigned oldo = c->stackLength;
        calculateAndVerifyStack(c);
        assert(oldo == c->stackLength and "Invalid stack layout reported");

        assert((uintptr_t)(c + 1) + pad4(c->codeSize) +
                   c->srcLength * sizeof(Code::SrclistEntry) &&
               "Invalid code length reported");

        Opcode* cptr = c->code();
        Opcode* start = cptr;
        Opcode* end = start + c->codeSize;
        while (true) {
            assert(cptr < end);
            BC cur = BC::decode(cptr, c);
            switch (hasSources(cur.bc)) {
            case Sources::Required:
                assert(c->getSrcIdxAt(cptr, true) != 0);
                break;
            case Sources::NotNeeded:
                assert(c->getSrcIdxAt(cptr, true) == 0);
                break;
            case Sources::May: {
            }
            }
            if (*cptr == Opcode::br_ || *cptr == Opcode::brobj_ ||
                *cptr == Opcode::brtrue_ || *cptr == Opcode::brfalse_) {
                int off = *reinterpret_cast<int*>(cptr + 1);
                assert(cptr + cur.size() + off >= start &&
                       cptr + cur.size() + off < end);
            }
            if (*cptr == Opcode::ldvar_) {
                unsigned* argsIndex = reinterpret_cast<Immediate*>(cptr + 1);
                assert(*argsIndex < cp_pool_length(ctx) and
                       "Invalid arglist index");
                SEXP sym = cp_pool_at(ctx, *argsIndex);
                assert(TYPEOF(sym) == SYMSXP);
                assert(strlen(CHAR(PRINTNAME(sym))));
            }
            if (*cptr == Opcode::promise_) {
                unsigned* promidx = reinterpret_cast<Immediate*>(cptr + 1);
                objs.push_back(c->getPromise(*promidx));
            }
            if (*cptr == Opcode::call_implicit_ ||
                *cptr == Opcode::named_call_implicit_) {
                uint32_t nargs = *reinterpret_cast<Immediate*>(cptr + 1);

                for (size_t i = 0, e = nargs; i != e; ++i) {
                    uint32_t offset = cur.callExtra().immediateCallArguments[i];
                    if (offset == MISSING_ARG_IDX || offset == DOTS_ARG_IDX)
                        continue;
                    objs.push_back(c->getPromise(offset));
                }
                if (*cptr == Opcode::named_call_implicit_) {
                    for (size_t i = 0, e = nargs; i != e; ++i) {
                        uint32_t offset = cur.callExtra().callArgumentNames[i];
                        if (offset) {
                            SEXP name = cp_pool_at(ctx, offset);
                            assert(TYPEOF(name) == SYMSXP ||
                                   name == R_NilValue);
                        }
                    }
                }
            }
            if (*cptr == Opcode::named_call_) {
                uint32_t nargs = *reinterpret_cast<Immediate*>(cptr + 1);
                for (size_t i = 0, e = nargs; i != e; ++i) {
                    uint32_t offset = cur.callExtra().callArgumentNames[i];
                    if (offset) {
                        SEXP name = cp_pool_at(ctx, offset);
                        assert(TYPEOF(name) == SYMSXP || name == R_NilValue);
                    }
                }
            }

            if ((cur.isJmp() && cur.immediate.offset < 0) || cur.isExit())
                sawReturnOrBackjump = true;
            else if (cur.bc != Opcode::nop_)
                sawReturnOrBackjump = false;

            cptr += cur.size();
            if (cptr == start + c->codeSize) {
                assert(sawReturnOrBackjump);
                break;
            }
        }
    }
}

} // namespace rir
