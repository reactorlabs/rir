#ifndef RJIT_RIR_BC_INC
#define RJIT_RIR_BC_INC

#include <cassert>
#include <cstddef>
#include <cstdint>
#include <cstring>

#include "R/r.h"
#include "common.h"

#include <array>
#include <vector>

#include "ir/RuntimeFeedback.h"

// type  for constant & ast pool indices
typedef uint32_t Immediate;

// type  signed immediate values (unboxed ints)
typedef uint32_t SignedImmediate;

// type of relative jump offset (all jumps are relative)
typedef int32_t JumpOffset;

typedef unsigned FunctionIndex;
typedef unsigned ArgumentsCount;

namespace rir {

/* ============================================================================
 * ==== Bytecode
 *
 * This file contains all definitions concerning Bytecodes.
 * Bytecodes are variable length, they all start with a Opcode type tag which is
 * followed by a variable sized immediate argument.
 *
 * The BC class contains constructor methods and all the infrastructure needed
 * to decode/write bytecodes to/from a byte stream.
 *
 * To add a bytecode:
 * 1. add it to Opcode
 * 2. add a constructor method to class BC
 * 3. add an entry to immediate_size in BC.h
 * 4. implement it (ie. resolve all switch statement warnings)
 */

// ============================================================
// ==== BC types
//

struct Code;

enum class Opcode : uint8_t {

#define DEF_INSTR(name, ...) name,
#include "insns.h"

    // A label/jump target (used internally by CodeEditor only!)
    label,

    num_of

};

// ============================================================
// ==== Creation and decoding of Bytecodes
//
// * Static factory functions to create an instance of a bytecode
//   which can be pushed onto a CodeStream
// * read and advance to read the next bytecode from an array

class CodeStream;
class BC {
  public:
    // ============================================================
    // ==== immediate argument types
    //
    // index into the constant pool
    typedef Immediate PoolIdx;
    // index into a functions array of code objects
    typedef Immediate FunIdx;
    typedef Immediate NumArgs;
    // index into arguments
    typedef Immediate ArgIdx;
    // jmp offset
    typedef int32_t Jmp;
    typedef Jmp Label;
    struct CallFixedArgs {
        NumArgs nargs;
        Immediate ast;
    };
    struct StaticCallFixedArgs {
        NumArgs nargs;
        Immediate ast;
        Immediate target;
    };
    struct GuardFunArgs {
        Immediate name;
        Immediate expected;
        Immediate id;
    };
    typedef Immediate Guard;
    typedef Immediate NumLocals;
    struct LocalsCopy {
        Immediate target;
        Immediate source;
    };

    static constexpr size_t MAX_NUM_ARGS = 1L << (8 * sizeof(PoolIdx));
    static constexpr size_t MAX_POOL_IDX = 1L << (8 * sizeof(PoolIdx));
    static constexpr size_t MAX_JMP = (1L << ((8 * sizeof(Jmp)) - 1)) - 1;
    static constexpr size_t MIN_JMP = -(1L << ((8 * sizeof(Jmp)) - 1));

    // This is only used internally in the BC handle objects
    // On the bytecode stream each immediate argument uses only the actual
    // space required.
    union ImmediateArguments {
        StaticCallFixedArgs staticCallFixedArgs;
        CallFixedArgs callFixedArgs;
        GuardFunArgs guard_fun_args;
        Guard guard_id;
        PoolIdx pool;
        FunIdx fun;
        ArgIdx arg_idx;
        Jmp offset;
        uint32_t i;
        NumLocals loc;
        LocalsCopy loc_cpy;
        CallFeedback callFeedback;
        TypeFeedback binopFeedback[2];
        ImmediateArguments() { memset(this, 0, sizeof(ImmediateArguments)); }
    };

    static Immediate readImmediate(Opcode** pc) {
        Immediate i = *(Immediate*)*pc;
        *pc = (Opcode*)((uintptr_t)*pc + sizeof(Immediate));
        return i;
    }

    Opcode bc;
    ImmediateArguments immediate;

    std::vector<ArgIdx> immediateCallArguments;
    std::vector<PoolIdx> callArgumentNames;

    BC() : bc(Opcode::invalid_) {}

    BC(Opcode* pc) {
        bc = *pc;
        pc++;
        immediate = decodeImmediateArguments(bc, pc);
        // Read implicit promise argument offsets
        if (bc == Opcode::call_implicit_ ||
            bc == Opcode::named_call_implicit_) {
            pc += sizeof(CallFixedArgs);
            for (size_t i = 0; i < immediate.callFixedArgs.nargs; ++i)
                immediateCallArguments.push_back(readImmediate(&pc));
        }
        // Read named arguments
        if (bc == Opcode::named_call_ || bc == Opcode::named_call_implicit_) {
            for (size_t i = 0; i < immediate.callFixedArgs.nargs; ++i)
                callArgumentNames.push_back(readImmediate(&pc));
        }
    }

    BC operator=(BC other) {
        bc = other.bc;
        immediate = other.immediate;
        return other;
    }

    bool is(Opcode aBc) { return bc == aBc; }

    inline size_t size() {
        // Those are the 3 variable length BC we have
        // call implicit has the promise offsets in the bc stream
        if (bc == Opcode::call_implicit_)
            return immediate.callFixedArgs.nargs * sizeof(FunIdx) +
                   fixedSize(bc);
        // named call has the names in the bc stream
        if (bc == Opcode::named_call_)
            return immediate.callFixedArgs.nargs * sizeof(FunIdx) +
                   fixedSize(bc);
        // named call implicit has both the promargs and names in the bc stream
        if (bc == Opcode::named_call_implicit_)
            return immediate.callFixedArgs.nargs * 2 * sizeof(FunIdx) +
                   fixedSize(bc);

        // the others have no variable length part
        return fixedSize(bc);
    }

    inline size_t popCount() {
        // return also is a leave
        assert(bc != Opcode::return_);
        if (bc == Opcode::call_)
            return immediate.callFixedArgs.nargs + 1;
        if (bc == Opcode::static_call_)
            return immediate.staticCallFixedArgs.nargs;
        return popCount(bc);
    }
    inline size_t pushCount() { return pushCount(bc); }

    // Used to serialize bc to CodeStream
    void write(CodeStream& cs) const;

    // Print it to the stream passed as argument
    void print(std::ostream& out) const;
    void printImmediateArgs(std::ostream& out) const;
    void printNames(std::ostream& out) const;
    void printProfile(std::ostream& out) const;

    // Accessors to load immediate constant from the pool
    SEXP immediateConst() const;

    inline static Opcode* jmpTarget(Opcode* pos) {
        BC bc = BC::decode(pos);
        assert(bc.isJmp());
        return (Opcode*)((uintptr_t)pos + bc.size() + bc.immediate.offset);
    }

    bool isCall() const {
        return bc == Opcode::call_implicit_ || bc == Opcode::call_ ||
               bc == Opcode::named_call_ ||
               bc == Opcode::named_call_implicit_ || bc == Opcode::static_call_;
    }

    bool hasPromargs() const {
        return bc == Opcode::call_implicit_ ||
               bc == Opcode::named_call_implicit_ || bc == Opcode::promise_ ||
               bc == Opcode::push_code_;
    }

    bool isCondJmp() const {
        return bc == Opcode::brtrue_ || bc == Opcode::brfalse_ ||
               bc == Opcode::brobj_ || bc == Opcode::beginloop_;
    }

    bool isUncondJmp() const { return bc == Opcode::br_; }

    bool isJmp() const { return isCondJmp() || isUncondJmp(); }

    bool isPure() { return isPure(bc); }

    bool isReturn() const {
        return bc == Opcode::ret_ || bc == Opcode::return_;
    }

    bool isLabel() const { return bc == Opcode::label; }

    bool isGuard() const {
        return bc == Opcode::guard_fun_ || bc == Opcode::guard_env_;
    }

    // This code performs the same as `BC::decode(pc).size()`, but for
    // performance reasons, it avoids actually creating the BC object.
    // This is important, as it is very performance critical.
    RIR_INLINE static unsigned size(rir::Opcode* pc) {
        auto bc = *pc;
        switch (bc) {
        // First handle the varlength BCs. In all three cases the number of
        // call arguments is the 2nd immediate argument and the
        // instructions have 2 fixed length immediates. After that there are
        // narg varlen immediates for the first two and 2*narg varlen
        // immediates in the last case.
        case Opcode::call_implicit_:
        case Opcode::named_call_: {
            pc++;
            Immediate nargs = *(Immediate*)pc;
            return 1 + (2 + nargs) * sizeof(Immediate);
        }
        case Opcode::named_call_implicit_: {
            pc++;
            Immediate nargs = *(Immediate*)pc;
            return 1 + (2 + 2 * nargs) * sizeof(Immediate);
        }
        default: {}
        }
        return fixedSize(bc);
    }

    RIR_INLINE static Opcode* next(rir::Opcode* pc) { return pc + size(pc); }

    // If the decoded BC is not needed, you should use next, since it is much
    // faster.
    inline static BC advance(Opcode** pc) __attribute__((warn_unused_result)) {
        BC cur(*pc);
        *pc = (Opcode*)((uintptr_t)(*pc) + cur.size());
        return cur;
    }

    inline static BC decode(Opcode* pc) {
        BC cur(pc);
        return cur;
    }

    // ==== Factory methods
    // to create new BC objects, which can be streamed to a CodeStream
    inline static BC nop();
    inline static BC makeEnv();
    inline static BC callerEnv();
    inline static BC getEnv();
    inline static BC setEnv();
    inline static BC push(SEXP constant);
    inline static BC push(double constant);
    inline static BC push(int constant);
    inline static BC push_code(FunIdx i);
    inline static BC ldfun(SEXP sym);
    inline static BC ldvar(SEXP sym);
    inline static BC ldvarNoForce(SEXP sym);
    inline static BC ldvarSuper(SEXP sym);
    inline static BC ldvarNoForceSuper(SEXP sym);
    inline static BC ldlval(SEXP sym);
    inline static BC ldddvar(SEXP sym);
    inline static BC ldarg(uint32_t offset);
    inline static BC ldloc(uint32_t offset);
    inline static BC stloc(uint32_t offset);
    inline static BC copyloc(uint32_t target, uint32_t source);
    inline static BC promise(FunIdx prom);
    inline static BC ret();
    inline static BC pop();
    inline static BC force();
    inline static BC asast();
    inline static BC stvar(SEXP sym);
    inline static BC stvarSuper(SEXP sym);
    inline static BC missing(SEXP sym);
    inline static BC checkMissing();
    inline static BC subassign1();
    inline static BC subassign2(SEXP sym);
    inline static BC length();
    inline static BC names();
    inline static BC setNames();
    inline static BC alloc(int type);
    inline static BC asbool();
    inline static BC beginloop(Jmp);
    inline static BC endcontext();
    inline static BC brtrue(Jmp);
    inline static BC brfalse(Jmp);
    inline static BC br(Jmp);
    inline static BC brobj(Jmp);
    inline static BC label(Jmp);
    inline static BC dup();
    inline static BC dup2();
    inline static BC forSeqSize();
    inline static BC inc();
    inline static BC close();
    inline static BC add();
    inline static BC mul();
    inline static BC div();
    inline static BC pow();
    inline static BC idiv();
    inline static BC mod();
    inline static BC sub();
    inline static BC uplus();
    inline static BC uminus();
    inline static BC Not();
    inline static BC lt();
    inline static BC gt();
    inline static BC le();
    inline static BC ge();
    inline static BC eq();
    inline static BC identical();
    inline static BC ne();
    inline static BC seq();
    inline static BC colon();
    inline static BC makeUnique();
    inline static BC setShared();
    inline static BC asLogical();
    inline static BC lglOr();
    inline static BC lglAnd();
    inline static BC guardName(SEXP, SEXP);
    inline static BC guardNamePrimitive(SEXP);
    inline static BC guardEnv(uint32_t id);
    inline static BC isfun();
    inline static BC invisible();
    inline static BC visible();
    inline static BC extract1_1();
    inline static BC extract1_2();
    inline static BC extract2_1();
    inline static BC extract2_2();
    inline static BC swap();
    inline static BC put(uint32_t);
    inline static BC pick(uint32_t);
    inline static BC pull(uint32_t);
    inline static BC is(uint32_t);
    inline static BC isObj();
    inline static BC return_();
    inline static BC int3();
    inline static BC callImplicit(const std::vector<FunIdx>& args, SEXP ast);
    inline static BC callImplicit(const std::vector<FunIdx>& args,
                                  const std::vector<SEXP>& names, SEXP ast);
    inline static BC call(size_t nargs, SEXP ast);
    inline static BC call(size_t nargs, const std::vector<SEXP>& names,
                          SEXP ast);
    inline static BC staticCall(size_t nargs, SEXP ast, SEXP target);
    inline static BC recordCall();
    inline static BC recordBinop();

  private:
    explicit BC(Opcode bc) : bc(bc) {}
    BC(Opcode bc, ImmediateArguments immediate)
        : bc(bc), immediate(immediate) {}
    BC(Opcode bc, ImmediateArguments immediate, const std::vector<FunIdx>& args,
       const std::vector<PoolIdx>& names)
        : bc(bc), immediate(immediate), immediateCallArguments(args),
          callArgumentNames(names) {}

    static unsigned RIR_INLINE fixedSize(Opcode bc) {
        switch (bc) {
#define DEF_INSTR(name, imm, opop, opush, pure)                                \
    case Opcode::name:                                                         \
        return imm * sizeof(Immediate) + 1;
#include "insns.h"
        default:
            return 0;
        }
    }

    static char const* name(Opcode bc) {
        switch (bc) {
#define DEF_INSTR(name, imm, opop, opush, pure)                                \
    case Opcode::name:                                                         \
        return #name;
#include "insns.h"
        default:
            return "???";
        }
    }

    static unsigned pushCount(Opcode bc) {
        switch (bc) {
#define DEF_INSTR(name, imm, opop, opush, pure)                                \
    case Opcode::name:                                                         \
        return opush;
#include "insns.h"
        default:
            assert(false);
            return 0;
        }
    }

    static unsigned popCount(Opcode bc) {
        switch (bc) {
#define DEF_INSTR(name, imm, opop, opush, pure)                                \
    case Opcode::name:                                                         \
        assert(opop != -1);                                                    \
        return opop;
#include "insns.h"
        default:
            assert(false);
            return 0;
        }
    }

    static unsigned isPure(Opcode bc) {
        switch (bc) {
#define DEF_INSTR(name, imm, opop, opush, pure)                                \
    case Opcode::name:                                                         \
        return pure;
#include "insns.h"
        case Opcode::label:
            return false;
        default:
            assert(false);
            return 0;
        }
    }

    inline static ImmediateArguments decodeImmediateArguments(Opcode bc,
                                                              Opcode* pc) {
        ImmediateArguments immediate;
        switch (bc) {
        case Opcode::push_:
        case Opcode::ldfun_:
        case Opcode::ldvar_:
        case Opcode::ldvar_noforce_:
        case Opcode::ldvar_super_:
        case Opcode::ldvar_noforce_super_:
        case Opcode::ldlval_:
        case Opcode::ldddvar_:
        case Opcode::stvar_:
        case Opcode::stvar_super_:
        case Opcode::missing_:
        case Opcode::subassign2_:
            immediate.pool = *(PoolIdx*)pc;
            break;
        case Opcode::call_implicit_:
        case Opcode::named_call_implicit_:
        case Opcode::call_:
        case Opcode::named_call_:
            immediate.callFixedArgs = *(CallFixedArgs*)pc;
            break;
        case Opcode::static_call_:
            immediate.staticCallFixedArgs = *(StaticCallFixedArgs*)pc;
            break;
        case Opcode::guard_env_:
            immediate.guard_id = *(uint32_t*)pc;
            break;
        case Opcode::guard_fun_:
            immediate.guard_fun_args = *(GuardFunArgs*)pc;
            break;
        case Opcode::promise_:
        case Opcode::push_code_:
            immediate.fun = *(FunIdx*)pc;
            break;
        case Opcode::br_:
        case Opcode::brtrue_:
        case Opcode::brobj_:
        case Opcode::brfalse_:
        case Opcode::label:
        case Opcode::beginloop_:
            immediate.offset = *(Jmp*)pc;
            break;
        case Opcode::pick_:
        case Opcode::pull_:
        case Opcode::is_:
        case Opcode::put_:
        case Opcode::alloc_:
            immediate.i = *(uint32_t*)pc;
            break;
        case Opcode::ldarg_:
            immediate.arg_idx = *(ArgIdx*)pc;
            break;
        case Opcode::ldloc_:
        case Opcode::stloc_:
            immediate.loc = *(NumLocals*)pc;
            break;
        case Opcode::movloc_:
            immediate.loc_cpy = *(LocalsCopy*)pc;
            break;
        case Opcode::record_call_:
            immediate.callFeedback = *(CallFeedback*)pc;
            break;
        case Opcode::record_binop_:
            immediate.binopFeedback[0] = *((TypeFeedback*)pc);
            immediate.binopFeedback[1] = *((TypeFeedback*)pc + 1);
            break;
        case Opcode::nop_:
        case Opcode::make_env_:
        case Opcode::get_env_:
        case Opcode::caller_env_:
        case Opcode::set_env_:
        case Opcode::for_seq_size_:
        case Opcode::extract1_1_:
        case Opcode::extract2_1_:
        case Opcode::extract1_2_:
        case Opcode::extract2_2_:
        case Opcode::close_:
        case Opcode::ret_:
        case Opcode::pop_:
        case Opcode::force_:
        case Opcode::asast_:
        case Opcode::asbool_:
        case Opcode::dup_:
        case Opcode::dup2_:
        case Opcode::swap_:
        case Opcode::int3_:
        case Opcode::make_unique_:
        case Opcode::set_shared_:
        case Opcode::aslogical_:
        case Opcode::lgl_and_:
        case Opcode::lgl_or_:
        case Opcode::inc_:
        case Opcode::add_:
        case Opcode::mul_:
        case Opcode::div_:
        case Opcode::idiv_:
        case Opcode::mod_:
        case Opcode::pow_:
        case Opcode::seq_:
        case Opcode::colon_:
        case Opcode::sub_:
        case Opcode::uplus_:
        case Opcode::uminus_:
        case Opcode::not_:
        case Opcode::lt_:
        case Opcode::gt_:
        case Opcode::le_:
        case Opcode::ge_:
        case Opcode::eq_:
        case Opcode::identical_:
        case Opcode::ne_:
        case Opcode::return_:
        case Opcode::isfun_:
        case Opcode::invisible_:
        case Opcode::visible_:
        case Opcode::endcontext_:
        case Opcode::subassign1_:
        case Opcode::length_:
        case Opcode::names_:
        case Opcode::set_names_:
        case Opcode::isobj_:
        case Opcode::check_missing_:
            break;
        case Opcode::invalid_:
        case Opcode::num_of:
            assert(false);
            break;
        }
        return immediate;
    }
};

} // rir

#endif
