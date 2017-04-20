#ifndef RJIT_RIR_BC_INC
#define RJIT_RIR_BC_INC

#include <cstddef>
#include <cstdint>
#include <cassert>
#include <cstring>

#include "R/r.h"

#include <vector>

#include "interpreter/interp_data.h"

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
enum class Opcode : OpcodeT {

#define DEF_INSTR(name, ...) name,
#include "insns.h"

    // A label/jump target (used internally by CodeEditor only!)
    label,

    num_of

};

// TODO the immediate argument types should follow the C immediate types

// ============================================================
// ==== immediate argument types
//
#pragma pack(push)
#pragma pack(0)

// index into the constant pool
typedef uint32_t PoolIdxT;
// index into a functions array of code objects
typedef uint32_t FunIdxT;
// number of arguments
typedef uint32_t NumArgsT;
// jmp offset
typedef int32_t JmpT;
typedef JmpT LabelT;
typedef struct {
    uint32_t call_id;
    uint32_t nargs;
} CallArgs;
typedef struct {
    uint32_t name;
    uint32_t expected;
    uint32_t id;
} GuardFunArgs;
typedef uint32_t GuardT;
#pragma pack(pop)

static constexpr size_t MAX_NUM_ARGS = 1L << (8 * sizeof(PoolIdxT));
static constexpr size_t MAX_POOL_IDX = 1L << (8 * sizeof(PoolIdxT));
static constexpr size_t MAX_JMP = (1L << ((8 * sizeof(JmpT)) - 1)) - 1;
static constexpr size_t MIN_JMP = -(1L << ((8 * sizeof(JmpT)) - 1));

// ============================================================
// ==== Creation and decoding of Bytecodes
//
// * Static factory functions to create an instance of a bytecode
//   which can be pushed onto a CodeStream
// * read and advance to read the next bytecode from an array

class CallSite;
class CodeStream;
class BC {
  public:
    // This is only used internally in the BC handle objects
    // On the bytecode stream each immediate argument uses only the actual
    // space required.
    union ImmediateT {
        CallArgs call_args;
        GuardFunArgs guard_fun_args;
        GuardT guard_id;
        PoolIdxT pool;
        FunIdxT fun;
        NumArgsT arg_idx;
        JmpT offset;
        uint32_t i;
    };

    BC() : bc(Opcode::invalid_), immediate({{0}}) {}
    BC operator=(BC other) {
        bc = other.bc;
        immediate = other.immediate;
        return other;
    }

    bool operator==(const BC& other) const;

    bool is(Opcode aBc) { return bc == aBc; }

    Opcode bc;
    ImmediateT immediate;

    inline size_t size() { return size(bc); }
    inline size_t popCount() {
        // return also is a leave
        assert(bc != Opcode::return_);
        if (bc == Opcode::call_stack_)
            return immediate.call_args.nargs + 1;
        if (bc == Opcode::static_call_stack_ || bc == Opcode::dispatch_stack_)
            return immediate.call_args.nargs;
        return popCount(bc);
    }
    inline size_t pushCount() { return pushCount(bc); }

    // Used to serialize bc to CodeStream
    void write(CodeStream& cs) const;

    // Print it to stdout
    void print();
    void print(CallSite cs);
    void printArgs(CallSite cs);
    void printNames(CallSite cs);
    void printProfile(CallSite cs);

    // Accessors to load immediate constant from the pool
    SEXP immediateConst();

    // Return the callsite of this BC, needs the cassSites buffer as input
    CallSite callSite(Code* code);

    inline static Opcode* jmpTarget(Opcode* pos) {
        BC bc = BC::decode(pos);
        assert(bc.isJmp());
        return (Opcode*)((uintptr_t)pos + bc.size() + bc.immediate.offset);
    }

    bool isCallsite() {
        return bc == Opcode::call_ || bc == Opcode::dispatch_ ||
               bc == Opcode::call_stack_ || bc == Opcode::dispatch_stack_ ||
               bc == Opcode::static_call_stack_;
    }

    bool hasPromargs() {
        return bc == Opcode::call_ || bc == Opcode::dispatch_ ||
               bc == Opcode::promise_ || bc == Opcode::push_code_;
    }

    bool isJmp() {
        return bc == Opcode::br_ || bc == Opcode::brtrue_ ||
               bc == Opcode::brfalse_ || bc == Opcode::brobj_ ||
               bc == Opcode::beginloop_;
    }

    bool isPure() { return isPure(bc); }

    bool isReturn() { return bc == Opcode::ret_ || bc == Opcode::return_; }

    bool isGuard() {
        return bc == Opcode::guard_fun_ || bc == Opcode::guard_env_;
    }

    // ==== BC decoding logic
    inline static BC advance(Opcode** pc);
    inline static BC decode(Opcode* pc);

    // ==== Factory methods
    // to create new BC objects, which can be streamed to a CodeStream
    inline static BC nop();
    inline static BC push(SEXP constant);
    inline static BC push(double constant);
    inline static BC push(int constant);
    inline static BC push_code(FunIdxT i);
    inline static BC ldfun(SEXP sym);
    inline static BC ldvar(SEXP sym);
    inline static BC ldvar2(SEXP sym);
    inline static BC ldlval(SEXP sym);
    inline static BC ldarg(SEXP sym);
    inline static BC ldddvar(SEXP sym);
    inline static BC promise(FunIdxT prom);
    inline static BC ret();
    inline static BC pop();
    inline static BC force();
    inline static BC asast();
    inline static BC stvar(SEXP sym);
    inline static BC stvar2(SEXP sym);
    inline static BC missing(SEXP sym);
    inline static BC subassign();
    inline static BC subassign2(SEXP sym);
    inline static BC length();
    inline static BC names();
    inline static BC setNames();
    inline static BC alloc(int type);
    inline static BC asbool();
    inline static BC beginloop(JmpT);
    inline static BC endcontext();
    inline static BC brtrue(JmpT);
    inline static BC brfalse(JmpT);
    inline static BC br(JmpT);
    inline static BC brobj(JmpT);
    inline static BC label(JmpT);
    inline static BC dup();
    inline static BC dup2();
    inline static BC testBounds();
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
    inline static BC extract1();
    inline static BC subset1();
    inline static BC extract2();
    inline static BC subset2();
    inline static BC swap();
    inline static BC put(uint32_t);
    inline static BC pick(uint32_t);
    inline static BC pull(uint32_t);
    inline static BC is(uint32_t);
    inline static BC return_();
    inline static BC int3();

  private:
    explicit BC(Opcode bc) : bc(bc), immediate({{0}}) {}
    BC(Opcode bc, ImmediateT immediate) : bc(bc), immediate(immediate) {}

    static unsigned size(Opcode bc) {
        switch (bc) {
#define DEF_INSTR(name, imm, opop, opush, pure)                                \
    case Opcode::name:                                                         \
        return imm * sizeof(ArgT) + 1;
#include "insns.h"
        default:
            return 0;
        }
    }

    static unsigned immCount(Opcode bc) {
        switch (bc) {
#define DEF_INSTR(name, imm, opop, opush, pure)                                \
    case Opcode::name:                                                         \
        return imm;
#include "insns.h"
        default:
            assert(false);
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

    friend class CodeEditor;
    friend class CodeStream;
};

class CallSite {
  public:
    BC bc;
    CallSiteStruct* cs = nullptr;

    CallSite() {}
    CallSite(BC bc, CallSiteStruct* cs);

    bool isValid() { return cs != nullptr; }
    NumArgsT nargs() { return cs->nargs; }
    SEXP call();
    const FunIdxT* args() { return CallSite_args(cs); }
    FunIdxT arg(NumArgsT idx) { return CallSite_args(cs)[idx]; }
    bool hasNames() { return cs->hasNames; }
    bool hasTarget() { return cs->hasTarget; }
    bool hasProfile() { return cs->hasProfile; }
    bool hasImmediateArgs() { return cs->hasImmediateArgs; }
    SEXP selector();
    SEXP name(NumArgsT idx);
    SEXP target();

    CallSiteProfile* profile() { return CallSite_profile(cs); }
};

} // rir

#endif
