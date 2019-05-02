#ifndef RJIT_RIR_BC_INC
#define RJIT_RIR_BC_INC

#include <cassert>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <memory>

#include "R/r.h"
#include "common.h"

#include <array>
#include <vector>

#include "runtime/Assumptions.h"
#include "runtime/TypeFeedback.h"

#include "BC_noarg_list.h"

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

enum class Opcode : uint8_t {

#define DEF_INSTR(name, ...) name,
#include "insns.h"

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
        Assumptions given;
    };
    struct StaticCallFixedArgs {
        NumArgs nargs;
        Immediate ast;
        Assumptions given;
        Immediate targetClosure;
        Immediate versionHint;
    };
    struct CallBuiltinFixedArgs {
        NumArgs nargs;
        Immediate ast;
        Immediate builtin;
    };
    struct GuardFunArgs {
        Immediate name;
        Immediate expected;
        Immediate id;
    };
    typedef Immediate NumLocals;
    struct LocalsCopy {
        Immediate target;
        Immediate source;
    };
    struct MkEnvFixedArgs {
        NumArgs nargs;
        SignedImmediate context;
    };

    static constexpr size_t MAX_NUM_ARGS = 1L << (8 * sizeof(PoolIdx));
    static constexpr size_t MAX_POOL_IDX = 1L << (8 * sizeof(PoolIdx));
    static constexpr size_t MAX_JMP = (1L << ((8 * sizeof(Jmp)) - 1)) - 1;
    static constexpr size_t MIN_JMP = -(1L << ((8 * sizeof(Jmp)) - 1));

    // This is only used internally in the BC handle objects
    // On the bytecode stream each immediate argument uses only the actual
    // space required.
    union ImmediateArguments {
        MkEnvFixedArgs mkEnvFixedArgs;
        StaticCallFixedArgs staticCallFixedArgs;
        CallFixedArgs callFixedArgs;
        CallBuiltinFixedArgs callBuiltinFixedArgs;
        GuardFunArgs guard_fun_args;
        PoolIdx pool;
        FunIdx fun;
        ArgIdx arg_idx;
        Jmp offset;
        uint32_t i;
        NumLocals loc;
        LocalsCopy loc_cpy;
        ObservedCallees callFeedback;
        ObservedValues typeFeedback;
        ImmediateArguments() { memset(this, 0, sizeof(ImmediateArguments)); }
    };

    static Immediate readImmediate(Opcode** pc) {
        Immediate i;
        memcpy(&i, *pc, sizeof(Immediate));
        *pc = (Opcode*)((uintptr_t)*pc + sizeof(Immediate));
        return i;
    }

    Opcode bc;
    ImmediateArguments immediate;

    BC() : bc(Opcode::invalid_) {}

    BC(const BC& other) = delete;
    BC& operator=(const BC& other) = delete;

    BC(BC&& other)
        : bc(other.bc), immediate(other.immediate),
          extraInformation(std::move(other.extraInformation)) {}
    BC& operator=(BC&& other) {
        bc = other.bc;
        immediate = std::move(other.immediate);
        extraInformation = std::move(other.extraInformation);
        return *this;
    }

    bool is(Opcode aBc) const { return bc == aBc; }

    inline size_t size() const {
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

        if (bc == Opcode::mk_env_ || bc == Opcode::mk_stub_env_)
            return immediate.mkEnvFixedArgs.nargs * sizeof(FunIdx) +
                   fixedSize(bc);

        // the others have no variable length part
        return fixedSize(bc);
    }

    inline size_t popCount() {
        // return also is a leave
        assert(bc != Opcode::return_);
        if (bc == Opcode::call_ || bc == Opcode::named_call_)
            return immediate.callFixedArgs.nargs + 1;
        if (bc == Opcode::static_call_)
            return immediate.staticCallFixedArgs.nargs;
        if (bc == Opcode::call_builtin_)
            return immediate.callBuiltinFixedArgs.nargs;
        if (bc == Opcode::mk_env_ || bc == Opcode::mk_stub_env_)
            return immediate.mkEnvFixedArgs.nargs + 1;
        if (bc == Opcode::popn_)
            return immediate.i;
        return popCount(bc);
    }
    inline size_t pushCount() { return pushCount(bc); }

    // Used to serialize bc to CodeStream
    void write(CodeStream& cs) const;

    static void deserialize(SEXP refTable, R_inpstream_t inp, Opcode* code,
                            size_t codeSize);
    static void serialize(SEXP refTable, R_outpstream_t out, const Opcode* code,
                          size_t codeSize);

    // Print it to the stream passed as argument
    void print(std::ostream& out) const;
    void printImmediateArgs(std::ostream& out) const;
    void printNames(std::ostream& out, const std::vector<PoolIdx>&) const;
    void printProfile(std::ostream& out) const;
    void printOpcode(std::ostream& out) const;

    // Accessors to load immediate constant from the pool
    SEXP immediateConst() const;

    inline static Opcode* jmpTarget(Opcode* pos) {
        BC bc = decodeShallow(pos);
        assert(bc.isJmp());
        return (Opcode*)((uintptr_t)pos + bc.size() + bc.immediate.offset);
    }

    bool isCall() const {
        return bc == Opcode::call_implicit_ || bc == Opcode::call_ ||
               bc == Opcode::named_call_ ||
               bc == Opcode::named_call_implicit_ ||
               bc == Opcode::static_call_ || bc == Opcode::call_builtin_;
    }

    bool hasPromargs() const {
        return bc == Opcode::call_implicit_ ||
               bc == Opcode::named_call_implicit_ || bc == Opcode::promise_ ||
               bc == Opcode::push_code_;
    }

    void addMyPromArgsTo(std::vector<FunIdx>& proms) {
        switch (bc) {
        case Opcode::push_code_:
        case Opcode::promise_:
            proms.push_back(immediate.arg_idx);
            break;
        case Opcode::named_call_implicit_:
        case Opcode::call_implicit_: {
            auto& in = callExtra().immediateCallArguments;
            std::copy(in.begin(), in.end(), std::back_inserter(proms));
            break;
        }
        default: {}
        }
    }

    bool isCondJmp() const {
        return bc == Opcode::brtrue_ || bc == Opcode::brfalse_ ||
               bc == Opcode::brobj_ || bc == Opcode::beginloop_;
    }

    bool isUncondJmp() const { return bc == Opcode::br_; }

    bool isJmp() const { return isCondJmp() || isUncondJmp(); }

    bool isPure() { return isPure(bc); }

    bool isExit() const {
        return bc == Opcode::ret_ || bc == Opcode::return_ ||
               bc == Opcode::deopt_;
    }

    // This code performs the same as `BC::decode(pc).size()`, but for
    // performance reasons, it avoids actually creating the BC object.
    // This is important, as it is very performance critical.
    RIR_INLINE static unsigned size(rir::Opcode* pc) {
        auto bc = *pc;
        switch (bc) {
        // First handle the varlength BCs. In all three cases the number of
        // call arguments is the 2nd immediate argument and the
        // instructions have 4 fixed length immediates. After that there are
        // narg varlen immediates for the first two and 2*narg varlen
        // immediates in the last case.
        case Opcode::call_implicit_:
        case Opcode::named_call_: {
            pc++;
            Immediate nargs;
            memcpy(&nargs, pc, sizeof(Immediate));
            return 1 + (4 + nargs) * sizeof(Immediate);
        }
        case Opcode::named_call_implicit_: {
            pc++;
            Immediate nargs;
            memcpy(&nargs, pc, sizeof(Immediate));
            return 1 + (4 + 2 * nargs) * sizeof(Immediate);
        }
        case Opcode::mk_stub_env_:
        case Opcode::mk_env_: {
            pc++;
            Immediate nargs;
            memcpy(&nargs, pc, sizeof(Immediate));
            return 1 + (2 + nargs) * sizeof(Immediate);
        }
        default: {}
        }
        return fixedSize(bc);
    }

    RIR_INLINE static Opcode* next(rir::Opcode* pc) { return pc + size(pc); }

    // If the decoded BC is not needed, you should use next, since it is much
    // faster.
    inline static BC advance(Opcode** pc, Code* code)
        __attribute__((warn_unused_result)) {
        BC cur = decode(*pc, code);
        *pc = (Opcode*)((uintptr_t)(*pc) + cur.size());
        return cur;
    }

    // ==== Factory methods
    // to create new BC objects, which can be streamed to a CodeStream
#define V(NESTED, name, name_) inline static BC name();
BC_NOARGS(V, _)
#undef V
    inline static BC recordCall();
    inline static BC recordBinop();
    inline static BC recordType();
    inline static BC popn(unsigned n);
    inline static BC push(SEXP constant);
    inline static BC push(double constant);
    inline static BC push(int constant);
    inline static BC push_from_pool(PoolIdx idx);
    inline static BC push_code(FunIdx i);
    inline static BC ldfun(SEXP sym);
    inline static BC ldvar(SEXP sym);
    inline static BC ldvarForUpdate(SEXP sym);
    inline static BC ldvarNoForce(SEXP sym);
    inline static BC ldvarSuper(SEXP sym);
    inline static BC ldvarNoForceSuper(SEXP sym);
    inline static BC ldddvar(SEXP sym);
    inline static BC ldarg(uint32_t offset);
    inline static BC ldloc(uint32_t offset);
    inline static BC stloc(uint32_t offset);
    inline static BC copyloc(uint32_t target, uint32_t source);
    inline static BC promise(FunIdx prom);
    inline static BC stvar(SEXP sym);
    inline static BC starg(SEXP sym);
    inline static BC stvarSuper(SEXP sym);
    inline static BC missing(SEXP sym);
    inline static BC alloc(int type);
    inline static BC asint(bool ceil);
    inline static BC pushContext(Jmp);
    inline static BC beginloop(Jmp);
    inline static BC brtrue(Jmp);
    inline static BC brfalse(Jmp);
    inline static BC br(Jmp);
    inline static BC brobj(Jmp);
    inline static BC label(Jmp);
    inline static BC guardName(SEXP, SEXP);
    inline static BC guardNamePrimitive(SEXP);
    inline static BC put(uint32_t);
    inline static BC pick(uint32_t);
    inline static BC pull(uint32_t);
    inline static BC is(uint32_t);
    inline static BC is(TypeChecks);
    inline static BC deopt(SEXP);
    inline static BC callImplicit(const std::vector<FunIdx>& args, SEXP ast,
                                  const Assumptions& given);
    inline static BC callImplicit(const std::vector<FunIdx>& args,
                                  const std::vector<SEXP>& names, SEXP ast,
                                  const Assumptions& given);
    inline static BC call(size_t nargs, SEXP ast, const Assumptions& given);
    inline static BC call(size_t nargs, const std::vector<SEXP>& names,
                          SEXP ast, const Assumptions& given);
    inline static BC staticCall(size_t nargs, SEXP ast, SEXP targetClosure,
                                SEXP targetVersion, const Assumptions& given);
    inline static BC callBuiltin(size_t nargs, SEXP ast, SEXP target);

    inline static BC mkEnv(const std::vector<SEXP>& names,
                           SignedImmediate contextPos, bool stub);

    inline static BC decode(Opcode* pc, const Code* code) {
        BC cur;
        cur.decodeFixlen(pc);
        cur.decodeExtraInformation(pc, code);
        return cur;
    }

    inline static BC decodeShallow(Opcode* pc) {
        BC cur;
        cur.decodeFixlen(pc);
        return cur;
    }

  private:
    // Some Bytecodes need extra information. For example in the case of the
    // call feedback bytecode, the recorded call targets are in the code
    // objects extra pool. Or for the variable length call bytecodes we need a
    // vector to store arguments. For those bytecodes we allocate an extra
    // information struct to hold those things.
    struct ExtraInformation {};
    struct CallInstructionExtraInformation : public ExtraInformation {
        std::vector<BC::ArgIdx> immediateCallArguments;
        std::vector<BC::PoolIdx> callArgumentNames;
    };
    struct CallFeedbackExtraInformation : public ExtraInformation {
        std::vector<SEXP> targets;
    };
    struct MkEnvExtraInformation : public ExtraInformation {
        std::vector<BC::PoolIdx> names;
    };
    std::unique_ptr<ExtraInformation> extraInformation = nullptr;

  public:
    MkEnvExtraInformation& mkEnvExtra() const {
        assert((bc == Opcode::mk_env_ || bc == Opcode::mk_stub_env_) &&
               "Not a varlen call instruction");
        assert(extraInformation.get() &&
               "missing extra information. created through decodeShallow?");
        return *static_cast<MkEnvExtraInformation*>(extraInformation.get());
    }

    CallInstructionExtraInformation& callExtra() const {
        switch (bc) {
        case Opcode::call_implicit_:
        case Opcode::named_call_implicit_:
        case Opcode::named_call_:
            break;
        default:
            assert(false && "Not a varlen call instruction");
        }
        assert(extraInformation.get() &&
               "missing extra information. created through decodeShallow?");
        return *static_cast<CallInstructionExtraInformation*>(
            extraInformation.get());
    }

    CallFeedbackExtraInformation& callFeedbackExtra() const {
        assert(bc == Opcode::record_call_ && "not a record call instruction");
        assert(extraInformation.get() &&
               "missing extra information. created through decodeShallow?");
        return *static_cast<CallFeedbackExtraInformation*>(
            extraInformation.get());
    }

  private:
    void allocExtraInformation() {
        assert(extraInformation == nullptr);

        switch (bc) {
        case Opcode::invalid_:
            assert(false && "run decodeFixlen first!");
            break;

        case Opcode::call_implicit_:
        case Opcode::named_call_implicit_:
        case Opcode::named_call_: {
            extraInformation.reset(new CallInstructionExtraInformation);
            break;
        }
        case Opcode::record_call_: {
            extraInformation.reset(new CallFeedbackExtraInformation);
            break;
        }
        case Opcode::mk_stub_env_:
        case Opcode::mk_env_: {
            extraInformation.reset(new MkEnvExtraInformation);
            break;
        }
        default: {}
        }
    }

    void decodeExtraInformation(Opcode* pc, const Code* code) {
        allocExtraInformation();
        pc++; // skip bc

        switch (bc) {
        case Opcode::call_implicit_:
        case Opcode::named_call_implicit_:
        case Opcode::named_call_: {
            pc += sizeof(CallFixedArgs);

            // Read implicit promise argument offsets
            if (bc == Opcode::call_implicit_ ||
                bc == Opcode::named_call_implicit_) {
                for (size_t i = 0; i < immediate.callFixedArgs.nargs; ++i)
                    callExtra().immediateCallArguments.push_back(
                        readImmediate(&pc));
            }
            // Read named arguments
            if (bc == Opcode::named_call_ ||
                bc == Opcode::named_call_implicit_) {
                for (size_t i = 0; i < immediate.callFixedArgs.nargs; ++i)
                    callExtra().callArgumentNames.push_back(readImmediate(&pc));
            }

            break;
        }
        case Opcode::mk_stub_env_:
        case Opcode::mk_env_: {
            pc += sizeof(MkEnvFixedArgs);
            for (size_t i = 0; i < immediate.mkEnvFixedArgs.nargs; ++i)
                mkEnvExtra().names.push_back(readImmediate(&pc));
            break;
        }

        case Opcode::record_call_: {
            // Read call target feedback from the extra pool
            for (size_t i = 0; i < immediate.callFeedback.numTargets; ++i)
                callFeedbackExtra().targets.push_back(
                    immediate.callFeedback.getTarget(code, i));
            break;
        }
        default: {}
        }
    }

    inline void decodeFixlen(Opcode* pc) {
        bc = *pc;
        pc++;
        immediate = decodeImmediateArguments(bc, pc);
    }

    // Those two should stay private. If you want to decode a BC use BC::decode
    // instead. To create BC's use the static builder methods.
    explicit BC(Opcode bc) : bc(bc) { allocExtraInformation(); }
    BC(Opcode bc, const ImmediateArguments& i) : bc(bc), immediate(i) {
        allocExtraInformation();
    }

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
        default:
            assert(false);
            return 0;
        }
    }

    inline static ImmediateArguments decodeImmediateArguments(Opcode bc,
                                                              Opcode* pc) {
        ImmediateArguments immediate;
        switch (bc) {
        case Opcode::deopt_:
        case Opcode::push_:
        case Opcode::ldfun_:
        case Opcode::ldvar_:
        case Opcode::ldvar_for_update_:
        case Opcode::ldvar_noforce_:
        case Opcode::ldvar_super_:
        case Opcode::ldvar_noforce_super_:
        case Opcode::ldddvar_:
        case Opcode::stvar_:
        case Opcode::starg_:
        case Opcode::stvar_super_:
        case Opcode::missing_:
            memcpy(&immediate.pool, pc, sizeof(PoolIdx));
            break;
        case Opcode::call_implicit_:
        case Opcode::named_call_implicit_:
        case Opcode::call_:
        case Opcode::named_call_:
            memcpy(&immediate.callFixedArgs,
                   reinterpret_cast<CallFixedArgs*>(pc), sizeof(CallFixedArgs));
            break;
        case Opcode::mk_stub_env_:
        case Opcode::mk_env_:
            memcpy(&immediate.mkEnvFixedArgs, pc, sizeof(MkEnvFixedArgs));
            break;
        case Opcode::call_builtin_:
            memcpy(&immediate.callBuiltinFixedArgs, pc,
                   sizeof(CallBuiltinFixedArgs));
            break;
        case Opcode::static_call_:
            memcpy(&immediate.staticCallFixedArgs, pc,
                   sizeof(StaticCallFixedArgs));
            break;
        case Opcode::guard_fun_:
            memcpy(&immediate.guard_fun_args, pc, sizeof(GuardFunArgs));
            break;
        case Opcode::promise_:
        case Opcode::push_code_:
            memcpy(&immediate.fun, pc, sizeof(FunIdx));
            break;
        case Opcode::br_:
        case Opcode::brtrue_:
        case Opcode::brobj_:
        case Opcode::brfalse_:
        case Opcode::beginloop_:
        case Opcode::push_context_:
            memcpy(&immediate.offset, pc, sizeof(Jmp));
            break;
        case Opcode::popn_:
        case Opcode::pick_:
        case Opcode::pull_:
        case Opcode::is_:
        case Opcode::put_:
        case Opcode::alloc_:
            memcpy(&immediate.i, pc, sizeof(uint32_t));
            break;
        case Opcode::ldarg_:
            memcpy(&immediate.arg_idx, pc, sizeof(ArgIdx));
            break;
        case Opcode::ldloc_:
        case Opcode::stloc_:
            memcpy(&immediate.loc, pc, sizeof(NumLocals));
            break;
        case Opcode::movloc_:
            memcpy(&immediate.loc_cpy, pc, sizeof(LocalsCopy));
            break;
        case Opcode::record_call_:
            memcpy(&immediate.callFeedback, pc, sizeof(ObservedCallees));
            break;
        case Opcode::record_type_:
            memcpy(&immediate.typeFeedback, pc, sizeof(ObservedValues));
            break;
#define V(NESTED, name, name_) case Opcode::name_##_:
BC_NOARGS(V, _)
#undef V
            break;
        case Opcode::invalid_:
        case Opcode::num_of:
            assert(false);
            break;
        }
        return immediate;
    }

    friend class CodeVerifier;
};

} // namespace rir

#endif
