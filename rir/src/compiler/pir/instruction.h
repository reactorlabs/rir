#ifndef COMPILER_INSTRUCTION_H
#define COMPILER_INSTRUCTION_H

#include "R/r.h"
#include "env.h"
#include "instruction_list.h"
#include "ir/BC_inc.h"
#include "ir/Deoptimization.h"
#include "pir.h"
#include "singleton_values.h"
#include "tag.h"
#include "value.h"

#include <array>
#include <cassert>
#include <cstdint>
#include <deque>
#include <functional>
#include <iostream>

/*
 * This file provides implementations for all instructions
 *
 * The list of all instructions can be found in instruction_list.h
 *
 * Instructions are either FixedLength or VariableLength.
 *
 * Every instruction is also a Value, and can therefore be used as an argument
 * for other instructions.
 *
 * Instructions have an InstructionDescription, which gives us basic
 * information about its effects and environment interactions.
 *
 * If an instruction needs an environment (ie. if its EnvAccess > None), it
 * needs to have a dedicated environment argument. This dedicated environment
 * input is (for technical reasons) the last argument of fixed length
 * instructions and the first argument for variable length instructions. There
 * is some machinery to enforce passing an environment to the respective
 * superclassses.
 *
 * Every instruction has a unique instruction tag, which is used to "Cast" an
 * Intruction* to the particular instruction type.
 *
 * Every instruction (since it is a value) has a return type and every argument
 * has a type.
 *
 */

namespace rir {
enum class Opcode : uint8_t;
struct Code;

namespace pir {

class BB;
class Closure;
class Phi;

struct InstrArg : public std::pair<Value*, PirType> {
    InstrArg(Value* v, PirType t) : std::pair<Value*, PirType>(v, t) {
        assert(v->tag != Tag::_UNUSED_);
    }
    InstrArg() : std::pair<Value*, PirType>(nullptr, PirType::bottom()) {}
    Value*& val() { return first; }
    PirType& type() { return second; }
    Value* val() const { return first; }
    PirType type() const { return second; }
};

// EnvAccess specifies if an instruction has an environment argument
// (ie. EnvAccess > None), and if yes, what kind of interactions with that
// environment can happen.
enum class EnvAccess : uint8_t {
    None,
    Capture, // only needs a reference, does not load/store
    Read,
    Write,
    Leak,
};

// Effect that can be produced by an instruction.
enum class Effect : uint8_t {
    None,
    Warn,
    Error,
    Print,
    Write,
    Any,
};

class Instruction : public Value {
  public:
    struct InstructionUID : public std::pair<unsigned, unsigned> {
        InstructionUID(unsigned a, unsigned b)
            : std::pair<unsigned, unsigned>(a, b) {}
        unsigned bb() const { return first; }
        unsigned idx() const { return second; }
    };

    Instruction(Tag tag, PirType t, unsigned srcIdx)
        : Value(t, tag), srcIdx(srcIdx) {}

    virtual bool hasEffect() const = 0;
    virtual bool changesEnv() const = 0;
    virtual bool leaksEnv() const = 0;
    virtual bool mayAccessEnv() const = 0;
    virtual bool hasEnv() const = 0;

    virtual size_t nargs() const = 0;

    virtual Instruction* clone() const = 0;

    Value* baseValue() override;
    bool isInstruction() final { return true; }

    BB* bb_ = nullptr;
    BB* bb() {
        assert(bb_);
        return bb_;
    }

    unsigned srcIdx = 0;

    virtual ~Instruction() {}

    InstructionUID id();

    const char* name() { return tagToStr(tag); }

    Instruction* hasSingleUse();
    void replaceUsesWith(Value* val);
    void replaceUsesAndSwapWith(Instruction* val,
                                std::vector<Instruction*>::iterator it);
    void replaceUsesIn(Value* val, BB* target);
    bool unused();

    virtual void printEnv(std::ostream& out, bool tty);
    virtual void printArgs(std::ostream& out, bool tty);
    virtual void print(std::ostream& out, bool tty = false);
    void printRef(std::ostream& out) override;
    void print() { print(std::cerr, true); }

    virtual InstrArg& arg(size_t pos) = 0;
    virtual const InstrArg& arg(size_t pos) const = 0;

    bool leaksArg(Value* val) {
        // TODO: for escape analysis we use hasEnv || hasEffect as a very crude
        // approximation whether this instruction leaks arguments. We should do
        // better.
        return hasEnv() || hasEffect();
    }

    typedef std::function<void(Value*)> ArgumentValueIterator;
    typedef std::function<void(const InstrArg&)> ArgumentIterator;
    typedef std::function<void(InstrArg&)> MutableArgumentIterator;

    void eachArg(Instruction::ArgumentValueIterator it) const {
        for (size_t i = 0; i < nargs(); ++i)
            it(arg(i).val());
    }

    void eachArg(Instruction::ArgumentIterator it) const {
        for (size_t i = 0; i < nargs(); ++i)
            it(arg(i));
    }

    void eachArg(Instruction::MutableArgumentIterator it) {
        for (size_t i = 0; i < nargs(); ++i)
            it(arg(i));
    }

    void eachArgRev(Instruction::ArgumentValueIterator it) const {
        for (size_t i = 0; i < nargs(); ++i)
            it(arg(nargs() - 1 - i).val());
    }

    static Instruction* Cast(Value* v) {
        switch (v->tag) {
#define V(Name) case Tag::Name:
            COMPILER_INSTRUCTIONS(V)
#undef V
            return static_cast<Instruction*>(v);
        default: {}
        }
        return nullptr;
    }

    virtual Value* env() const {
        assert(!mayAccessEnv() &&
               "subclass must override env() if it uses env");
        assert(false && "this instruction has no env");
    }
    virtual void env(Value* env) {
        assert(!mayAccessEnv() &&
               "subclass must override env() if it uses env");
        assert(false && "this instruction has no env");
    }
    virtual void elideEnv() {
        assert(!mayAccessEnv() && "subclass must override elideEnv() if it "
                                  "speculatively removes environments");
        assert(false && "this instruction has no env");
    }
    virtual size_t envSlot() const {
        assert(!mayAccessEnv() &&
               "subclass must override envSlot() if it uses env");
        assert(false && "this instruction has no env");
    }
};

template <Tag ITAG, class Base, Effect EFFECT, EnvAccess ENV, class ArgStore>
class InstructionImplementation : public Instruction {
  protected:
    ArgStore args_;

  public:
    InstructionImplementation(PirType resultType, unsigned srcIdx)
        : Instruction(ITAG, resultType, srcIdx), args_({}) {}
    InstructionImplementation(PirType resultType, const ArgStore& args,
                              unsigned srcIdx)
        : Instruction(ITAG, resultType, srcIdx), args_(args) {}

    InstructionImplementation& operator=(InstructionImplementation&) = delete;
    InstructionImplementation() = delete;

    Instruction* clone() const override {
        assert(Base::Cast(this));
        return new Base(*static_cast<const Base*>(this));
    }

    struct InstrDescription {
        bool HasEffect;
        bool MayAccessEnv;
        bool ChangesEnv;
        bool LeaksEnv;
    };

    static constexpr InstrDescription Description = {
        EFFECT > Effect::None, ENV > EnvAccess::None, ENV >= EnvAccess::Write,
        ENV == EnvAccess::Leak};

    bool hasEffect() const final { return Description.HasEffect; }
    bool mayAccessEnv() const final { return Description.MayAccessEnv; }
    bool changesEnv() const final { return Description.ChangesEnv; }
    bool leaksEnv() const final { return Description.LeaksEnv; }
    bool hasEnv() const final {
        return mayAccessEnv() && env() != Env::elided();
    }

    static const Base* Cast(const Value* i) {
        if (i->tag == ITAG)
            return static_cast<const Base*>(i);
        return nullptr;
    }

    static Base* Cast(Value* i) {
        if (i->tag == ITAG)
            return static_cast<Base*>(i);
        return nullptr;
    }

    static void Cast(Value* i, std::function<void(Base*)> m) {
        Base* b = Cast(i);
        if (b)
            m(b);
    }

    static const void Cast(const Value* i, std::function<void(const Base*)> m) {
        Base* b = Cast(i);
        if (b)
            m(b);
    }

    size_t nargs() const override { return args_.size(); }

    const InstrArg& arg(size_t pos) const override final {
        assert(pos < nargs());
        return args_[pos];
    }

    InstrArg& arg(size_t pos) override final {
        assert(pos < nargs());
        return args_[pos];
    }
};

template <Tag ITAG, class Base, size_t ARGS, Effect EFFECT, EnvAccess ENV>
// cppcheck-suppress noConstructor
class FixedLenInstruction
    : public InstructionImplementation<ITAG, Base, EFFECT, ENV,
                                       std::array<InstrArg, ARGS>> {
  public:
    typedef InstructionImplementation<ITAG, Base, EFFECT, ENV,
                                      std::array<InstrArg, ARGS>>
        Super;
    using Super::arg;
    using Super::Description;
    size_t nargs() const override { return ARGS; }

    template <unsigned POS>
    InstrArg& arg() {
        static_assert(POS < ARGS, "This instruction has fewer arguments");
        return arg(POS);
    }

    template <unsigned POS>
    InstrArg& arg() const {
        static_assert(POS < ARGS, "This instruction has fewer arguments");
        return arg(POS);
    }

    FixedLenInstruction(PirType resultType, unsigned srcIdx = 0)
        : Super(resultType, {}, srcIdx) {
        static_assert(ARGS == 0, "This instruction expects more arguments");
    }

    FixedLenInstruction(PirType resultType, const std::array<PirType, ARGS>& at,
                        const std::array<Value*, ARGS>& arg,
                        unsigned srcIdx = 0)
        : Super(resultType, ArgsZip(arg, at), srcIdx) {}

    FixedLenInstruction(PirType resultType,
                        const std::array<InstrArg, ARGS>& args,
                        unsigned srcIdx = 0)
        : Super(resultType, args, srcIdx) {}

  private:
    // Some helpers to combine args and environment into one array
    struct ArgsZip : public std::array<InstrArg, ARGS> {
        ArgsZip(const std::array<Value*, ARGS>& a,
                const std::array<PirType, ARGS>& t) {
            for (size_t i = 0; i < ARGS; ++i) {
                (*this)[i].val() = a[i];
                (*this)[i].type() = t[i];
            }
        }
    };
};

template <Tag ITAG, class Base, size_t ARGS, Effect EFFECT, EnvAccess ENV>
class FixedLenInstructionWithEnvSlot
    : public FixedLenInstruction<ITAG, Base, ARGS, EFFECT, ENV> {
  public:
    typedef FixedLenInstruction<ITAG, Base, ARGS, EFFECT, ENV> Super;
    using Super::arg;
    using Super::Description;

    static constexpr size_t EnvSlot = ARGS - 1;

    FixedLenInstructionWithEnvSlot(PirType resultType, Value* env,
                                   unsigned srcIdx = 0)
        : Super(resultType, ArgsZip({}, {}, env), srcIdx) {
        static_assert(ARGS <= 1, "This instruction expects more arguments");
    }

    FixedLenInstructionWithEnvSlot(PirType resultType,
                                   const std::array<PirType, ARGS - 1>& at,
                                   const std::array<Value*, ARGS - 1>& arg,
                                   Value* env, unsigned srcIdx = 0)
        : Super(resultType, ArgsZip(arg, at, env), srcIdx) {}

    Value* env() const final override { return arg(EnvSlot).val(); }
    void env(Value* env) final override { arg(EnvSlot).val() = env; }
    void elideEnv() final override { arg(EnvSlot).val() = Env::elided(); }
    size_t envSlot() const final override { return EnvSlot; }

  private:
    // Combines args and types into one array and adds the environment at the
    // EnvSlot position into it.
    struct ArgsZip : public std::array<InstrArg, ARGS> {
        ArgsZip(const std::array<Value*, ARGS - 1>& a,
                const std::array<PirType, ARGS - 1>& t, Value* env) {
            static_assert(EnvSlot == ARGS - 1, "");
            (*this)[EnvSlot].val() = env;
            (*this)[EnvSlot].type() = RType::env;
            for (size_t i = 0; i < EnvSlot; ++i) {
                (*this)[i].val() = a[i];
                (*this)[i].type() = t[i];
            }
        }
    };
};

template <Tag ITAG, class Base, Effect EFFECT, EnvAccess ENV>
class VarLenInstruction
    : public InstructionImplementation<ITAG, Base, EFFECT, ENV,
                                       std::vector<InstrArg>> {

  public:
    typedef InstructionImplementation<ITAG, Base, EFFECT, ENV,
                                      std::vector<InstrArg>>
        Super;
    using Super::arg;
    using Super::args_;
    using Super::Description;
    using Super::nargs;

    virtual void pushArg(Value* a, PirType t) {
        assert(a);
        args_.push_back(InstrArg(a, t));
    }
    virtual void pushArg(Value* a) { pushArg(a, a->type); }
    virtual void popArg() {
        assert(args_.size() > 0);
        args_.pop_back();
    }

    VarLenInstruction(PirType return_type, unsigned srcIdx = 0)
        : Super(return_type, srcIdx) {}
};

template <Tag ITAG, class Base, Effect EFFECT, EnvAccess ENV>
class VarLenInstructionWithEnvSlot
    : public VarLenInstruction<ITAG, Base, EFFECT, ENV> {
  public:
    typedef VarLenInstruction<ITAG, Base, EFFECT, ENV> Super;
    using Super::arg;
    using Super::args_;
    using Super::Description;
    using Super::pushArg;

    // The env slot is always the last element of the args_ vector
    VarLenInstructionWithEnvSlot(PirType resultType, Value* env,
                                 unsigned srcIdx = 0)
        : Super(resultType, srcIdx) {
        Super::pushArg(env, RType::env);
    }

    void pushArg(Value* a, PirType t) override final {
        assert(a);
        assert(args_.size() > 0);
        assert(args_.back().type() == RType::env);
        // extend vector and move the environment to the end
        args_.push_back(args_.back());
        args_[args_.size() - 2] = InstrArg(a, t);
    }
    void popArg() override final {
        assert(args_.size() > 1);
        assert(args_.back().type() == RType::env);
        args_[args_.size() - 2] = args_[args_.size() - 1];
        args_.pop_back();
        assert(args_.back().type() == RType::env);
    }

    Value* env() const final override { return args_.back().val(); }
    void env(Value* env) final override { args_.back().val() = env; }

    size_t envSlot() const final override { return args_.size() - 1; }
};

extern std::ostream& operator<<(std::ostream& out,
                                Instruction::InstructionUID id);

#define FLI(type, nargs, io, env)                                              \
    type:                                                                      \
  public                                                                       \
    FixedLenInstruction<Tag::type, type, nargs, io, env>

#define FLIE(type, nargs, io, env)                                             \
    type:                                                                      \
  public                                                                       \
    FixedLenInstructionWithEnvSlot<Tag::type, type, nargs, io, env>

#define VLI(type, io, env)                                                     \
    type:                                                                      \
  public                                                                       \
    VarLenInstruction<Tag::type, type, io, env>

#define VLIE(type, io, env)                                                    \
    type:                                                                      \
  public                                                                       \
    VarLenInstructionWithEnvSlot<Tag::type, type, io, env>

class FLI(LdConst, 0, Effect::None, EnvAccess::None) {
  public:
    LdConst(SEXP c, PirType t) : FixedLenInstruction(t), c(c) {}
    explicit LdConst(SEXP c) : FixedLenInstruction(PirType(c)), c(c) {}
    SEXP c;
    void printArgs(std::ostream& out, bool tty) override;
};

class FLIE(LdFun, 1, Effect::Any, EnvAccess::Write) {
  public:
    SEXP varName;

    LdFun(const char* name, Value* env)
        : FixedLenInstructionWithEnvSlot(RType::closure, env),
          varName(Rf_install(name)) {}
    LdFun(SEXP name, Value* env)
        : FixedLenInstructionWithEnvSlot(RType::closure, env), varName(name) {
        assert(TYPEOF(name) == SYMSXP);
    }

    void printArgs(std::ostream& out, bool tty) override;
};

class FLIE(LdVar, 1, Effect::None, EnvAccess::Read) {
  public:
    SEXP varName;

    LdVar(const char* name, Value* env)
        : FixedLenInstructionWithEnvSlot(PirType::any(), env),
          varName(Rf_install(name)) {}
    LdVar(SEXP name, Value* env)
        : FixedLenInstructionWithEnvSlot(PirType::any(), env), varName(name) {
        assert(TYPEOF(name) == SYMSXP);
    }

    void printArgs(std::ostream& out, bool tty) override;
};

class FLI(ForSeqSize, 1, Effect::Error, EnvAccess::None) {
  public:
    explicit ForSeqSize(Value* val)
        : FixedLenInstruction(PirType(RType::integer).scalar(),
                              {{PirType::val()}}, {{val}}) {}
};

class FLI(LdArg, 0, Effect::None, EnvAccess::None) {
  public:
    size_t id;

    explicit LdArg(size_t id)
        : FixedLenInstruction(PirType::valOrLazy()), id(id) {}

    void printArgs(std::ostream& out, bool tty) override;
};

class FLI(ChkMissing, 1, Effect::Warn, EnvAccess::None) {
  public:
    explicit ChkMissing(Value* in)
        : FixedLenInstruction(PirType::valOrLazy(), {{PirType::any()}},
                              {{in}}) {}
};

class FLI(ChkClosure, 1, Effect::Warn, EnvAccess::None) {
  public:
    explicit ChkClosure(Value* in)
        : FixedLenInstruction(RType::closure, {{PirType::val()}}, {{in}}) {}
};

class FLIE(StVarSuper, 2, Effect::None, EnvAccess::Write) {
  public:
    StVarSuper(SEXP name, Value* val, Value* env)
        : FixedLenInstructionWithEnvSlot(PirType::voyd(), {{PirType::val()}},
                                         {{val}}, env),
          varName(name) {}

    StVarSuper(const char* name, Value* val, Value* env)
        : FixedLenInstructionWithEnvSlot(PirType::voyd(), {{PirType::val()}},
                                         {{val}}, env),
          varName(Rf_install(name)) {}

    SEXP varName;
    Value* val() { return arg(0).val(); }
    using FixedLenInstructionWithEnvSlot::env;

    void printArgs(std::ostream& out, bool tty) override;
};

class FLIE(LdVarSuper, 1, Effect::None, EnvAccess::Read) {
  public:
    LdVarSuper(SEXP name, Value* env)
        : FixedLenInstructionWithEnvSlot(PirType::any(), env), varName(name) {}

    LdVarSuper(const char* name, Value* env)
        : FixedLenInstructionWithEnvSlot(PirType::any(), env),
          varName(Rf_install(name)) {}

    SEXP varName;

    void printArgs(std::ostream& out, bool tty) override;
};

class FLIE(StVar, 2, Effect::None, EnvAccess::Write) {
  public:
    StVar(SEXP name, Value* val, Value* env)
        : FixedLenInstructionWithEnvSlot(PirType::voyd(), {{PirType::val()}},
                                         {{val}}, env),
          varName(name) {}

    StVar(const char* name, Value* val, Value* env)
        : FixedLenInstructionWithEnvSlot(PirType::voyd(), {{PirType::val()}},
                                         {{val}}, env),
          varName(Rf_install(name)) {}

    SEXP varName;
    Value* val() { return arg(0).val(); }
    using FixedLenInstructionWithEnvSlot::env;

    void printArgs(std::ostream& out, bool tty) override;
};

class FLI(Branch, 1, Effect::None, EnvAccess::None) {
  public:
    explicit Branch(Value* test)
        : FixedLenInstruction(PirType::voyd(), {{NativeType::test}}, {{test}}) {
    }
    void printArgs(std::ostream& out, bool tty) override;
};

class FLI(Return, 1, Effect::None, EnvAccess::None) {
  public:
    explicit Return(Value* ret)
        : FixedLenInstruction(PirType::voyd(), {{PirType::val()}}, {{ret}}) {}
};

class Promise;
class FLIE(MkArg, 2, Effect::None, EnvAccess::Capture) {
    Promise* prom_;

  public:
    MkArg(Promise* prom, Value* v, Value* env)
        : FixedLenInstructionWithEnvSlot(
              RType::prom, {{PirType::valOrMissing()}}, {{v}}, env),
          prom_(prom) {
        assert(eagerArg() == v);
    }
    MkArg(Value* v, Value* env)
        : FixedLenInstructionWithEnvSlot(RType::prom, {{PirType::val()}}, {{v}},
                                         env),
          prom_(nullptr) {
        assert(eagerArg() == v);
    }

    typedef std::function<void(Promise*)> PromMaybe;
    typedef std::function<void(Value*)> EagerMaybe;

    Value* eagerArg() const { return arg(0).val(); }
    void updatePromise(Promise* p) { prom_ = p; }
    Promise* prom() { return prom_; }

    void ifEager(EagerMaybe maybe) {
        if (eagerArg() != Missing::instance())
            maybe(eagerArg());
    }

    void printArgs(std::ostream& out, bool tty) override;

    Value* promEnv() const { return env(); }
};

class FLI(Seq, 3, Effect::None, EnvAccess::None) {
  public:
    Seq(Value* start, Value* end, Value* step)
        : FixedLenInstruction(
              PirType::num(),
              // TODO: require scalars, but this needs some cast support
              {{PirType::val(), PirType::val(), PirType::val()}},
              {{start, end, step}}) {}
};

class FLIE(MkCls, 4, Effect::None, EnvAccess::Capture) {
  public:
    MkCls(Value* fml, Value* code, Value* src, Value* lexicalEnv)
        : FixedLenInstructionWithEnvSlot(
              RType::closure, {{PirType::list(), RType::code, PirType::any()}},
              {{fml, code, src}}, lexicalEnv) {}

    Value* lexicalEnv() const { return env(); }

  private:
    using FixedLenInstructionWithEnvSlot::env;
};

class FLIE(MkFunCls, 1, Effect::None, EnvAccess::Capture) {
  public:
    Closure* fun;
    SEXP fml, code, src;
    MkFunCls(Closure* fun, Value* lexicalEnv, SEXP fml, SEXP code, SEXP src);
    void printArgs(std::ostream&, bool tty) override;

    Value* lexicalEnv() const { return env(); }
};

class FLIE(Force, 2, Effect::Any, EnvAccess::Leak) {
  public:
    Force(Value* in, Value* env)
        : FixedLenInstructionWithEnvSlot(PirType::val(), {{PirType::any()}},
                                         {{in}}, env) {}
    Value* input() const { return arg(0).val(); }
};

class FLI(CastType, 1, Effect::None, EnvAccess::None) {
  public:
    CastType(Value* in, PirType from, PirType to)
        : FixedLenInstruction(to, {{from}}, {{in}}) {}
};

class FLI(AsLogical, 1, Effect::Warn, EnvAccess::None) {
  public:
    AsLogical(Value* in, unsigned srcIdx)
        : FixedLenInstruction(RType::logical, {{PirType::val()}}, {{in}},
                              srcIdx) {}
};

class FLI(AsTest, 1, Effect::None, EnvAccess::None) {
  public:
    explicit AsTest(Value* in)
        : FixedLenInstruction(NativeType::test, {{RType::logical}}, {{in}}) {}
};

class FLIE(Subassign1_1D, 4, Effect::None, EnvAccess::Leak) {
  public:
    Subassign1_1D(Value* val, Value* vec, Value* idx, Value* env,
                  unsigned srcIdx)
        : FixedLenInstructionWithEnvSlot(
              PirType::val(),
              {{PirType::val(), PirType::val(), PirType::val()}},
              {{val, vec, idx}}, env, srcIdx) {}
    Value* rhs() { return arg(0).val(); }
    Value* lhsValue() { return arg(1).val(); }
    Value* idx() { return arg(2).val(); }
};

class FLIE(Subassign2_1D, 4, Effect::None, EnvAccess::Leak) {
  public:
    Subassign2_1D(Value* val, Value* vec, Value* idx, Value* env,
                  unsigned srcIdx)
        : FixedLenInstructionWithEnvSlot(
              PirType::val(),
              {{PirType::val(), PirType::val(), PirType::val()}},
              {{val, vec, idx}}, env, srcIdx) {}
    Value* rhs() { return arg(0).val(); }
    Value* lhsValue() { return arg(1).val(); }
    Value* idx() { return arg(2).val(); }
};

class FLIE(Extract1_1D, 3, Effect::None, EnvAccess::Leak) {
  public:
    Extract1_1D(Value* vec, Value* idx, Value* env, unsigned srcIdx)
        : FixedLenInstructionWithEnvSlot(PirType::val(),
                                         {{PirType::val(), PirType::val()}},
                                         {{vec, idx}}, env, srcIdx) {}
};

class FLIE(Extract2_1D, 3, Effect::None, EnvAccess::Leak) {
  public:
    Extract2_1D(Value* vec, Value* idx, Value* env, unsigned srcIdx)
        : FixedLenInstructionWithEnvSlot(PirType::val().scalar(),
                                         {{PirType::val(), PirType::val()}},
                                         {{vec, idx}}, env, srcIdx) {}
};

class FLIE(Extract1_2D, 4, Effect::None, EnvAccess::Leak) {
  public:
    Extract1_2D(Value* vec, Value* idx1, Value* idx2, Value* env,
                unsigned srcIdx)
        : FixedLenInstructionWithEnvSlot(
              PirType::val(),
              {{PirType::val(), PirType::val(), PirType::val()}},
              {{vec, idx1, idx2}}, env, srcIdx) {}
};

class FLIE(Extract2_2D, 4, Effect::None, EnvAccess::Leak) {
  public:
    Extract2_2D(Value* vec, Value* idx1, Value* idx2, Value* env,
                unsigned srcIdx)
        : FixedLenInstructionWithEnvSlot(
              PirType::val().scalar(),
              {{PirType::val(), PirType::val(), PirType::val()}},
              {{vec, idx1, idx2}}, env, srcIdx) {}
};

class FLI(Inc, 1, Effect::None, EnvAccess::None) {
  public:
    explicit Inc(Value* v)
        : FixedLenInstruction(PirType(RType::integer).scalar(),
                              {{PirType(RType::integer).scalar()}}, {{v}}) {}
};

class FLI(Is, 1, Effect::None, EnvAccess::None) {
  public:
    Is(uint32_t sexpTag, Value* v)
        : FixedLenInstruction(PirType(RType::logical).scalar(),
                              {{PirType::val()}}, {{v}}),
          sexpTag(sexpTag) {}
    uint32_t sexpTag;

    void printArgs(std::ostream& out, bool tty) override;
};

class FLI(IsObject, 1, Effect::None, EnvAccess::None) {
  public:
    explicit IsObject(Value* v)
        : FixedLenInstruction(NativeType::test, {{PirType::val()}}, {{v}}) {}
};

class FLI(LdFunctionEnv, 0, Effect::None, EnvAccess::None) {
  public:
    LdFunctionEnv() : FixedLenInstruction(RType::env) {}
};

class FLI(SetShared, 1, Effect::Write, EnvAccess::None) {
  public:
    explicit SetShared(Value* v)
        : FixedLenInstruction(v->type, {{v->type}}, {{v}}) {}
};

class FLI(PirCopy, 1, Effect::None, EnvAccess::None) {
  public:
    explicit PirCopy(Value* v)
        : FixedLenInstruction(v->type, {{v->type}}, {{v}}) {}
    void print(std::ostream& out, bool tty) override;
};

class FLI(Identical, 2, Effect::None, EnvAccess::None) {
  public:
    Identical(Value* a, Value* b)
        : FixedLenInstruction(NativeType::test,
                              {{PirType::any(), PirType::any()}}, {{a, b}}) {}
};

// Effect::Any prevents this instruction from being optimized away
class FLI(Int3, 0, Effect::Any, EnvAccess::None) {
  public:
    Int3() : FixedLenInstruction(PirType::voyd()) {}
};

#define BINOP(Name, Type)                                                      \
    class FLIE(Name, 3, Effect::None, EnvAccess::Leak) {                       \
      public:                                                                  \
        Name(Value* lhs, Value* rhs, Value* env, unsigned srcIdx)              \
            : FixedLenInstructionWithEnvSlot(                                  \
                  Type, {{PirType::val(), PirType::val()}}, {{lhs, rhs}}, env, \
                  srcIdx) {}                                                   \
    }

BINOP(Mul, PirType::val());
BINOP(Div, PirType::val());
BINOP(IDiv, PirType::val());
BINOP(Mod, PirType::val());
BINOP(Add, PirType::val());
BINOP(Colon, PirType::val());
BINOP(Pow, PirType::val());
BINOP(Sub, PirType::val());
BINOP(Gte, RType::logical);
BINOP(Lte, RType::logical);
BINOP(Gt, RType::logical);
BINOP(Lt, RType::logical);
BINOP(Neq, RType::logical);
BINOP(Eq, RType::logical);

#undef BINOP

#define BINOP_NOENV(Name, Type)                                                \
    class FLI(Name, 2, Effect::None, EnvAccess::None) {                        \
      public:                                                                  \
        Name(Value* lhs, Value* rhs)                                           \
            : FixedLenInstruction(Type, {{PirType::val(), PirType::val()}},    \
                                  {{lhs, rhs}}) {}                             \
    }

BINOP_NOENV(LAnd, RType::logical);
BINOP_NOENV(LOr, RType::logical);

#undef BINOP_NOENV

#define UNOP(Name)                                                             \
    class FLIE(Name, 2, Effect::None, EnvAccess::Leak) {                       \
      public:                                                                  \
        Name(Value* v, Value* env, unsigned srcIdx)                            \
            : FixedLenInstructionWithEnvSlot(                                  \
                  PirType::val(), {{PirType::val()}}, {{v}}, env, srcIdx) {}   \
    }

UNOP(Not);
UNOP(Plus);
UNOP(Minus);
UNOP(Length);

#undef UNOP

// Common interface to all call instructions
class CallInstruction {
  public:
    virtual size_t nCallArgs() = 0;
    virtual void eachCallArg(Instruction::ArgumentValueIterator it) = 0;
    static CallInstruction* CastCall(Value* v);
};

// Default call instruction. Closure expression (ie. expr left of `(`) is
// evaluated at runtime and arguments are passed as promises.
class VLIE(Call, Effect::Any, EnvAccess::Leak), public CallInstruction {
  public:
    Value* cls() { return arg(0).val(); }

    Call(Value * callerEnv, Value * fun, const std::vector<Value*>& args,
         unsigned srcIdx)
        : VarLenInstructionWithEnvSlot(PirType::valOrLazy(), callerEnv,
                                       srcIdx) {
        pushArg(fun, RType::closure);
        for (unsigned i = 0; i < args.size(); ++i)
            pushArg(args[i], PirType::val());
    }

    size_t nCallArgs() override { return nargs() - 2; };
    void eachCallArg(Instruction::ArgumentValueIterator it) override {
        for (size_t i = 0; i < nCallArgs(); ++i)
            it(arg(i + 1).val());
    }

    Value* callerEnv() { return env(); }

    void printArgs(std::ostream & out, bool tty) override;
};

class VLIE(NamedCall, Effect::Any, EnvAccess::Leak), public CallInstruction {
  public:
    std::vector<SEXP> names;

    Value* cls() { return arg(0).val(); }

    NamedCall(Value * callerEnv, Value * fun, const std::vector<Value*>& args,
              const std::vector<BC::PoolIdx>& names_, unsigned srcIdx);

    size_t nCallArgs() override { return nargs() - 2; };
    void eachCallArg(Instruction::ArgumentValueIterator it) override {
        for (size_t i = 0; i < nCallArgs(); ++i)
            it(arg(i + 1).val());
    }

    Value* callerEnv() { return env(); }
    void printArgs(std::ostream & out, bool tty) override;
};

class FLIE(CallImplicit, 2, Effect::Any, EnvAccess::Leak) {
  public:
    std::vector<Promise*> promises;
    std::vector<SEXP> names;

    Value* cls() { return arg(0).val(); }

    CallImplicit(Value* callerEnv, Value* fun,
                 const std::vector<Promise*>& args,
                 const std::vector<SEXP>& names_, unsigned srcIdx)
        : FixedLenInstructionWithEnvSlot(PirType::valOrLazy(),
                                         {{PirType::closure()}}, {{fun}},
                                         callerEnv, srcIdx),
          promises(args), names(names_) {}

    Value* callerEnv() { return env(); }
    void printArgs(std::ostream& out, bool tty) override;
};

// Call instruction for lazy, but staticatlly resolved calls. Closure is
// specified as `cls_`, args passed as promises.
class VLIE(StaticCall, Effect::Any, EnvAccess::Leak), public CallInstruction {
    Closure* cls_;
    SEXP origin_;

  public:
    Closure* cls() { return cls_; }
    SEXP origin() { return origin_; }

    StaticCall(Value * callerEnv, Closure * cls,
               const std::vector<Value*>& args, SEXP origin, unsigned srcIdx)
        : VarLenInstructionWithEnvSlot(PirType::valOrLazy(), callerEnv, srcIdx),
          cls_(cls), origin_(origin) {
        for (unsigned i = 0; i < args.size(); ++i)
            pushArg(args[i], PirType::val());
    }

    size_t nCallArgs() override { return nargs() - 1; };
    void eachCallArg(Instruction::ArgumentValueIterator it) override {
        for (size_t i = 0; i < nCallArgs(); ++i)
            it(arg(i).val());
    }

    void printArgs(std::ostream & out, bool tty) override;
    Value* callerEnv() { return env(); }
};

typedef SEXP (*CCODE)(SEXP, SEXP, SEXP, SEXP);

class VLIE(CallBuiltin, Effect::Any, EnvAccess::Leak), public CallInstruction {
  public:
    SEXP blt;
    const CCODE builtin;
    int builtinId;

    CallBuiltin(Value * callerEnv, SEXP builtin,
                const std::vector<Value*>& args, unsigned srcIdx);

    size_t nCallArgs() override { return nargs() - 1; };
    void eachCallArg(Instruction::ArgumentValueIterator it) override {
        for (size_t i = 0; i < nCallArgs(); ++i)
            it(arg(i).val());
    }
    void printArgs(std::ostream & out, bool tty) override;
    Value* callerEnv() { return env(); }
};

class VLI(CallSafeBuiltin, Effect::None, EnvAccess::None),
    public CallInstruction {
  public:
    SEXP blt;
    const CCODE builtin;
    int builtinId;

    CallSafeBuiltin(SEXP builtin, const std::vector<Value*>& args,
                    unsigned srcIdx);

    size_t nCallArgs() override { return nargs(); };
    void eachCallArg(Instruction::ArgumentValueIterator it) override {
        eachArg(it);
    }

    void printArgs(std::ostream & out, bool tty) override;
};

class VLIE(MkEnv, Effect::None, EnvAccess::Capture) {
  public:
    std::vector<SEXP> varName;

    typedef std::function<void(SEXP name, Value* val)> LocalVarIt;
    typedef std::function<void(SEXP name, InstrArg&)> MutableLocalVarIt;

    void eachLocalVar(MutableLocalVarIt it) {
        for (size_t i = 0; i < envSlot(); ++i)
            it(varName[i], arg(i));
    }

    void eachLocalVar(LocalVarIt it) const {
        for (size_t i = 0; i < envSlot(); ++i)
            it(varName[i], arg(i).val());
    }

    void eachLocalVarRev(LocalVarIt it) const {
        for (long i = envSlot() - 1; i >= 0; --i)
            it(varName[i], arg(i).val());
    }

    MkEnv(Value* lexicalEnv, const std::vector<SEXP>& names, Value** args)
        : VarLenInstructionWithEnvSlot(RType::env, lexicalEnv), varName(names) {
        for (unsigned i = 0; i < varName.size(); ++i)
            pushArg(args[i], PirType::any());
    }

    Value* lexicalEnv() const { return env(); }

    void printArgs(std::ostream& out, bool tty) override;
    void printEnv(std::ostream& out, bool tty) override final{};

    size_t nLocals() { return nargs() - 1; }
};

class VLI(Phi, Effect::None, EnvAccess::None) {
  public:
    std::vector<BB*> input;
    Phi() : VarLenInstruction(PirType::any()) {}
    Phi(const std::initializer_list<Value*>& vals,
        const std::initializer_list<BB*>& inputs)
        : VarLenInstruction(PirType::any()) {
        assert(vals.size() == inputs.size());
        for (auto i : inputs)
            input.push_back(i);
        for (auto a : vals)
            VarLenInstruction::pushArg(a);
        assert(nargs() == inputs.size());
    }
    void printArgs(std::ostream& out, bool tty) override;
    bool updateType();
    void pushArg(Value* a, PirType t) override {
        assert(false && "use addInput");
    }
    void pushArg(Value* a) override { assert(false && "use addInput"); }
    void addInput(BB* in, Value* arg) {
        input.push_back(in);
        args_.push_back(InstrArg(arg, arg->type));
    }
    typedef std::function<void(BB* bb, Value*)> PhiArgumentIterator;

    void eachArg(PhiArgumentIterator it) const {
        for (size_t i = 0; i < nargs(); ++i)
            it(input[i], arg(i).val());
    }
};

struct RirStack {
  private:
    typedef std::deque<Value*> Stack;
    Stack stack;

  public:
    void push(Value* v) { stack.push_back(v); }
    Value* pop() {
        assert(!empty());
        auto v = stack.back();
        stack.pop_back();
        return v;
    }
    Value*& at(unsigned i) {
        assert(i < size());
        return stack[stack.size() - 1 - i];
    }
    Value* at(unsigned i) const {
        assert(i < size());
        return stack[stack.size() - 1 - i];
    }
    Value* top() const {
        assert(!empty());
        return stack.back();
    }
    bool empty() const { return stack.empty(); }
    size_t size() const { return stack.size(); }
    void clear() { stack.clear(); }
    Stack::const_iterator begin() const { return stack.cbegin(); }
    Stack::const_iterator end() const { return stack.cend(); }
    Stack::iterator begin() { return stack.begin(); }
    Stack::iterator end() { return stack.end(); }
};

class VLIE(Safepoint, Effect::Any, EnvAccess::Leak) {
  public:
    bool inlined = false;
    Opcode* pc;
    rir::Code* code;
    size_t stackSize;

    Safepoint(Value* env, rir::Code* code, Opcode* pc, const RirStack& stack)
        : VarLenInstructionWithEnvSlot(NativeType::safepoint, env), pc(pc),
          code(code), stackSize(stack.size()) {
        for (auto& v : stack)
            pushArg(v);
    }

    void updateNext(Safepoint* s) {
        assert(inlined);
        assert(arg(stackSize).type() == NativeType::safepoint);
        arg(stackSize).val() = s;
    }

    void next(Safepoint* s) {
        assert(!inlined);
        inlined = true;
        pushArg(s, NativeType::safepoint);
    }

    Safepoint* next() const {
        if (inlined) {
            auto r = Cast(arg(stackSize).val());
            assert(r);
            return r;
        } else {
            return nullptr;
        }
    }

    Value* tos() { return arg(stackSize - 1).val(); }

    void popStack() {
        stackSize--;
        // Move the next() ptr
        if (inlined)
            arg(stackSize) = arg(stackSize + 1);
        popArg();
    }

    void printArgs(std::ostream& out, bool tty) override;
    void printEnv(std::ostream& out, bool tty) override final{};
};

class FLI(Deopt, 1, Effect::Any, EnvAccess::None) {
  public:
    explicit Deopt(Safepoint* safepoint)
        : FixedLenInstruction(PirType::voyd(), {{NativeType::safepoint}},
                              {{safepoint}}) {}
    Safepoint* safepoint();
};

class VLI(ScheduledDeopt, Effect::Any, EnvAccess::None) {
  public:
    std::vector<FrameInfo> frames;
    ScheduledDeopt() : VarLenInstruction(PirType::voyd()) {}
    void consumeSafepoints(Deopt* deopt);
    void printArgs(std::ostream& out, bool tty) override;
};

#undef FLI
#undef VLI
#undef FLIE
#undef VLIE
} // namespace pir
} // namespace rir

#endif
