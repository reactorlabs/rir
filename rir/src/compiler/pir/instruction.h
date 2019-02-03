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

#include <algorithm>
#include <array>
#include <cassert>
#include <cstdint>
#include <deque>
#include <functional>
#include <iostream>
#include <unordered_set>

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
struct DispatchTable;
struct Code;

namespace pir {

class BB;
class Closure;
class Phi;

struct InstrArg {
  private:
    PirType type_;
    Value* val_;

  public:
    InstrArg(Value* v, PirType t) : type_(t), val_(v) {
        assert(v->tag != Tag::_UNUSED_);
    }
    InstrArg() : type_(PirType::bottom()), val_(nullptr) {}
    Value*& val() { return val_; }
    PirType& type() { return type_; }
    Value* val() const { return val_; }
    PirType type() const { return type_; }
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
// This is a trivial lattice, any effect with higher order contains all the
// lower order effects.
enum class Effect : uint8_t {
    None,
    // Instruction doesn't really have effects itself, but it should not be
    // hoisted over any other instruction with effect. Example: Assume
    Order,
    // Instruction might produce a warning. Example: AsTest warns if the
    // vector used in an if condition has length > 1
    Warn,
    // Instruction might produce an error. Example: ForSeqSize raises an
    // error if the collection to loop over is not indexable.
    Error,
    // Instruction might force promises
    Force,
    // Instruction might use reflection
    Reflection,
    Any,
};

// Controlflow of instruction.
enum class Controlflow : uint8_t {
    None,
    Exit,
    Branch,
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
    virtual bool mayUseReflection() const = 0;
    virtual bool mayForcePromises() const = 0;
    virtual bool changesEnv() const = 0;
    virtual bool leaksEnv() const = 0;
    virtual bool mayAccessEnv() const = 0;
    virtual bool hasEnv() const = 0;
    virtual bool exits() const = 0;
    virtual bool branches() const = 0;
    virtual bool branchOrExit() const = 0;

    virtual size_t nargs() const = 0;

    virtual Instruction* clone() const = 0;

    const Value* cFollowCasts() const override final;
    const Value* cFollowCastsAndForce() const override final;
    bool isInstruction() override final { return true; }
    virtual bool envOnlyForObj();

    bool validIn(Code* code) const override final;

    BB* bb_ = nullptr;
    BB* bb() const {
        assert(bb_);
        return bb_;
    }

    unsigned srcIdx = 0;

    virtual ~Instruction() {}

    InstructionUID id() const;

    virtual const char* name() const { return tagToStr(tag); }

    Instruction* hasSingleUse();
    void eraseAndRemove();
    void replaceUsesWith(Value* val);
    void replaceUsesAndSwapWith(Instruction* val,
                                std::vector<Instruction*>::iterator it);
    void replaceUsesIn(Value* val, BB* target);
    bool unused();

    virtual void updateType(){};

    virtual void printEnv(std::ostream& out, bool tty) const;
    virtual void printArgs(std::ostream& out, bool tty) const;
    virtual void print(std::ostream& out, bool tty = false) const;
    void printRef(std::ostream& out) const override final;
    void print() const { print(std::cerr, true); }

    virtual InstrArg& arg(size_t pos) = 0;
    virtual const InstrArg& arg(size_t pos) const = 0;

    bool leaksArg(Value* val) {
        // TODO: for escape analysis we use leaksEnv || hasEffect as a very
        // crude approximation whether this instruction leaks arguments. We
        // should do better.
        return leaksEnv() || hasEffect();
    }

    typedef std::function<bool(Value*)> ArgumentValuePredicateIterator;
    typedef std::function<void(Value*)> ArgumentValueIterator;
    typedef std::function<void(const InstrArg&)> ArgumentIterator;
    typedef std::function<void(InstrArg&)> MutableArgumentIterator;

    bool anyArg(Instruction::ArgumentValuePredicateIterator it) const {
        for (size_t i = 0; i < nargs(); ++i)
            if (it(arg(i).val()))
                return true;
        return false;
    }

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
    void elideEnv() { arg(envSlot()).val() = Env::elided(); }
    virtual size_t envSlot() const {
        assert(!mayAccessEnv() &&
               "subclass must override envSlot() if it uses env");
        assert(false && "this instruction has no env");
    }
};

template <Tag ITAG, class Base, Effect EFFECT, EnvAccess ENV, Controlflow CF,
          class ArgStore>
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

    static constexpr bool mayAccessEnv_ = ENV > EnvAccess::None;
    static constexpr bool mayChangeEnv_ = ENV >= EnvAccess::Write;
    static constexpr bool mayLeakEnv_ = ENV >= EnvAccess::Leak;

    bool hasEffect() const override { return EFFECT > Effect::None; }
    bool mayForcePromises() const override final {
        return EFFECT >= Effect::Force;
    }
    bool mayUseReflection() const override final {
        return EFFECT > Effect::Reflection;
    }
    bool mayAccessEnv() const override final { return mayAccessEnv_; }
    bool changesEnv() const override final { return hasEnv() && mayChangeEnv_; }
    bool leaksEnv() const override final { return hasEnv() && mayLeakEnv_; }
    bool hasEnv() const override final {
        return mayAccessEnv() && env() != Env::elided();
    }
    bool exits() const override final { return CF == Controlflow::Exit; }
    bool branches() const override final { return CF == Controlflow::Branch; }
    bool branchOrExit() const override final { return branches() || exits(); }

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

template <Tag ITAG, class Base, size_t ARGS, Effect EFFECT, EnvAccess ENV,
          Controlflow CF = Controlflow::None>
// cppcheck-suppress noConstructor
class FixedLenInstruction
    : public InstructionImplementation<ITAG, Base, EFFECT, ENV, CF,
                                       std::array<InstrArg, ARGS>> {
  public:
    typedef InstructionImplementation<ITAG, Base, EFFECT, ENV, CF,
                                      std::array<InstrArg, ARGS>>
        Super;
    using Super::arg;
    size_t nargs() const override { return ARGS; }

    template <unsigned POS>
    InstrArg& arg() {
        static_assert(POS < ARGS, "This instruction has fewer arguments");
        return arg(POS);
    }

    template <unsigned POS>
    const InstrArg& arg() const {
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

template <Tag ITAG, class Base, size_t ARGS, Effect EFFECT, EnvAccess ENV,
          Controlflow CF = Controlflow::None>
class FixedLenInstructionWithEnvSlot
    : public FixedLenInstruction<ITAG, Base, ARGS, EFFECT, ENV, CF> {
  public:
    typedef FixedLenInstruction<ITAG, Base, ARGS, EFFECT, ENV, CF> Super;
    using Super::arg;

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

template <Tag ITAG, class Base, Effect EFFECT, EnvAccess ENV,
          Controlflow CF = Controlflow::None>
class VarLenInstruction
    : public InstructionImplementation<ITAG, Base, EFFECT, ENV, CF,
                                       std::vector<InstrArg>> {

  public:
    typedef InstructionImplementation<ITAG, Base, EFFECT, ENV, CF,
                                      std::vector<InstrArg>>
        Super;
    using Super::arg;
    using Super::args_;
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

template <Tag ITAG, class Base, Effect EFFECT, EnvAccess ENV,
          Controlflow CF = Controlflow::None>
class VarLenInstructionWithEnvSlot
    : public VarLenInstruction<ITAG, Base, EFFECT, ENV, CF> {
  public:
    typedef VarLenInstruction<ITAG, Base, EFFECT, ENV, CF> Super;
    using Super::arg;
    using Super::args_;
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
    void printArgs(std::ostream& out, bool tty) const override;
};

class FLIE(LdFun, 2, Effect::Any, EnvAccess::Write) {
  public:
    SEXP varName;

    LdFun(const char* name, Value* env)
        : FixedLenInstructionWithEnvSlot(RType::closure, {{PirType::any()}},
                                         {{Tombstone::closure()}}, env),
          varName(Rf_install(name)) {}
    LdFun(SEXP name, Value* env)
        : FixedLenInstructionWithEnvSlot(RType::closure, {{PirType::any()}},
                                         {{Tombstone::closure()}}, env),
          varName(name) {
        assert(TYPEOF(name) == SYMSXP);
    }

    void clearGuessedBinding() { arg<0>().val() = Tombstone::closure(); }

    void guessedBinding(Value* val) { arg<0>().val() = val; }

    Value* guessedBinding() const {
        if (arg<0>().val() != Tombstone::closure())
            return arg<0>().val();
        return nullptr;
    }

    void printArgs(std::ostream& out, bool tty) const override;
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

    void printArgs(std::ostream& out, bool tty) const override;
};

class FLI(ForSeqSize, 1, Effect::Error, EnvAccess::None) {
  public:
    explicit ForSeqSize(Value* val)
        : FixedLenInstruction(PirType(RType::integer).scalar().notObject(),
                              {{PirType::val()}}, {{val}}) {}
};

class FLI(LdArg, 0, Effect::None, EnvAccess::None) {
  public:
    size_t id;

    explicit LdArg(size_t id)
        : FixedLenInstruction(PirType::valOrLazy()), id(id) {}

    void printArgs(std::ostream& out, bool tty) const override;
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
    Value* val() const { return arg(0).val(); }
    using FixedLenInstructionWithEnvSlot::env;

    void printArgs(std::ostream& out, bool tty) const override;
};

class FLIE(LdVarSuper, 1, Effect::None, EnvAccess::Read) {
  public:
    LdVarSuper(SEXP name, Value* env)
        : FixedLenInstructionWithEnvSlot(PirType::any(), env), varName(name) {}

    LdVarSuper(const char* name, Value* env)
        : FixedLenInstructionWithEnvSlot(PirType::any(), env),
          varName(Rf_install(name)) {}

    SEXP varName;

    void printArgs(std::ostream& out, bool tty) const override;
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
    Value* val() const { return arg(0).val(); }
    using FixedLenInstructionWithEnvSlot::env;

    void printArgs(std::ostream& out, bool tty) const override;
};

class Branch
    : public FixedLenInstruction<Tag::Branch, Branch, 1, Effect::None,
                                 EnvAccess::None, Controlflow::Branch> {
  public:
    explicit Branch(Value * test)
        : FixedLenInstruction(PirType::voyd(), {{NativeType::test}}, {{test}}) {
    }
    void printArgs(std::ostream& out, bool tty) const override;
};

class Return : public FixedLenInstruction<Tag::Return, Return, 1, Effect::None,
                                          EnvAccess::None, Controlflow::Exit> {
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
    void eagerArg(Value* eager) { arg(0).val() = eager; }

    void updatePromise(Promise* p) { prom_ = p; }
    Promise* prom() const { return prom_; }

    void ifEager(EagerMaybe maybe) {
        if (eagerArg() != Missing::instance())
            maybe(eagerArg());
    }

    void printArgs(std::ostream& out, bool tty) const override;

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
    Closure* cls;
    DispatchTable* originalBody;
    MkFunCls(Closure* cls, DispatchTable* originalBody, Value* lexicalEnv);
    void printArgs(std::ostream&, bool tty) const override;

    Value* lexicalEnv() const { return env(); }
};

class FLIE(Force, 2, Effect::Any, EnvAccess::Leak) {
  public:
    // Set to true if we are sure that the promise will be forced here
    bool strict = false;
    Force(Value* in, Value* env)
        : FixedLenInstructionWithEnvSlot(in->type.forced(), {{PirType::any()}},
                                         {{in}}, env) {}
    Value* input() const { return arg(0).val(); }
    const char* name() const override { return strict ? "Force!" : "Force"; }
    bool hasEffect() const override final { return input()->type.maybeLazy(); }
    void updateType() override final { type = arg<0>().val()->type.forced(); }
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

class FLI(AsTest, 1, Effect::Error, EnvAccess::None) {
  public:
    explicit AsTest(Value* in)
        : FixedLenInstruction(NativeType::test, {{PirType::any()}}, {{in}}) {}
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
        : FixedLenInstruction(PirType(RType::integer).scalar().notObject(),
                              {{PirType(RType::integer).scalar().notObject()}},
                              {{v}}) {}
};

class FLI(Is, 1, Effect::None, EnvAccess::None) {
  public:
    Is(uint32_t sexpTag, Value* v)
        : FixedLenInstruction(PirType(RType::logical).scalar(),
                              {{PirType::val()}}, {{v}}),
          sexpTag(sexpTag) {}
    uint32_t sexpTag;

    void printArgs(std::ostream& out, bool tty) const override;
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

class FLI(EnsureNamed, 1, Effect::None, EnvAccess::None) {
  public:
    explicit EnsureNamed(Value* v)
        : FixedLenInstruction(v->type, {{v->type}}, {{v}}) {}
    void updateType() override final { type = arg<0>().val()->type; }
};

class FLI(SetShared, 1, Effect::None, EnvAccess::None) {
  public:
    explicit SetShared(Value* v)
        : FixedLenInstruction(v->type, {{v->type}}, {{v}}) {}
    void updateType() override final { type = arg<0>().val()->type; }
};

class FLI(PirCopy, 1, Effect::None, EnvAccess::None) {
  public:
    explicit PirCopy(Value* v)
        : FixedLenInstruction(v->type, {{v->type}}, {{v}}) {}
    void print(std::ostream& out, bool tty) const override;
    void updateType() override final { type = arg<0>().val()->type; }
};

// Effect::Any prevents this instruction from being optimized away
class FLI(Nop, 0, Effect::Any, EnvAccess::None) {
  public:
    explicit Nop() : FixedLenInstruction(PirType::voyd()) {}
};

class FLI(Identical, 2, Effect::None, EnvAccess::None) {
  public:
    Identical(Value* a, Value* b)
        : FixedLenInstruction(NativeType::test,
                              {{PirType::any(), PirType::any()}}, {{a, b}}) {}
};

#define V(NESTED, name, Name)\
class FLI(Name, 0, Effect::Any, EnvAccess::None) {\
  public:\
    Name() : FixedLenInstruction(PirType::voyd()) {}\
};
SIMPLE_INSTRUCTIONS(V, _)
#undef V

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

/*
 *  Collects metadata about the current state of variables
 *  eventually needed for deoptimization purposes
 */
class VLIE(FrameState, Effect::None, EnvAccess::Read) {
  public:
    bool inlined = false;
    Opcode* pc;
    rir::Code* code;
    size_t stackSize;

    FrameState(Value* env, rir::Code* code, Opcode* pc, const RirStack& stack)
        : VarLenInstructionWithEnvSlot(NativeType::frameState, env), pc(pc),
          code(code), stackSize(stack.size()) {
        for (auto& v : stack)
            pushArg(v);
    }

    void updateNext(FrameState* s) {
        assert(inlined);
        assert(arg(stackSize).type() == NativeType::frameState);
        arg(stackSize).val() = s;
    }

    void next(FrameState* s) {
        assert(!inlined);
        inlined = true;
        pushArg(s, NativeType::frameState);
    }

    FrameState* next() const {
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

    void printArgs(std::ostream& out, bool tty) const override;
    void printEnv(std::ostream& out, bool tty) const override final{};
};

// Common interface to all call instructions
class CallInstruction {
  public:
    virtual size_t nCallArgs() const = 0;
    virtual void eachCallArg(Instruction::ArgumentValueIterator it) const = 0;
    virtual void eachCallArg(Instruction::MutableArgumentIterator it) = 0;
    static CallInstruction* CastCall(Value* v);
    virtual void clearFrameState(){};
    virtual Closure* tryGetCls() const { return nullptr; }
    Assumptions inferAvailableAssumptions() const;
    virtual bool hasNamedArgs() const { return false; }
    ClosureVersion* dispatch(Closure*) const;
};

// Default call instruction. Closure expression (ie. expr left of `(`) is
// evaluated at runtime and arguments are passed as promises.
class VLIE(Call, Effect::Any, EnvAccess::Leak), public CallInstruction {
  public:
    Value* cls() const { return arg(1).val(); }

    Call(Value * callerEnv, Value * fun, const std::vector<Value*>& args,
         Value* fs, unsigned srcIdx)
        : VarLenInstructionWithEnvSlot(PirType::valOrLazy(), callerEnv,
                                       srcIdx) {
        assert(fs);
        pushArg(fs, NativeType::frameState);
        pushArg(fun, RType::closure);
        for (unsigned i = 0; i < args.size(); ++i)
            pushArg(args[i], RType::prom);
    }

    Closure* tryGetCls() const override final {
        if (auto mk = MkFunCls::Cast(cls()->followCastsAndForce()))
            return mk->cls;
        return nullptr;
    }

    size_t nCallArgs() const override { return nargs() - 3; };
    void eachCallArg(Instruction::ArgumentValueIterator it) const override {
        for (size_t i = 0; i < nCallArgs(); ++i)
            it(arg(i + 2).val());
    }
    void eachCallArg(Instruction::MutableArgumentIterator it) override {
        for (size_t i = 0; i < nCallArgs(); ++i)
            it(arg(i + 2));
    }

    const FrameState* frameState() const {
        return FrameState::Cast(arg(0).val());
    }
    void clearFrameState() override { arg(0).val() = Tombstone::framestate(); };

    Value* callerEnv() { return env(); }

    void printArgs(std::ostream & out, bool tty) const override;
};

class VLIE(NamedCall, Effect::Any, EnvAccess::Leak), public CallInstruction {
  public:
    std::vector<SEXP> names;

    Value* cls() const { return arg(0).val(); }

    Closure* tryGetCls() const override final {
        if (auto mk = MkFunCls::Cast(cls()->followCastsAndForce()))
            return mk->cls;
        return nullptr;
    }

    bool hasNamedArgs() const override { return true; }

    NamedCall(Value * callerEnv, Value * fun, const std::vector<Value*>& args,
              const std::vector<BC::PoolIdx>& names_, unsigned srcIdx);

    size_t nCallArgs() const override { return nargs() - 2; };
    void eachCallArg(Instruction::ArgumentValueIterator it) const override {
        for (size_t i = 0; i < nCallArgs(); ++i)
            it(arg(i + 1).val());
    }
    void eachCallArg(Instruction::MutableArgumentIterator it) override {
        for (size_t i = 0; i < nCallArgs(); ++i)
            it(arg(i + 1));
    }

    Value* callerEnv() { return env(); }
    void printArgs(std::ostream & out, bool tty) const override;
};

class FLIE(CallImplicit, 2, Effect::Any, EnvAccess::Leak) {
    const std::vector<Promise*> promises;

  public:
    void eachArg(const std::function<void(Promise*)>&) const;
    const std::vector<SEXP> names;

    Value* cls() const { return arg(0).val(); }

    CallImplicit(Value* callerEnv, Value* fun,
                 const std::vector<Promise*>& args,
                 const std::vector<SEXP>& names_, unsigned srcIdx);

    Value* callerEnv() { return env(); }
    void printArgs(std::ostream& out, bool tty) const override;
};

// Call instruction for lazy, but staticatlly resolved calls. Closure is
// specified as `cls_`, args passed as promises.
class VLIE(StaticCall, Effect::Any, EnvAccess::Leak), public CallInstruction {
    Closure* cls_;

  public:
    ClosureVersion* hint = nullptr;

    Closure* cls() const { return cls_; }
    void cls(Closure * cls) { cls_ = cls; }

    Closure* tryGetCls() const override final { return cls(); }

    StaticCall(Value * callerEnv, Closure * cls,
               const std::vector<Value*>& args, FrameState* fs,
               unsigned srcIdx);

    size_t nCallArgs() const override { return nargs() - 2; };
    void eachCallArg(Instruction::ArgumentValueIterator it) const override {
        for (size_t i = 0; i < nCallArgs(); ++i)
            it(arg(i + 1).val());
    }
    void eachCallArg(Instruction::MutableArgumentIterator it) override {
        for (size_t i = 0; i < nCallArgs(); ++i)
            it(arg(i + 1));
    }

    const FrameState* frameState() const {
        return FrameState::Cast(arg(0).val());
    }
    void clearFrameState() override { arg(0).val() = Tombstone::framestate(); };

    void printArgs(std::ostream & out, bool tty) const override;
    Value* callerEnv() { return env(); }

    ClosureVersion* dispatch() const;

    ClosureVersion* optimisticDispatch() const;
};

typedef SEXP (*CCODE)(SEXP, SEXP, SEXP, SEXP);

class VLIE(CallBuiltin, Effect::Any, EnvAccess::Leak), public CallInstruction {
  public:
    SEXP blt;
    const CCODE builtin;
    int builtinId;

    size_t nCallArgs() const override { return nargs() - 1; };
    void eachCallArg(Instruction::ArgumentValueIterator it) const override {
        for (size_t i = 0; i < nCallArgs(); ++i)
            it(arg(i).val());
    }
    void eachCallArg(Instruction::MutableArgumentIterator it) override {
        for (size_t i = 0; i < nCallArgs(); ++i)
            it(arg(i));
    }
    void printArgs(std::ostream & out, bool tty) const override;
    Value* callerEnv() { return env(); }

  private:
    CallBuiltin(Value * callerEnv, SEXP builtin,
                const std::vector<Value*>& args, unsigned srcIdx);
    friend class BuiltinCallFactory;
};

class VLI(CallSafeBuiltin, Effect::None, EnvAccess::None),
    public CallInstruction {
  public:
    SEXP blt;
    const CCODE builtin;
    int builtinId;

    size_t nCallArgs() const override { return nargs(); };
    void eachCallArg(Instruction::ArgumentValueIterator it) const override {
        eachArg(it);
    }
    void eachCallArg(Instruction::MutableArgumentIterator it) override {
        eachArg(it);
    }

    void printArgs(std::ostream & out, bool tty) const override;

    CallSafeBuiltin(SEXP builtin, const std::vector<Value*>& args,
                    unsigned srcIdx);
};

class BuiltinCallFactory {
  public:
    static Instruction* New(Value* callerEnv, SEXP builtin,
                            const std::vector<Value*>& args, unsigned srcIdx);
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

    void printArgs(std::ostream& out, bool tty) const override;
    void printEnv(std::ostream& out, bool tty) const override final{};

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
        std::copy(inputs.begin(), inputs.end(), std::back_inserter(input));
        for (auto a : vals)
            VarLenInstruction::pushArg(a);
        assert(nargs() == inputs.size());
    }
    void printArgs(std::ostream& out, bool tty) const override;
    void updateType() override final;
    void pushArg(Value* a, PirType t) override {
        assert(false && "use addInput");
    }
    void pushArg(Value* a) override { assert(false && "use addInput"); }
    void addInput(BB* in, Value* arg) {
        input.push_back(in);
        args_.push_back(InstrArg(arg, PirType::any()));
    }
    void removeInputs(const std::unordered_set<BB*>& del);

    typedef std::function<void(BB* bb, Value*)> PhiArgumentIterator;

    void eachArg(PhiArgumentIterator it) const {
        for (size_t i = 0; i < nargs(); ++i)
            it(input[i], arg(i).val());
    }
};

// Instructions targeted specially for speculative optimization

/*
 *  Must be the last instruction of a BB with two childs. One should
 *  contain a deopt. Checkpoint takes either branch at random
 *  to ensure the optimizer consider deopt and non-deopt cases.
 */
class Checkpoint
    : public FixedLenInstruction<Tag::Checkpoint, Checkpoint, 0, Effect::None,
                                 EnvAccess::None, Controlflow::Branch> {
  public:
    Checkpoint() : FixedLenInstruction(NativeType::checkpoint) {}
    void printArgs(std::ostream& out, bool tty) const override;
    BB* deoptBranch();
};

/*
 * Replaces the current execution context with the one described by the
 * referenced framestate and jump to the deoptimized version of the
 * code at the point the framestate stores
 */

class Deopt : public FixedLenInstruction<Tag::Deopt, Deopt, 1, Effect::None,
                                         EnvAccess::None, Controlflow::Exit> {
  public:
    explicit Deopt(FrameState* frameState)
        : FixedLenInstruction(PirType::voyd(), {{NativeType::frameState}},
                              {{frameState}}) {}
    FrameState* frameState();
};

/*
 * if the test fails, jump to the deopt branch of the checkpoint.
 */

class FLI(Assume, 2, Effect::Order, EnvAccess::None) {
  public:
    bool assumeTrue = true;
    Assume(Value* test, Value* checkpoint)
        : FixedLenInstruction(PirType::voyd(),
                              {{NativeType::test, NativeType::checkpoint}},
                              {{test, checkpoint}}) {}

    Checkpoint* checkpoint() { return Checkpoint::Cast(arg(1).val()); }
    void checkpoint(Checkpoint* cp) { arg(1).val() = cp; }
    Value* condition() { return arg(0).val(); }
    Assume* Not() {
        assumeTrue = !assumeTrue;
        return this;
    }
    const char* name() const override {
        return assumeTrue ? "Assume" : "AssumeNot";
    }
};

class ScheduledDeopt
    : public VarLenInstruction<Tag::ScheduledDeopt, ScheduledDeopt,
                               Effect::None, EnvAccess::None,
                               Controlflow::Exit> {
  public:
    std::vector<FrameInfo> frames;
    ScheduledDeopt() : VarLenInstruction(PirType::voyd()) {}
    void consumeFrameStates(Deopt* deopt);
    void printArgs(std::ostream& out, bool tty) const override;
};

#undef FLI
#undef VLI
#undef FLIE
#undef VLIE
} // namespace pir
} // namespace rir

#endif
