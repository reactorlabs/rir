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
enum class HasEnvSlot : uint8_t { Yes, No };

// Effect that can be produced by an instruction.
enum class Effect : uint8_t {
    // Changes R_Visible
    Visibility,
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
    // Instruction might leak some of it's arguments
    LeakArg,

    ChangesContexts,
    ReadsEnv,
    WritesEnv,
    LeaksEnv,

    TriggerDeopt,

    // Instruction might execute more R code
    ExecuteCode,

    FIRST = Visibility,
    LAST = ExecuteCode,
};
typedef EnumSet<Effect> Effects;

// Controlflow of instruction.
enum class Controlflow : uint8_t {
    None,
    Exit,
    Branch,
};

// How an instruction modifies visibility
enum class VisibilityFlag : uint8_t {
    On,
    Off,
    Unknown,
};

class Instruction : public Value {
  public:
    struct InstructionUID : public std::pair<unsigned, unsigned> {
        InstructionUID(unsigned a, unsigned b)
            : std::pair<unsigned, unsigned>(a, b) {}
        unsigned bb() const { return first; }
        unsigned idx() const { return second; }
    };

    Instruction(Tag tag, PirType t, Effects effects, unsigned srcIdx)
        : Value(t, tag), effects(effects), srcIdx(srcIdx) {}

    Effects effects;

  public:
    void clearEffects() { effects.reset(); }
    void clearVisibility() { effects.reset(Effect::Visibility); }
    void clearLeaksEnv() { effects.reset(Effect::LeaksEnv); }
    bool hasEffect() const { return !effects.empty(); }
    bool hasVisibility() const { return effects.contains(Effect::Visibility); }

    Effects getObservableEffects() const {
        auto e = effects;
        // Those are effects, and we are required to have them in the correct
        // order. But they are not "doing" anything on their own. If e.g.
        // instructions with those effects are unused, we can remove them.
        e.reset(Effect::LeakArg);
        e.reset(Effect::ReadsEnv);
        e.reset(Effect::LeaksEnv);
        return e;
    }

    bool hasObservableEffects() const {
        return !getObservableEffects().empty();
    }

    bool hasImpureEffects() const {
        auto e = getObservableEffects();
        // Yes visibility is a global effect. We try to preserve it. But geting
        // it wrong is not a strong correctness issue.
        e.reset(Effect::Visibility);
        return !e.empty();
    }

    bool isDeoptBarrier() const { return hasImpureEffects(); }
    // TODO: Add verify, then replace with effects.includes(Effect::LeakArg)
    bool leaksArg(Value* val) const {
        return leaksEnv() || effects.includes(Effect::LeakArg);
    }

    void maskEffectsAndTypeOnNonObjects(PirType tmask,
                                        Effects mask = Effects(Effect::Error) |
                                                       Effect::Warn |
                                                       Effect::Visibility) {
        bool maybeObj = false;
        eachArg([&](Value* v) {
            if (mayHaveEnv() && env() == v)
                return;
            if (v->type.maybeObj())
                maybeObj = true;
        });
        if (!maybeObj) {
            effects = effects & mask;
            type = type & tmask;
        }
    }

    // Removes e if all arguments are argt
    void maskEffect(PirType argt, Effect e) {
        bool mask = true;
        eachArg([&](Value* v) {
            if (mayHaveEnv() && env() == v)
                return;
            if (!v->type.isA(argt))
                mask = false;
        });
        if (mask)
            effects.reset(e);
    }

    void updateScalarOnScalarInputs() {
        if (type.maybeObj())
            return;
        bool scalar = true;
        eachArg([&](Value* v) {
            if (mayHaveEnv() && env() == v)
                return;
            if (!v->type.isScalar())
                scalar = false;
        });
        if (scalar)
            type.setScalar();
    }

    bool readsEnv() const {
        return hasEnv() && effects.includes(Effect::ReadsEnv);
    }
    bool changesEnv() const {
        return hasEnv() && effects.includes(Effect::WritesEnv);
    }
    bool leaksEnv() const {
        return hasEnv() && effects.includes(Effect::LeaksEnv);
    }

    // Instructions can be deduplicated if they have different effects,
    // unless these effects are different
    Effects gvnEffects() const {
        return effects & ~(Effects(Effect::Error) | Effect::Warn |
                           Effect::Visibility | Effect::Force);
    }

    virtual size_t gvnBase() const = 0;

    virtual bool mayHaveEnv() const = 0;
    virtual bool hasEnv() const = 0;
    virtual bool exits() const = 0;
    virtual bool branches() const = 0;
    virtual bool branchOrExit() const = 0;
    virtual VisibilityFlag visibilityFlag() const = 0;

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
    void replaceUsesWithLimits(Value* val, BB* start,
                               Instruction* stop = nullptr);
    bool usesAreOnly(BB*, std::unordered_set<Tag>);
    bool usesDoNotInclude(BB*, std::unordered_set<Tag>);
    bool unused();

    virtual void updateType(){};

    virtual void printEffects(std::ostream& out, bool tty) const;
    virtual void printArgs(std::ostream& out, bool tty) const;
    virtual void printGraphArgs(std::ostream& out, bool tty) const;
    virtual void printGraphBranches(std::ostream& out, size_t bbId) const;
    virtual void printEnv(std::ostream& out, bool tty) const;
    virtual void print(std::ostream& out, bool tty = false) const;
    void printGraph(std::ostream& out, bool tty = false) const;
    void printRef(std::ostream& out) const override final;
    void print() const { print(std::cerr, true); }

    virtual InstrArg& arg(size_t pos) = 0;
    virtual const InstrArg& arg(size_t pos) const = 0;

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

    void eachArg(const Instruction::ArgumentValueIterator& it) const {
        for (size_t i = 0; i < nargs(); ++i)
            it(arg(i).val());
    }

    void eachArg(const Instruction::ArgumentIterator& it) const {
        for (size_t i = 0; i < nargs(); ++i)
            it(arg(i));
    }

    void eachArg(const Instruction::MutableArgumentIterator& it) {
        for (size_t i = 0; i < nargs(); ++i)
            it(arg(i));
    }

    void eachArgRev(const Instruction::ArgumentValueIterator& it) const {
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
        assert(!mayHaveEnv() && "subclass must override env() if it uses env");
        assert(false && "this instruction has no env");
        return nullptr;
    }
    virtual void env(Value* env) {
        assert(!mayHaveEnv() && "subclass must override env() if it uses env");
        assert(false && "this instruction has no env");
    }
    void elideEnv() { arg(envSlot()).val() = Env::elided(); }
    virtual size_t envSlot() const {
        assert(!mayHaveEnv() &&
               "subclass must override envSlot() if it uses env");
        assert(false && "this instruction has no env");
        return -1;
    }
};

template <Tag ITAG, class Base, Effects::StoreType INITIAL_EFFECTS,
          HasEnvSlot ENV, Controlflow CF, class ArgStore>
class InstructionImplementation : public Instruction {
  protected:
    ArgStore args_;

  public:
    InstructionImplementation(PirType resultType, unsigned srcIdx)
        : Instruction(ITAG, resultType, INITIAL_EFFECTS, srcIdx), args_({}) {}
    InstructionImplementation(PirType resultType, const ArgStore& args,
                              unsigned srcIdx)
        : Instruction(ITAG, resultType, INITIAL_EFFECTS, srcIdx), args_(args) {}

    InstructionImplementation& operator=(InstructionImplementation&) = delete;
    InstructionImplementation() = delete;

    Instruction* clone() const override {
        assert(Base::Cast(this));
        return new Base(*static_cast<const Base*>(this));
    }

    size_t gvnBase() const override {
        return hash_combine((size_t)ITAG, gvnEffects().to_i());
    };

    bool mayHaveEnv() const override final { return ENV == HasEnvSlot::Yes; }
    bool hasEnv() const override final {
        return mayHaveEnv() && env() != Env::elided();
    }
    bool exits() const override final { return CF == Controlflow::Exit; }
    bool branches() const override final { return CF == Controlflow::Branch; }
    bool branchOrExit() const override final { return branches() || exits(); }
    VisibilityFlag visibilityFlag() const override {
        return VisibilityFlag::Unknown;
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

template <Tag ITAG, class Base, size_t ARGS, Effects::StoreType INITIAL_EFFECT,
          HasEnvSlot ENV, Controlflow CF = Controlflow::None>
// cppcheck-suppress noConstructor
class FixedLenInstruction
    : public InstructionImplementation<ITAG, Base, INITIAL_EFFECT, ENV, CF,
                                       std::array<InstrArg, ARGS>> {
  public:
    typedef InstructionImplementation<ITAG, Base, INITIAL_EFFECT, ENV, CF,
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

template <Tag ITAG, class Base, size_t ARGS, Effects::StoreType INITIAL_EFFECT,
          HasEnvSlot ENV, Controlflow CF = Controlflow::None>
class FixedLenInstructionWithEnvSlot
    : public FixedLenInstruction<ITAG, Base, ARGS, INITIAL_EFFECT, ENV, CF> {
  public:
    typedef FixedLenInstruction<ITAG, Base, ARGS, INITIAL_EFFECT, ENV, CF>
        Super;
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

template <Tag ITAG, class Base, Effects::StoreType INITIAL_EFFECT,
          HasEnvSlot ENV, Controlflow CF = Controlflow::None>
class VarLenInstruction
    : public InstructionImplementation<ITAG, Base, INITIAL_EFFECT, ENV, CF,
                                       std::vector<InstrArg>> {

  public:
    typedef InstructionImplementation<ITAG, Base, INITIAL_EFFECT, ENV, CF,
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

template <Tag ITAG, class Base, Effects::StoreType INITIAL_EFFECT,
          HasEnvSlot ENV, Controlflow CF = Controlflow::None>
class VarLenInstructionWithEnvSlot
    : public VarLenInstruction<ITAG, Base, INITIAL_EFFECT, ENV, CF> {
  public:
    typedef VarLenInstruction<ITAG, Base, INITIAL_EFFECT, ENV, CF> Super;
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

#define FLI(type, nargs, io)                                                   \
    type:                                                                      \
  public                                                                       \
    FixedLenInstruction<Tag::type, type, nargs, Effects(io), HasEnvSlot::No>

#define FLIE(type, nargs, io)                                                  \
    type:                                                                      \
  public                                                                       \
    FixedLenInstructionWithEnvSlot<Tag::type, type, nargs, Effects(io),        \
                                   HasEnvSlot::Yes>

#define VLI(type, io)                                                          \
    type:                                                                      \
  public                                                                       \
    VarLenInstruction<Tag::type, type, Effects(io), HasEnvSlot::No>

#define VLIE(type, io)                                                         \
    type:                                                                      \
  public                                                                       \
    VarLenInstructionWithEnvSlot<Tag::type, type, Effects(io), HasEnvSlot::Yes>

class FLI(LdConst, 0, Effects::None()) {
  public:
    BC::PoolIdx idx;
    SEXP c() const;
    LdConst(SEXP c, PirType t);
    explicit LdConst(SEXP c);
    explicit LdConst(int i);
    void printArgs(std::ostream& out, bool tty) const override;
    size_t gvnBase() const override {
        return hash_combine(InstructionImplementation::gvnBase(), c());
    }
    int minReferenceCount() const override { return MAX_REFCOUNT; }
};

class FLIE(LdFun, 2, Effects::Any()) {
  public:
    SEXP varName;
    SEXP hint = nullptr;

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

    size_t gvnBase() const override {
        return hash_combine(InstructionImplementation::gvnBase(), varName);
    }

    int minReferenceCount() const override { return MAX_REFCOUNT; }
};

class FLIE(LdVar, 1, Effects() | Effect::Error | Effect::ReadsEnv) {
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

    size_t gvnBase() const override {
        return hash_combine(InstructionImplementation::gvnBase(), varName);
    }

    int minReferenceCount() const override { return 1; }
};

class FLI(ForSeqSize, 1, Effect::Error) {
  public:
    explicit ForSeqSize(Value* val)
        : FixedLenInstruction(PirType(RType::integer).scalar().notObject(),
                              {{PirType::val()}}, {{val}}) {}
};

class FLI(LdArg, 0, Effects::None()) {
  public:
    size_t id;

    explicit LdArg(size_t id) : FixedLenInstruction(PirType::any()), id(id) {}

    void printArgs(std::ostream& out, bool tty) const override;

    size_t gvnBase() const override {
        return hash_combine(InstructionImplementation::gvnBase(), id);
    }
    int minReferenceCount() const override { return MAX_REFCOUNT; }
};

class FLIE(Missing, 1, Effects() | Effect::ReadsEnv) {
  public:
    SEXP varName;
    explicit Missing(SEXP varName, Value* env)
        : FixedLenInstructionWithEnvSlot(PirType::simpleScalarLogical(), env),
          varName(varName) {}
    void printArgs(std::ostream& out, bool tty) const override;

    size_t gvnBase() const override {
        return hash_combine(InstructionImplementation::gvnBase(), varName);
    }
};

class FLI(ChkMissing, 1, Effect::Warn) {
  public:
    explicit ChkMissing(Value* in)
        : FixedLenInstruction(in->type.notMissing(), {{PirType::any()}},
                              {{in}}) {}
};

class FLI(ChkClosure, 1, Effect::Warn) {
  public:
    explicit ChkClosure(Value* in)
        : FixedLenInstruction(RType::closure, {{PirType::val()}}, {{in}}) {}
};

class FLIE(StVarSuper, 2, Effects() | Effect::ReadsEnv | Effect::WritesEnv) {
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

    size_t gvnBase() const override {
        return hash_combine(InstructionImplementation::gvnBase(), varName);
    }
};

class FLIE(LdVarSuper, 1, Effects() | Effect::Error | Effect::ReadsEnv) {
  public:
    LdVarSuper(SEXP name, Value* env)
        : FixedLenInstructionWithEnvSlot(PirType::any(), env), varName(name) {}

    LdVarSuper(const char* name, Value* env)
        : FixedLenInstructionWithEnvSlot(PirType::any(), env),
          varName(Rf_install(name)) {}

    SEXP varName;

    void printArgs(std::ostream& out, bool tty) const override;

    size_t gvnBase() const override {
        return hash_combine(InstructionImplementation::gvnBase(), varName);
    }

    int minReferenceCount() const override { return 1; }
};

class FLIE(StVar, 2, Effect::WritesEnv) {
  public:
    bool isStArg = false;
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

    size_t gvnBase() const override {
        return hash_combine(InstructionImplementation::gvnBase(), varName);
    }
};

// Pseudo Instruction. Is actually a StVar with a flag set.
class StArg : public StVar {
  public:
    StArg(SEXP name, Value* val, Value* env) : StVar(name, val, env) {
        arg<0>().type() = PirType::any();
        isStArg = true;
    }
};

class Branch
    : public FixedLenInstruction<Tag::Branch, Branch, 1, Effects::None(),
                                 HasEnvSlot::No, Controlflow::Branch> {
  public:
    explicit Branch(Value* test)
        : FixedLenInstruction(PirType::voyd(), {{NativeType::test}}, {{test}}) {
    }
    void printArgs(std::ostream& out, bool tty) const override;
    void printGraphArgs(std::ostream& out, bool tty) const override;
    void printGraphBranches(std::ostream& out, size_t bbId) const override;
};

class Return
    : public FixedLenInstruction<Tag::Return, Return, 1, Effects::None(),
                                 HasEnvSlot::No, Controlflow::Exit> {
  public:
    explicit Return(Value* ret)
        : FixedLenInstruction(PirType::voyd(), {{PirType::val()}}, {{ret}}) {}
};

class Promise;
class FLIE(MkArg, 2, Effects::None()) {
    Promise* prom_;

  public:
    bool noReflection = false;

    MkArg(Promise* prom, Value* v, Value* env)
        : FixedLenInstructionWithEnvSlot(RType::prom, {{PirType::val()}}, {{v}},
                                         env),
          prom_(prom) {
        assert(eagerArg() == v);
    }
    MkArg(Value* v, Value* env)
        : FixedLenInstructionWithEnvSlot(RType::prom, {{PirType::val()}}, {{v}},
                                         env),
          prom_(nullptr) {
        assert(eagerArg() == v);
    }

    Value* eagerArg() const { return arg(0).val(); }
    void eagerArg(Value* eager) { arg(0).val() = eager; }

    void updatePromise(Promise* p) { prom_ = p; }
    Promise* prom() const { return prom_; }

    bool isEager() const { return eagerArg() != UnboundValue::instance(); }

    void printArgs(std::ostream& out, bool tty) const override;

    Value* promEnv() const { return env(); }

    size_t gvnBase() const override {
        return hash_combine(InstructionImplementation::gvnBase(), prom_);
    }

    int minReferenceCount() const override { return MAX_REFCOUNT; }
};

class FLI(Seq, 3, Effects::None()) {
  public:
    Seq(Value* start, Value* end, Value* step)
        : FixedLenInstruction(
              PirType::any(),
              // TODO: require scalars, but this needs some cast support
              {{PirType::val(), PirType::val(), PirType::val()}},
              {{start, end, step}}) {}
    void updateType() override final {
        maskEffectsAndTypeOnNonObjects(PirType::num().notObject());
    }
};

class FLIE(MkCls, 4, Effects::None()) {
  public:
    MkCls(Value* fml, Value* code, Value* src, Value* lexicalEnv)
        : FixedLenInstructionWithEnvSlot(
              RType::closure, {{PirType::list(), RType::code, PirType::any()}},
              {{fml, code, src}}, lexicalEnv) {}

    Value* lexicalEnv() const { return env(); }

    int minReferenceCount() const override { return MAX_REFCOUNT; }

  private:
    using FixedLenInstructionWithEnvSlot::env;
};

class FLIE(MkFunCls, 1, Effects::None()) {
  public:
    Closure* cls;
    DispatchTable* originalBody;
    MkFunCls(Closure* cls, DispatchTable* originalBody, Value* lexicalEnv);
    void printArgs(std::ostream&, bool tty) const override;

    Value* lexicalEnv() const { return env(); }

    size_t gvnBase() const override {
        return hash_combine(InstructionImplementation::gvnBase(), cls);
    }

    int minReferenceCount() const override { return MAX_REFCOUNT; }
};

class FLIE(Force, 2, Effects::Any()) {
  public:
    // Set to true if we are sure that the promise will be forced here
    bool strict = false;
    Force(Value* in, Value* env)
        : FixedLenInstructionWithEnvSlot(in->type.forced(), {{PirType::any()}},
                                         {{in}}, env) {}
    Value* input() const { return arg(0).val(); }
    const char* name() const override { return strict ? "Force!" : "Force"; }
    void updateType() override final {
        type = arg<0>().val()->type.forced();
        if (!input()->type.maybeLazy()) {
            effects.reset();
        }
    }
    int minReferenceCount() const override { return MAX_REFCOUNT; }
};

class FLI(CastType, 1, Effects::None()) {
  public:
    CastType(Value* in, PirType from, PirType to)
        : FixedLenInstruction(to, {{from}}, {{in}}) {}
};

class FLI(AsLogical, 1, Effect::Error) {
  public:
    Value* val() { return arg<0>().val(); }

    AsLogical(Value* in, unsigned srcIdx)
        : FixedLenInstruction(PirType::simpleScalarLogical(),
                              {{PirType::val()}}, {{in}}, srcIdx) {}

    void updateType() override final {
        if (val()->type.isA((PirType() | RType::logical | RType::integer |
                             RType::real | RType::str | RType::cplx)
                                .notObject())) {
            effects.reset(Effect::Error);
        }
    }
};

class FLI(AsTest, 1, Effects() | Effect::Error | Effect::Warn) {
  public:
    Value* val() { return arg<0>().val(); }

    explicit AsTest(Value* in)
        : FixedLenInstruction(NativeType::test, {{PirType::val()}}, {{in}}) {}

    void updateType() override final {
        if (val()->type.isScalar())
            effects.reset(Effect::Warn);
        // Error on NA, hard to exclude
    }
};

class FLI(AsInt, 1, Effect::Error) {
  public:
    bool ceil;

    explicit AsInt(Value* in, bool ceil_)
        : FixedLenInstruction(PirType(RType::integer).scalar().notObject(),
                              {{PirType::any()}}, {{in}}),
          ceil(ceil_) {}

    size_t gvnBase() const override {
        return hash_combine(InstructionImplementation::gvnBase(), ceil);
    }
};

class FLIE(Subassign1_1D, 4, Effects::Any()) {
  public:
    Subassign1_1D(Value* val, Value* vec, Value* idx, Value* env,
                  unsigned srcIdx)
        : FixedLenInstructionWithEnvSlot(
              PirType::valOrLazy(),
              {{PirType::val(), PirType::val(), PirType::val()}},
              {{val, vec, idx}}, env, srcIdx) {}
    Value* rhs() { return arg(0).val(); }
    Value* lhs() { return arg(1).val(); }
    Value* idx() { return arg(2).val(); }
    void updateType() override final {
        maskEffectsAndTypeOnNonObjects(lhs()->type | rhs()->type);
    }
};

class FLIE(Subassign2_1D, 4, Effects::Any()) {
  public:
    Subassign2_1D(Value* val, Value* vec, Value* idx, Value* env,
                  unsigned srcIdx)
        : FixedLenInstructionWithEnvSlot(
              PirType::valOrLazy(),
              {{PirType::val(), PirType::val(), PirType::val()}},
              {{val, vec, idx}}, env, srcIdx) {}
    Value* rhs() { return arg(0).val(); }
    Value* lhs() { return arg(1).val(); }
    Value* idx() { return arg(2).val(); }
    void updateType() override final {
        maskEffectsAndTypeOnNonObjects(lhs()->type | rhs()->type);
    }
};

class FLIE(Subassign1_2D, 5, Effects::Any()) {
  public:
    Subassign1_2D(Value* val, Value* mtx, Value* idx1, Value* idx2, Value* env,
                  unsigned srcIdx)
        : FixedLenInstructionWithEnvSlot(PirType::valOrLazy(),
                                         {{PirType::val(), PirType::val(),
                                           PirType::val(), PirType::val()}},
                                         {{val, mtx, idx1, idx2}}, env,
                                         srcIdx) {}
    Value* rhs() { return arg(0).val(); }
    Value* lhs() { return arg(1).val(); }
    Value* idx1() { return arg(2).val(); }
    Value* idx2() { return arg(3).val(); }
    void updateType() override final {
        maskEffectsAndTypeOnNonObjects(lhs()->type | rhs()->type);
    }
};

class FLIE(Subassign2_2D, 5, Effects::Any()) {
  public:
    Subassign2_2D(Value* val, Value* mtx, Value* idx1, Value* idx2, Value* env,
                  unsigned srcIdx)
        : FixedLenInstructionWithEnvSlot(PirType::valOrLazy(),
                                         {{PirType::val(), PirType::val(),
                                           PirType::val(), PirType::val()}},
                                         {{val, mtx, idx1, idx2}}, env,
                                         srcIdx) {}
    Value* rhs() { return arg(0).val(); }
    Value* lhs() { return arg(1).val(); }
    Value* idx1() { return arg(2).val(); }
    Value* idx2() { return arg(3).val(); }
    void updateType() override final {
        maskEffectsAndTypeOnNonObjects(lhs()->type | rhs()->type);
    }
};

class FLIE(Extract1_1D, 3, Effects::Any()) {
  public:
    Extract1_1D(Value* vec, Value* idx, Value* env, unsigned srcIdx)
        : FixedLenInstructionWithEnvSlot(PirType::valOrLazy(),
                                         {{PirType::val(), PirType::val()}},
                                         {{vec, idx}}, env, srcIdx) {}
    Value* vec() { return arg(0).val(); }
    Value* idx() { return arg(1).val(); }
    void updateType() override final {
        maskEffectsAndTypeOnNonObjects(vec()->type.elem());
    }
};

class FLIE(Extract2_1D, 3, Effects::Any()) {
  public:
    Extract2_1D(Value* vec, Value* idx, Value* env, unsigned srcIdx)
        : FixedLenInstructionWithEnvSlot(PirType::valOrLazy(),
                                         {{PirType::val(), PirType::val()}},
                                         {{vec, idx}}, env, srcIdx) {}
    Value* vec() { return arg(0).val(); }
    Value* idx() { return arg(1).val(); }
    void updateType() override final {
        maskEffectsAndTypeOnNonObjects(vec()->type.elem());
    }
};

class FLIE(Extract1_2D, 4, Effects::Any()) {
  public:
    Extract1_2D(Value* vec, Value* idx1, Value* idx2, Value* env,
                unsigned srcIdx)
        : FixedLenInstructionWithEnvSlot(
              PirType::valOrLazy(),
              {{PirType::val(), PirType::val(), PirType::val()}},
              {{vec, idx1, idx2}}, env, srcIdx) {}
    Value* vec() { return arg(0).val(); }
    Value* idx1() { return arg(1).val(); }
    Value* idx2() { return arg(2).val(); }
    void updateType() override final {
        maskEffectsAndTypeOnNonObjects(vec()->type.elem());
    }
};

class FLIE(Extract2_2D, 4, Effects::Any()) {
  public:
    Extract2_2D(Value* vec, Value* idx1, Value* idx2, Value* env,
                unsigned srcIdx)
        : FixedLenInstructionWithEnvSlot(
              PirType::valOrLazy(),
              {{PirType::val(), PirType::val(), PirType::val()}},
              {{vec, idx1, idx2}}, env, srcIdx) {}
    Value* vec() { return arg(0).val(); }
    Value* idx1() { return arg(1).val(); }
    Value* idx2() { return arg(2).val(); }
    void updateType() override final {
        maskEffectsAndTypeOnNonObjects(vec()->type.elem());
    }
};

class FLI(Inc, 1, Effects::None()) {
  public:
    explicit Inc(Value* v)
        : FixedLenInstruction(PirType(RType::integer).scalar().notObject(),
                              {{PirType(RType::integer).scalar().notObject()}},
                              {{v}}) {}
};

class FLI(Dec, 1, Effects::None()) {
  public:
    explicit Dec(Value* v)
        : FixedLenInstruction(PirType(RType::integer).scalar().notObject(),
                              {{PirType(RType::integer).scalar().notObject()}},
                              {{v}}) {}
};

class FLI(Is, 1, Effects::None()) {
  public:
    Is(uint32_t sexpTag, Value* v)
        : FixedLenInstruction(PirType::simpleScalarLogical(),
                              {{PirType::val()}}, {{v}}),
          sexpTag(sexpTag) {}
    uint32_t sexpTag;

    void printArgs(std::ostream& out, bool tty) const override;
};

class FLI(IsType, 1, Effects::None()) {
  public:
    const PirType typeTest;
    IsType(PirType type, Value* v)
        : FixedLenInstruction(NativeType::test, {{PirType::any()}}, {{v}}),
          typeTest(type) {}

    void printArgs(std::ostream& out, bool tty) const override;
};

class FLI(LdFunctionEnv, 0, Effects::None()) {
  public:
    LdFunctionEnv() : FixedLenInstruction(RType::env) {}
};

class FLI(Visible, 0, Effect::Visibility) {
  public:
    explicit Visible() : FixedLenInstruction(PirType::voyd()) {}
    VisibilityFlag visibilityFlag() const override {
        return VisibilityFlag::On;
    }
};

class FLI(Invisible, 0, Effect::Visibility) {
  public:
    explicit Invisible() : FixedLenInstruction(PirType::voyd()) {}
    VisibilityFlag visibilityFlag() const override {
        return VisibilityFlag::Off;
    }
};

class FLI(PirCopy, 1, Effects::None()) {
  public:
    explicit PirCopy(Value* v)
        : FixedLenInstruction(v->type, {{v->type}}, {{v}}) {}
    void print(std::ostream& out, bool tty) const override;
    void updateType() override final { type = arg<0>().val()->type; }
    int minReferenceCount() const override {
        return arg<0>().val()->minReferenceCount();
    }
};

// Effects::Any() prevents this instruction from being optimized away
class FLI(Nop, 0, Effects::Any()) {
  public:
    explicit Nop() : FixedLenInstruction(PirType::voyd()) {}
};

class FLI(Identical, 2, Effects::None()) {
  public:
    Identical(Value* a, Value* b)
        : FixedLenInstruction(NativeType::test,
                              {{PirType::any(), PirType::any()}}, {{a, b}}) {}
};

class FLIE(Colon, 3, Effects::Any()) {
  public:
    Colon(Value* lhs, Value* rhs, Value* env, unsigned srcIdx)
        : FixedLenInstructionWithEnvSlot(PirType::valOrLazy(),
                                         {{PirType::val(), PirType::val()}},
                                         {{lhs, rhs}}, env, srcIdx) {}
    void updateType() override final {}
    VisibilityFlag visibilityFlag() const override {
        if (lhs()->type.isA(PirType::simpleScalar()) &&
            rhs()->type.isA(PirType::simpleScalar())) {
            return VisibilityFlag::On;
        } else {
            return VisibilityFlag::Unknown;
        }
    }
    Value* lhs() const { return arg<0>().val(); }
    Value* rhs() const { return arg<1>().val(); }
};

#define V(NESTED, name, Name)                                                  \
    class FLI(Name, 0, Effects::Any()) {                                       \
      public:                                                                  \
        Name() : FixedLenInstruction(PirType::voyd()) {}                       \
    };
SIMPLE_INSTRUCTIONS(V, _)
#undef V

#define BINOP(Name, Type, SafeType)                                            \
    class FLIE(Name, 3, Effects::Any()) {                                      \
      public:                                                                  \
        Name(Value* lhs, Value* rhs, Value* env, unsigned srcIdx)              \
            : FixedLenInstructionWithEnvSlot(                                  \
                  PirType::valOrLazy(), {{PirType::val(), PirType::val()}},    \
                  {{lhs, rhs}}, env, srcIdx) {}                                \
        VisibilityFlag visibilityFlag() const override {                       \
            if (lhs()->type.isA(PirType::num().notObject()) &&                 \
                rhs()->type.isA(PirType::num().notObject())) {                 \
                return VisibilityFlag::On;                                     \
            } else {                                                           \
                return VisibilityFlag::Unknown;                                \
            }                                                                  \
        }                                                                      \
        void updateType() override final {                                     \
            maskEffectsAndTypeOnNonObjects(Type);                              \
            maskEffect(SafeType.notObject(), Effect::Warn);                    \
            maskEffect(SafeType.notObject().scalar(), Effect::Error);          \
            updateScalarOnScalarInputs();                                      \
        }                                                                      \
        Value* lhs() const { return arg<0>().val(); }                          \
        Value* rhs() const { return arg<1>().val(); }                          \
    }

BINOP(Mul, lhs()->type | rhs()->type, PirType::num());
BINOP(Div, PirType::val().notObject(), PirType::num());
BINOP(IDiv, lhs()->type | rhs()->type, PirType::num());
BINOP(Mod, lhs()->type | rhs()->type, PirType::num());
BINOP(Add, lhs()->type | rhs()->type, PirType::num());
BINOP(Pow, lhs()->type | rhs()->type, PirType::num());
BINOP(Sub, lhs()->type | rhs()->type, PirType::num());
BINOP(Gte, PirType(RType::logical).notObject(), PirType::atomOrSimpleVec());
BINOP(Lte, PirType(RType::logical).notObject(), PirType::atomOrSimpleVec());
BINOP(Gt, PirType(RType::logical).notObject(), PirType::atomOrSimpleVec());
BINOP(Lt, PirType(RType::logical).notObject(), PirType::atomOrSimpleVec());
BINOP(Neq, PirType(RType::logical).notObject(), PirType::atomOrSimpleVec());
BINOP(Eq, PirType(RType::logical).notObject(), PirType::atomOrSimpleVec());

#undef BINOP

#define BINOP_NOENV(Name, Type)                                                \
    class FLI(Name, 2, Effects::None()) {                                      \
      public:                                                                  \
        Name(Value* lhs, Value* rhs)                                           \
            : FixedLenInstruction(Type, {{PirType::val(), PirType::val()}},    \
                                  {{lhs, rhs}}) {}                             \
    }

BINOP_NOENV(LAnd, PirType::simpleScalarLogical());
BINOP_NOENV(LOr, PirType::simpleScalarLogical());

#undef BINOP_NOENV

#define UNOP(Name, SafeType)                                                   \
    class FLIE(Name, 2, Effects::Any()) {                                      \
      public:                                                                  \
        Name(Value* v, Value* env, unsigned srcIdx)                            \
            : FixedLenInstructionWithEnvSlot(PirType::valOrLazy(),             \
                                             {{PirType::val()}}, {{v}}, env,   \
                                             srcIdx) {}                        \
        VisibilityFlag visibilityFlag() const override {                       \
            if (arg<0>().val()->type.isA(PirType::num().notObject())) {        \
                return VisibilityFlag::On;                                     \
            } else {                                                           \
                return VisibilityFlag::Unknown;                                \
            }                                                                  \
        }                                                                      \
        void updateType() override final {                                     \
            maskEffectsAndTypeOnNonObjects(arg<0>().val()->type);              \
            maskEffect(SafeType.notObject(), Effect::Warn);                    \
            maskEffect(SafeType.notObject().scalar(), Effect::Error);          \
            updateScalarOnScalarInputs();                                      \
        }                                                                      \
    }

UNOP(Not, PirType::num());
UNOP(Plus, PirType::num());
UNOP(Minus, PirType::num());

#undef UNOP

class FLI(Length, 1, Effects::None()) {
  public:
    explicit Length(Value* v)
        : FixedLenInstruction(PirType::simpleScalarInt(), {{PirType::val()}},
                              {{v}}) {}
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

/*
 *  Collects metadata about the current state of variables
 *  eventually needed for deoptimization purposes
 */
class VLIE(FrameState, Effect::LeaksEnv) {
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
        auto& pos = arg(stackSize);
        assert(pos.type() == NativeType::frameState);
        pos.val() = s;
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

    size_t gvnBase() const override { return (size_t)this; }
};

// Common interface to all call instructions
class CallInstruction {
  public:
    virtual size_t nCallArgs() const = 0;
    virtual void
    eachCallArg(const Instruction::ArgumentValueIterator& it) const = 0;
    virtual void
    eachCallArg(const Instruction::MutableArgumentIterator& it) = 0;
    static CallInstruction* CastCall(Value* v);
    virtual void clearFrameState(){};
    virtual Closure* tryGetCls() const { return nullptr; }
    Assumptions inferAvailableAssumptions() const;
    virtual bool hasNamedArgs() const { return false; }
    ClosureVersion* tryDispatch(Closure*) const;
};

// Default call instruction. Closure expression (ie. expr left of `(`) is
// evaluated at runtime and arguments are passed as promises.
class VLIE(Call, Effects::Any()), public CallInstruction {
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
            pushArg(args[i], PirType(RType::prom) | RType::missing);
    }

    Closure* tryGetCls() const override final {
        if (auto mk = MkFunCls::Cast(cls()->followCastsAndForce()))
            return mk->cls;
        return nullptr;
    }

    size_t nCallArgs() const override { return nargs() - 3; };
    void eachCallArg(const Instruction::ArgumentValueIterator& it)
        const override {
        for (size_t i = 0; i < nCallArgs(); ++i)
            it(arg(i + 2).val());
    }
    void eachCallArg(const Instruction::MutableArgumentIterator& it) override {
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

class VLIE(NamedCall, Effects::Any()), public CallInstruction {
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
    void eachCallArg(const Instruction::ArgumentValueIterator& it)
        const override {
        for (size_t i = 0; i < nCallArgs(); ++i)
            it(arg(i + 1).val());
    }
    void eachCallArg(const Instruction::MutableArgumentIterator& it) override {
        for (size_t i = 0; i < nCallArgs(); ++i)
            it(arg(i + 1));
    }

    Value* callerEnv() { return env(); }
    void printArgs(std::ostream & out, bool tty) const override;
};

// Call instruction for lazy, but staticatlly resolved calls. Closure is
// specified as `cls_`, args passed as promises.
class VLIE(StaticCall, Effects::Any()), public CallInstruction {
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
    void eachCallArg(const Instruction::ArgumentValueIterator& it)
        const override {
        for (size_t i = 0; i < nCallArgs(); ++i)
            it(arg(i + 1).val());
    }
    void eachCallArg(const Instruction::MutableArgumentIterator& it) override {
        for (size_t i = 0; i < nCallArgs(); ++i)
            it(arg(i + 1));
    }

    const FrameState* frameState() const {
        return FrameState::Cast(arg(0).val());
    }
    void clearFrameState() override { arg(0).val() = Tombstone::framestate(); };

    void printArgs(std::ostream & out, bool tty) const override;
    Value* callerEnv() { return env(); }

    ClosureVersion* tryDispatch() const;

    ClosureVersion* tryOptimisticDispatch() const;

    size_t gvnBase() const override {
        return hash_combine(InstructionImplementation::gvnBase(), cls_);
    }
};

typedef SEXP (*CCODE)(SEXP, SEXP, SEXP, SEXP);

class VLIE(CallBuiltin, Effects::Any()), public CallInstruction {
  public:
    SEXP blt;
    const CCODE builtin;
    int builtinId;

    size_t nCallArgs() const override { return nargs() - 1; };
    void eachCallArg(const Instruction::ArgumentValueIterator& it)
        const override {
        for (size_t i = 0; i < nCallArgs(); ++i)
            it(arg(i).val());
    }
    void eachCallArg(const Instruction::MutableArgumentIterator& it) override {
        for (size_t i = 0; i < nCallArgs(); ++i)
            it(arg(i));
    }
    void printArgs(std::ostream & out, bool tty) const override;
    Value* callerEnv() { return env(); }

    size_t gvnBase() const override {
        return hash_combine(InstructionImplementation::gvnBase(), blt);
    }

    VisibilityFlag visibilityFlag() const override;

  private:
    CallBuiltin(Value * callerEnv, SEXP builtin,
                const std::vector<Value*>& args, unsigned srcIdx);
    friend class BuiltinCallFactory;
};

class VLI(CallSafeBuiltin,
          Effects(Effect::Warn) | Effect::Error | Effect::Visibility),
    public CallInstruction {
  public:
    SEXP blt;
    const CCODE builtin;
    int builtinId;

    size_t nCallArgs() const override { return nargs(); };
    void eachCallArg(const Instruction::ArgumentValueIterator& it)
        const override {
        eachArg(it);
    }
    void eachCallArg(const Instruction::MutableArgumentIterator& it) override {
        eachArg(it);
    }

    void printArgs(std::ostream & out, bool tty) const override;

    CallSafeBuiltin(SEXP builtin, const std::vector<Value*>& args,
                    unsigned srcIdx);

    size_t gvnBase() const override {
        return hash_combine(InstructionImplementation::gvnBase(), blt);
    }

    VisibilityFlag visibilityFlag() const override;
};

class BuiltinCallFactory {
  public:
    static Instruction* New(Value* callerEnv, SEXP builtin,
                            const std::vector<Value*>& args, unsigned srcIdx);
};

class VLIE(MkEnv, Effects::None()) {
  public:
    std::vector<SEXP> varName;
    bool stub = false;
    int context = 1;

    typedef std::function<void(SEXP name, Value* val)> LocalVarIt;
    typedef std::function<void(SEXP name, InstrArg&)> MutableLocalVarIt;

    RIR_INLINE void eachLocalVar(MutableLocalVarIt it) {
        for (size_t i = 0; i < envSlot(); ++i)
            it(varName[i], arg(i));
    }

    RIR_INLINE void eachLocalVar(LocalVarIt it) const {
        for (size_t i = 0; i < envSlot(); ++i)
            it(varName[i], arg(i).val());
    }

    RIR_INLINE void eachLocalVarRev(LocalVarIt it) const {
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
    const char* name() const override { return stub ? "(MkEnv)" : "MKEnv"; }

    size_t nLocals() { return nargs() - 1; }

    size_t gvnBase() const override { return (size_t)this; }

    int minReferenceCount() const override { return MAX_REFCOUNT; }
};

class FLI(IsObject, 1, Effects::None()) {
  public:
    explicit IsObject(Value* v)
        : FixedLenInstruction(NativeType::test, {{PirType::val()}}, {{v}}) {}
};

class FLIE(IsEnvStub, 1, Effects::None()) {
  public:
    explicit IsEnvStub(MkEnv* e)
        : FixedLenInstructionWithEnvSlot(NativeType::test, e) {}
};

class FLIE(PushContext, 3, Effect::ChangesContexts) {
  public:
    PushContext(Value* ast, Value* op, Value* sysparent)
        : FixedLenInstructionWithEnvSlot(NativeType::context,
                                         {{PirType::any(), PirType::closure()}},
                                         {{ast, op}}, sysparent) {}
};

class FLI(PopContext, 2, Effect::ChangesContexts) {
  public:
    PopContext(Value* res, PushContext* push)
        : FixedLenInstruction(PirType::voyd(),
                              {{PirType::any(), NativeType::context}},
                              {{res, push}}) {}
    PushContext* push() { return PushContext::Cast(arg<1>().val()); }
};

class VLI(Phi, Effects::None()) {
    std::vector<BB*> input;

  public:
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
        SLOWASSERT(std::find(input.begin(), input.end(), in) == input.end() &&
                   "Duplicate PHI input block");
        input.push_back(in);
        args_.push_back(InstrArg(arg, PirType::any()));
    }
    BB* inputAt(size_t i) const { return input.at(i); }
    void updateInputAt(size_t i, BB* bb) {
        SLOWASSERT(std::find(input.begin(), input.end(), bb) == input.end() &&
                   "Duplicate PHI input block");
        input[i] = bb;
    }
    const std::vector<BB*>& inputs() { return input; }
    void removeInputs(const std::unordered_set<BB*>& del);

    typedef std::function<void(BB* bb, Value*)> PhiArgumentIterator;
    void eachArg(const PhiArgumentIterator& it) const {
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
class Checkpoint : public FixedLenInstruction<Tag::Checkpoint, Checkpoint, 0,
                                              Effects::None(), HasEnvSlot::No,
                                              Controlflow::Branch> {
  public:
    Checkpoint() : FixedLenInstruction(NativeType::checkpoint) {}
    void printArgs(std::ostream& out, bool tty) const override;
    void printGraphArgs(std::ostream& out, bool tty) const override;
    void printGraphBranches(std::ostream& out, size_t bbId) const override;
    BB* deoptBranch();
};

/*
 * Replaces the current execution context with the one described by the
 * referenced framestate and jump to the deoptimized version of the
 * code at the point the framestate stores
 */

class Deopt : public FixedLenInstruction<Tag::Deopt, Deopt, 1, Effects::Any(),
                                         HasEnvSlot::No, Controlflow::Exit> {
  public:
    explicit Deopt(FrameState* frameState)
        : FixedLenInstruction(PirType::voyd(), {{NativeType::frameState}},
                              {{frameState}}) {}
    FrameState* frameState();
};

/*
 * if the test fails, jump to the deopt branch of the checkpoint.
 */

class FLI(Assume, 2, Effect::TriggerDeopt) {
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
                               Effects::None(), HasEnvSlot::No,
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
