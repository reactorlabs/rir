#ifndef COMPILER_INSTRUCTION_H
#define COMPILER_INSTRUCTION_H

#include "R/r.h"
#include "bc/BC_inc.h"
#include "compiler/rir2pir/rir2pir.h"
#include "env.h"
#include "instruction_list.h"
#include "pir.h"
#include "runtime/ArglistOrder.h"
#include "runtime/Deoptimization.h"
#include "runtime/TypeFeedback.h"
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
#include <set>
#include <sstream>
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
    // Instruction might produce a warning. Example: CheckTrueFalse warns if the
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
    LeaksArg,

    ChangesContexts,
    ReadsEnv,
    WritesEnv,
    LeaksEnv,

    TriggerDeopt,

    // Instruction might execute more R code
    ExecuteCode,

    UpdatesMetadata,

    // If we speculatively optimize an instruction then we must set this flag
    // to avoid it getting hoisted over its assumption. Take care when removing
    // or masking this flag. Most of the time it is not correct to remove it,
    // e.g. the type of inputs to an instructions might already be based on
    // assumptions.
    DependsOnAssume,

    // Modifies an input. For example update promise.
    MutatesArgument,

    FIRST = Visibility,
    LAST = MutatesArgument,
};
typedef EnumSet<Effect, uint16_t> Effects;

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

struct TypeFeedback {
    PirType type = PirType::optimistic();
    Value* value = nullptr;
    FeedbackOrigin feedbackOrigin;
};
struct CallFeedback {
    FeedbackOrigin feedbackOrigin;
    size_t taken = 0;
    SEXP monomorphic = nullptr;
    SEXPTYPE type = NILSXP;
    bool stableEnv = false;
};

class DominanceGraph;
class MkEnv;
class FrameState;
class Instruction : public Value {
  public:
    struct InstructionUID : public std::pair<unsigned, unsigned> {
        InstructionUID(unsigned a, unsigned b)
            : std::pair<unsigned, unsigned>(a, b) {}
        unsigned bb() const { return first; }
        unsigned idx() const { return second; }
    };

    Instruction(Tag tag, PirType t, Effects effects, unsigned srcIdx)
        : Value(t, tag), effects(effects), typeFeedback_(nullptr),
          srcIdx(srcIdx) {}

    Effects effects;

  public:
    bool deleted = false;
    bool typeFeedbackUsed = false;
    void clearEffects() { effects.reset(); }
    void clearVisibility() { effects.reset(Effect::Visibility); }
    void clearLeaksEnv() { effects.reset(Effect::LeaksEnv); }
    bool hasEffect() const { return !effects.empty(); }
    bool hasVisibility() const { return effects.contains(Effect::Visibility); }
    bool mayUseReflection() const {
        return effects.contains(Effect::Reflection);
    }

    std::shared_ptr<TypeFeedback> typeFeedback_;
    bool hasTypeFeedback() const { return typeFeedback_.get(); }
    bool hasCallFeedback() const { return callFeedback_.get(); }
    const TypeFeedback& typeFeedback() const {
        if (typeFeedback_.get())
            return *typeFeedback_;
        const static TypeFeedback none;
        return none;
    }
    TypeFeedback& updateTypeFeedback() {
        if (typeFeedback_.get())
            return *typeFeedback_;
        typeFeedback_.reset(new TypeFeedback());
        return updateTypeFeedback();
    }
    void typeFeedback(const TypeFeedback& feedback) {
        typeFeedback_.reset(new TypeFeedback(feedback));
    }

    std::shared_ptr<CallFeedback> callFeedback_;
    const CallFeedback& callFeedback() const {
        if (callFeedback_.get())
            return *callFeedback_;
        const static CallFeedback none;
        return none;
    }
    CallFeedback& updateCallFeedback() {
        if (callFeedback_.get())
            return *callFeedback_;
        callFeedback_.reset(new CallFeedback());
        return updateCallFeedback();
    }
    void callFeedback(const CallFeedback& feedback) {
        callFeedback_.reset(new CallFeedback(feedback));
    }

    Effects getObservableEffects() const {
        auto e = effects;
        // Those are effects, and we are required to have them in the correct
        // order. But they are not "doing" anything on their own. If e.g.
        // instructions with those effects are unused, we can remove them.
        e.reset(Effect::LeaksArg);
        e.reset(Effect::ReadsEnv);
        e.reset(Effect::LeaksEnv);
        e.reset(Effect::DependsOnAssume);
        return e;
    }

    bool hasObservableEffects() const {
        return !getObservableEffects().empty();
    }

    Effects getStrongEffects() const {
        auto e = getObservableEffects();
        // Yes visibility is a global effect. We try to preserve it. But geting
        // it wrong is not a strong correctness issue.
        e.reset(Effect::Visibility);
        e.reset(Effect::UpdatesMetadata);
        return e;
    }

    bool hasStrongEffects() const { return !getStrongEffects().empty(); }

    bool isDeoptBarrier() const {
        auto e = getStrongEffects();
        e.reset(Effect::TriggerDeopt);
        // Error exits function, so we will never roll back before that effect
        e.reset(Effect::Error);
        return !e.empty();
    }

    bool mayObserveContext(MkEnv* c = nullptr) const;

    bool leaksArg() const { return effects.includes(Effect::LeaksArg); }
    bool readsEnv() const {
        return hasEnv() && effects.includes(Effect::ReadsEnv);
    }
    bool changesEnv() const {
        return hasEnv() && effects.includes(Effect::WritesEnv);
    }
    bool leaksEnv() const {
        return hasEnv() && effects.includes(Effect::LeaksEnv);
    }

    void clearFrameState();
    FrameState* frameState() const;
    virtual Value* frameStateOrTs() const { return Tombstone::framestate(); }
    virtual void updateFrameState(Value*) { assert(false); }

    virtual unsigned cost() const { return 1; }

    virtual size_t tagHash() const = 0;
    virtual size_t gvnBase() const { return 0; }

    virtual bool mayHaveEnv() const = 0;
    virtual bool hasEnv() const = 0;
    virtual bool exits() const = 0;
    virtual bool branches() const = 0;
    virtual bool branchOrExit() const = 0;
    virtual bool isTypecheck() const = 0;
    virtual VisibilityFlag visibilityFlag() const = 0;

    virtual size_t nargs() const = 0;

    virtual Instruction* clone() const = 0;

    const Value* cFollowCasts() const override final;
    const Value* cFollowCastsAndForce() const override final;
    const Value* cFollowDownCastsAndForce() const override final;
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

    virtual std::string name() const { return tagToStr(tag); }

    Instruction* hasSingleUse();
    void eraseAndRemove();

    void replaceUsesIn(
        Value* val, BB* target,
        const std::function<void(Instruction*, size_t)>& postAction =
            [](Instruction*, size_t) {},
        const std::function<bool(Instruction*)>& replaceOnly =
            [](Instruction*) { return true; }) override final;

    void replaceUsesWith(
        Value* val,
        const std::function<void(Instruction*, size_t)>& postAction =
            [](Instruction*, size_t) {},
        const std::function<bool(Instruction*)>& replaceOnly =
            [](Instruction*) { return true; });

    void replaceUsesAndSwapWith(Instruction* val,
                                std::vector<Instruction*>::iterator it);

    void replaceDominatedUses(
        Instruction* replacement, const DominanceGraph& dom,
        const std::initializer_list<Tag>& skip = {},
        const std::function<void(Instruction*)>& postAction = [](Instruction*) {
        });
    void replaceUsesOfValue(Value* old, Value* rpl);

    bool usesAreOnly(BB*, std::unordered_set<Tag>);
    bool usesDoNotInclude(BB*, std::unordered_set<Tag>);

    typedef std::function<PirType(Value*)> GetType;

    virtual PirType inferType(const GetType& at = [](Value* v) {
        return v->type;
    }) const {
        return type;
    }
    virtual Effects inferEffects(const GetType& at = [](Value* v) {
        return v->type;
    }) const {
        return effects;
    }

    void updateTypeAndEffects() {
        auto isRType = type.isRType();
        assert(!type.isVoid() || !isRType);
        type = inferType();
        // Can happen in unreachable code when we have conflicting speculations
        if (isRType && type.isVoid())
            type = PirType::val();
        effects = inferEffects();
    }

    PirType mergedInputType(const GetType& getType = [](Value* v) {
        return v->type;
    }) const {
        PirType t = PirType::bottom();
        eachArg([&](Value* arg) {
            if (!mayHaveEnv() || env() != arg)
                t = t | getType(arg);
        });
        return t;
    }

    virtual void pushArg(Value* a, PirType t) {
        assert(false && "Must be varlen instruction");
    }
    virtual void pushArg(Value* a) {
        assert(false && "Must be varlen instruction");
    }
    virtual void popArg() { assert(false && "Must be varlen instruction"); }

    bool nonObjectArgs();

  protected:
    constexpr static Effects errorWarnVisible =
        Effects(Effect::Error) | Effect::Warn | Effect::Visibility |
        Effect::DependsOnAssume;

    template <typename Result>
    Result ifNonObjectArgs(const GetType& getType, Result then,
                           Result otherwise) const {
        if (!getType(arg(0).val()).maybeObj())
            return then;
        return otherwise;
    }

    template <typename Result>
    Result ifScalarArgs(const GetType& getType, Result then,
                        Result otherwise) const {
        if (mergedInputType(getType).isScalar())
            return then;
        return otherwise;
    }

  protected:
    PirType inferredTypeForArithmeticInstruction(const GetType& getType) const {
        auto m = mergedInputType(getType);
        if (!m.maybeObj()) {
            auto t = PirType::bottom();
            eachArg([&](Value* v) {
                if (!mayHaveEnv() || v != env())
                    t = t.mergeWithConversion(getType(v));
            });

            // Everything but numbers throws an error
            t = t & PirType::num().notMissing().orAttribsOrObj();

            // TRUE + TRUE == 2
            if (m.maybe(RType::logical))
                t = t.orT(RType::integer).notT(RType::logical);

            // If either argument is complex the result will be complex
            // If one or both arguments are numeric, the result will
            // be numeric
            eachArg([&](Value* v) {
                if (getType(v).isA(RType::cplx))
                    t = t.notT(RType::real).notT(RType::integer);
                if (getType(v).isA(RType::real))
                    t = t.notT(RType::integer);
            });

            // If both arguments are of type integer, the type of the result
            // of ‘/’ and ‘^’ is numeric
            if (tag == Tag::Div || tag == Tag::Pow)
                if (!t.maybe(RType::cplx))
                    t = t.orT(RType::real).notT(RType::integer);

            // The binop result becomes NA if it can't be represented in a
            // fixpoint integer (e.g. INT_MAX + 1 == NA)
            // The condition checks if at least one of the arguments is an
            // integer (doesn't happen with only logicals), and the result is an
            // integer (doesn't happen with real coercion)
            if (m.maybe(RType::integer) && t.maybe(RType::integer))
                t = t.orNAOrNaN();

            // 0 / 0 = NaN
            // 0 %% 0 = NaN or NA_integer_
            if (tag == Tag::Div || tag == Tag::Mod)
                t = t.orNAOrNaN();

            return type & t;
        }
        return type;
    }

    PirType inferredTypeForLogicalInstruction(const GetType& getType) const {
        auto t = mergedInputType(getType);
        if (!t.maybeObj()) {
            auto res = PirType(RType::logical).orAttribsOrObj();
            if (t.isScalar())
                res = res.scalar();
            if (!t.maybeHasAttrs())
                res = res.noAttribsOrObject();
            if (t.isSimpleScalar())
                res = res.simpleScalar();
            if (!t.maybeNAOrNaN())
                res = res.notNAOrNaN();
            return type & res;
        }
        return type;
    }

    Effects
    inferredEffectsForArithmeticInstruction(const GetType& getType) const {
        auto e = effects;
        auto t = mergedInputType(getType);
        if (!t.maybeObj())
            e = e & errorWarnVisible;
        if (t.isA(PirType::num().notObject())) {
            // 0-sized input might error
            if (t.isScalar()) {
                e.reset(Effect::Error);
                e.reset(Effect::Warn);
            }
        }
        return e;
    }

    Effects inferredEffectsForLogicalInstruction(const GetType& getType) const {
        auto e = effects;
        auto t = mergedInputType(getType);
        if (!t.maybeObj())
            e = e & errorWarnVisible;
        if (t.isA(PirType::atomOrSimpleVec().notObject())) {
            // 0-sized input might error
            if (t.isScalar()) {
                e.reset(Effect::Error);
                e.reset(Effect::Warn);
            }
        }
        return e;
    }

  public:
    virtual void printEffects(std::ostream& out, bool tty) const;
    virtual void printArgs(std::ostream& out, bool tty) const;
    virtual void printGraphArgs(std::ostream& out, bool tty) const;
    virtual void printGraphBranches(std::ostream& out, size_t bbId) const;
    virtual void printEnv(std::ostream& out, bool tty) const;
    virtual void print(std::ostream& out, bool tty = false) const;
    void printGraph(std::ostream& out, bool tty = false) const;
    void printRef(std::ostream& out) const override final;
    std::string getRef() const;
    void print() const { print(std::cerr, true); }
    void printRecursive(std::ostream& out, int i) const {
        if (i == 0)
            return;
        eachArg([&](Value* v) {
            if (auto j = Instruction::Cast(v))
                j->printRecursive(out, i - 1);
        });
        print(out, false);
        out << "\n";
    }

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

    bool allNonEnvArgs(Instruction::ArgumentValuePredicateIterator it) const {
        for (size_t i = 0; i < nargs(); ++i)
            if (!(mayHaveEnv() && i == envSlot()) && !it(arg(i).val()))
                return false;
        return true;
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
        default: {
        }
        }
        return nullptr;
    }

    static const Instruction* Cast(const Value* v) {
        switch (v->tag) {
#define V(Name) case Tag::Name:
            COMPILER_INSTRUCTIONS(V)
#undef V
            return static_cast<const Instruction*>(v);
        default: {
        }
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
static_assert(sizeof(Instruction) <= 72, "Bloated instructions...");

template <Tag ITAG, class Base, Effects::StoreType INITIAL_EFFECTS,
          HasEnvSlot ENV, Controlflow CF, class ArgStore>
class InstructionImplementation : public Instruction {
  protected:
    ArgStore args_;

  public:
    InstructionImplementation(PirType resultType, unsigned srcIdx)
        : Instruction(ITAG, resultType, Effects(INITIAL_EFFECTS), srcIdx),
          args_({}) {}
    InstructionImplementation(PirType resultType, const ArgStore& args,
                              unsigned srcIdx)
        : Instruction(ITAG, resultType, Effects(INITIAL_EFFECTS), srcIdx),
          args_(args) {}

    InstructionImplementation& operator=(InstructionImplementation&) = delete;
    InstructionImplementation() = delete;

    Instruction* clone() const override {
        assert(Base::Cast(this));
        return new Base(*static_cast<const Base*>(this));
    }

    size_t tagHash() const override final { return std::hash<Tag>()(ITAG); }

    bool mayHaveEnv() const override final { return ENV == HasEnvSlot::Yes; }
    bool hasEnv() const override final {
        return mayHaveEnv() && env() != Env::elided();
    }
    bool isTypecheck() const override { return false; }
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

    explicit FixedLenInstruction(PirType resultType, unsigned srcIdx = 0)
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
            (*this)[EnvSlot].type() = PirType::env();
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

    void pushArg(Value* a, PirType t) override {
        assert(a);
        args_.push_back(InstrArg(a, t));
    }
    void pushArg(Value* a) override { pushArg(a, a->type); }
    void popArg() override {
        assert(args_.size() > 0);
        args_.pop_back();
    }

    explicit VarLenInstruction(PirType return_type, unsigned srcIdx = 0)
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
        Super::pushArg(env, PirType::env());
    }

    void pushArg(Value* a, PirType t) override {
        assert(a);
        assert(args_.size() > 0);
        assert(args_.back().type() == PirType::env());
        // extend vector and move the environment to the end
        args_.push_back(args_.back());
        args_[args_.size() - 2] = InstrArg(a, t);
    }
    void popArg() override final {
        assert(args_.size() > 1);
        assert(args_.back().type() == PirType::env());
        args_[args_.size() - 2] = args_[args_.size() - 1];
        args_.pop_back();
        assert(args_.back().type() == PirType::env());
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
    FixedLenInstruction<Tag::type, type, nargs,                                \
                        static_cast<Effects::StoreType>(Effects(io)),          \
                        HasEnvSlot::No>

#define FLIE(type, nargs, io)                                                  \
    type:                                                                      \
  public                                                                       \
    FixedLenInstructionWithEnvSlot<                                            \
        Tag::type, type, nargs, static_cast<Effects::StoreType>(Effects(io)),  \
        HasEnvSlot::Yes>

#define VLI(type, io)                                                          \
    type:                                                                      \
  public                                                                       \
    VarLenInstruction<Tag::type, type,                                         \
                      static_cast<Effects::StoreType>(Effects(io)),            \
                      HasEnvSlot::No>

#define VLIE(type, io)                                                         \
    type:                                                                      \
  public                                                                       \
    VarLenInstructionWithEnvSlot<Tag::type, type,                              \
                                 static_cast<Effects::StoreType>(Effects(io)), \
                                 HasEnvSlot::Yes>

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
class VLIE(FrameState, Effects() | Effect::ReadsEnv) {
  public:
    bool inlined = false;
    Opcode* pc;
    rir::Code* code;
    size_t stackSize;
    bool inPromise;

    size_t gvnBase() const override {
        return hash_combine(
            hash_combine(hash_combine(hash_combine(tagHash(), inlined), pc),
                         code),
            stackSize);
    }

    FrameState(Value* env, rir::Code* code, Opcode* pc, const RirStack& stack,
               bool inPromise)
        : VarLenInstructionWithEnvSlot(NativeType::frameState, env), pc(pc),
          code(code), stackSize(stack.size()), inPromise(inPromise) {
        for (auto& v : stack)
            pushArg(v, PirType::any());
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
    void printEnv(std::ostream& out, bool tty) const override final {}
};

class FLIE(LdFun, 2, Effects::Any()) {
  private:
    SEXP hint_ = nullptr;
    FeedbackOrigin hintOrigin_;

  public:
    SEXP hint() { return hint_; }
    const FeedbackOrigin& hintOrigin() { return hintOrigin_; }
    void hint(SEXP hint, const FeedbackOrigin& hintOrigin) {
        hint_ = hint;
        hintOrigin_ = hintOrigin;
    }
    SEXP varName;
    bool hintHasStableEnv = false;

    LdFun(const char* name, Value* env)
        : FixedLenInstructionWithEnvSlot(PirType::function(),
                                         {{PirType::any()}},
                                         {{Tombstone::closure()}}, env),
          varName(Rf_install(name)) {}
    LdFun(SEXP name, Value* env)
        : FixedLenInstructionWithEnvSlot(PirType::function(),
                                         {{PirType::any()}},
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

    int minReferenceCount() const override { return MAX_REFCOUNT; }
};

class FLIE(LdVar, 1, Effects() | Effect::Error | Effect::ReadsEnv) {
  public:
    SEXP varName;
    bool forUpdate = false;

    LdVar(const char* name, Value* env)
        : FixedLenInstructionWithEnvSlot(PirType::any(), env),
          varName(Rf_install(name)) {}
    LdVar(SEXP name, Value* env)
        : FixedLenInstructionWithEnvSlot(PirType::any(), env), varName(name) {
        assert(TYPEOF(name) == SYMSXP);
    }

    void printArgs(std::ostream& out, bool tty) const override;

    int minReferenceCount() const override { return 1; }
};

class FLI(ToForSeq, 1, Effect::Error) {
  public:
    explicit ToForSeq(Value* val)
        : FixedLenInstruction(val->type.maybeObj()
                                  ? val->type.notObject().orT(RType::chr)
                                  : val->type,
                              {{PirType::val()}}, {{val}}) {}

    size_t gvnBase() const override { return tagHash(); }

    PirType inferType(const GetType& getType) const override final {
        auto it = getType(arg(0).val());
        if (it.maybeObj())
            return type & it.notObject().orT(RType::chr);
        return type & it;
    }
};

class FLI(Length, 1, Effects::None()) {
  public:
    explicit Length(Value* val)
        : FixedLenInstruction(
              PirType(RType::integer).simpleScalar().notObject(),
              {{PirType::val()}}, {{val}}) {}
    size_t gvnBase() const override { return tagHash(); }
};

class FLI(LdArg, 0, Effects::None()) {
  public:
    size_t pos;

    explicit LdArg(size_t pos)
        : FixedLenInstruction(PirType::any()), pos(pos) {}

    void printArgs(std::ostream& out, bool tty) const override;

    size_t gvnBase() const override { return hash_combine(tagHash(), pos); }
    int minReferenceCount() const override { return MAX_REFCOUNT; }
};

class FLIE(Missing, 1, Effects() | Effect::ReadsEnv | Effect::Error) {
  public:
    SEXP varName;
    explicit Missing(SEXP varName, Value* env)
        : FixedLenInstructionWithEnvSlot(PirType::simpleScalarLogical(), env),
          varName(varName) {}
    void printArgs(std::ostream& out, bool tty) const override;
};

class FLI(ChkMissing, 1, Effect::Error) {
  public:
    explicit ChkMissing(Value* in)
        // Check missing on the missing value will throw an error. If we would
        // set the type to MissingArg::instance()->type.notMissing() then this
        // would be void, which will mess up the consumer instructions (even
        // though they will never be executed due to the error, it would still
        // confuse the compiler...)
        : FixedLenInstruction(in->type, {{PirType::any()}}, {{in}}) {}
    size_t gvnBase() const override { return tagHash(); }

    PirType inferType(const GetType& getType) const override final {
        auto t = getType(arg<0>().val()).notMissing();

        if (t.isVoid())
            return type;
        return t;
    }
};

class FLI(ChkFunction, 1, Effect::Error) {
  public:
    explicit ChkFunction(Value* in)
        : FixedLenInstruction(PirType::function(), {{PirType::val()}}, {{in}}) {
    }
    size_t gvnBase() const override { return tagHash(); }
};

class FLIE(StVarSuper, 2,
           Effects() | Effect::ReadsEnv | Effect::WritesEnv |
               Effect::LeaksArg) {
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

class FLIE(LdVarSuper, 1, Effects() | Effect::Error | Effect::ReadsEnv) {
  public:
    LdVarSuper(SEXP name, Value* env)
        : FixedLenInstructionWithEnvSlot(PirType::any(), env), varName(name) {}

    LdVarSuper(const char* name, Value* env)
        : FixedLenInstructionWithEnvSlot(PirType::any(), env),
          varName(Rf_install(name)) {}

    SEXP varName;

    void printArgs(std::ostream& out, bool tty) const override;

    int minReferenceCount() const override { return 1; }
};

class FLIE(StVar, 2, Effects(Effect::WritesEnv) | Effect::LeaksArg) {
  public:
    bool isStArg = false;

    StVar(SEXP name, Value* val, Value* env, PirType expected = PirType::val())
        : FixedLenInstructionWithEnvSlot(PirType::voyd(), {{expected}}, {{val}},
                                         env),
          varName(name) {}

    StVar(const char* name, Value* val, Value* env,
          PirType expected = PirType::val())
        : FixedLenInstructionWithEnvSlot(PirType::voyd(), {{expected}}, {{val}},
                                         env),
          varName(Rf_install(name)) {}

    SEXP varName;
    Value* val() const { return arg(0).val(); }
    using FixedLenInstructionWithEnvSlot::env;

    std::string name() const override { return isStArg ? "StArg" : "StVar"; }
    void printArgs(std::ostream& out, bool tty) const override;
};

// Pseudo Instruction. Is actually a StVar with a flag set.
class StArg : public StVar {
  public:
    StArg(SEXP name, Value* val, Value* env)
        : StVar(name, val, env, PirType::any()) {
        isStArg = true;
    }
};

class Branch
    : public FixedLenInstruction<Tag::Branch, Branch, 1, Effects::NoneI(),
                                 HasEnvSlot::No, Controlflow::Branch> {
  public:
    bool deoptTrigger = false;

    explicit Branch(Value* test)
        : FixedLenInstruction(PirType::voyd(), {{PirType::test()}}, {{test}}) {}
    void printArgs(std::ostream& out, bool tty) const override;
    void printGraphArgs(std::ostream& out, bool tty) const override;
    void printGraphBranches(std::ostream& out, size_t bbId) const override;
};

class NonLocalReturn
    : public FixedLenInstructionWithEnvSlot<Tag::NonLocalReturn, NonLocalReturn,
                                            2, Effects::AnyI(), HasEnvSlot::Yes,
                                            Controlflow::Exit> {
  public:
    explicit NonLocalReturn(Value* ret, Value* env)
        : FixedLenInstructionWithEnvSlot(PirType::voyd(), {{PirType::val()}},
                                         {{ret}}, env) {}
};

class Return : public FixedLenInstruction<Tag::Return, Return, 1,
                                          static_cast<Effects::StoreType>(
                                              Effects(Effect::LeaksArg)),
                                          HasEnvSlot::No, Controlflow::Exit> {
  public:
    explicit Return(Value* ret)
        : FixedLenInstruction(PirType::voyd(), {{PirType::val()}}, {{ret}}) {}
};

class Unreachable : public FixedLenInstruction<Tag::Unreachable, Unreachable, 0,
                                               Effects::NoneI(), HasEnvSlot::No,
                                               Controlflow::Exit> {
  public:
    explicit Unreachable() : FixedLenInstruction(PirType::voyd(), {{}}, {{}}) {}
};

class Promise;
class FLIE(MkArg, 2, Effects::None()) {
    Promise* prom_;

  public:
    bool noReflection = false;

    MkArg(Promise* prom, Value* v, Value* env);

    Value* eagerArg() const { return arg(0).val(); }
    void eagerArg(Value* eager) {
        arg(0).val() = eager;
        assert(isEager());
        noReflection = true;
        // Environment is not needed once a promise is evaluated
        elideEnv();
    }

    void updatePromise(Promise* p) { prom_ = p; }
    Promise* prom() const { return prom_; }

    bool isEager() const { return eagerArg() != UnboundValue::instance(); }

    void printArgs(std::ostream& out, bool tty) const override;

    Value* promEnv() const { return env(); }

    size_t gvnBase() const override { return hash_combine(tagHash(), prom_); }

    int minReferenceCount() const override { return MAX_REFCOUNT; }

    bool usesPromEnv() const;
};

class FLI(UpdatePromise, 2,
          Effects(Effect::MutatesArgument) | Effect::LeaksArg) {
  public:
    UpdatePromise(MkArg* prom, Value* v)
        : FixedLenInstruction(PirType::voyd(), {{RType::prom, PirType::val()}},
                              {{prom, v}}) {}
    MkArg* mkarg() const { return MkArg::Cast(arg(0).val()); }
};

class FLIE(MkCls, 1, Effects::None()) {
  private:
    Closure* cls;

  public:
    Closure* tryGetCls() { return cls; }
    void setCls(Closure* c) { cls = c; }
    DispatchTable* originalBody;
    SEXP formals;
    SEXP srcRef;
    MkCls(Closure* cls, SEXP formals, SEXP srcRef, DispatchTable* originalBody,
          Value* lexicalEnv);
    void printArgs(std::ostream&, bool tty) const override;

    Value* lexicalEnv() const { return env(); }

    int minReferenceCount() const override { return MAX_REFCOUNT; }

    size_t gvnBase() const override {
        // Lazily compiled cls might be still missing
        if (!cls)
            return 0;
        return hash_combine(tagHash(), cls);
    }
};

class FLIE(Force, 3, Effects::Any()) {
  public:
    // Set to true if we are sure that the promise will be forced here
    bool strict = false;

    // Observed behavior for speculation
    using ArgumentKind = ObservedValues::StateBeforeLastForce;
    ArgumentKind observed = ArgumentKind::unknown;

    Force(Value* in, Value* env, Value* fs)
        : FixedLenInstructionWithEnvSlot(
              in->type.forced(), {{PirType::any(), NativeType::frameState}},
              {{in, fs}}, env) {
        if (auto mk = MkArg::Cast(in)) {
            if (mk->noReflection) {
                elideEnv();
                effects.reset(Effect::Reflection);
            }
        }
        assert(!type.isVoid());
        updateTypeAndEffects();
    }
    Value* input() const { return arg(0).val(); }

    Value* frameStateOrTs() const override final { return arg<1>().val(); }
    void updateFrameState(Value* fs) override final { arg<1>().val() = fs; }

    std::string name() const override {
        std::stringstream ss;
        ss << "Force";
        if (strict)
            ss << "!";
        if (observed == ArgumentKind::promise)
            ss << "<lazy>";
        else if (observed == ArgumentKind::evaluatedPromise)
            ss << "<wrapped>";
        else if (observed == ArgumentKind::value)
            ss << "<value>";
        return ss.str();
    }
    void printArgs(std::ostream& out, bool tty) const override;

    PirType inferType(const GetType& getType) const override final;

    Effects inferEffects(const GetType& getType) const override final {
        auto e = getType(input()).maybeLazy() ? effects : Effects();
        if (auto mk = MkArg::Cast(input()->followCastsAndForce())) {
            if (mk->noReflection)
                e.reset(Effect::Reflection);
        }
        return e;
    }
    int minReferenceCount() const override { return 0; }

    size_t gvnBase() const override {
        if (effects.contains(Effect::ExecuteCode))
            return 0;
        return tagHash();
    }
};

class FLI(CastType, 1, Effects::None()) {
  public:
    enum Kind { Upcast, Downcast };
    const Kind kind;
    unsigned cost() const override final { return 0; }
    CastType(Value* in, Kind k, PirType from, PirType to)
        : FixedLenInstruction(to, {{from}}, {{in}}), kind(k) {}
    size_t gvnBase() const override {
        return hash_combine(
            hash_combine(hash_combine(tagHash(), type), arg<0>().type()), kind);
    }
    PirType inferType(const GetType& getType) const override final {
        if (kind == Downcast) {
            auto t = getType(arg(0).val()) & type;
            if (!t.isVoid()) // can happen in dead code
                return t;
        }
        return type;
    }
    void printArgs(std::ostream& out, bool tty) const override;
};

class FLI(AsLogical, 1, Effect::Error) {
  public:
    Value* val() const { return arg<0>().val(); }

    AsLogical(Value* in, unsigned srcIdx)
        : FixedLenInstruction(PirType::simpleScalarLogical(),
                              {{PirType::val()}}, {{in}}, srcIdx) {}

    Effects inferEffects(const GetType& getType) const override final {
        if (getType(val()).isA((PirType() | RType::logical | RType::integer |
                                RType::real | RType::str | RType::cplx)
                                   .noAttribsOrObject())) {
            return Effects::None();
        }
        return effects;
    }
    size_t gvnBase() const override { return tagHash(); }
};

class FLI(AsSwitchIdx, 1, Effects::None()) {
  public:
    Value* val() const { return arg<0>().val(); }
    explicit AsSwitchIdx(Value* in)
        : FixedLenInstruction(PirType::simpleScalarInt(), {{PirType::val()}},
                              {{in}}) {}

    size_t gvnBase() const override { return tagHash(); }
};

class FLI(CheckTrueFalse, 1, Effects() | Effect::Error | Effect::Warn) {
  public:
    Value* val() const { return arg<0>().val(); }

    explicit CheckTrueFalse(Value* in)
        : FixedLenInstruction(PirType::simpleScalarLogical().notNAOrNaN(),
                              {{PirType::val()}}, {{in}}) {}

    Effects inferEffects(const GetType& getType) const override final {
        if (getType(val()).isScalar())
            return effects & ~Effects(Effect::Warn);
        // Error on NA, hard to exclude
        return effects;
    }
    size_t gvnBase() const override { return tagHash(); }
};

class FLI(ColonInputEffects, 2, Effects() | Effect::Error | Effect::Warn) {
  public:
    explicit ColonInputEffects(Value* lhs, Value* rhs, unsigned srcIdx)
        : FixedLenInstruction(PirType::test(),
                              {{PirType::val(), PirType::val()}}, {{lhs, rhs}},
                              srcIdx) {}

    Value* lhs() const { return arg<0>().val(); }
    Value* rhs() const { return arg<1>().val(); }

    Effects inferEffects(const GetType& getType) const override final {
        if (getType(lhs()).isA(PirType::num().scalar()) &&
            getType(rhs()).isA(PirType::num().scalar())) {
            return Effects::None();
        } else {
            return effects;
        }
    }

    int minReferenceCount() const override { return MAX_REFCOUNT; }
};

class FLI(ColonCastLhs, 1, Effect::Error) {
  public:
    explicit ColonCastLhs(Value* lhs, unsigned srcIdx)
        : FixedLenInstruction(PirType::intReal().scalar().notNAOrNaN(),
                              {{PirType::val()}}, {{lhs}}, srcIdx) {}

    Value* lhs() const { return arg<0>().val(); }

    PirType inferType(const GetType& getType) const override final {
        if (getType(lhs()).isA(RType::integer)) {
            return PirType(RType::integer).scalar().notNAOrNaN();
        } else {
            return type;
        }
    }
};

class FLI(ColonCastRhs, 2, Effect::Error) {
  public:
    explicit ColonCastRhs(Value* newLhs, Value* rhs, unsigned srcIdx)
        : FixedLenInstruction(
              PirType::intReal().scalar().notNAOrNaN(),
              {{PirType::intReal().scalar().notNAOrNaN(), PirType::val()}},
              {{newLhs, rhs}}, srcIdx) {}

    Value* newLhs() const { return arg<0>().val(); }

    PirType inferType(const GetType& getType) const override final {
        // This is intended - lhs type determines rhs
        if (getType(newLhs()).isA(RType::integer)) {
            return PirType(RType::integer).scalar().notNAOrNaN();
        } else {
            return type;
        }
    }
};

class FLIE(Subassign1_1D, 4, Effects::Any()) {
  public:
    Subassign1_1D(Value* val, Value* vec, Value* idx, Value* env,
                  unsigned srcIdx)
        : FixedLenInstructionWithEnvSlot(
              PirType::val(),
              {{PirType::val(), PirType::val(), PirType::val()}},
              {{val, vec, idx}}, env, srcIdx) {}
    Value* val() const { return arg(0).val(); }
    Value* vec() const { return arg(1).val(); }
    Value* idx() const { return arg(2).val(); }

    PirType inferType(const GetType& getType) const override final {
        bool maybeStringIdx = getType(idx()).maybe(PirType() | RType::str);
        auto restricted =
            type &
            (getType(vec()).mergeWithConversion(getType(val())).orNotScalar());
        if (maybeStringIdx)
            restricted = restricted.orNameAttrs();
        return ifNonObjectArgs(getType, restricted, type);
    }
    Effects inferEffects(const GetType& getType) const override final {
        return ifNonObjectArgs(getType, effects & errorWarnVisible, effects);
    }
};

class FLI(SetVecElt, 3, Effect::Error) {
  public:
    SetVecElt(Value* val, Value* vec, Value* idx)
        : FixedLenInstruction(
              PirType::val(),
              {{PirType::val(), PirType::val(), PirType::simpleScalarInt()}},
              {{val, vec, idx}}) {}
    Value* val() const { return arg(0).val(); }
    Value* vec() const { return arg(1).val(); }
    Value* idx() const { return arg(2).val(); }

    PirType inferType(const GetType& getType) const override final {
        auto restricted =
            type &
            (getType(vec()).mergeWithConversion(getType(val())).orNotScalar());
        return restricted;
    }
};

class FLIE(Subassign2_1D, 4, Effects::Any()) {
  public:
    Subassign2_1D(Value* val, Value* vec, Value* idx, Value* env,
                  unsigned srcIdx)
        : FixedLenInstructionWithEnvSlot(
              PirType::val(),
              {{PirType::val(), PirType::val(), PirType::val()}},
              {{val, vec, idx}}, env, srcIdx) {}
    Value* val() const { return arg(0).val(); }
    Value* vec() const { return arg(1).val(); }
    Value* idx() const { return arg(2).val(); }

    PirType inferType(const GetType& getType) const override final {
        bool maybeStringIdx = getType(idx()).maybe(PirType() | RType::str);
        auto restricted =
            type &
            (getType(vec()).mergeWithConversion(getType(val())).orNotScalar());
        if (maybeStringIdx)
            restricted = restricted.orNameAttrs();
        return ifNonObjectArgs(getType, restricted, type);
    }
    Effects inferEffects(const GetType& getType) const override final {
        return ifNonObjectArgs(getType, effects & errorWarnVisible, effects);
    }
};

class FLIE(Subassign1_2D, 5, Effects::Any()) {
  public:
    Subassign1_2D(Value* val, Value* mtx, Value* idx1, Value* idx2, Value* env,
                  unsigned srcIdx)
        : FixedLenInstructionWithEnvSlot(PirType::val(),
                                         {{PirType::val(), PirType::val(),
                                           PirType::val(), PirType::val()}},
                                         {{val, mtx, idx1, idx2}}, env,
                                         srcIdx) {}
    Value* val() const { return arg(0).val(); }
    Value* vec() const { return arg(1).val(); }
    Value* idx1() const { return arg(2).val(); }
    Value* idx2() const { return arg(3).val(); }

    PirType inferType(const GetType& getType) const override final {
        bool maybeStringIdx = getType(idx1()).maybe(PirType() | RType::str) ||
                              getType(idx2()).maybe(PirType() | RType::str);
        auto restricted =
            type &
            (getType(vec()).mergeWithConversion(getType(val())).orNotScalar());
        if (maybeStringIdx)
            restricted = restricted.orNameAttrs();
        return ifNonObjectArgs(getType, restricted, type);
    }
    Effects inferEffects(const GetType& getType) const override final {
        return ifNonObjectArgs(getType, effects & errorWarnVisible, effects);
    }
};

class FLIE(Subassign2_2D, 5, Effects::Any()) {
  public:
    Subassign2_2D(Value* val, Value* mtx, Value* idx1, Value* idx2, Value* env,
                  unsigned srcIdx)
        : FixedLenInstructionWithEnvSlot(PirType::val(),
                                         {{PirType::val(), PirType::val(),
                                           PirType::val(), PirType::val()}},
                                         {{val, mtx, idx1, idx2}}, env,
                                         srcIdx) {}
    Value* val() const { return arg(0).val(); }
    Value* vec() const { return arg(1).val(); }
    Value* idx1() const { return arg(2).val(); }
    Value* idx2() const { return arg(3).val(); }

    PirType inferType(const GetType& getType) const override final {
        bool maybeStringIdx = getType(idx1()).maybe(PirType() | RType::str) ||
                              getType(idx2()).maybe(PirType() | RType::str);
        auto restricted =
            type &
            (getType(vec()).mergeWithConversion(getType(val())).orNotScalar());
        if (maybeStringIdx)
            restricted = restricted.orNameAttrs();
        return ifNonObjectArgs(getType, restricted, type);
    }
    Effects inferEffects(const GetType& getType) const override final {
        return ifNonObjectArgs(getType, effects & errorWarnVisible, effects);
    }
};

class FLIE(Subassign1_3D, 6, Effects::Any()) {
  public:
    Subassign1_3D(Value* val, Value* mtx, Value* idx1, Value* idx2, Value* idx3,
                  Value* env, unsigned srcIdx)
        : FixedLenInstructionWithEnvSlot(
              PirType::val(),
              {{PirType::val(), PirType::val(), PirType::val(), PirType::val(),
                PirType::val()}},
              {{val, mtx, idx1, idx2, idx3}}, env, srcIdx) {}
    Value* val() const { return arg(0).val(); }
    Value* vec() const { return arg(1).val(); }
    Value* idx1() const { return arg(2).val(); }
    Value* idx2() const { return arg(3).val(); }
    Value* idx3() const { return arg(4).val(); }

    PirType inferType(const GetType& getType) const override final {
        bool maybeStringIdx = getType(idx1()).maybe(PirType() | RType::str) ||
                              getType(idx2()).maybe(PirType() | RType::str) ||
                              getType(idx3()).maybe(PirType() | RType::str);
        auto restricted =
            type &
            (getType(vec()).mergeWithConversion(getType(val())).orNotScalar());
        if (maybeStringIdx)
            restricted = restricted.orNameAttrs();
        return ifNonObjectArgs(getType, restricted, type);
    }
    Effects inferEffects(const GetType& getType) const override final {
        return ifNonObjectArgs(getType, effects & errorWarnVisible, effects);
    }
};

class FLIE(Extract1_1D, 3, Effects::Any()) {
  public:
    Extract1_1D(Value* vec, Value* idx, Value* env, unsigned srcIdx)
        : FixedLenInstructionWithEnvSlot(PirType::val(),
                                         {{PirType::val(), PirType::any()}},
                                         {{vec, idx}}, env, srcIdx) {}
    Value* vec() const { return arg(0).val(); }
    Value* idx() const { return arg(1).val(); }

    PirType inferType(const GetType& getType) const override final;
    Effects inferEffects(const GetType& getType) const override final {
        return ifNonObjectArgs(getType, effects & errorWarnVisible, effects);
    }
    size_t gvnBase() const override {
        if (effects.contains(Effect::ExecuteCode))
            return 0;
        return tagHash();
    }
};

class FLIE(Extract2_1D, 3, Effects::Any()) {
  public:
    Extract2_1D(Value* vec, Value* idx, Value* env, unsigned srcIdx)
        : FixedLenInstructionWithEnvSlot(PirType::val(),
                                         {{PirType::val(), PirType::any()}},
                                         {{vec, idx}}, env, srcIdx) {}
    Value* vec() const { return arg(0).val(); }
    Value* idx() const { return arg(1).val(); }

    PirType inferType(const GetType& getType) const override final {
        return ifNonObjectArgs(
            getType, type & getType(vec()).extractType(getType(idx())), type);
    }
    Effects inferEffects(const GetType& getType) const override final {
        return ifNonObjectArgs(getType, effects & errorWarnVisible, effects);
    }
    size_t gvnBase() const override {
        if (effects.contains(Effect::ExecuteCode))
            return 0;
        return tagHash();
    }
};

class FLIE(Extract1_2D, 4, Effects::Any()) {
  public:
    Extract1_2D(Value* vec, Value* idx1, Value* idx2, Value* env,
                unsigned srcIdx)
        : FixedLenInstructionWithEnvSlot(
              PirType::val(),
              {{PirType::val(), PirType::any(), PirType::any()}},
              {{vec, idx1, idx2}}, env, srcIdx) {}
    Value* vec() const { return arg(0).val(); }
    Value* idx1() const { return arg(1).val(); }
    Value* idx2() const { return arg(2).val(); }

    PirType inferType(const GetType& getType) const override final {
        return ifNonObjectArgs(
            getType,
            type & getType(vec()).subsetType(getType(idx1()) | getType(idx2())),
            type);
    }
    Effects inferEffects(const GetType& getType) const override final {
        return ifNonObjectArgs(getType, effects & errorWarnVisible, effects);
    }
    size_t gvnBase() const override {
        if (effects.contains(Effect::ExecuteCode))
            return 0;
        return tagHash();
    }
};

class FLIE(Extract2_2D, 4, Effects::Any()) {
  public:
    Extract2_2D(Value* vec, Value* idx1, Value* idx2, Value* env,
                unsigned srcIdx)
        : FixedLenInstructionWithEnvSlot(
              PirType::val(),
              {{PirType::val(), PirType::any(), PirType::any()}},
              {{vec, idx1, idx2}}, env, srcIdx) {}
    Value* vec() const { return arg(0).val(); }
    Value* idx1() const { return arg(1).val(); }
    Value* idx2() const { return arg(2).val(); }

    PirType inferType(const GetType& getType) const override final {
        return ifNonObjectArgs(getType,
                               type & getType(vec()).extractType(
                                          getType(idx1()) | getType(idx2())),
                               type);
    }
    Effects inferEffects(const GetType& getType) const override final {
        return ifNonObjectArgs(getType, effects & errorWarnVisible, effects);
    }
    size_t gvnBase() const override {
        if (effects.contains(Effect::ExecuteCode))
            return 0;
        return tagHash();
    }
};

class FLIE(Extract1_3D, 5, Effects::Any()) {
  public:
    Extract1_3D(Value* vec, Value* idx1, Value* idx2, Value* idx3, Value* env,
                unsigned srcIdx)
        : FixedLenInstructionWithEnvSlot(PirType::val(),
                                         {{PirType::val(), PirType::any(),
                                           PirType::any(), PirType::any()}},
                                         {{vec, idx1, idx2, idx3}}, env,
                                         srcIdx) {}
    Value* vec() const { return arg(0).val(); }
    Value* idx1() const { return arg(1).val(); }
    Value* idx2() const { return arg(2).val(); }
    Value* idx3() const { return arg(3).val(); }

    PirType inferType(const GetType& getType) const override final {
        return ifNonObjectArgs(
            getType,
            type & getType(vec()).subsetType(getType(idx1()) | getType(idx2())),
            type);
    }
    Effects inferEffects(const GetType& getType) const override final {
        return ifNonObjectArgs(getType, effects & errorWarnVisible, effects);
    }
    size_t gvnBase() const override {
        if (effects.contains(Effect::ExecuteCode))
            return 0;
        return tagHash();
    }
};

class FLI(Is, 1, Effects::None()) {
  public:
    Is(BC::RirTypecheck typecheck, Value* v)
        : FixedLenInstruction(PirType::simpleScalarLogical(),
                              {{PirType::val()}}, {{v}}),
          typecheck(typecheck) {}
    BC::RirTypecheck typecheck;
    PirType upperBound() const;
    PirType lowerBound() const;

    void printArgs(std::ostream& out, bool tty) const override;

    size_t gvnBase() const override {
        return hash_combine(tagHash(), typecheck);
    }
};

class FLI(IsType, 1, Effects::None()) {
  public:
    const PirType typeTest;
    IsType(PirType type, Value* v)
        : FixedLenInstruction(PirType::test(), {{PirType::any()}}, {{v}}),
          typeTest(type) {
        if (v->type.maybeNAOrNaN() && !type.maybeNAOrNaN())
            assert(type.isSimpleScalar());
    }

    void printArgs(std::ostream& out, bool tty) const override;

    bool isTypecheck() const override final { return true; }
    size_t gvnBase() const override {
        return hash_combine(tagHash(), typeTest);
    }
};

class FLI(LdFunctionEnv, 0, Effects::None()) {
  public:
    LdFunctionEnv() : FixedLenInstruction(PirType::env()) {}
    bool stub = false;
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

class FLI(Names, 1, Effects::None()) {
  public:
    explicit Names(Value* v)
        : FixedLenInstruction(PirType(RType::str) | RType::nil,
                              {{PirType::val()}}, {{v}}) {}
    size_t gvnBase() const override { return tagHash(); }
};

class FLI(SetNames, 2, Effect::Error) {
  public:
    explicit SetNames(Value* v, Value* names)
        : FixedLenInstruction(v->type, {{PirType::val(), PirType::val()}},
                              {{v, names}}) {}
    size_t gvnBase() const override { return tagHash(); }
};

class FLI(PirCopy, 1, Effects::None()) {
  public:
    explicit PirCopy(Value* v)
        : FixedLenInstruction(v->type, {{v->type}}, {{v}}) {}
    void print(std::ostream& out, bool tty) const override;
    int minReferenceCount() const override {
        return arg<0>().val()->minReferenceCount();
    }
    PirType inferType(const GetType& getType) const override final {
        return getType(arg<0>().val());
    }
    size_t gvnBase() const override { return tagHash(); }
};

// Effects::Any() prevents this instruction from being optimized away
class FLI(Nop, 0, Effects::Any()) {
  public:
    explicit Nop() : FixedLenInstruction(PirType::voyd()) {}
};

class FLI(Identical, 2, Effects::None()) {
  public:
    Identical(Value* a, Value* b, PirType t)
        : FixedLenInstruction(PirType::test(), {{t, t}}, {{a, b}}) {}
    size_t gvnBase() const override { return tagHash(); }
};

class FLI(Inc, 1, Effects::None()) {
  public:
    explicit Inc(Value* v)
        : FixedLenInstruction(PirType(RType::integer).simpleScalar(),
                              {{PirType(RType::integer).simpleScalar()}},
                              {{v}}) {}
    size_t gvnBase() const override { return tagHash(); }
};

#define V(NESTED, name, Name)                                                  \
    class FLI(Name, 0, Effects::Any()) {                                       \
      public:                                                                  \
        Name() : FixedLenInstruction(PirType::voyd()) {}                       \
    };
SIMPLE_INSTRUCTIONS(V, _)
#undef V

template <typename BASE, Tag TAG>
class Unop
    : public FixedLenInstructionWithEnvSlot<TAG, BASE, 2, Effects::AnyI(),
                                            HasEnvSlot::Yes> {
  public:
    typedef FixedLenInstructionWithEnvSlot<TAG, BASE, 2, Effects::AnyI(),
                                           HasEnvSlot::Yes>
        Super;
    Unop(Value* val, Value* env, unsigned srcIdx)
        : Super(PirType::val(), {{PirType::val()}}, {{val}}, env, srcIdx) {}

    using Super::arg;
    using Super::effects;
    using Super::mergedInputType;
    using Super::tagHash;

    Value* val() const { return arg(0).val(); }

    VisibilityFlag visibilityFlag() const override final {
        return VisibilityFlag::Unknown;
    }

    size_t gvnBase() const override {
        if (effects.contains(Effect::ExecuteCode))
            return 0;
        return tagHash();
    }
};

template <typename BASE, Tag TAG>
class ArithmeticUnop : public Unop<BASE, TAG> {
  public:
    typedef Unop<BASE, TAG> Super;

    ArithmeticUnop(Value* val, Value* env, unsigned srcIdx)
        : Super(val, env, srcIdx) {}

    using Super::inferredEffectsForArithmeticInstruction;
    using Super::inferredTypeForArithmeticInstruction;
    using typename Super::GetType;
    PirType inferType(const GetType& getType) const override {
        return inferredTypeForArithmeticInstruction(getType);
    }
    Effects inferEffects(const GetType& getType) const override {
        return inferredEffectsForArithmeticInstruction(getType);
    }
};

#define ARITHMETIC_UNOP(Kind)                                                  \
    class Kind : public ArithmeticUnop<Kind, Tag::Kind> {                      \
      public:                                                                  \
        Kind(Value* val, Value* env, unsigned srcIdx)                          \
            : ArithmeticUnop<Kind, Tag::Kind>(val, env, srcIdx) {}             \
        PirType inferType(const GetType& getType) const override {             \
            return inferredTypeForArithmeticInstruction(getType);              \
        }                                                                      \
    }

ARITHMETIC_UNOP(Plus);
ARITHMETIC_UNOP(Minus);

#undef ARITHMETIC_UNOP

template <typename BASE, Tag TAG>
class LogicalUnop : public Unop<BASE, TAG> {
  public:
    typedef Unop<BASE, TAG> Super;

    LogicalUnop(Value* val, Value* env, unsigned srcIdx)
        : Super(val, env, srcIdx) {}

    using Super::inferredEffectsForLogicalInstruction;
    using Super::inferredTypeForLogicalInstruction;
    using typename Super::GetType;
    PirType inferType(const GetType& getType) const override {
        return inferredTypeForLogicalInstruction(getType);
    }
    Effects inferEffects(const GetType& getType) const override {
        return inferredEffectsForLogicalInstruction(getType);
    }
};

#define LOGICAL_UNOP(Kind)                                                     \
    class Kind : public LogicalUnop<Kind, Tag::Kind> {                         \
      public:                                                                  \
        Kind(Value* val, Value* env, unsigned srcIdx)                          \
            : LogicalUnop<Kind, Tag::Kind>(val, env, srcIdx) {}                \
    }

LOGICAL_UNOP(Not);

#undef LOGICAL_UNOP

template <typename BASE, Tag TAG>
class Binop
    : public FixedLenInstructionWithEnvSlot<TAG, BASE, 3, Effects::AnyI(),
                                            HasEnvSlot::Yes> {
  public:
    typedef FixedLenInstructionWithEnvSlot<TAG, BASE, 3, Effects::AnyI(),
                                           HasEnvSlot::Yes>
        Super;

    Binop(Value* lhs, Value* rhs, Value* env, unsigned srcIdx)
        : Super(PirType::val(), {{PirType::val(), PirType::val()}},
                {{lhs, rhs}}, env, srcIdx) {}

    using Super::arg;
    using Super::effects;
    using Super::tagHash;
    Value* lhs() const { return arg(0).val(); }
    Value* rhs() const { return arg(1).val(); }

    VisibilityFlag visibilityFlag() const override final {
        return VisibilityFlag::Unknown;
    }

    size_t gvnBase() const override {
        if (effects.contains(Effect::ExecuteCode))
            return 0;
        return tagHash();
    }
};

template <typename BASE, Tag TAG>
class ArithmeticBinop : public Binop<BASE, TAG> {
  public:
    typedef Binop<BASE, TAG> Super;

    ArithmeticBinop(Value* lhs, Value* rhs, Value* env, unsigned srcIdx)
        : Super(lhs, rhs, env, srcIdx) {}

    using Super::inferredEffectsForArithmeticInstruction;
    using Super::inferredTypeForArithmeticInstruction;
    using typename Super::GetType;
    PirType inferType(const GetType& getType) const override {
        auto t = inferredTypeForArithmeticInstruction(getType);
        assert(!t.isVoid());
        return t;
    }
    Effects inferEffects(const GetType& getType) const override {
        return inferredEffectsForArithmeticInstruction(getType);
    }
};

#define ARITHMETIC_BINOP(Kind)                                                 \
    class Kind : public ArithmeticBinop<Kind, Tag::Kind> {                     \
      public:                                                                  \
        Kind(Value* lhs, Value* rhs, Value* env, unsigned srcIdx)              \
            : ArithmeticBinop<Kind, Tag::Kind>(lhs, rhs, env, srcIdx) {}       \
    }

ARITHMETIC_BINOP(Add);
ARITHMETIC_BINOP(Sub);
ARITHMETIC_BINOP(Mul);
ARITHMETIC_BINOP(Div);
ARITHMETIC_BINOP(IDiv);
ARITHMETIC_BINOP(Mod);
ARITHMETIC_BINOP(Pow);

#undef ARITHMETIC_BINOP

template <typename BASE, Tag TAG>
class LogicalBinop : public Binop<BASE, TAG> {
  public:
    typedef Binop<BASE, TAG> Super;

    LogicalBinop(Value* lhs, Value* rhs, Value* env, unsigned srcIdx)
        : Super(lhs, rhs, env, srcIdx) {}

    using Super::inferredEffectsForLogicalInstruction;
    using Super::inferredTypeForLogicalInstruction;
    using typename Super::GetType;
    PirType inferType(const GetType& getType) const override {
        return inferredTypeForLogicalInstruction(getType);
    }
    Effects inferEffects(const GetType& getType) const override {
        return inferredEffectsForLogicalInstruction(getType);
    }
};

#define LOGICAL_BINOP(Kind)                                                    \
    class Kind : public LogicalBinop<Kind, Tag::Kind> {                        \
      public:                                                                  \
        Kind(Value* lhs, Value* rhs, Value* env, unsigned srcIdx)              \
            : LogicalBinop<Kind, Tag::Kind>(lhs, rhs, env, srcIdx) {}          \
    }

LOGICAL_BINOP(Eq);
LOGICAL_BINOP(Neq);
LOGICAL_BINOP(Lt);
LOGICAL_BINOP(Lte);
LOGICAL_BINOP(Gt);
LOGICAL_BINOP(Gte);

#undef LOGICAL_BINOP

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

class FLIE(Colon, 3, Effects::Any()) {
  public:
    Colon(Value* lhs, Value* rhs, Value* env, unsigned srcIdx)
        : FixedLenInstructionWithEnvSlot(PirType::val(),
                                         {{PirType::val(), PirType::val()}},
                                         {{lhs, rhs}}, env, srcIdx) {}
    VisibilityFlag visibilityFlag() const override {
        if (lhs()->type.isA(PirType::anySimpleScalar()) &&
            rhs()->type.isA(PirType::anySimpleScalar())) {
            return VisibilityFlag::On;
        } else {
            return VisibilityFlag::Unknown;
        }
    }
    Value* lhs() const { return arg<0>().val(); }
    Value* rhs() const { return arg<1>().val(); }

    PirType inferType(const GetType& getType) const override;

    Effects inferEffects(const GetType& getType) const override {
        return inferredEffectsForArithmeticInstruction(getType);
    }
};

// Common interface to all call instructions
class CallInstruction {
  public:
    static constexpr double UnknownTaken = -1;
    double taken = UnknownTaken;

    static CallInstruction* CastCall(Value* v);

    virtual size_t nCallArgs() const = 0;

    typedef std::function<void(SEXP, Value*)> NamedArgumentValueIterator;
    typedef std::function<void(SEXP, InstrArg&)> MutableNamedArgumentIterator;

    void eachCallArg(const Instruction::ArgumentValueIterator& it) const {
        eachNamedCallArg([&](SEXP, Value* v) { it(v); });
    }
    void eachCallArg(const Instruction::MutableArgumentIterator& it) {
        eachNamedCallArg([&](SEXP, InstrArg& a) { it(a); });
    }

    virtual void
    eachNamedCallArg(const NamedArgumentValueIterator& it) const = 0;
    virtual void eachNamedCallArg(const MutableNamedArgumentIterator& it) = 0;
    virtual const InstrArg& callArg(size_t pos) const = 0;
    virtual InstrArg& callArg(size_t pos) = 0;
    virtual Closure* tryGetCls() const { return nullptr; }
    virtual Value* tryGetClsArg() const { return nullptr; }
    virtual Context inferAvailableAssumptions() const;
    virtual bool hasNamedArgs() const { return false; }
    virtual bool isReordered() const { return false; }
    virtual ArglistOrder::CallArglistOrder const& getArgOrderOrig() const {
        assert(false);
        static ArglistOrder::CallArglistOrder empty;
        return empty;
    }
    ClosureVersion* tryDispatch(Closure*) const;
};

// Default call instruction. Closure expression (ie. expr left of `(`) is
// evaluated at runtime and arguments are passed as promises.
class VLIE(Call, Effects::Any()), public CallInstruction {
  public:
    Call(Value * callerEnv, Value * fun, const std::vector<Value*>& args,
         Value* fs, unsigned srcIdx);

    Value* cls() const { return arg(1).val(); }
    void cls(Value * v) { arg(1).val() = v; }

    Value* tryGetClsArg() const override final {
        return cls()->followCastsAndForce();
    }
    Closure* tryGetCls() const override final {
        if (auto mk = MkCls::Cast(cls()->followCastsAndForce()))
            return mk->tryGetCls();
        return nullptr;
    }

    size_t nCallArgs() const override { return nargs() - 3; }

    void eachNamedCallArg(const NamedArgumentValueIterator& it) const override {
        for (size_t i = 0; i < nCallArgs(); ++i)
            it(R_NilValue, arg(i + 2).val());
    }
    void eachNamedCallArg(const MutableNamedArgumentIterator& it) override {
        for (size_t i = 0; i < nCallArgs(); ++i)
            it(R_NilValue, arg(i + 2));
    }
    const InstrArg& callArg(size_t pos) const override final {
        assert(pos < nCallArgs());
        return arg(pos + 2);
    }
    InstrArg& callArg(size_t pos) override final {
        assert(pos < nCallArgs());
        return arg(pos + 2);
    }

    Value* frameStateOrTs() const override final { return arg(0).val(); }
    void updateFrameState(Value * fs) override final { arg(0).val() = fs; }

    Value* callerEnv() { return env(); }

    void printArgs(std::ostream & out, bool tty) const override;
};

class VLIE(NamedCall, Effects::Any()), public CallInstruction {
  public:
    NamedCall(Value * callerEnv, Value * fun, const std::vector<Value*>& args,
              const std::vector<SEXP>& names_, Value* fs, unsigned srcIdx);
    NamedCall(Value * callerEnv, Value * fun, const std::vector<Value*>& args,
              const std::vector<BC::PoolIdx>& names_, Value* fs,
              unsigned srcIdx);

    std::vector<SEXP> names;
    bool hasNamedArgs() const override { return true; }

    Value* cls() const { return arg(1).val(); }

    Value* tryGetClsArg() const override final {
        return cls()->followCastsAndForce();
    }
    Closure* tryGetCls() const override final {
        if (auto mk = MkCls::Cast(cls()->followCastsAndForce()))
            return mk->tryGetCls();
        return nullptr;
    }

    size_t nCallArgs() const override { return nargs() - 3; }

    void eachNamedCallArg(const NamedArgumentValueIterator& it) const override {
        for (size_t i = 0; i < nCallArgs(); ++i)
            it(names[i], arg(i + 2).val());
    }
    void eachNamedCallArg(const MutableNamedArgumentIterator& it) override {
        for (size_t i = 0; i < nCallArgs(); ++i)
            it(names[i], arg(i + 2));
    }
    const InstrArg& callArg(size_t pos) const override final {
        assert(pos < nCallArgs());
        return arg(pos + 2);
    }
    InstrArg& callArg(size_t pos) override final {
        assert(pos < nCallArgs());
        return arg(pos + 2);
    }

    Value* frameStateOrTs() const override final { return arg(0).val(); }
    void updateFrameState(Value * fs) override final { arg(0).val() = fs; }

    Value* callerEnv() { return env(); }

    void printArgs(std::ostream & out, bool tty) const override;
};

// Call instruction for lazy, but staticatlly resolved calls. Closure is
// specified as `cls_`, args passed as promises.
class VLIE(StaticCall, Effects::Any()), public CallInstruction {
    Closure* cls_;
    ArglistOrder::CallArglistOrder argOrderOrig;

  public:
    StaticCall(Value * callerEnv, ClosureVersion * clsVersion,
               Context givenContext, const std::vector<Value*>& args,
               const ArglistOrder::CallArglistOrder& argOrderOrig, Value* fs,
               unsigned srcIdx, Value* runtimeClosure = Tombstone::closure());

    Context givenContext;

    ClosureVersion* hint = nullptr;

    Closure* cls() const { return cls_; }
    void cls(Closure * cls) { cls_ = cls; }

    Closure* tryGetCls() const override final { return cls(); }

    size_t nCallArgs() const override { return nargs() - 3; }

    void eachNamedCallArg(const NamedArgumentValueIterator& it) const override {
        for (size_t i = 0; i < nCallArgs(); ++i)
            it(R_NilValue, arg(i + 2).val());
    }
    void eachNamedCallArg(const MutableNamedArgumentIterator& it) override {
        for (size_t i = 0; i < nCallArgs(); ++i)
            it(R_NilValue, arg(i + 2));
    }
    const InstrArg& callArg(size_t pos) const override final {
        assert(pos < nCallArgs());
        return arg(pos + 2);
    }
    InstrArg& callArg(size_t pos) override final {
        assert(pos < nCallArgs());
        return arg(pos + 2);
    }

    bool isReordered() const override final { return !argOrderOrig.empty(); }
    ArglistOrder::CallArglistOrder const& getArgOrderOrig()
        const override final {
        return argOrderOrig;
    }

    PirType inferType(const GetType& getType) const override final;
    Effects inferEffects(const GetType& getType) const override final;

    Value* frameStateOrTs() const override final { return arg(0).val(); }
    void updateFrameState(Value * fs) override final { arg(0).val() = fs; }

    Value* runtimeClosure() const { return arg(1).val(); }

    void printArgs(std::ostream & out, bool tty) const override;

    Value* callerEnv() { return env(); }

    ClosureVersion* tryDispatch() const;

    ClosureVersion* tryOptimisticDispatch() const;

    Context inferAvailableAssumptions() const override final {
        static Context minimal = ([]() {
            Context m;
            m.add(Assumption::CorrectOrderOfArguments);
            m.add(Assumption::StaticallyArgmatched);
            m.add(Assumption::NotTooManyArguments);
            return m;
        })();
        auto res = CallInstruction::inferAvailableAssumptions() | givenContext;
        assert((res & minimal) == minimal);
        return res;
    }
};

typedef SEXP (*CCODE)(SEXP, SEXP, SEXP, SEXP);

class VLIE(CallBuiltin, Effects::Any()), public CallInstruction {
  public:
    SEXP builtinSexp;
    const CCODE builtin;
    int builtinId;

    size_t nCallArgs() const override { return nargs() - 1; }
    void eachNamedCallArg(const NamedArgumentValueIterator& it) const override {
        for (size_t i = 0; i < nCallArgs(); ++i)
            it(R_NilValue, arg(i).val());
    }
    void eachNamedCallArg(const MutableNamedArgumentIterator& it) override {
        for (size_t i = 0; i < nCallArgs(); ++i)
            it(R_NilValue, arg(i));
    }

    const InstrArg& callArg(size_t pos) const override final {
        assert(pos < nCallArgs());
        return arg(pos);
    }
    InstrArg& callArg(size_t pos) override final {
        assert(pos < nCallArgs());
        return arg(pos);
    }

    void printArgs(std::ostream & out, bool tty) const override;
    Value* callerEnv() { return env(); }

    VisibilityFlag visibilityFlag() const override;
    Value* frameStateOrTs() const override final {
        return Tombstone::framestate();
    }

  private:
    CallBuiltin(Value * callerEnv, SEXP builtin,
                const std::vector<Value*>& args, unsigned srcIdx);
    friend class BuiltinCallFactory;
};

class VLI(CallSafeBuiltin, Effects(Effect::Warn) | Effect::Error |
                               Effect::Visibility | Effect::DependsOnAssume),
    public CallInstruction {
  public:
    SEXP builtinSexp;
    const CCODE builtin;
    int builtinId;

    size_t nCallArgs() const override { return nargs(); }

    void eachNamedCallArg(const NamedArgumentValueIterator& it) const override {
        for (size_t i = 0; i < nCallArgs(); ++i)
            it(R_NilValue, arg(i).val());
    }
    void eachNamedCallArg(const MutableNamedArgumentIterator& it) override {
        for (size_t i = 0; i < nCallArgs(); ++i)
            it(R_NilValue, arg(i));
    }

    const InstrArg& callArg(size_t pos) const override final {
        return arg(pos);
    }
    InstrArg& callArg(size_t pos) override final { return arg(pos); }

    void printArgs(std::ostream & out, bool tty) const override;

    CallSafeBuiltin(SEXP builtin, const std::vector<Value*>& args,
                    unsigned srcIdx);

    PirType inferType(const GetType& at = [](Value* v) { return v->type; })
        const override final;

    VisibilityFlag visibilityFlag() const override;
    Value* frameStateOrTs() const override final {
        return Tombstone::framestate();
    }

    size_t gvnBase() const override;
};

class BuiltinCallFactory {
  public:
    static Instruction* New(Value* callerEnv, SEXP builtin,
                            const std::vector<Value*>& args, unsigned srcIdx);
};

class VLIE(MkEnv, Effect::LeaksArg) {
  public:
    std::vector<SEXP> varName;
    std::vector<bool> missing;
    bool stub = false;
    bool neverStub = false;
    int context = 1;

    typedef std::function<void(SEXP name, Value* val, bool missing)> LocalVarIt;
    typedef std::function<void(SEXP name, InstrArg&, bool& missing)>
        MutableLocalVarIt;

    inline void eachLocalVar(MutableLocalVarIt it) {
        for (size_t i = 0; i < nLocals(); ++i) {
            bool m = missing[i];
            it(varName[i], arg(i), m);
            missing[i] = m;
        }
    }

    inline void eachLocalVar(LocalVarIt it) const {
        for (size_t i = 0; i < nLocals(); ++i) {
            it(varName[i], arg(i).val(), missing[i]);
        }
    }

    inline void eachLocalVarRev(LocalVarIt it) const {
        for (size_t i = 0; i < nLocals(); ++i) {
            size_t ri = nLocals() - i - 1;
            it(varName[ri], arg(ri).val(), missing[ri]);
        }
    }

    MkEnv(Value* lexicalEnv, const std::vector<SEXP>& names, Value** args,
          const std::vector<bool>& missing)
        : VarLenInstructionWithEnvSlot(PirType::env(), lexicalEnv),
          varName(names), missing(missing) {
        for (unsigned i = 0; i < varName.size(); ++i) {
            MkEnv::pushArg(args[i], PirType::any());
        }
    }

    MkEnv(Value* lexicalEnv, const std::vector<SEXP>& names, Value** args)
        : VarLenInstructionWithEnvSlot(PirType::env(), lexicalEnv),
          varName(names) {
        for (unsigned i = 0; i < varName.size(); ++i) {
            MkEnv::pushArg(args[i], PirType::any());
        }
    }

    void pushArg(Value* a, PirType t) override final {
        VarLenInstructionWithEnvSlot::pushArg(a, t);
        missing.push_back(a == MissingArg::instance());
    }

    void pushArg(Value* a) override final {
        VarLenInstructionWithEnvSlot::pushArg(a);
        missing.push_back(a == MissingArg::instance());
    }

    Value* lexicalEnv() const { return env(); }

    void printArgs(std::ostream& out, bool tty) const override;
    void printEnv(std::ostream& out, bool tty) const override final {}
    std::string name() const override { return stub ? "(MkEnv)" : "MkEnv"; }

    size_t nLocals() const { return nargs() - 1; }

    int minReferenceCount() const override { return MAX_REFCOUNT; }

    bool contains(SEXP name) const {
        for (const auto& n : varName)
            if (name == n)
                return true;
        return false;
    }

    unsigned indexOf(SEXP name) const {
        unsigned i = 0;
        for (const auto& n : varName) {
            if (name == n)
                return i;
            ++i;
        }
        assert(false);
        return -1;
    }

    const InstrArg& argNamed(SEXP name) const { return arg(indexOf(name)); }
};

class FLIE(MaterializeEnv, 1, Effects::None()) {
  public:
    explicit MaterializeEnv(MkEnv* e)
        : FixedLenInstructionWithEnvSlot(PirType::env(), e) {}
};

class FLIE(IsEnvStub, 1, Effect::ReadsEnv) {
  public:
    explicit IsEnvStub(MkEnv* e)
        : FixedLenInstructionWithEnvSlot(PirType::test(), e) {}
};

class VLIE(PushContext, Effects(Effect::ChangesContexts) | Effect::LeaksArg |
                            Effect::LeaksEnv) {
    ArglistOrder::CallArglistOrder argOrderOrig;

  public:
    PushContext(Value* ast, Value* op, CallInstruction* call, Value* sysparent)
        : VarLenInstructionWithEnvSlot(NativeType::context, sysparent) {
        call->eachCallArg([&](Value* v) { pushArg(v, PirType::any()); });
        pushArg(ast, PirType::any());
        pushArg(op, PirType::function());
        if (call->isReordered()) {
            argOrderOrig = call->getArgOrderOrig();
        }
    }

    size_t narglist() const { return nargs() - 3; }

    Value* op() const {
        auto op = arg(nargs() - 2).val();
        assert(op->type.isA(PirType::function()));
        return op;
    }
    Value* ast() const { return arg(nargs() - 3).val(); }

    bool isReordered() const { return !argOrderOrig.empty(); }
    ArglistOrder::CallArglistOrder const& getArgOrderOrig() const {
        return argOrderOrig;
    }
};

class FLI(PopContext, 2, Effect::ChangesContexts) {
  public:
    PopContext(Value* res, PushContext* push)
        : FixedLenInstruction(PirType::val(),
                              {{PirType::val(), NativeType::context}},
                              {{res, push}}) {}
    PushContext* push() const { return PushContext::Cast(arg<1>().val()); }
    Value* result() const { return arg<0>().val(); }
};

class FLI(DropContext, 0, Effect::ChangesContexts) {
  public:
    DropContext() : FixedLenInstruction(PirType::voyd()) {}
};

class FLIE(LdDots, 1, Effect::ReadsEnv) {
  public:
    std::vector<SEXP> names;
    explicit LdDots(Value* env)
        : FixedLenInstructionWithEnvSlot(PirType::dotsArg(), {}, {}, env) {}
};

class FLI(ExpandDots, 1, Effects::None()) {
  public:
    std::vector<SEXP> names;
    explicit ExpandDots(Value* dots)
        : FixedLenInstruction(RType::expandedDots, {{PirType::dotsArg()}},
                              {{dots}}) {}
};

class VLI(DotsList, Effect::LeaksArg) {
  public:
    std::vector<SEXP> names;
    DotsList() : VarLenInstruction(RType::dots) {}

    void addInput(SEXP name, Value* val) {
        names.push_back(name);
        VarLenInstruction::pushArg(val, val->type);
    }

    void pushArg(Value* a, PirType t) override {
        assert(false && "use addInput");
    }
    void pushArg(Value* a) override { assert(false && "use addInput"); }

    void printArgs(std::ostream& out, bool tty) const override;

    typedef std::function<void(SEXP name, Value* val)> LocalVarIt;
    inline void eachElement(LocalVarIt it) const {
        for (size_t i = 0; i < nargs(); ++i)
            it(names[i], arg(i).val());
    }
    inline void eachElementRev(LocalVarIt it) const {
        for (long i = nargs() - 1; i >= 0; --i)
            it(names[i], arg(i).val());
    }
};

class VLI(Phi, Effects::None()) {
    std::vector<BB*> input;

  public:
    Phi() : VarLenInstruction(PirType::any()) {}
    explicit Phi(const PirType& t) : VarLenInstruction(t) {}
    explicit Phi(const std::initializer_list<std::pair<BB*, Value*>>& inputs)
        : VarLenInstruction(PirType::any()) {
        for (auto a : inputs)
            addInput(a.first, a.second);
        assert(nargs() == inputs.size());
    }
    void printArgs(std::ostream& out, bool tty) const override;
    PirType inferType(const GetType& getType) const override final {
        if (type.isRType())
            return mergedInputType(getType);
        return Instruction::inferType(getType);
    }
    void pushArg(Value* a, PirType t) override {
        assert(false && "use addInput");
    }
    void pushArg(Value* a) override { assert(false && "use addInput"); }
    void addInput(BB* in, Value* arg) {
        SLOWASSERT(std::find(input.begin(), input.end(), in) == input.end() &&
                   "Duplicate PHI input block");
        input.push_back(in);
        args_.push_back(InstrArg(arg, arg->type.isRType()
                                          ? (arg->type.maybePromiseWrapped()
                                                 ? PirType::any()
                                                 : PirType::val())
                                          : arg->type));
    }
    BB* inputAt(size_t i) const { return input.at(i); }
    void updateInputAt(size_t i, BB* bb) {
        SLOWASSERT(std::find(input.begin(), input.end(), bb) == input.end() &&
                   "Duplicate PHI input block");
        input[i] = bb;
    }
    const std::vector<BB*>& inputs() { return input; }
    void removeInputs(const std::unordered_set<BB*>& del);

    typedef std::function<void(BB*, Value*)> PhiArgumentIterator;
    void eachArg(const PhiArgumentIterator& it) const {
        for (size_t i = 0; i < nargs(); ++i)
            it(input[i], arg(i).val());
    }
    typedef std::function<void(BB*, InstrArg&)> MutablePhiArgumentIterator;
    void eachArg(const MutablePhiArgumentIterator& it) {
        for (size_t i = 0; i < nargs(); ++i)
            it(input[i], arg(i));
    }

    size_t gvnBase() const override { return tagHash(); }
};

// Instructions targeted specially for speculative optimization

/*
 *  Must be the last instruction of a BB with two childs. One should
 *  contain a deopt. Checkpoint takes either branch at random
 *  to ensure the optimizer consider deopt and non-deopt cases.
 */
class Checkpoint : public FixedLenInstruction<Tag::Checkpoint, Checkpoint, 0,
                                              Effects::NoneI(), HasEnvSlot::No,
                                              Controlflow::Branch> {
  public:
    Checkpoint() : FixedLenInstruction(NativeType::checkpoint) {}
    void printArgs(std::ostream& out, bool tty) const override;
    void printGraphArgs(std::ostream& out, bool tty) const override;
    void printGraphBranches(std::ostream& out, size_t bbId) const override;
    BB* deoptBranch();
    BB* nextBB();
};

/*
 * Replaces the current execution context with the one described by the
 * referenced framestate and jump to the deoptimized version of the
 * code at the point the framestate stores
 */

class Deopt : public FixedLenInstruction<Tag::Deopt, Deopt, 3, Effects::AnyI(),
                                         HasEnvSlot::No, Controlflow::Exit> {
  public:
    bool escapedEnv = true;

    explicit Deopt(FrameState* frameState);

    Value* frameStateOrTs() const override final { return arg<0>().val(); }
    FrameState* frameState() const {
        return FrameState::Cast(frameStateOrTs());
    }

    bool hasDeoptReason() const;

    Value* deoptReason() const { return arg<1>().val(); }
    Value* deoptTrigger() const { return arg<2>().val(); }

    void setDeoptReason(Value* dr, Value* dt) {
        assert(!hasDeoptReason());
        arg<1>().val() = dr;
        arg<2>().val() = dt;
    }

    void printArgs(std::ostream& out, bool tty) const override;
};

/*
 * if the test fails, jump to the deopt branch of the checkpoint.
 */

class FLI(Assume, 2, Effect::TriggerDeopt) {
  public:
    bool assumeTrue = true;
    const DeoptReason reason;

    Assume(Value* test, Value* checkpoint, const DeoptReason& r,
           bool expectation = true)
        : FixedLenInstruction(PirType::voyd(),
                              {{PirType::test(), NativeType::checkpoint}},
                              {{test, checkpoint}}),
          assumeTrue(expectation), reason(r) {
        assert(reason.reason != DeoptReason::DeadCall &&
               reason.reason != DeoptReason::Unknown);
    }

    Checkpoint* checkpoint() const { return Checkpoint::Cast(arg(1).val()); }
    void checkpoint(Checkpoint* cp) { arg(1).val() = cp; }
    Value* condition() const { return arg(0).val(); }
    std::string name() const override {
        return assumeTrue ? "Assume" : "AssumeNot";
    }

    bool triviallyHolds() const {
        auto pirBool =
            assumeTrue ? (Value*)True::instance() : (Value*)False::instance();
        return arg(0).val() == pirBool;
    }

    Value* valueUnderTest() const;

    void printArgs(std::ostream& out, bool tty) const override;
};

#undef FLI
#undef VLI
#undef FLIE
#undef VLIE

} // namespace pir
} // namespace rir

#endif
