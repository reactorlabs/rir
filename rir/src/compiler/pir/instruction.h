#ifndef COMPILER_INSTRUCTION_H
#define COMPILER_INSTRUCTION_H

#include "R/r.h"
#include "instruction_list.h"
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
    virtual bool hasEnv() const = 0;
    virtual bool accessesEnv() const = 0;

    virtual size_t nargs() const = 0;
    virtual size_t envSlot() const = 0;
    virtual Value* env() const = 0;
    virtual void env(Value*) = 0;

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
    void replaceUsesIn(Value* val, BB* target);
    bool unused();

    virtual void printArgs(std::ostream& out);
    virtual void print(std::ostream&);
    void printRef(std::ostream& out) override;
    void print() { print(std::cerr); }

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

    void operator=(InstructionImplementation&) = delete;
    InstructionImplementation() = delete;

    Instruction* clone() const override {
        assert(Base::Cast(this));
        return new Base(*static_cast<const Base*>(this));
    }

    struct InstrDescription {
        bool HasEffect;
        bool ChangesEnv;
        bool LeaksEnv;
        bool HasEnv;
        bool AccessesEnv;
    };

    static constexpr InstrDescription Description = {
        EFFECT > Effect::None, ENV >= EnvAccess::Write, ENV == EnvAccess::Leak,
        ENV > EnvAccess::None, ENV > EnvAccess::Capture};

    bool hasEffect() const final { return Description.HasEffect; }
    bool changesEnv() const final { return Description.ChangesEnv; }
    bool leaksEnv() const final { return Description.LeaksEnv; }
    bool hasEnv() const final { return Description.HasEnv; }
    bool accessesEnv() const final { return Description.AccessesEnv; }

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
class FixedLenInstruction
    : public InstructionImplementation<ITAG, Base, EFFECT, ENV,
                                       std::array<InstrArg, ARGS>> {
  public:
    typedef InstructionImplementation<ITAG, Base, EFFECT, ENV,
                                      std::array<InstrArg, ARGS>>
        Super;
    using Super::arg;
    using Super::Description;

    static_assert(
        !Description.HasEnv || ARGS > 0,
        "This instruction needs at least 1 argument slot for the env");

    constexpr static size_t ENV_SLOT = Description.HasEnv ? ARGS - 1 : -1;

    size_t nargs() const override { return ARGS; }
    size_t envSlot() const override { return ENV_SLOT; }

    template <unsigned POS>
    InstrArg& arg() {
        static_assert(POS != ENV_SLOT, "use env() instead");
        static_assert(POS < ARGS, "This instruction has fewer arguments");
        return arg(POS);
    }

    Value* env() const override {
        assert(Description.HasEnv);
        return arg(ENV_SLOT).val();
    }

    void env(Value* v) override {
        assert(v);
        assert(Description.HasEnv);
        arg(ENV_SLOT).val() = v;
    }

    FixedLenInstruction(PirType resultType, Value* env, unsigned srcIdx = 0)
        : Super(resultType, ArgsZip(env), srcIdx) {
        assert(env);
        static_assert(Description.HasEnv,
                      "Invalid constructor for instruction without env");
        static_assert(ARGS == 1, "This instruction expects more arguments");
    }

    FixedLenInstruction(PirType resultType, unsigned srcIdx = 0)
        : Super(resultType, {}, srcIdx) {
        static_assert(!Description.HasEnv,
                      "Invalid constructor for instruction with env");
        static_assert(ARGS == 0, "This instruction expects more arguments");
    }

    FixedLenInstruction(PirType resultType,
                        const std::array<PirType, ARGS - 1>& at,
                        const std::array<Value*, ARGS - 1>& arg, Value* env,
                        unsigned srcIdx = 0)
        : Super(resultType, ArgsZip(arg, at, env), srcIdx) {
        assert(env);
        static_assert(Description.HasEnv,
                      "Invalid constructor for instruction without env");
    }

    FixedLenInstruction(PirType resultType, const std::array<PirType, ARGS>& at,
                        const std::array<Value*, ARGS>& arg,
                        unsigned srcIdx = 0)
        : Super(resultType, ArgsZip(arg, at), srcIdx) {
        static_assert(!Description.HasEnv,
                      "Invalid constructor for instruction with env");
    }

  private:
    // Some helpers to combine args and environment into one array
    struct ArgsZip : public std::array<InstrArg, ARGS> {
        ArgsZip(const std::array<Value*, ARGS - 1>& a,
                const std::array<PirType, ARGS - 1>& t, Value* env) {
            static_assert(ENV_SLOT < ARGS, "");
            (*this)[ENV_SLOT].val() = env;
            (*this)[ENV_SLOT].type() = RType::env;
            for (size_t i = 0; i < ARGS - 1; ++i) {
                (*this)[i].val() = a[i];
                (*this)[i].type() = t[i];
            }
        }
        ArgsZip(Value* env)
            : std::array<InstrArg, ARGS>({{InstrArg(env, RType::env)}}) {}

        ArgsZip(const std::array<Value*, ARGS>& a,
                const std::array<PirType, ARGS>& t) {
            for (size_t i = 0; i < ARGS; ++i) {
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

    size_t envSlot() const override {
        assert(nargs() > 0);
        return nargs() - 1;
    }

    Value* env() const override {
        assert(Description.HasEnv);
        assert(arg(envSlot()).type() == RType::env);
        return arg(envSlot()).val();
    }

    void env(Value* v) override {
        assert(v);
        assert(Description.HasEnv);
        assert(arg(envSlot()).type() == RType::env);
        arg(envSlot()).val() = v;
    }

    void pushArg(Value* a, PirType t) {
        assert(a);
        if (!Description.HasEnv) {
            args_.push_back(InstrArg(a, t));
            return;
        }
        args_.push_back(args_.back());
        args_[nargs() - 2] = InstrArg(a, t);
    }

    void pushArg(Value* a) { pushArg(a, a->type); }

    VarLenInstruction(PirType return_type, unsigned srcIdx = 0)
        : Super(return_type, srcIdx) {
        static_assert(!Description.HasEnv,
                      "This instruction needs an environment");
    }

    VarLenInstruction(PirType return_type, Value* env, unsigned srcIdx = 0)
        : Super(return_type, srcIdx) {
        assert(env);
        static_assert(Description.HasEnv,
                      "This instruction has no environment access");
        args_.push_back(InstrArg(env, RType::env));
    }
};

extern std::ostream& operator<<(std::ostream& out,
                                Instruction::InstructionUID id);

#define FLI(type, nargs, io, env)                                              \
    type:                                                                      \
  public                                                                       \
    FixedLenInstruction<Tag::type, type, nargs, io, env>

class FLI(LdConst, 0, Effect::None, EnvAccess::None) {
  public:
    LdConst(SEXP c, PirType t) : FixedLenInstruction(t), c(c) {}
    LdConst(SEXP c) : FixedLenInstruction(PirType(c)), c(c) {}
    SEXP c;
    void printArgs(std::ostream& out) override;
};

class FLI(LdFun, 1, Effect::Any, EnvAccess::Write) {
  public:
    SEXP varName;

    LdFun(const char* name, Value* env)
        : FixedLenInstruction(RType::closure, env), varName(Rf_install(name)) {}
    LdFun(SEXP name, Value* env)
        : FixedLenInstruction(RType::closure, env), varName(name) {
        assert(TYPEOF(name) == SYMSXP);
    }

    void printArgs(std::ostream& out) override;
};

class FLI(LdVar, 1, Effect::None, EnvAccess::Read) {
  public:
    SEXP varName;

    LdVar(const char* name, Value* env)
        : FixedLenInstruction(PirType::any(), env), varName(Rf_install(name)) {}
    LdVar(SEXP name, Value* env)
        : FixedLenInstruction(PirType::any(), env), varName(name) {
        assert(TYPEOF(name) == SYMSXP);
    }

    void printArgs(std::ostream& out) override;
};

class FLI(ForSeqSize, 1, Effect::Error, EnvAccess::None) {
  public:
    ForSeqSize(Value* val)
        : FixedLenInstruction(PirType(RType::integer).scalar(),
                              {{PirType::val()}}, {{val}}) {}
};

class FLI(LdArg, 0, Effect::None, EnvAccess::None) {
  public:
    size_t id;

    LdArg(size_t id) : FixedLenInstruction(PirType::valOrLazy()), id(id) {}

    void printArgs(std::ostream& out) override;
};

class FLI(ChkMissing, 1, Effect::Warn, EnvAccess::None) {
  public:
    ChkMissing(Value* in)
        : FixedLenInstruction(PirType::valOrLazy(), {{PirType::any()}},
                              {{in}}) {}
};

class FLI(ChkClosure, 1, Effect::Warn, EnvAccess::None) {
  public:
    ChkClosure(Value* in)
        : FixedLenInstruction(RType::closure, {{PirType::val()}}, {{in}}) {}
};

class FLI(StVarSuper, 2, Effect::None, EnvAccess::Write) {
  public:
    StVarSuper(SEXP name, Value* val, Value* env)
        : FixedLenInstruction(PirType::voyd(), {{PirType::val()}}, {{val}},
                              env),
          varName(name) {}

    StVarSuper(const char* name, Value* val, Value* env)
        : FixedLenInstruction(PirType::voyd(), {{PirType::val()}}, {{val}},
                              env),
          varName(Rf_install(name)) {}

    SEXP varName;
    Value* val() { return arg<0>().val(); }

    void printArgs(std::ostream& out) override;
};

class FLI(LdVarSuper, 1, Effect::None, EnvAccess::Read) {
  public:
    LdVarSuper(SEXP name, Value* env)
        : FixedLenInstruction(PirType::any(), env), varName(name) {}

    LdVarSuper(const char* name, Value* env)
        : FixedLenInstruction(PirType::any(), env), varName(Rf_install(name)) {}

    SEXP varName;

    void printArgs(std::ostream& out) override;
};

class FLI(StVar, 2, Effect::None, EnvAccess::Write) {
  public:
    StVar(SEXP name, Value* val, Value* env)
        : FixedLenInstruction(PirType::voyd(), {{PirType::val()}}, {{val}},
                              env),
          varName(name) {}

    StVar(const char* name, Value* val, Value* env)
        : FixedLenInstruction(PirType::voyd(), {{PirType::val()}}, {{val}},
                              env),
          varName(Rf_install(name)) {}

    SEXP varName;
    Value* val() { return arg<0>().val(); }

    void printArgs(std::ostream& out) override;
};

class FLI(Branch, 1, Effect::None, EnvAccess::None) {
  public:
    Branch(Value* test)
        : FixedLenInstruction(PirType::voyd(), {{NativeType::test}}, {{test}}) {
    }
    void printArgs(std::ostream& out) override;
};

class FLI(Return, 1, Effect::None, EnvAccess::None) {
  public:
    Return(Value* ret)
        : FixedLenInstruction(PirType::voyd(), {{PirType::val()}}, {{ret}}) {}
};

class Promise;
class FLI(MkArg, 2, Effect::None, EnvAccess::Capture) {
  public:
    Promise* prom;
    MkArg(Promise* prom, Value* v, Value* env)
        : FixedLenInstruction(RType::prom, {{PirType::valOrMissing()}}, {{v}},
                              env),
          prom(prom) {
        assert(eagerArg() == v);
    }
    MkArg(Value* v, Value* env)
        : FixedLenInstruction(RType::prom, {{PirType::val()}}, {{v}}, env),
          prom(nullptr) {
        assert(eagerArg() == v);
    }

    typedef std::function<void(Promise*)> PromMaybe;
    typedef std::function<void(Value*)> EagerMaybe;

    Value* eagerArg() { return arg<0>().val(); }

    void ifEager(EagerMaybe maybe) {
        if (eagerArg() != Missing::instance())
            maybe(eagerArg());
    }

    void printArgs(std::ostream& out) override;
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

class FLI(MkCls, 4, Effect::None, EnvAccess::Capture) {
  public:
    MkCls(Value* fml, Value* code, Value* src, Value* parent)
        : FixedLenInstruction(RType::closure,
                              {{PirType::list(), RType::code, PirType::any()}},
                              {{fml, code, src}}, parent) {}
};

class FLI(MkFunCls, 1, Effect::None, EnvAccess::Capture) {
  public:
    Closure* fun;
    SEXP fml, code, src;
    MkFunCls(Closure* fun, Value* parent, SEXP fml, SEXP code, SEXP src);
    void printArgs(std::ostream&) override;
};

class FLI(Force, 2, Effect::Any, EnvAccess::Leak) {
  public:
    Force(Value* in, Value* env)
        : FixedLenInstruction(PirType::val(), {{PirType::any()}}, {{in}}, env) {
    }
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
    AsTest(Value* in)
        : FixedLenInstruction(NativeType::test, {{RType::logical}}, {{in}}) {}
};

class FLI(Subassign1_1D, 3, Effect::None, EnvAccess::None) {
  public:
    Subassign1_1D(Value* vec, Value* index, Value* val)
        : FixedLenInstruction(
              PirType::val(),
              {{PirType::val(), PirType::val(), PirType::val()}},
              {{vec, index, val}}) {}
};

class FLI(Subassign2_1D, 3, Effect::None, EnvAccess::None) {
  public:
    Subassign2_1D(Value* vec, Value* index, Value* value, SEXP sym)
        : FixedLenInstruction(
              PirType::val(),
              {{PirType::val(), PirType::val(), PirType::val()}},
              {{vec, index, value}}),
          sym(sym) {}
    SEXP sym;
};

class FLI(Extract1_1D, 3, Effect::None, EnvAccess::Leak) {
  public:
    Extract1_1D(Value* vec, Value* idx, Value* env, unsigned srcIdx)
        : FixedLenInstruction(PirType::val(),
                              {{PirType::val(), PirType::val()}}, {{vec, idx}},
                              env, srcIdx) {}
};

class FLI(Extract2_1D, 3, Effect::None, EnvAccess::Leak) {
  public:
    Extract2_1D(Value* vec, Value* idx, Value* env, unsigned srcIdx)
        : FixedLenInstruction(PirType::val().scalar(),
                              {{PirType::val(), PirType::val()}}, {{vec, idx}},
                              env, srcIdx) {}
};

class FLI(Extract1_2D, 4, Effect::None, EnvAccess::Leak) {
  public:
    Extract1_2D(Value* vec, Value* idx1, Value* idx2, Value* env,
                unsigned srcIdx)
        : FixedLenInstruction(
              PirType::val(),
              {{PirType::val(), PirType::val(), PirType::val()}},
              {{vec, idx1, idx2}}, env, srcIdx) {}
};

class FLI(Extract2_2D, 4, Effect::None, EnvAccess::Leak) {
  public:
    Extract2_2D(Value* vec, Value* idx1, Value* idx2, Value* env,
                unsigned srcIdx)
        : FixedLenInstruction(
              PirType::val().scalar(),
              {{PirType::val(), PirType::val(), PirType::val()}},
              {{vec, idx1, idx2}}, env, srcIdx) {}
};

class FLI(Inc, 1, Effect::None, EnvAccess::None) {
  public:
    Inc(Value* v)
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

    void printArgs(std::ostream& out) override;
};

class FLI(IsObject, 1, Effect::None, EnvAccess::None) {
  public:
    IsObject(Value* v)
        : FixedLenInstruction(NativeType::test, {{PirType::val()}}, {{v}}) {}
};

class FLI(LdFunctionEnv, 0, Effect::None, EnvAccess::None) {
  public:
    LdFunctionEnv() : FixedLenInstruction(RType::env) {}
};

class FLI(SetShared, 1, Effect::Write, EnvAccess::None) {
  public:
    SetShared(Value* v) : FixedLenInstruction(v->type, {{v->type}}, {{v}}) {}
};

class FLI(PirCopy, 1, Effect::None, EnvAccess::None) {
  public:
    PirCopy(Value* v) : FixedLenInstruction(v->type, {{v->type}}, {{v}}) {}
    void print(std::ostream& out) override;
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
    class FLI(Name, 3, Effect::None, EnvAccess::Leak) {                        \
      public:                                                                  \
        Name(Value* lhs, Value* rhs, Value* env, unsigned srcIdx)              \
            : FixedLenInstruction(Type, {{PirType::val(), PirType::val()}},    \
                                  {{lhs, rhs}}, env, srcIdx) {}                \
    }

BINOP(Gte, PirType::val());
BINOP(Lte, PirType::val());
BINOP(Mul, PirType::val());
BINOP(Div, PirType::val());
BINOP(IDiv, PirType::val());
BINOP(Mod, PirType::val());
BINOP(Add, PirType::val());
BINOP(Colon, PirType::val());
BINOP(Pow, PirType::val());
BINOP(Sub, PirType::val());
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
    class FLI(Name, 2, Effect::None, EnvAccess::Leak) {                        \
      public:                                                                  \
        Name(Value* v, Value* env, unsigned srcIdx)                            \
            : FixedLenInstruction(PirType::val(), {{PirType::val()}}, {{v}},   \
                                  env, srcIdx) {}                              \
    }

UNOP(Not);
UNOP(Plus);
UNOP(Minus);
UNOP(Length);

#undef UNOP
#undef FLI

#define VLI(type, io, env)                                                     \
    type:                                                                      \
  public                                                                       \
    VarLenInstruction<Tag::type, type, io, env>

// Common interface to all call instructions
class CallInstruction {
  public:
    virtual size_t nCallArgs() = 0;
    virtual void eachCallArg(Instruction::ArgumentValueIterator it) = 0;
    virtual void eachCallArgRev(Instruction::ArgumentValueIterator it) = 0;
    static CallInstruction* CastCall(Value* v);
};

template <Tag ITAG, class Base, Effect EFFECT, EnvAccess ENV,
          bool HAS_TARGET_ARG>
class CallInstructionImplementation
    : public VarLenInstruction<ITAG, Base, EFFECT, ENV>,
      public CallInstruction {
  public:
    typedef VarLenInstruction<ITAG, Base, EFFECT, ENV> Super;
    CallInstructionImplementation(PirType returnType, Value* env,
                                  unsigned srcIdx)
        : Super(returnType, env, srcIdx) {}
    CallInstructionImplementation(PirType returnType, unsigned srcIdx)
        : Super(returnType, srcIdx) {}

    using Super::arg;
    using Super::args_;
    using Super::Description;
    using Super::nargs;

    static constexpr size_t CallArgOffset = (HAS_TARGET_ARG ? 1 : 0);
    size_t nCallArgs() final {
        return nargs() - (HAS_TARGET_ARG ? 1 : 0) -
               (Description.HasEnv ? 1 : 0);
    }

    void eachCallArg(Instruction::ArgumentValueIterator it) override {
        for (size_t i = 0; i < nCallArgs(); ++i)
            it(arg(i + CallArgOffset).val());
    }

    void eachCallArgRev(Instruction::ArgumentValueIterator it) override {
        for (int i = nCallArgs() - 1; i >= 0; --i)
            it(arg(i + CallArgOffset).val());
    }
};

#define ACallInstructionImplementation(type, io, env, callArgOffset)           \
    type:                                                                      \
  public                                                                       \
    CallInstructionImplementation<Tag::type, type, io, env, callArgOffset>

// Default call instruction. Closure expression (ie. expr left of `(`) is
// evaluated at runtime and arguments are passed as promises.
class ACallInstructionImplementation(Call, Effect::Any, EnvAccess::Leak, true) {
  public:
    constexpr static size_t clsIdx = 0;

    Value* cls() { return arg(clsIdx).val(); }

    Call(Value * e, Value * fun, const std::vector<Value*>& args,
         unsigned srcIdx)
        : CallInstructionImplementation(PirType::valOrLazy(), e, srcIdx) {
        pushArg(fun, RType::closure);
        for (unsigned i = 0; i < args.size(); ++i)
            pushArg(args[i], PirType::val());
    }
};

// Call instruction for lazy, but staticatlly resolved calls. Closure is
// specified as `cls_`, args passed as promises.
class ACallInstructionImplementation(StaticCall, Effect::Any, EnvAccess::Leak,
                                     false) {
    Closure* cls_;
    SEXP origin_;

  public:
    Closure* cls() { return cls_; }
    SEXP origin() { return origin_; }

    StaticCall(Value * e, Closure * cls, const std::vector<Value*>& args,
               SEXP origin, unsigned srcIdx)
        : CallInstructionImplementation(PirType::valOrLazy(), e, srcIdx),
          cls_(cls), origin_(origin) {
        for (unsigned i = 0; i < args.size(); ++i)
            pushArg(args[i], PirType::val());
    }

    void printArgs(std::ostream&) override;
};

typedef SEXP (*CCODE)(SEXP, SEXP, SEXP, SEXP);

class ACallInstructionImplementation(CallBuiltin, Effect::Any, EnvAccess::Leak,
                                     false) {
  public:
    SEXP blt;
    const CCODE builtin;
    int builtinId;

    CallBuiltin(Value * e, SEXP builtin, const std::vector<Value*>& args,
                unsigned srcIdx);

    void printArgs(std::ostream & out) override;
};

class ACallInstructionImplementation(CallSafeBuiltin, Effect::None,
                                     EnvAccess::None, false) {
  public:
    SEXP blt;
    const CCODE builtin;
    int builtinId;

    CallSafeBuiltin(SEXP builtin, const std::vector<Value*>& args,
                    unsigned srcIdx);

    void printArgs(std::ostream & out) override;
};

class VLI(MkEnv, Effect::None, EnvAccess::Capture) {
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

    MkEnv(Value* parent, const std::vector<SEXP>& names, Value** args)
        : VarLenInstruction(RType::env, parent), varName(names) {
        for (unsigned i = 0; i < varName.size(); ++i)
            pushArg(args[i], PirType::any());
    }

    Value* parent() { return env(); }
    void parent(Value* v) { env(v); }

    void printArgs(std::ostream& out) override;

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
    void printArgs(std::ostream& out) override;
    bool updateType();
    template <bool E = false>
    inline void pushArg(Value* a) {
        static_assert(E, "use addInput");
    }
    void addInput(BB* in, Value* arg) {
        input.push_back(in);
        VarLenInstruction::pushArg(arg);
    }
    typedef std::function<void(BB* bb, Value*)> PhiArgumentIterator;

    void eachArg(PhiArgumentIterator it) const {
        for (size_t i = 0; i < nargs(); ++i)
            it(input[i], arg(i).val());
    }
};

class VLI(Deopt, Effect::Any, EnvAccess::Leak) {
  public:
    struct Frame {
        Opcode* pc;
        rir::Code* code;
    };
    std::vector<Frame> frames;

    Deopt(Value* env, rir::Code* code, Opcode* pc,
          const std::deque<Value*>& stack)
        : VarLenInstruction(PirType::voyd(), env) {
        frames.push_back({pc, code});
        for (size_t i = 0; i < stack.size(); ++i)
            pushArg(stack[i], PirType::any());
    }

    void printArgs(std::ostream& out) override;
};

#undef VLI
} // namespace pir
} // namespace rir

#endif
