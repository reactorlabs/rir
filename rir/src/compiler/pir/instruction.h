#ifndef COMPILER_INSTRUCTION_H
#define COMPILER_INSTRUCTION_H

#include "R/r.h"
#include "instruction_list.h"
#include "pir.h"
#include "tag.h"
#include "value.h"

#include <array>
#include <cassert>
#include <cstdint>
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

namespace pir {

class BB;
class Closure;
class Phi;

struct InstrArg : public std::pair<Value*, PirType> {
    InstrArg(Value* v, PirType t) : std::pair<Value*, PirType>(v, t) {}
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

    Instruction(Tag tag, PirType t) : Value(t, tag) {}

    bool mightIO() const;
    bool changesEnv() const;
    bool leaksEnv() const;
    bool hasEnv() const;
    bool accessesEnv() const;

    virtual size_t nargs() const = 0;
    virtual Value* env() const = 0;
    virtual void env(Value*) = 0;

    virtual Instruction* clone() const = 0;

    BB* bb_;
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
    void printRef(std::ostream& out);
    void print() { print(std::cerr); }

    virtual InstrArg& arg(size_t pos) = 0;
    virtual const InstrArg& arg(size_t pos) const = 0;

    typedef std::function<void(Value*)> ArgumentValueIterator;
    typedef std::function<void(Instruction*)> ArgumentInstructionIterator;
    typedef std::function<void(const InstrArg&)> ArgumentIterator;
    typedef std::function<void(InstrArg&)> MutableArgumentIterator;

    void eachArg(Instruction::ArgumentValueIterator it) const {
        for (size_t i = 0; i < nargs(); ++i)
            it(arg(i).val());
    }

    void eachInstructionArg(Instruction::ArgumentInstructionIterator it) const {
        for (size_t i = 0; i < nargs(); ++i) {
            auto in = Instruction::Cast(arg(i).val());
            if (in)
                it(in);
        }
    }

    void eachArg(Instruction::ArgumentIterator it) const {
        for (size_t i = 0; i < nargs(); ++i)
            it(arg(i));
    }

    void eachArg(Instruction::MutableArgumentIterator it) {
        for (size_t i = 0; i < nargs(); ++i)
            it(arg(i));
    }

    void eachStackArg(Instruction::ArgumentInstructionIterator it) const {
        for (int i = nargs() - 1; i >= 0; --i) {
            // skip non-instructions (ie. static environments)
            auto instr = Instruction::Cast(arg(i).val());
            if (instr)
                it(instr);
        }
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

    struct Description {
        const bool mightIO;
        const bool changesEnv;
        const bool leaksEnv;
        const bool hasEnv;
        const bool accessEnv;
        Description(Effect effect, EnvAccess env)
            : mightIO(effect > Effect::None),
              changesEnv(env >= EnvAccess::Write),
              leaksEnv(env == EnvAccess::Leak), hasEnv(env > EnvAccess::None),
              accessEnv(env > EnvAccess::Capture) {}
    };
};

template <Tag ITAG, class Base, Effect EFFECT, EnvAccess ENV, class ArgStore>
class InstructionImplementation : public Instruction {
  protected:
    ArgStore args_;

  public:
    InstructionImplementation(PirType resultType)
        : Instruction(ITAG, resultType), args_({}) {}
    InstructionImplementation(PirType resultType, const ArgStore& args)
        : Instruction(ITAG, resultType), args_(args) {}

    void operator=(InstructionImplementation&) = delete;
    InstructionImplementation() = delete;

    Instruction* clone() const override {
        assert(Base::Cast(this));
        return new Base(*static_cast<const Base*>(this));
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

    const InstrArg& arg(size_t pos) const override final { return args_[pos]; }

    InstrArg& arg(size_t pos) override final { return args_[pos]; }

    const static Instruction::Description getDescription() {
        return Instruction::Description(EFFECT, ENV);
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

    static_assert(
        ENV == EnvAccess::None || ARGS > 0,
        "This instruction needs at least 1 argument slot for the env");

    size_t nargs() const override { return ARGS; }

    template <unsigned POS>
    InstrArg& arg() {
        static_assert(POS < ARGS, "This instruction has fewer arguments");
        return arg(POS);
    }

    Value* env() const override {
        // TODO find a better way
        assert(ENV > EnvAccess::None);
        return arg(ARGS - 1).val();
    }

    void env(Value* v) override {
        assert(v);
        // TODO find a better way
        assert(ENV > EnvAccess::None);
        arg(ARGS - 1).val() = v;
    }

    FixedLenInstruction(PirType resultType, Value* env)
        : Super(resultType, ArgsZip(env)) {
        assert(env);
        static_assert(ENV != EnvAccess::None,
                      "Invalid constructor for instruction without env");
        static_assert(ARGS == 1, "This instruction expects more arguments");
    }

    FixedLenInstruction(PirType resultType) : Super(resultType, {}) {
        static_assert(ENV == EnvAccess::None,
                      "Invalid constructor for instruction with env");
        static_assert(ARGS == 0, "This instruction expects more arguments");
    }

    FixedLenInstruction(PirType resultType,
                        const std::array<PirType, ARGS - 1>& at,
                        const std::array<Value*, ARGS - 1>& arg, Value* env)
        : Super(resultType, ArgsZip(arg, at, env)) {
        assert(env);
        static_assert(ENV != EnvAccess::None,
                      "Invalid constructor for instruction without env");
    }

    FixedLenInstruction(PirType resultType, const std::array<PirType, ARGS>& at,
                        const std::array<Value*, ARGS>& arg)
        : Super(resultType, ArgsZip(arg, at)) {
        static_assert(ENV == EnvAccess::None,
                      "Invalid constructor for instruction with env");
    }

  private:
    // Some helpers to combine args and environment into one array
    struct ArgsZip : public std::array<InstrArg, ARGS> {
        ArgsZip(const std::array<Value*, ARGS - 1>& a,
                const std::array<PirType, ARGS - 1>& t, Value* env) {
            for (size_t i = 0; i < ARGS - 1; ++i) {
                (*this)[i].val() = a[i];
                (*this)[i].type() = t[i];
            }
            (*this)[ARGS - 1].val() = env;
            (*this)[ARGS - 1].type() = RType::env;
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
    using Super::nargs;
    using Super::arg;
    using Super::args_;

    Value* env() const override {
        // TODO find a better way
        assert(ENV != EnvAccess::None);
        return arg(0).val();
    }

    void env(Value* v) override {
        assert(v);
        // TODO find a better way
        assert(ENV != EnvAccess::None);
        arg(0).val() = v;
    }

    void pushArg(Value* a) {
        args_.push_back(InstrArg(a, a->type));
        assert(ENV == EnvAccess::None || arg(0).type() == RType::env);
    }

    void pushArg(Value* a, PirType t) {
        args_.push_back(InstrArg(a, t));
        assert(ENV == EnvAccess::None || arg(0).type() == RType::env);
    }

    VarLenInstruction(PirType return_type) : Super(return_type) {
        static_assert(ENV == EnvAccess::None,
                      "This instruction needs an environment");
    }

    VarLenInstruction(PirType return_type, Value* env) : Super(return_type) {
        assert(env);
        static_assert(ENV > EnvAccess::None,
                      "This instruction has no environment access");
        pushArg(env, RType::env);
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
        : FixedLenInstruction(PirType::voyd(), env), varName(name) {}

    LdVarSuper(const char* name, Value* env)
        : FixedLenInstruction(PirType::voyd(), env), varName(Rf_install(name)) {
    }

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
          prom(prom) {}
    MkArg(Value* v, Value* env)
        : FixedLenInstruction(RType::prom, {{PirType::val()}}, {{v}}, env),
          prom(nullptr) {}
    typedef std::function<void(Promise*)> PromMaybe;

    void printArgs(std::ostream& out) override;
};

class FLI(Seq, 3, Effect::None, EnvAccess::None) {
  public:
    Seq(Value* start, Value* end, Value* step)
        : FixedLenInstruction(
              PirType::num(),
              {{PirType::num().scalar(), PirType::num().scalar(),
                PirType::num().scalar()}},
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

class FLI(Force, 1, Effect::Any, EnvAccess::None) {
  public:
    Force(Value* in)
        : FixedLenInstruction(PirType::val(), {{PirType::any()}}, {{in}}) {}
};

class FLI(CastType, 1, Effect::None, EnvAccess::None) {
  public:
    CastType(Value* in, PirType from, PirType to)
        : FixedLenInstruction(to, {{from}}, {{in}}) {}
};

class FLI(AsLogical, 1, Effect::Warn, EnvAccess::None) {
  public:
    AsLogical(Value* in)
        : FixedLenInstruction(RType::logical, {{PirType::val()}}, {{in}}) {}
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
              {{vec, index, value}}), sym(sym) {}
    SEXP sym;
};

class FLI(Extract1_1D, 2, Effect::None, EnvAccess::None) {
  public:
    Extract1_1D(Value* vec, Value* idx)
        : FixedLenInstruction(PirType::val(),
                              {{PirType::val(), PirType::val()}},
                              {{vec, idx}}) {}
};

class FLI(Extract2_1D, 2, Effect::None, EnvAccess::None) {
  public:
    Extract2_1D(Value* vec, Value* idx)
        : FixedLenInstruction(PirType::val().scalar(),
                              {{PirType::val(), PirType::val()}},
                              {{vec, idx}}) {}
};

class FLI(Extract1_2D, 3, Effect::None, EnvAccess::None) {
  public:
    Extract1_2D(Value* vec, Value* idx1, Value* idx2)
        : FixedLenInstruction(
              PirType::val(),
              {{PirType::val(), PirType::val(), PirType::val()}},
              {{vec, idx1, idx2}}) {}
};

class FLI(Extract2_2D, 3, Effect::None, EnvAccess::None) {
  public:
    Extract2_2D(Value* vec, Value* idx1, Value* idx2)
        : FixedLenInstruction(
              PirType::val().scalar(),
              {{PirType::val(), PirType::val(), PirType::val()}},
              {{vec, idx1, idx2}}) {}
};

class FLI(Inc, 1, Effect::None, EnvAccess::None) {
  public:
    Inc(Value* v)
        : FixedLenInstruction(PirType(RType::integer).scalar(),
                              {{PirType(RType::integer).scalar()}}, {{v}}) {}
};

class FLI(Is, 1, Effect::None, EnvAccess::None) {
  public:
    Is(uint32_t tag, Value* v)
        : FixedLenInstruction(PirType(RType::logical).scalar(),
                              {{PirType::val()}}, {{v}}),
          tag(tag) {}
    uint32_t tag;

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

class FLI(PirCopy, 1, Effect::None, EnvAccess::None) {
  public:
    PirCopy(Value* v) : FixedLenInstruction(v->type, {{v->type}}, {{v}}) {}
    void print(std::ostream& out) override;
};

#define SAFE_BINOP(Name, Type)                                                 \
    class FLI(Name, 2, Effect::None, EnvAccess::None) {                        \
      public:                                                                  \
        Name(Value* lhs, Value* rhs, unsigned src)                             \
            : FixedLenInstruction(Type, {{PirType::val(), PirType::val()}},    \
                                  {{lhs, rhs}}) {                              \
            srcIdx = src;                                                      \
        }                                                                      \
    }

SAFE_BINOP(Gte, PirType::val());
SAFE_BINOP(Lte, PirType::val());
SAFE_BINOP(Mul, PirType::val());
SAFE_BINOP(Div, PirType::val());
SAFE_BINOP(IDiv, PirType::val());
SAFE_BINOP(Mod, PirType::val());
SAFE_BINOP(Add, PirType::val());
SAFE_BINOP(Colon, PirType::val());
SAFE_BINOP(Pow, PirType::val());
SAFE_BINOP(Sub, PirType::val());
SAFE_BINOP(Gt, RType::logical);
SAFE_BINOP(Lt, RType::logical);
SAFE_BINOP(Neq, RType::logical);
SAFE_BINOP(Eq, RType::logical);
SAFE_BINOP(LAnd, RType::logical);
SAFE_BINOP(LOr, RType::logical);

#undef SAFE_BINOP

#define SAFE_UNOP(Name)                                                        \
    class FLI(Name, 1, Effect::None, EnvAccess::None) {                        \
      public:                                                                  \
        Name(Value* v)                                                         \
            : FixedLenInstruction(PirType::val(), {{PirType::val()}}, {{v}}) { \
        }                                                                      \
    }

SAFE_UNOP(Not);
SAFE_UNOP(Plus);
SAFE_UNOP(Minus);
SAFE_UNOP(Length);

#undef SAFE_UNOP
#undef FLI

#define VLI(type, io, env)                                                     \
    type:                                                                      \
  public                                                                       \
    VarLenInstruction<Tag::type, type, io, env>

// Common interface to all call instructions
struct CallInstructionI {
    virtual size_t nCallArgs() = 0;
    virtual void eachCallArg(Instruction::ArgumentValueIterator it) = 0;
};

// Default call instruction. Closure expression (ie. expr left of `(`) is
// evaluated at runtime and arguments are passed as promises.
class VLI(Call, Effect::Any, EnvAccess::Leak), public CallInstructionI {
  public:
    constexpr static size_t clsIdx = 1;
    constexpr static size_t callArgOffset = 2;

    Value* cls() { return arg(clsIdx).val(); }
    size_t nCallArgs() override { return nargs() - callArgOffset; }

    Call(Value * e, Value * fun, const std::vector<Value*>& args, unsigned src)
        : VarLenInstruction(PirType::valOrLazy(), e) {
        this->pushArg(fun, RType::closure);
        for (unsigned i = 0; i < args.size(); ++i)
            this->pushArg(args[i], RType::prom);
        srcIdx = src;
    }

    void eachCallArg(ArgumentValueIterator it) override {
        for (size_t i = 0; i < nCallArgs(); ++i) {
            it(arg(i + callArgOffset).val());
        }
    }
};

// Call instruction for lazy, but staticatlly resolved calls. Closure is
// specified as `cls_`, args passed as promises.
class VLI(StaticCall, Effect::Any, EnvAccess::Leak), public CallInstructionI {
    Closure* cls_;
    SEXP origin_;

  public:
    constexpr static size_t callArgOffset = 1;

    Closure* cls() { return cls_; }
    SEXP origin() { return origin_; }
    size_t nCallArgs() override { return nargs() - callArgOffset; }

    StaticCall(Value * e, Closure * cls, const std::vector<Value*>& args,
               unsigned src, SEXP origin)
        : VarLenInstruction(PirType::valOrLazy(), e), cls_(cls),
          origin_(origin) {
        for (unsigned i = 0; i < args.size(); ++i)
            this->pushArg(args[i], RType::prom);
    }

    void eachCallArg(ArgumentValueIterator it) override {
        for (size_t i = 0; i < nCallArgs(); ++i) {
            it(arg(i + callArgOffset).val());
        }
    }

    void printArgs(std::ostream&) override;
};

// Call instruction for eager, staticatlly resolved calls. Closure is
// specified as `cls_`, args passed as values.
class VLI(StaticEagerCall, Effect::Any, EnvAccess::Leak),
    public CallInstructionI {
    Closure* cls_;
    SEXP origin_;

  public:
    constexpr static size_t callArgOffset = 1;

    Closure* cls() { return cls_; }
    SEXP origin() { return origin_; }
    size_t nCallArgs() override { return nargs() - callArgOffset; }

    StaticEagerCall(Value * e, Closure * cls, const std::vector<Value*>& args,
                    unsigned src, SEXP origin)
        : VarLenInstruction(PirType::valOrLazy(), e), cls_(cls),
          origin_(origin) {
        for (unsigned i = 0; i < args.size(); ++i)
            this->pushArg(args[i], PirType::val());
        srcIdx = src;
    }

    void eachCallArg(ArgumentValueIterator it) override {
        for (size_t i = 0; i < nCallArgs(); ++i) {
            it(arg(i + callArgOffset).val());
        }
    }

    void printArgs(std::ostream&) override;
};

struct CallInstruction {
    static CallInstructionI* Cast(Value* v) {
        switch (v->tag) {
        case Tag::Call:
            return Call::Cast(v);
        case Tag::StaticCall:
            return StaticCall::Cast(v);
        case Tag::StaticEagerCall:
            return StaticEagerCall::Cast(v);
        default: {}
        }
        return nullptr;
    }
};

typedef SEXP (*CCODE)(SEXP, SEXP, SEXP, SEXP);

class VLI(CallBuiltin, Effect::Any, EnvAccess::Write) {
  public:
    SEXP blt;
    const CCODE builtin;
    int builtinId;

    size_t nCallArgs() {
        // do not count environment
        return nargs() - 1;
    }

    CallBuiltin(Value* e, SEXP builtin, const std::vector<Value*>& args,
                unsigned src);

    void eachCallArg(ArgumentValueIterator it) {
        // skip environment at index 0
        for (size_t i = 1; i < nargs(); ++i) {
            it(arg(i).val());
        }
    }

    void printArgs(std::ostream& out) override;
};

class VLI(CallSafeBuiltin, Effect::None, EnvAccess::None) {
  public:
    SEXP blt;
    const CCODE builtin;
    int builtinId;

    CallSafeBuiltin(SEXP builtin, const std::vector<Value*>& args,
                    unsigned src);

    void printArgs(std::ostream& out) override;
};

class VLI(MkEnv, Effect::None, EnvAccess::Capture) {
  public:
    std::vector<SEXP> varName;

    typedef std::function<void(SEXP name, Value* val)> LocalVarIt;
    typedef std::function<void(SEXP name, InstrArg&)> MutableLocalVarIt;

    void eachLocalVar(LocalVarIt it) const {
        for (size_t i = 1; i < nargs(); ++i)
            it(varName[i - 1], arg(i).val());
    }

    void eachLocalVar(MutableLocalVarIt it) {
        for (size_t i = 1; i < nargs(); ++i)
            it(varName[i - 1], arg(i));
    }

    MkEnv(Value* parent, const std::vector<SEXP>& names, Value** args)
        : VarLenInstruction(RType::env, parent), varName(names) {
        for (unsigned i = 0; i < varName.size(); ++i)
            this->pushArg(args[i], PirType::any());
    }

    Value* parent() { return arg(0).val(); }
    void parent(Value* v) { arg(0).val() = v; }

    void printArgs(std::ostream& out) override;

    void eachLocalVar(ArgumentIterator it) {
        for (size_t i = 0; i < nLocals(); ++i) {
            it(arg(i + 1));
        }
    }

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
    void updateType();
    template <bool E = false>
    inline void pushArg(Value* a) {
        static_assert(E, "use addInput");
    }
    void addInput(BB* in, Value* arg) {
        input.push_back(in);
        VarLenInstruction::pushArg(arg);
    }
    typedef std::function<void(BB* bb, Value*)> PhiArgumentIterator;
    typedef std::function<void(BB* bb, Instruction*)>
        PhiArgumentInstructionIterator;

    void eachArg(PhiArgumentIterator it) const {
        for (size_t i = 0; i < nargs(); ++i)
            it(input[i], arg(i).val());
    }

    void eachInstructionArg(PhiArgumentInstructionIterator it) const {
        for (size_t i = 0; i < nargs(); ++i) {
            auto in = Instruction::Cast(arg(i).val());
            if (in)
                it(input[i], in);
        }
    }
};

class VLI(Deopt, Effect::Any, EnvAccess::Leak) {
  public:
    Opcode* pc;

    Deopt(Value* env, Opcode* pc, size_t stackSize, Value** stack)
        : VarLenInstruction(PirType::voyd(), env), pc(pc) {
        for (unsigned i = 0; i < stackSize; ++i)
            pushArg(stack[i], PirType::any());
    }

    void printArgs(std::ostream& out) override;
};

#undef VLI
} // namespace pir
} // namespace rir

#endif
