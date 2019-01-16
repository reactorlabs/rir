#include "instruction.h"
#include "pir_impl.h"

#include "../util/safe_builtins_list.h"
#include "../util/visitor.h"
#include "R/Funtab.h"
#include "utils/Pool.h"
#include "utils/Terminal.h"
#include "utils/capture_out.h"

#include <algorithm>
#include <cassert>
#include <iomanip>
#include <sstream>

extern "C" SEXP deparse1line(SEXP call, Rboolean abbrev);

namespace {
using namespace rir::pir;

static size_t getMaxInstructionNameLength() {
    size_t max = 0;
    size_t cur;
#define V(n)                                                                   \
    cur = std::string(#n).length();                                            \
    if (cur > max)                                                             \
        max = cur;
    COMPILER_INSTRUCTIONS(V)
#undef V
    return max;
}
static size_t maxInstructionNameLength = getMaxInstructionNameLength();

static_assert(static_cast<unsigned>(Tag::_UNUSED_) == 0, "");
} // namespace

namespace rir {
namespace pir {

extern std::ostream& operator<<(std::ostream& out,
                                Instruction::InstructionUID id) {
    out << std::get<0>(id) << "." << std::get<1>(id);
    return out;
}

void printPaddedInstructionName(std::ostream& out, const std::string& name) {
    out << std::left << std::setw(maxInstructionNameLength + 1) << name << " ";
}

void printPaddedTypeAndRef(std::ostream& out, Instruction* i) {
    std::ostringstream buf;
    buf << i->type;
    out << std::left << std::setw(7) << buf.str() << " ";
    buf.str("");
    if (i->type != PirType::voyd()) {
        i->printRef(buf);
        out << std::setw(5) << buf.str() << " = ";
    } else {
        out << "        ";
    }
}

bool Instruction::validIn(Code* code) const { return bb()->owner == code; }

void Instruction::printArgs(std::ostream& out, bool tty) {
    size_t n = nargs();
    size_t env = hasEnv() ? envSlot() : n + 1;

    for (size_t i = 0; i < n; ++i) {
        if (i != env) {
            arg(i).val()->printRef(out);
            if (i + 1 < n && (i + 1) != env)
                out << ", ";
        }
    }
    if (hasEnv())
        out << ", ";
}

void Instruction::print(std::ostream& out, bool tty) {
    printPaddedTypeAndRef(out, this);
    printPaddedInstructionName(out, name());
    printArgs(out, tty);
    printEnv(out, tty);
}

void Phi::removeInputs(const std::unordered_set<BB*>& deletedBBs) {
    auto bbIter = input.begin();
    auto argIter = args_.begin();
    while (argIter != args_.end()) {
        if (deletedBBs.count(*bbIter)) {
            bbIter = input.erase(bbIter);
            argIter = args_.erase(argIter);
        } else {
            argIter++;
            bbIter++;
        }
    }
    assert(bbIter == input.end());
}

void Instruction::printEnv(std::ostream& out, bool tty) {
    if (hasEnv()) {
        if (tty) {
            if (leaksEnv())
                ConsoleColor::magenta(out);
            else if (changesEnv())
                ConsoleColor::red(out);
            else
                ConsoleColor::yellow(out);
        }

        env()->printRef(out);

        if (tty)
            ConsoleColor::clear(out);
    }
}

void Instruction::printRef(std::ostream& out) {
    if (type == RType::env)
        out << "e" << id();
    else
        out << "%" << id();
};

Instruction::InstructionUID Instruction::id() {
    return InstructionUID(bb()->id, bb()->indexOf(this));
}

bool Instruction::unused() {
    if (type == PirType::voyd())
        return true;
    return Visitor::check(bb(), [&](Instruction* i) {
        bool unused = true;
        i->eachArg([&](Value* v) { unused = unused && (v != this); });
        return unused;
    });
}

Instruction* Instruction::hasSingleUse() {
    size_t seen = 0;
    Instruction* usage;
    Visitor::check(bb(), [&](Instruction* i) {
        i->eachArg([&](Value* v) {
            if (v == this) {
                usage = i;
                seen++;
            }
        });
        return seen <= 1;
    });
    if (seen == 1)
        return usage;
    return nullptr;
}

void Instruction::eraseAndRemove() { bb()->remove(this); }

void Instruction::replaceUsesIn(Value* replace, BB* target) {
    Visitor::run(target, [&](Instruction* i) {
        i->eachArg([&](InstrArg& arg) {
            if (arg.val() == this)
                arg.val() = replace;
        });
    });
}

void Instruction::replaceUsesWith(Value* replace) {
    replaceUsesIn(replace, bb());
}

void Instruction::replaceUsesAndSwapWith(
    Instruction* replace, std::vector<Instruction*>::iterator it) {
    replaceUsesWith(replace);
    bb()->replace(it, replace);
}

Value* Instruction::followCasts() const {
    if (auto cast = CastType::Cast(this))
        return cast->arg<0>().val()->followCasts();
    if (auto shared = SetShared::Cast(this))
        return shared->arg<0>().val()->followCasts();
    if (auto chk = ChkClosure::Cast(this))
        return chk->arg<0>().val()->followCasts();
    return const_cast<Instruction*>(this);
}

Value* Instruction::followCastsAndForce() const {
    if (auto cast = CastType::Cast(this))
        return cast->arg<0>().val()->followCastsAndForce();
    if (auto force = Force::Cast(this))
        return force->input()->followCastsAndForce();
    if (auto mkarg = MkArg::Cast(this))
        if (mkarg->eagerArg() != Missing::instance())
            return mkarg->eagerArg();
    if (auto shared = SetShared::Cast(this))
        return shared->arg<0>().val()->followCastsAndForce();
    if (auto chk = ChkClosure::Cast(this))
        return chk->arg<0>().val()->followCastsAndForce();
    return const_cast<Instruction*>(this);
}

bool Instruction::envOnlyForObj() {
#define V(Name)                                                                \
    if (Name::Cast(this)) {                                                    \
        return true;                                                           \
    }
    BINOP_INSTRUCTIONS(V)
    VECTOR_RW_INSTRUCTIONS(V)
#undef V
    return false;
}

void LdConst::printArgs(std::ostream& out, bool tty) {
    std::string val;
    {
        CaptureOut rec;
        Rf_PrintValue(c);
        val = rec.oneline(40);
    }
    out << val;
}

void Branch::printArgs(std::ostream& out, bool tty) {
    FixedLenInstruction::printArgs(out, tty);
    out << " -> BB" << bb()->trueBranch()->id << " (if true) | BB"
        << bb()->falseBranch()->id << " (if false)";
}

void MkArg::printArgs(std::ostream& out, bool tty) {
    eagerArg()->printRef(out);
    out << ", " << *prom() << ", ";
}

void LdVar::printArgs(std::ostream& out, bool tty) {
    out << CHAR(PRINTNAME(varName)) << ", ";
}

void LdFun::printArgs(std::ostream& out, bool tty) {
    out << CHAR(PRINTNAME(varName)) << ", ";
    if (guessedBinding()) {
        out << "<";
        guessedBinding()->printRef(out);
        out << ">, ";
    }
}

void LdArg::printArgs(std::ostream& out, bool tty) { out << id; }

void StVar::printArgs(std::ostream& out, bool tty) {
    out << CHAR(PRINTNAME(varName)) << ", ";
    val()->printRef(out);
    out << ", ";
}

void StVarSuper::printArgs(std::ostream& out, bool tty) {
    out << CHAR(PRINTNAME(varName)) << ", ";
    val()->printRef(out);
    out << ", ";
}

void LdVarSuper::printArgs(std::ostream& out, bool tty) {
    out << CHAR(PRINTNAME(varName));
    out << ", ";
}

void MkEnv::printArgs(std::ostream& out, bool tty) {
    eachLocalVar([&](SEXP name, Value* v) {
        out << CHAR(PRINTNAME(name)) << "=";
        v->printRef(out);
        out << ", ";
    });
    out << "parent=";
    Instruction::printEnv(out, tty);
}

void Is::printArgs(std::ostream& out, bool tty) {
    arg<0>().val()->printRef(out);
    out << ", " << Rf_type2char(sexpTag);
}

void Phi::updateType() {
    type = arg(0).val()->type;
    eachArg([&](BB*, Value* v) -> void { type = type | v->type; });
}

void Phi::printArgs(std::ostream& out, bool tty) {
    if (nargs() > 0) {
        for (size_t i = 0; i < nargs(); ++i) {
            arg(i).val()->printRef(out);
            out << ":BB" << input[i]->id;
            if (i + 1 < nargs())
                out << ", ";
        }
    }
}

void PirCopy::print(std::ostream& out, bool tty) {
    printPaddedTypeAndRef(out, this);
    arg(0).val()->printRef(out);
}

CallSafeBuiltin::CallSafeBuiltin(SEXP builtin, const std::vector<Value*>& args,
                                 unsigned srcIdx)
    : VarLenInstruction(PirType::val(), srcIdx), blt(builtin),
      builtin(getBuiltin(builtin)), builtinId(getBuiltinNr(builtin)) {
    for (unsigned i = 0; i < args.size(); ++i)
        this->pushArg(args[i], PirType::val());
}

CallBuiltin::CallBuiltin(Value* env, SEXP builtin,
                         const std::vector<Value*>& args, unsigned srcIdx)
    : VarLenInstructionWithEnvSlot(PirType::val(), env, srcIdx), blt(builtin),
      builtin(getBuiltin(builtin)), builtinId(getBuiltinNr(builtin)) {
    for (unsigned i = 0; i < args.size(); ++i)
        this->pushArg(args[i], PirType::val());
}

Instruction* BuiltinCallFactory::New(Value* callerEnv, SEXP builtin,
                                     const std::vector<Value*>& args,
                                     unsigned srcIdx) {
    if (SafeBuiltinsList::always(builtin))
        return new CallSafeBuiltin(builtin, args, srcIdx);
    else
        return new CallBuiltin(callerEnv, builtin, args, srcIdx);
}

static void printCallArgs(std::ostream& out, CallInstruction* call) {
    out << "(";

    size_t i = 0;
    size_t n = call->nCallArgs();
    call->eachCallArg([&](Value* v) {
        v->printRef(out);
        if (i < n - 1)
            out << ", ";
        i++;
    });
    out << ") ";
}

void CallBuiltin::printArgs(std::ostream& out, bool tty) {
    out << getBuiltinName(builtinId);
    printCallArgs(out, this);
}

void CallSafeBuiltin::printArgs(std::ostream& out, bool tty) {
    out << getBuiltinName(builtinId);
    printCallArgs(out, this);
}

void FrameState::printArgs(std::ostream& out, bool tty) {
    out << code << "+" << pc - code->code();
    out << ": [";
    long s = stackSize;
    eachArg([&](Value* i) {
        if (s) {
            s--;
            i->printRef(out);
            if (s)
                out << ", ";
        }
    });
    out << "], env=";
    Instruction::printEnv(out, tty);
    if (next()) {
        out << ", next=";
        next()->printRef(out);
    }
}

void ScheduledDeopt::consumeFrameStates(Deopt* deopt) {
    std::vector<FrameState*> frameStates;
    {
        auto sp = deopt->frameState();
        do {
            frameStates.push_back(sp);
            sp = sp->next();
        } while (sp);
    }
    for (auto spi = frameStates.rbegin(); spi != frameStates.rend(); spi++) {
        auto sp = *spi;
        frames.push_back({sp->pc, sp->code, sp->stackSize});
        for (size_t i = 0; i < sp->stackSize; i++)
            pushArg(sp->arg(i).val());
        pushArg(sp->env());
    }
}

void ScheduledDeopt::printArgs(std::ostream& out, bool tty) {
    size_t n = 0;
    for (auto& f : frames)
        n += f.stackSize + 1;
    assert(n == nargs());

    size_t argpos = 0;
    for (auto& f : frames) {
        out << f.code << "+" << f.pc - f.code->code();
        out << ": [";
        long s = f.stackSize;
        while (s) {
            s--;
            arg(argpos++).val()->printRef(out);
            if (s)
                out << ", ";
        }
        out << "], env=";
        if (tty)
            ConsoleColor::magenta(out);
        arg(argpos++).val()->printRef(out);
        if (tty)
            ConsoleColor::clear(out);
        if (argpos < nargs()) {
            out << "; ";
        }
    }
}

MkFunCls::MkFunCls(Closure* fun, Value* lexicalEnv, SEXP fml, SEXP code,
                   SEXP src)
    : FixedLenInstructionWithEnvSlot(RType::closure, lexicalEnv), fun(fun),
      fml(fml), code(code), src(src) {
}

void MkFunCls::printArgs(std::ostream& out, bool tty) {
    out << *fun;
    Instruction::printArgs(out, tty);
}

void StaticCall::printArgs(std::ostream& out, bool tty) {
    out << dispatch()->name();
    if (hint && hint != dispatch())
        out << "<hint: " << hint->nameSuffix() << ">";
    printCallArgs(out, this);
    if (frameState()) {
        frameState()->printRef(out);
        out << ", ";
    }
}

ClosureVersion* CallInstruction::dispatch(Closure* cls) const {
    auto res = cls->findCompatibleVersion(
        OptimizationContext(inferAvailableAssumptions()));
    if (!res) {
        std::cout << "DISPATCH FAILED! Available versions: \n";
        cls->eachVersion([&](ClosureVersion* v) {
            std::cout << "* ";
            for (auto a : v->assumptions())
                std::cout << a << " ";
            std::cout << "\n";
        });
        std::cout << "Available assumptions at callsite: \n";
        for (auto a : inferAvailableAssumptions())
            std::cout << a << " ";
        std::cout << "\n";
        assert(false);
    }
    return res;
}

ClosureVersion* StaticCall::dispatch() const {
    return CallInstruction::dispatch(cls());
}

StaticCall::StaticCall(Value* callerEnv, Closure* cls,
                       const std::vector<Value*>& args, SEXP origin,
                       FrameState* fs, unsigned srcIdx)
    : VarLenInstructionWithEnvSlot(PirType::val(), callerEnv, srcIdx),
      cls_(cls), origin_(origin) {
    assert(cls->nargs() == args.size());
    assert(fs);
    pushArg(fs, NativeType::frameState);
    for (unsigned i = 0; i < args.size(); ++i)
        pushArg(args[i], PirType::val() | RType::prom);
}

CallInstruction* CallInstruction::CastCall(Value* v) {
    switch (v->tag) {
    case Tag::Call:
        return Call::Cast(v);
    case Tag::StaticCall:
        return StaticCall::Cast(v);
    case Tag::CallBuiltin:
        return CallBuiltin::Cast(v);
    case Tag::CallSafeBuiltin:
        return CallSafeBuiltin::Cast(v);
    case Tag::NamedCall:
        return NamedCall::Cast(v);
    default: {}
    }
    return nullptr;
}

Assumptions CallInstruction::inferAvailableAssumptions() const {
    Assumptions given;
    if (!hasNamedArgs())
        given.set(Assumption::CorrectOrderOfArguments);
    if (auto cls = tryGetCls()) {
        if (cls->nargs() >= nCallArgs())
            given.set(Assumption::NotTooManyArguments);
        if (cls->nargs() <= nCallArgs())
            given.set(Assumption::NoMissingArguments);
    }
    given.set(Assumption::NotTooManyArguments);
    given.set(Assumption::EagerArgs);
    given.set(Assumption::NonObjectArgs);
    eachCallArg([&](Value* arg) {
        if (auto mk = MkArg::Cast(arg)) {
            if (mk->eagerArg() == Missing::instance()) {
                given.reset(Assumption::EagerArgs);
                given.reset(Assumption::NonObjectArgs);
                return;
            } else {
                arg = mk->eagerArg();
            }
        }
        if (arg->type.maybeLazy())
            given.reset(Assumption::EagerArgs);
        if (arg->type.maybeObj())
            given.reset(Assumption::NonObjectArgs);
        if (arg == Missing::instance())
            given.reset(Assumption::NoMissingArguments);
    });
    return given;
}

NamedCall::NamedCall(Value* callerEnv, Value* fun,
                     const std::vector<Value*>& args,
                     const std::vector<BC::PoolIdx>& names_, unsigned srcIdx)
    : VarLenInstructionWithEnvSlot(PirType::valOrLazy(), callerEnv, srcIdx) {
    assert(names_.size() == args.size());
    pushArg(fun, RType::closure);
    for (unsigned i = 0; i < args.size(); ++i) {
        pushArg(args[i], RType::prom);
        auto name = Pool::get(names_[i]);
        assert(TYPEOF(name) == SYMSXP || name == R_NilValue);
        names.push_back(name);
    }
}

void Call::printArgs(std::ostream& out, bool tty) {
    cls()->printRef(out);
    printCallArgs(out, this);
    if (frameState()) {
        frameState()->printRef(out);
        out << ", ";
    }
}

void NamedCall::printArgs(std::ostream& out, bool tty) {
    cls()->printRef(out);
    size_t nargs = nCallArgs();
    size_t i = 0;
    out << "(";
    eachCallArg([&](Value* a) {
        if (names[i] != R_NilValue)
            out << CHAR(PRINTNAME(names.at(i))) << " = ";
        a->printRef(out);
        if (i < nargs - 1)
            out << ", ";
        i++;
    });
    out << ") ";
}

CallImplicit::CallImplicit(Value* callerEnv, Value* fun,
                           const std::vector<Promise*>& args,
                           const std::vector<SEXP>& names_, unsigned srcIdx)
    : FixedLenInstructionWithEnvSlot(PirType::valOrLazy(),
                                     {{PirType::closure()}}, {{fun}}, callerEnv,
                                     srcIdx),
      promises(args), names(names_) {}

void CallImplicit::eachArg(const std::function<void(Promise*)>& action) const {
    for (auto prom : promises) {
        action(prom);
    }
}

void CallImplicit::printArgs(std::ostream& out, bool tty) {
    cls()->printRef(out);
    out << "(";
    for (size_t i = 0; i < promises.size(); ++i) {
        if (i < names.size() && names[i] != R_NilValue)
            out << CHAR(PRINTNAME(names[i])) << "=";
        out << "Prom(" << promises[i]->id << ")";
        if (i < promises.size() - 1)
            out << ", ";
    }
    out << ") ";
}

FrameState* Deopt::frameState() { return FrameState::Cast(arg<0>().val()); }

void Checkpoint::printArgs(std::ostream& out, bool tty) {
    FixedLenInstruction::printArgs(out, tty);
    out << " -> BB" << bb()->trueBranch()->id << " (default) | BB"
        << bb()->falseBranch()->id << " (if assume failed)";
}

BB* Checkpoint::deoptBranch() { return bb()->falseBranch(); }

} // namespace pir
} // namespace rir
