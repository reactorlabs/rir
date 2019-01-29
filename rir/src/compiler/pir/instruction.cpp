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

void printPaddedTypeAndRef(std::ostream& out, const Instruction* i) {
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

void Instruction::printArgs(std::ostream& out, bool tty) const {
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

void Instruction::print(std::ostream& out, bool tty) const {
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

void Instruction::printEnv(std::ostream& out, bool tty) const {
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

void Instruction::printRef(std::ostream& out) const {
    if (type == RType::env)
        out << "e" << id();
    else
        out << "%" << id();
};

Instruction::InstructionUID Instruction::id() const {
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
    if (replace->type.isRType() != type.isRType() ||
        (replace->type.maybePromiseWrapped() && !type.maybePromiseWrapped())) {
        std::cerr << "Trying to replace a ";
        type.print(std::cerr);
        std::cerr << " with a ";
        replace->type.print(std::cerr);
        std::cerr << "\n";
        printBacktrace();
        assert(false);
    }

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

const Value* Instruction::cFollowCasts() const {
    if (auto cast = CastType::Cast(this))
        return cast->arg<0>().val()->followCasts();
    if (auto shared = SetShared::Cast(this))
        return shared->arg<0>().val()->followCasts();
    if (auto chk = ChkClosure::Cast(this))
        return chk->arg<0>().val()->followCasts();
    return this;
}

const Value* Instruction::cFollowCastsAndForce() const {
    if (auto cast = CastType::Cast(this))
        return cast->arg<0>().val()->followCastsAndForce();
    if (auto force = Force::Cast(this))
        return force->input()->followCastsAndForce();
    if (auto mkarg = MkArg::Cast(this))
        if (mkarg->isEager())
            return mkarg->eagerArg()->followCastsAndForce();
    if (auto shared = SetShared::Cast(this))
        return shared->arg<0>().val()->followCastsAndForce();
    if (auto chk = ChkClosure::Cast(this))
        return chk->arg<0>().val()->followCastsAndForce();
    return this;
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

void LdConst::printArgs(std::ostream& out, bool tty) const {
    std::string val;
    {
        CaptureOut rec;
        Rf_PrintValue(c);
        val = rec.oneline(40);
    }
    out << val;
}

void Branch::printArgs(std::ostream& out, bool tty) const {
    FixedLenInstruction::printArgs(out, tty);
    out << " -> BB" << bb()->trueBranch()->id << " (if true) | BB"
        << bb()->falseBranch()->id << " (if false)";
}

void MkArg::printArgs(std::ostream& out, bool tty) const {
    eagerArg()->printRef(out);
    out << ", " << *prom() << ", ";
}

void Missing::printArgs(std::ostream& out, bool tty) const {
    out << CHAR(PRINTNAME(varName)) << ", ";
}

void LdVar::printArgs(std::ostream& out, bool tty) const {
    out << CHAR(PRINTNAME(varName)) << ", ";
}

void LdFun::printArgs(std::ostream& out, bool tty) const {
    out << CHAR(PRINTNAME(varName)) << ", ";
    if (guessedBinding()) {
        out << "<";
        guessedBinding()->printRef(out);
        out << ">, ";
    }
}

void LdArg::printArgs(std::ostream& out, bool tty) const { out << id; }

void StVar::printArgs(std::ostream& out, bool tty) const {
    if (keepMissing)
        out << "(keepMissing) ";
    out << CHAR(PRINTNAME(varName)) << ", ";
    val()->printRef(out);
    out << ", ";
}

void StVarSuper::printArgs(std::ostream& out, bool tty) const {
    out << CHAR(PRINTNAME(varName)) << ", ";
    val()->printRef(out);
    out << ", ";
}

void LdVarSuper::printArgs(std::ostream& out, bool tty) const {
    out << CHAR(PRINTNAME(varName));
    out << ", ";
}

void MkEnv::printArgs(std::ostream& out, bool tty) const {
    eachLocalVar([&](SEXP name, Value* v) {
        out << CHAR(PRINTNAME(name)) << "=";
        v->printRef(out);
        out << ", ";
    });
    out << "parent=";
    Instruction::printEnv(out, tty);
}

void Is::printArgs(std::ostream& out, bool tty) const {
    arg<0>().val()->printRef(out);
    out << ", " << Rf_type2char(sexpTag);
}

void Phi::updateType() {
    type = arg(0).val()->type;
    eachArg([&](BB*, Value* v) -> void { type = type | v->type; });
}

void Phi::printArgs(std::ostream& out, bool tty) const {
    if (nargs() > 0) {
        for (size_t i = 0; i < nargs(); ++i) {
            arg(i).val()->printRef(out);
            out << ":BB" << input[i]->id;
            if (i + 1 < nargs())
                out << ", ";
        }
    }
}

void PirCopy::print(std::ostream& out, bool tty) const {
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

static void printCallArgs(std::ostream& out, const CallInstruction* call) {
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

void CallBuiltin::printArgs(std::ostream& out, bool tty) const {
    out << getBuiltinName(builtinId);
    printCallArgs(out, this);
}

void CallSafeBuiltin::printArgs(std::ostream& out, bool tty) const {
    out << getBuiltinName(builtinId);
    printCallArgs(out, this);
}

void FrameState::printArgs(std::ostream& out, bool tty) const {
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

void ScheduledDeopt::printArgs(std::ostream& out, bool tty) const {
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

MkFunCls::MkFunCls(Closure* cls, DispatchTable* originalBody, Value* lexicalEnv)
    : FixedLenInstructionWithEnvSlot(RType::closure, lexicalEnv), cls(cls),
      originalBody(originalBody) {}

void MkFunCls::printArgs(std::ostream& out, bool tty) const {
    out << *cls;
    Instruction::printArgs(out, tty);
}

void StaticCall::printArgs(std::ostream& out, bool tty) const {
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
            std::cout << "* " << v->assumptions() << "\n";
        });
        std::cout << "Available assumptions at callsite: \n ";
        std::cout << inferAvailableAssumptions() << "\n";
        assert(false);
    }
    return res;
}

ClosureVersion* StaticCall::dispatch() const {
    return CallInstruction::dispatch(cls());
}

ClosureVersion* StaticCall::optimisticDispatch() const {
    auto dispatch = CallInstruction::dispatch(cls());
    if (!hint)
        return dispatch;

    return (hint->optimizationContext() < dispatch->optimizationContext())
               ? dispatch
               : hint;
}

StaticCall::StaticCall(Value* callerEnv, Closure* cls,
                       const std::vector<Value*>& args, FrameState* fs,
                       unsigned srcIdx)
    : VarLenInstructionWithEnvSlot(PirType::val(), callerEnv, srcIdx),
      cls_(cls) {
    assert(cls->nargs() >= args.size());
    assert(fs);
    pushArg(fs, NativeType::frameState);
    for (unsigned i = 0; i < args.size(); ++i)
        pushArg(args[i], PirType::val() | RType::prom);
    assert(dispatch());
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
        given.add(Assumption::CorrectOrderOfArguments);
    if (auto cls = tryGetCls()) {
        if (cls->nargs() >= nCallArgs()) {
            given.add(Assumption::NotTooManyArguments);
            auto missing = cls->nargs() - nCallArgs();
            given.numMissing(missing);
            given.add(Assumption::NotTooFewArguments);
        }
    }
    given.add(Assumption::NotTooManyArguments);

    // Make some optimistic assumptions, they might be reset below...
    given.add(Assumption::EagerArgs_);
    given.add(Assumption::NonObjectArgs_);
    given.add(Assumption::NoExplicitlyMissingArgs);

    size_t i = 0;
    eachCallArg([&](Value* arg) {
        if (auto mk = MkArg::Cast(arg)) {
            if (!mk->isEager()) {
                given.setEager(i, false);
                given.setNotObj(i, false);
                return;
            } else {
                arg = mk->eagerArg();
            }
        }
        if (arg == MissingArg::instance())
            given.remove(Assumption::NoExplicitlyMissingArgs);
        given.setEager(i, !arg->type.maybeLazy());
        given.setNotObj(i, !arg->type.maybeObj());
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

void Call::printArgs(std::ostream& out, bool tty) const {
    cls()->printRef(out);
    printCallArgs(out, this);
    if (frameState()) {
        frameState()->printRef(out);
        out << ", ";
    }
}

void NamedCall::printArgs(std::ostream& out, bool tty) const {
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

FrameState* Deopt::frameState() { return FrameState::Cast(arg<0>().val()); }

void Checkpoint::printArgs(std::ostream& out, bool tty) const {
    FixedLenInstruction::printArgs(out, tty);
    out << " -> BB" << bb()->trueBranch()->id << " (default) | BB"
        << bb()->falseBranch()->id << " (if assume failed)";
}

BB* Checkpoint::deoptBranch() { return bb()->falseBranch(); }

} // namespace pir
} // namespace rir
