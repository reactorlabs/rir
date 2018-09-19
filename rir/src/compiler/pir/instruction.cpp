#include "instruction.h"
#include "pir_impl.h"

#include "../util/visitor.h"
#include "R/Funtab.h"
#include "utils/Pool.h"
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

void Instruction::printArgs(std::ostream& out) {
    if (nargs() > 0) {
        for (size_t i = 0; i < nargs(); ++i) {
            arg(i).val()->printRef(out);
            if (i + 1 < nargs())
                out << ", ";
        }
    }
}

void Instruction::print(std::ostream& out) {
    std::ostringstream buf;
    buf << type;
    out << std::left << std::setw(7) << buf.str() << " ";
    buf.str("");
    if (type != PirType::voyd()) {
        printRef(buf);
        out << std::setw(5) << buf.str() << " = ";
        buf.str("");
    } else {
        out << "        ";
    }

    printPaddedInstructionName(out, name());
    printArgs(buf);
    out << std::setw(50) << buf.str();

    if (leaksEnv())
        out << " ; env leak";
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
    // TODO: better solution?
    if (tag == Tag::Branch || tag == Tag::Return)
        return false;

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

Value* Instruction::baseValue() {
    if (auto cast = CastType::Cast(this))
        return cast->arg<0>().val()->baseValue();
    if (auto force = Force::Cast(this))
        return force->input()->baseValue();
    if (auto shared = SetShared::Cast(this))
        return shared->arg<0>().val()->baseValue();
    return this;
}

void LdConst::printArgs(std::ostream& out) {
    std::string val;
    {
        CaptureOut rec;
        Rf_PrintValue(c);
        val = rec.oneline(40);
    }
    out << val;
}

void Branch::printArgs(std::ostream& out) {
    FixedLenInstruction::printArgs(out);
    out << " -> BB" << bb()->trueBranch()->id << " (if true) | BB"
        << bb()->falseBranch()->id << " (if false)";
}

void MkArg::printArgs(std::ostream& out) {
    eagerArg()->printRef(out);
    out << ", " << *prom << ", ";
    env()->printRef(out);
}

void LdVar::printArgs(std::ostream& out) {
    out << CHAR(PRINTNAME(varName)) << ", ";
    env()->printRef(out);
}

void LdFun::printArgs(std::ostream& out) {
    out << CHAR(PRINTNAME(varName)) << ", ";
    env()->printRef(out);
}

void LdArg::printArgs(std::ostream& out) { out << id; }

void StVar::printArgs(std::ostream& out) {
    out << CHAR(PRINTNAME(varName)) << ", ";
    val()->printRef(out);
    out << ", ";
    env()->printRef(out);
}

void StVarSuper::printArgs(std::ostream& out) {
    out << CHAR(PRINTNAME(varName)) << ", ";
    val()->printRef(out);
    out << ", ";
    env()->printRef(out);
}

void LdVarSuper::printArgs(std::ostream& out) {
    out << CHAR(PRINTNAME(varName)) << ", ";
    env()->printRef(out);
}

void MkEnv::printArgs(std::ostream& out) {
    eachLocalVar([&](SEXP name, Value* v) {
        out << CHAR(PRINTNAME(name)) << "=";
        v->printRef(out);
        out << ", ";
    });
    out << "parent=";
    lexicalEnv()->printRef(out);
}

void Is::printArgs(std::ostream& out) {
    arg<0>().val()->printRef(out);
    out << ", " << Rf_type2char(sexpTag);
}

bool Phi::updateType() {
    auto old = type;
    type = arg(0).val()->type;
    eachArg([&](BB*, Value* v) -> void { type = type | v->type; });
    return type != old;
}

void Phi::printArgs(std::ostream& out) {
    if (nargs() > 0) {
        for (size_t i = 0; i < nargs(); ++i) {
            arg(i).val()->printRef(out);
            out << ":BB" << input[i]->id;
            if (i + 1 < nargs())
                out << ", ";
        }
    }
}

void PirCopy::print(std::ostream& out) {
    std::ostringstream buf;
    buf << type;
    out << std::left << std::setw(7) << buf.str() << " ";
    buf.str("");
    printRef(buf);
    out << std::setw(5) << buf.str() << " = ";
    buf.str("");
    arg(0).val()->printRef(buf);
    out << std::setw(50) << buf.str();
}

CallSafeBuiltin::CallSafeBuiltin(SEXP builtin, const std::vector<Value*>& args,
                                 unsigned srcIdx)
    : VarLenInstruction(PirType::valOrLazy(), srcIdx), blt(builtin),
      builtin(getBuiltin(builtin)), builtinId(getBuiltinNr(builtin)) {
    for (unsigned i = 0; i < args.size(); ++i)
        this->pushArg(args[i], PirType::val());
}

CallBuiltin::CallBuiltin(Value* env, SEXP builtin,
                         const std::vector<Value*>& args, unsigned srcIdx)
    : VarLenInstructionWithEnvSlot(PirType::valOrLazy(), env, srcIdx),
      blt(builtin), builtin(getBuiltin(builtin)),
      builtinId(getBuiltinNr(builtin)) {
    for (unsigned i = 0; i < args.size(); ++i)
        this->pushArg(args[i], PirType::val());
}

static void printCallArgs(std::ostream& out, Instruction* call,
                          Value* callerEnv = nullptr) {
    size_t nargs = CallInstruction::CastCall(call)->nCallArgs();
    out << "(";
    for (size_t i = 0; i < nargs; ++i) {
        call->arg(i).val()->printRef(out);
        if (i < nargs - 1)
            out << ", ";
    }
    out << ") ";
    if (callerEnv)
        callerEnv->printRef(out);
}

void CallBuiltin::printArgs(std::ostream& out) {
    std::cout << getBuiltinName(builtinId);
    printCallArgs(out, this, callerEnv());
}

void CallSafeBuiltin::printArgs(std::ostream& out) {
    std::cout << getBuiltinName(builtinId);
    printCallArgs(out, this);
}

void Safepoint::printArgs(std::ostream& out) {
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
    env()->printRef(out);
    if (next()) {
        out << ", next=";
        next()->printRef(out);
    }
}

void ScheduledDeopt::consumeSafepoints(Deopt* deopt) {
    std::vector<Safepoint*> safepoints;
    {
        auto sp = deopt->safepoint();
        do {
            safepoints.push_back(sp);
            sp = sp->next();
        } while (sp);
    }
    for (auto spi = safepoints.rbegin(); spi != safepoints.rend(); spi++) {
        auto sp = *spi;
        frames.push_back({sp->pc, sp->code, sp->stackSize});
        for (size_t i = 0; i < sp->stackSize; i++)
            pushArg(sp->arg(i).val());
        pushArg(sp->env());
    }
}

void ScheduledDeopt::printArgs(std::ostream& out) {
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
        arg(argpos++).val()->printRef(out);
        if (argpos < nargs()) {
            out << "; ";
        }
    }
}

MkFunCls::MkFunCls(Closure* fun, Value* lexicalEnv, SEXP fml, SEXP code,
                   SEXP src)
    : FixedLenInstructionWithEnvSlot(RType::closure, lexicalEnv), fun(fun),
      fml(fml), code(code), src(src) {
    assert(fun->closureEnv() == Env::notClosed());
}

void MkFunCls::printArgs(std::ostream& out) {
    out << *fun;
    out << ", ";
    Instruction::printArgs(out);
}

void StaticCall::printArgs(std::ostream& out) {
    out << *cls_;
    printCallArgs(out, this, callerEnv());
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

NamedCall::NamedCall(Value* callerEnv, Value* fun,
                     const std::vector<Value*>& args,
                     const std::vector<BC::PoolIdx>& names_, unsigned srcIdx)
    : VarLenInstructionWithEnvSlot(PirType::valOrLazy(), callerEnv, srcIdx) {
    assert(names_.size() == args.size());
    pushArg(fun, RType::closure);
    for (unsigned i = 0; i < args.size(); ++i) {
        pushArg(args[i], PirType::val());
        auto name = Pool::get(names_[i]);
        assert(TYPEOF(name) == SYMSXP || name == R_NilValue);
        names.push_back(name);
    }
}

void Call::printArgs(std::ostream& out) {
    printCallArgs(out, this, callerEnv());
}

void NamedCall::printArgs(std::ostream& out) {
    size_t nargs = nCallArgs();
    out << "(";
    for (size_t i = 0; i < nargs; ++i) {
        if (names[i] != R_NilValue)
            out << CHAR(PRINTNAME(names.at(i))) << " = ";
        arg(i).val()->printRef(out);
        if (i < nargs - 1)
            out << ", ";
    }
    out << ") ";
    if (callerEnv())
        callerEnv()->printRef(out);
}

void CallImplicit::printArgs(std::ostream& out) {
    out << "(";
    for (size_t i = 0; i < promises.size(); ++i) {
        if (i < names.size() && names[i] != R_NilValue)
            out << CHAR(PRINTNAME(names[i])) << "=";
        out << "Prom(" << promises[i]->id << ")";
        if (i < promises.size() - 1)
            out << ", ";
    }
    out << ") ";
    callerEnv()->printRef(out);
}

Safepoint* Deopt::safepoint() { return Safepoint::Cast(arg<0>().val()); }

} // namespace pir
} // namespace rir
