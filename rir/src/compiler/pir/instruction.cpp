#include "instruction.h"
#include "pir_impl.h"

#include "../util/visitor.h"
#include "R/Funtab.h"
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

Value* Instruction::baseValue() {
    if (auto cast = CastType::Cast(this))
        return cast->arg<0>().val()->baseValue();
    if (auto force = Force::Cast(this))
        return force->arg<0>().val()->baseValue();
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
    out << " -> BB" << bb()->next1->id << " (if true) | BB" << bb()->next0->id
        << " (if false)";
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
    parent()->printRef(out);
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
    : CallInstructionImplementation(PirType::valOrLazy(), srcIdx), blt(builtin),
      builtin(getBuiltin(builtin)), builtinId(getBuiltinNr(builtin)) {
    for (unsigned i = 0; i < args.size(); ++i)
        this->pushArg(args[i], PirType::val());
}

CallBuiltin::CallBuiltin(Value* e, SEXP builtin,
                         const std::vector<Value*>& args, unsigned srcIdx)
    : CallInstructionImplementation(PirType::valOrLazy(), e, srcIdx),
      blt(builtin), builtin(getBuiltin(builtin)),
      builtinId(getBuiltinNr(builtin)) {
    for (unsigned i = 0; i < args.size(); ++i)
        this->pushArg(args[i], PirType::val());
}

void CallBuiltin::printArgs(std::ostream& out) {
    std::cout << getBuiltinName(builtinId) << ", ";
    Instruction::printArgs(out);
}

void CallSafeBuiltin::printArgs(std::ostream& out) {
    std::cout << getBuiltinName(builtinId) << ", ";
    Instruction::printArgs(out);
}

void Deopt::printArgs(std::ostream& out) {
    out << " ";
    for (auto frame : frames)
        out << frame.code << "@" << frame.pc;
    out << ", stack=[";
    for (size_t i = 0; i < nargs() - 1; ++i) {
        arg(i).val()->printRef(out);
        if (i + 2 < nargs())
            out << ", ";
    }
    out << "], env=";
    env()->printRef(out);
}

MkFunCls::MkFunCls(Closure* fun, Value* parent, SEXP fml, SEXP code, SEXP src)
    : FixedLenInstruction(RType::closure, parent), fun(fun), fml(fml),
      code(code), src(src) {
    assert(fun->closureEnv() == Env::notClosed());
}

void MkFunCls::printArgs(std::ostream& out) {
    out << *fun;
    out << ", ";
    Instruction::printArgs(out);
}

void StaticCall::printArgs(std::ostream& out) {
    out << *cls_;
    if (nargs() > 0)
        out << ", ";
    Instruction::printArgs(out);
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
    default: {}
    }
    return nullptr;
}

} // namespace pir
} // namespace rir
