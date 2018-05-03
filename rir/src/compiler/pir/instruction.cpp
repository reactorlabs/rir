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

namespace rir {
namespace pir {

static size_t getMaxInstructionNameLength() {
    size_t max = 0;
    size_t cur;
#define V(n)                                                                   \
    cur = std::string(#n).length();                                            \
    if (cur > max)                                                             \
        max = cur;
    COMPILER_INSTRUCTIONS(V)
    return max;
}
static size_t maxInstructionNameLength = getMaxInstructionNameLength();

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
    Visitor::run(bb(), [&](Instruction* i) {
        i->eachArg([&](InstrArg& arg) {
            if (arg.val() == this)
                arg.val() = replace;
        });
    });
}

void LdConst::printArgs(std::ostream& out) {
    std::string val;
    {
        CaptureOut rec;
        Rf_PrintValue(c);
        val = rec();
    }
    if (val.length() > 0)
        val.pop_back();
    std::replace(val.begin(), val.end(), '\n', ' ');
    if (val.length() > 40) {
        val.resize(47);
        val.append("...");
    }
    out << val;
}

void Branch::printArgs(std::ostream& out) {
    FixedLenInstruction::printArgs(out);
    out << " -> BB" << bb()->next1->id << " (if true) | BB" << bb()->next0->id
        << " (if false)";
}

void MkArg::printArgs(std::ostream& out) {
    arg<0>().val()->printRef(out);
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
    out << "parent=";
    arg(0).val()->printRef(out);
    if (nargs() > 1)
        out << ", ";
    for (unsigned i = 0; i < nargs() - 1; ++i) {
        out << CHAR(PRINTNAME(this->varName[i])) << "=";
        this->arg(i + 1).val()->printRef(out);
        if (i != nargs() - 2)
            out << ", ";
    }
}

void Phi::updateType() {
    type = arg(0).val()->type;
    eachArg([&](Value* v) -> void { type = type | v->type; });
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

CallSafeBuiltin::CallSafeBuiltin(SEXP builtin, const std::vector<Value*>& args)
    : VarLenInstruction(PirType::valOrLazy()), builtin(getBuiltin(builtin)),
      builtinId(getBuiltinNr(builtin)) {
    for (unsigned i = 0; i < args.size(); ++i)
        this->pushArg(args[i], PirType::val());
}

CallBuiltin::CallBuiltin(Value* e, SEXP builtin,
                         const std::vector<Value*>& args, unsigned src)
    : VarLenInstruction(PirType::valOrLazy(), e), blt(builtin),
      builtin(getBuiltin(builtin)), builtinId(getBuiltinNr(builtin)) {
    for (unsigned i = 0; i < args.size(); ++i)
        this->pushArg(args[i], PirType::val());
    srcIdx = src;
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
    out << "@" << pc << ", stack=[";
    for (size_t i = 1; i < nargs(); ++i) {
        arg(i).val()->printRef(out);
        if (i + 1 < nargs())
            out << ", ";
    }
    out << "], env=";
    env()->printRef(out);
}

MkFunCls::MkFunCls(Closure* fun, Value* parent)
    : FixedLenInstruction(RType::closure, parent), fun(fun) {
    assert(fun->closureEnv() == Env::notClosed());
}

void MkFunCls::printArgs(std::ostream& out) {
    out << *fun;
    out << ", ";
    Instruction::printArgs(out);
}

void StaticEagerCall::printArgs(std::ostream& out) {
    out << *cls_;
    if (nargs() > 0)
        out << ", ";
    Instruction::printArgs(out);
}

void StaticCall::printArgs(std::ostream& out) {
    out << *cls_;
    if (nargs() > 0)
        out << ", ";
    Instruction::printArgs(out);
}

} // namespace pir
} // namespace rir
