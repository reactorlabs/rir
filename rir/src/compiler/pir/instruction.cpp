#include "instruction.h"
#include "pir_impl.h"

#include "../analysis/query.h"
#include "../util/safe_builtins_list.h"
#include "../util/visitor.h"
#include "R/Funtab.h"
#include "R/Printing.h"
#include "R/Symbols.h"
#include "api.h"
#include "compiler/analysis/cfg.h"
#include "runtime/DispatchTable.h"
#include "utils/Pool.h"
#include "utils/Terminal.h"

#include <algorithm>
#include <cassert>
#include <iomanip>
#include <set>
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

constexpr Effects Instruction::errorWarnVisible;

static bool printInstructionId() {
    return DebugOptions::DefaultDebugOptions.flags.contains(
        DebugFlag::PrintInstructionIds);
}

std::string Instruction::getRef() const {
    std::stringstream ss;
    ss << "PIR";
    printRef(ss);
    return ss.str();
}

void Instruction::printRef(std::ostream& out) const {
    if (type == PirType::env())
        out << "e" << id();
    else
        out << "%" << id();
}

bool Instruction::mayObserveContext(MkEnv* e) const {
    if (!hasEnv())
        return false;
    if (!effects.contains(Effect::Reflection))
        return false;
    // This diverges slightly from gnur. We consider that promises cannot
    // observe contexts of the evaluating context. In gnur it so happens
    // that they can, but it is unclear if that is intended.
    // See pir_regression9.r tests for a counterexample.
    if (tag == Tag::Force)
        return false;
    if (e == nullptr)
        return true;
    while (e) {
        if (e == env())
            return true;
        e = MkEnv::Cast(e->lexicalEnv());
    }
    return false;
}

void printPaddedInstructionName(std::ostream& out, const std::string& name) {
    out << std::left << std::setw(maxInstructionNameLength + 1) << name << " ";
}

void printPaddedIdTypeRef(std::ostream& out, const Instruction* i) {
    if (printInstructionId()) {
        out << (void*)i << " ";
    }
    std::ostringstream buf;
    buf << i->type;
    out << std::left << std::setw(15) << buf.str() << " ";
    buf.str("");
    if (i->type != PirType::voyd()) {
        i->printRef(buf);
        out << std::setw(5) << buf.str() << " = ";
    } else {
        out << "        ";
    }
}

void Instruction::printEffects(std::ostream& out, bool tty) const {
    if (!hasEffect()) {
        out << " ";
        return;
    }
    const size_t totalEffs = (size_t)Effect::LAST - (size_t)Effect::FIRST;
    Effects eff;
    if (effects.count() > totalEffs / 2) {
        out << "!";
        eff = ~effects;
    } else {
        eff = effects;
    }
    for (auto it = eff.begin(); it != eff.end(); ++it) {
        Effect effect = *it;
        switch (effect) {
#define CASE(Name, Str)                                                        \
    case Effect::Name:                                                         \
        out << Str;                                                            \
        break;
            CASE(Visibility, "v")
            CASE(Warn, "w")
            CASE(Error, "e")
            CASE(Force, "f")
            CASE(Reflection, "r")
            CASE(LeaksArg, "l")
            CASE(ChangesContexts, "C")
            CASE(ReadsEnv, "R")
            CASE(WritesEnv, "W")
            CASE(LeaksEnv, "L")
            CASE(TriggerDeopt, "D")
            CASE(ExecuteCode, "X")
            CASE(DependsOnAssume, "d")
            CASE(MutatesArgument, "M")
            CASE(UpdatesMetadata, "m")
#undef CASE
        }
    }
}

void printPaddedEffects(std::ostream& out, bool tty, const Instruction* i) {
    std::ostringstream buf;
    i->printEffects(buf, tty);
    out << std::setw(6) << buf.str();
}

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

void Instruction::printGraphArgs(std::ostream& out, bool tty) const {
    printArgs(out, tty);
}

void Instruction::printGraphBranches(std::ostream& out, size_t bbId) const {
    assert(false);
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

void Instruction::print(std::ostream& out, bool tty) const {
    printPaddedIdTypeRef(out, this);
    printPaddedInstructionName(out, name());
    printPaddedEffects(out, tty, this);
    printArgs(out, tty);
    printEnv(out, tty);

    if (hasTypeFeedback() && tag != Tag::MkEnv) {
        out << "   <";
        if (typeFeedback().value)
            typeFeedback().value->printRef(out);
        else if (!typeFeedback().type.isVoid())
            out << typeFeedback().type;
        if (!typeFeedback().feedbackOrigin.isValid())
            out << "@?";
        out << ">";
    }
}

void Instruction::printGraph(std::ostream& out, bool tty) const {
    printPaddedIdTypeRef(out, this);
    printPaddedInstructionName(out, name());
    printPaddedEffects(out, tty, this);
    printGraphArgs(out, tty);
    printEnv(out, tty);
}

bool Instruction::validIn(Code* code) const { return bb()->owner == code; }

bool Instruction::nonObjectArgs() {
    auto answer = true;
    this->eachArg([&](Value* arg) {
        if (!answer)
            return;

        if (this->hasEnv() && this->env() == arg)
            return;
        if (!arg->followCastsAndForce()->type.maybeObj())
            return;
        if (arg->type.maybePromiseWrapped()) {
            answer = false;
            return;
        }

        auto fb = PirType::bottom();
        if (auto j = Instruction::Cast(arg))
            fb = j->typeFeedback().type;
        if (fb.isVoid()) {
            if (auto j = Instruction::Cast(arg->followCastsAndForce()))
                fb = j->typeFeedback().type;
        }

        if (fb.isVoid() || fb.maybeObj())
            answer = false;
    });
    return answer;
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

Instruction::InstructionUID Instruction::id() const {
    return InstructionUID(bb()->id, bb()->indexOf(this));
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

FrameState* Instruction::frameState() const {
    return FrameState::Cast(frameStateOrTs());
}

void Instruction::clearFrameState() {
    updateFrameState(Tombstone::framestate());
}

void Instruction::replaceUsesIn(
    Value* val, BB* target,
    const std::function<void(Instruction*, size_t)>& postAction,
    const std::function<bool(Instruction*)>& replaceOnly) {
    Value::replaceUsesIn(val, target, postAction, replaceOnly);

    // Propagate typefeedback
    if (auto rep = Instruction::Cast(val)) {
        if (typeFeedback_.get())
            if (!rep->type.isA(typeFeedback().type) &&
                rep->typeFeedback().type.isVoid())
                rep->typeFeedback(typeFeedback());
    }
}

void Instruction::replaceDominatedUses(
    Instruction* replace, const DominanceGraph& dom,
    const std::initializer_list<Tag>& skip,
    const std::function<void(Instruction*)>& postAction) {
    checkReplace(replace);

    auto start = false;

    auto stop = replace->bb() != bb() ? bb() : nullptr;
    Visitor::run(replace->bb(), stop, [&](BB* bb) {
        if (!dom.dominates(replace->bb(), bb))
            return;
        for (auto& i : *bb) {
            // First we need to find the position of the replacee, only after
            // this instruction is in scope we should start replacing
            if (!start) {
                if (i == replace)
                    start = true;
                continue;
            }

            if (skip.size() == 0 ||
                std::find(skip.begin(), skip.end(), i->tag) == skip.end()) {
                bool changed = false;
                i->eachArg([&](InstrArg& arg) {
                    if (arg.val() == this) {
                        arg.val() = replace;
                        changed = true;
                    }
                });
                if (changed) {
                    i->updateTypeAndEffects();
                    postAction(i);
                }
            }

            // If we reach the original instruction we have to stop replacing.
            // E.g. in  i->replaceReachableUses(j)
            //
            //   loop:
            //     i = ...
            //     i + 1
            //     j = ...
            //     j + 1
            //     goto loop
            //
            // we better not replace the i in i + 1.
            if (i == this)
                return;
        }
        assert(start);
    });

    // Propagate typefeedback
    if (typeFeedback_.get())
        if (auto rep = Instruction::Cast(replace)) {
            if (!rep->type.isA(typeFeedback().type) &&
                rep->typeFeedback().type.isVoid())
                rep->typeFeedback(typeFeedback());
        }
}

void Instruction::replaceUsesWith(
    Value* replace, const std::function<void(Instruction*, size_t)>& postAction,
    const std::function<bool(Instruction*)>& replaceOnly) {
    replaceUsesIn(replace, bb(), postAction, replaceOnly);
}

void Instruction::replaceUsesAndSwapWith(
    Instruction* replace, std::vector<Instruction*>::iterator it) {
    replaceUsesWith(replace);
    bb()->replace(it, replace);
}

bool Instruction::usesAreOnly(BB* target, std::unordered_set<Tag> tags) {
    return Visitor::check(target, [&](Instruction* i) -> bool {
        bool ok = true;
        i->eachArg([&](InstrArg& arg) {
            if (arg.val() == this && !tags.count(i->tag))
                ok = false;
        });
        return ok;
    });
}

void Instruction::replaceUsesOfValue(Value* old, Value* rpl) {
    this->eachArg([&](InstrArg& arg) {
        if (arg.val() == old)
            arg.val() = rpl;
    });
}

bool Instruction::usesDoNotInclude(BB* target, std::unordered_set<Tag> tags) {
    return Visitor::check(target, [&](Instruction* i) -> bool {
        bool ok = true;
        i->eachArg([&](InstrArg& arg) {
            if (arg.val() == this && tags.count(i->tag)) {
                ok = false;
            }
        });
        return ok;
    });
}

const Value* Instruction::cFollowCasts() const {
    if (auto cast = PirCopy::Cast(this))
        return cast->arg<0>().val()->followCasts();
    if (auto cast = CastType::Cast(this))
        return cast->arg<0>().val()->followCasts();
    if (auto chk = ChkFunction::Cast(this))
        return chk->arg<0>().val()->followCasts();
    if (auto chk = ChkMissing::Cast(this))
        return chk->arg<0>().val()->followCasts();
    return this;
}

const Value* Instruction::cFollowDownCastsAndForce() const {
    if (auto cast = PirCopy::Cast(this))
        return cast->arg<0>().val()->followCasts();
    if (auto cast = CastType::Cast(this)) {
        if (cast->kind == CastType::Downcast) {
            return cast->arg<0>().val()->followDownCastsAndForce();
        } else {
            if (auto mkarg = MkArg::Cast(cast->arg<0>().val()))
                if (mkarg->isEager())
                    return mkarg->eagerArg()->followDownCastsAndForce();
        }
    }
    if (auto force = Force::Cast(this))
        return force->input()->followDownCastsAndForce();
    if (auto mkarg = MkArg::Cast(this))
        if (mkarg->isEager())
            return mkarg->eagerArg()->followDownCastsAndForce();
    if (auto chk = ChkFunction::Cast(this))
        return chk->arg<0>().val()->followDownCastsAndForce();
    if (auto chk = ChkMissing::Cast(this))
        return chk->arg<0>().val()->followDownCastsAndForce();
    return this;
}

const Value* Instruction::cFollowCastsAndForce() const {
    if (auto cast = PirCopy::Cast(this))
        return cast->arg<0>().val()->followCasts();
    if (auto cast = CastType::Cast(this))
        return cast->arg<0>().val()->followCastsAndForce();
    if (auto force = Force::Cast(this))
        return force->input()->followCastsAndForce();
    if (auto mkarg = MkArg::Cast(this))
        if (mkarg->isEager())
            return mkarg->eagerArg()->followCastsAndForce();
    if (auto chk = ChkFunction::Cast(this))
        return chk->arg<0>().val()->followCastsAndForce();
    if (auto chk = ChkMissing::Cast(this))
        return chk->arg<0>().val()->followCastsAndForce();
    return this;
}

bool Instruction::envOnlyForObj() {
    switch (tag) {
#define V(Name) case Tag::Name:
        UNOP_INSTRUCTIONS(V)
        BINOP_INSTRUCTIONS(V)
        VECTOR_EXTRACT_INSTRUCTIONS(V)
        VECTOR_SUBASSIGN_INSTRUCTIONS(V)
#undef V
        return true;
    default:
        return false;
    }
}

void Branch::printArgs(std::ostream& out, bool tty) const {
    FixedLenInstruction::printArgs(out, tty);
    out << " -> BB" << bb()->trueBranch()->id << " (if true) | BB"
        << bb()->falseBranch()->id << " (if false)";
}

PirType Extract1_1D::inferType(const GetType& getType) const {
    auto res = ifNonObjectArgs(
        getType, type & getType(vec()).subsetType(getType(idx())), type);
    if (res.isA(PirType::num())) {
        if (auto c = Const::Cast(idx())) {
            if (IS_SIMPLE_SCALAR(c->c(), INTSXP)) {
                if (INTEGER(c->c())[0] >= 1)
                    res = res.simpleScalar();
            } else if (IS_SIMPLE_SCALAR(c->c(), REALSXP)) {
                if (REAL(c->c())[0] >= 1)
                    res = res.simpleScalar();
            }
        }
    }

    return res;
}

void CastType::printArgs(std::ostream& out, bool tty) const {
    out << (kind == Upcast ? "up " : "dn ");
    FixedLenInstruction::printArgs(out, tty);
}

void Branch::printGraphArgs(std::ostream& out, bool tty) const {
    FixedLenInstruction::printArgs(out, tty);
}

void Branch::printGraphBranches(std::ostream& out, size_t bbId) const {
    auto trueBB = bb()->trueBranch();
    auto falseBB = bb()->falseBranch();
    out << "  BB" << bbId << " -> BB" << trueBB->uid()
        << " [color=green];  // -> BB" << trueBB->id << "\n"
        << "  BB" << bbId << " -> BB" << falseBB->uid()
        << " [color=red];  // -> BB" << falseBB->id << "\n";
}

MkArg::MkArg(Promise* prom, Value* v, Value* env)
    : FixedLenInstructionWithEnvSlot(RType::prom, {{PirType::val()}}, {{v}},
                                     env),
      prom_(prom) {
    assert(eagerArg() == v);
    assert(!MkArg::Cast(eagerArg()->followCasts()));
    if (isEager()) {
        noReflection = true;
    }
}

void MkArg::printArgs(std::ostream& out, bool tty) const {
    eagerArg()->printRef(out);
    out << ", " << *prom();
    if (noReflection)
        out << " (!refl)";
    out << ", ";
}

bool MkArg::usesPromEnv() const {
    if (!isEager()) {
        // Note that the entry block is empty and jumps to the next block; this
        // is to ensure that it has no predecessors.
        BB* entry = prom()->entry;
        assert(entry->isEmpty() && entry->isJmp() &&
               !((const BB*)entry)->next()->isEmpty());
        BB* bb = entry->next();
        if (bb->size() > 0 && LdFunctionEnv::Cast(*bb->begin())) {
            return true;
        }
    }
    return false;
}

void Missing::printArgs(std::ostream& out, bool tty) const {
    out << CHAR(PRINTNAME(varName)) << ", ";
}

void LdVar::printArgs(std::ostream& out, bool tty) const {
    out << CHAR(PRINTNAME(varName)) << (forUpdate ? " !upd" : "") << ", ";
}

void LdFun::printArgs(std::ostream& out, bool tty) const {
    out << CHAR(PRINTNAME(varName)) << ", ";
    if (guessedBinding()) {
        out << "<";
        guessedBinding()->printRef(out);
        out << ">, ";
    }
    if (hint_ && hint_ != symbol::ambiguousCallTarget) {
        out << "<" << hint_ << ">, ";
    }
}

void LdArg::printArgs(std::ostream& out, bool tty) const { out << pos; }

void StVar::printArgs(std::ostream& out, bool tty) const {
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
    eachLocalVar([&](SEXP name, Value* v, bool miss) {
        out << CHAR(PRINTNAME(name));
        if (miss)
            out << "(miss)";
        out << "=";
        v->printRef(out);
        out << ", ";
    });
    out << "parent=";
    Instruction::printEnv(out, tty);
    out << ", context " << context;
}

void DotsList::printArgs(std::ostream& out, bool tty) const {
    size_t pos = 0;
    eachArg([&](Value* v) {
        auto n = names[pos++];
        if (n != R_NilValue)
            out << CHAR(PRINTNAME(n)) << "=";
        v->printRef(out);
        if (pos != names.size())
            out << ", ";
    });
}

void Is::printArgs(std::ostream& out, bool tty) const {
    arg<0>().val()->printRef(out);
    out << ", " << typecheck;
}

PirType Is::upperBound() const {
    switch (typecheck) {
    case BC::RirTypecheck::isNILSXP:
        return RType::nil;
    case BC::RirTypecheck::isLGLSXP:
        return RType::logical;
    case BC::RirTypecheck::isINTSXP:
        return RType::integer;
    case BC::RirTypecheck::isREALSXP:
        return RType::real;
    case BC::RirTypecheck::isCPLXSXP:
        return RType::cplx;
    case BC::RirTypecheck::isSTRSXP:
        return RType::str;
    case BC::RirTypecheck::isRAWSXP:
        return RType::raw;
    case BC::RirTypecheck::isLISTSXP:
        return PirType(RType::list) | RType::nil;
    case BC::RirTypecheck::isVECSXP:
        return PirType(RType::vec) | RType::list;
    case BC::RirTypecheck::isEXPRSXP:
        return RType::expressions;
    case BC::RirTypecheck::isNonObject:
        return PirType::any().notObject();
    case BC::RirTypecheck::isVector:
        return PirType::vecs();
    // an over-approximation
    case BC::RirTypecheck::isFactor:
        return PirType(RType::integer).orObject();
    }
    assert(false);
    return PirType::any();
}

PirType Is::lowerBound() const {
    // there is no precise under-approximation
    if (typecheck == BC::RirTypecheck::isFactor)
        return PirType::bottom();
    return upperBound();
}

void IsType::printArgs(std::ostream& out, bool tty) const {
    arg<0>().val()->printRef(out);
    out << " isA " << typeTest;
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
    printPaddedIdTypeRef(out, this);
    arg(0).val()->printRef(out);
}

CallSafeBuiltin::CallSafeBuiltin(SEXP builtin, const std::vector<Value*>& args,
                                 unsigned srcIdx)
    : VarLenInstruction(SafeBuiltinsList::returnsObj(getBuiltinNr(builtin))
                            ? PirType::val().notMissing()
                            : PirType::val().notMissing().notObject(),
                        srcIdx),
      builtinSexp(builtin), builtin(getBuiltin(builtin)),
      builtinId(getBuiltinNr(builtin)) {
    for (unsigned i = 0; i < args.size(); ++i)
        this->pushArg(args[i], PirType::val());
}

size_t CallSafeBuiltin::gvnBase() const {
    if (!SafeBuiltinsList::idempotent(builtinId)) {
        if (type.maybeObj() ||
            !SafeBuiltinsList::nonObjectIdempotent(builtinId))
            return 0;
    }
    return hash_combine(builtinId, tagHash());
}

PirType CallSafeBuiltin::inferType(const Instruction::GetType& getType) const {
    PirType inferred = PirType::bottom();
    std::string name = getBuiltinName(builtinSexp);

    static const std::unordered_set<std::string> bitwise = {
        "bitwiseXor", "bitwiseShiftL", "bitwiseShiftLR",
        "bitwiseAnd", "bitwiseNot",    "bitwiseOr"};
    if (bitwise.count(name)) {
        inferred = PirType(RType::integer);
        if (getType(callArg(0).val()).isSimpleScalar() &&
            getType(callArg(1).val()).isSimpleScalar())
            inferred = inferred.simpleScalar();
    }

    if ("length" == name) {
        inferred = (PirType() | RType::integer | RType::real)
                       .simpleScalar()
                       .orNAOrNaN();
    }

    int doSummary =
        "min" == name || "max" == name || "prod" == name || "sum" == name;
    if (name == "abs" || doSummary) {
        if (nCallArgs()) {
            auto m = PirType::bottom();
            for (size_t i = 0; i < nCallArgs(); ++i)
                m = m.mergeWithConversion(getType(callArg(i).val()));
            if (!m.maybeObj()) {
                auto lub = PirType::num().orAttribsOrObj();
                // Min/max support string comparison
                if (name == "min" || name == "max")
                    lub = lub | RType::str;
                inferred = m & lub;

                if (inferred.maybe(RType::logical))
                    inferred =
                        inferred.orT(RType::integer).notT(RType::logical);

                if (doSummary)
                    inferred = inferred.simpleScalar();
                if ("prod" == name)
                    inferred = inferred.orT(RType::real).notT(RType::integer);
                if ("abs" == name) {
                    if (inferred.maybe(RType::cplx))
                        inferred = inferred.orT(RType::real).notT(RType::cplx);
                }
            }
        }
    }

    if ("sqrt" == name) {
        if (nCallArgs()) {
            auto m = PirType::bottom();
            for (size_t i = 0; i < nCallArgs(); ++i)
                m = m.mergeWithConversion(getType(callArg(i).val()));
            if (!m.maybeObj()) {
                inferred = m & PirType::num().orAttribsOrObj();
                inferred = inferred.orT(RType::real).notT(RType::integer);
            }
        }
    }

    if ("as.integer" == name) {
        if (!getType(callArg(0).val()).maybeObj()) {
            inferred = PirType(RType::integer);
            if (getType(callArg(0).val()).isSimpleScalar())
                inferred = inferred.simpleScalar();
        }
    }

    if ("typeof" == name) {
        inferred = PirType(RType::str).simpleScalar();
    }

    static const std::unordered_set<std::string> vecTests = {
        "is.na", "is.nan", "is.finite", "is.infinite"};
    if (vecTests.count(name)) {
        if (!getType(callArg(0).val()).maybeObj()) {
            inferred = PirType(RType::logical);
            if (getType(callArg(0).val()).maybeHasAttrs())
                inferred = inferred.orAttribsOrObj().notObject();
            if (!getType(callArg(0).val()).maybeNotFastVecelt())
                inferred = inferred.fastVecelt();
            if (getType(callArg(0).val()).isSimpleScalar())
                inferred = inferred.simpleScalar();
        }
    }

    static const std::unordered_set<std::string> tests = {
        "is.vector",   "is.null",      "is.integer",
        "is.double",   "is.complex",   "is.character",
        "is.symbol",   "is.name",      "is.environment",
        "is.list",     "is.pairlist",  "is.expression",
        "is.raw",      "is.object",    "isS4",
        "is.numeric",  "is.matrix",    "is.array",
        "is.atomic",   "is.recursive", "is.call",
        "is.language", "is.function",  "all",
        "any"};

    if (tests.count(name)) {
        static const std::unordered_set<std::string> maybeDispatch = {"all",
                                                                      "any"};

        if (!maybeDispatch.count(name) || !getType(callArg(0).val()).maybeObj())
            inferred = PirType(RType::logical).simpleScalar().notNAOrNaN();
    }

    if ("c" == name) {
        inferred = mergedInputType(getType).collectionType(nCallArgs());
        // If at least one arg is non-nil, then the result is
        // also not nil
        if (inferred.maybe(RType::nil)) {
            auto notNil = false;
            eachArg([&](Value* v) {
                if (!v->type.maybe(RType::nil))
                    notNil = true;
            });
            if (notNil)
                inferred = inferred.notT(RType::nil);
        }
    }

    if ("vector" == name) {
        if (auto con = Const::Cast(arg(0).val())) {
            if (TYPEOF(con->c()) == STRSXP && XLENGTH(con->c()) == 1) {
                SEXPTYPE type = Rf_str2type(CHAR(STRING_ELT(con->c(), 0)));
                switch (type) {
                case LGLSXP:
                    inferred = RType::logical;
                    break;
                case INTSXP:
                    inferred = RType::integer;
                    break;
                case REALSXP:
                    inferred = RType::real;
                    break;
                case CPLXSXP:
                    inferred = RType::cplx;
                    break;
                case STRSXP:
                    inferred = RType::str;
                    break;
                case VECSXP:
                    inferred = RType::vec;
                    break;
                case RAWSXP:
                    inferred = RType::raw;
                    break;
                default:
                    assert(false);
                    break;
                }
            }
        }
    }

    if ("strsplit" == name) {
        inferred = PirType(RType::vec).orAttribsOrObj();
    }

    if (inferred != PirType::bottom())
        return inferred & type;

    return Instruction::inferType(getType);
}

CallBuiltin::CallBuiltin(Value* env, SEXP builtin,
                         const std::vector<Value*>& args, unsigned srcIdx)
    : VarLenInstructionWithEnvSlot(PirType::val(), env, srcIdx),
      builtinSexp(builtin), builtin(getBuiltin(builtin)),
      builtinId(getBuiltinNr(builtin)) {
    for (unsigned i = 0; i < args.size(); ++i)
        this->pushArg(args[i], PirType::val() | RType::expandedDots);
}

Instruction* BuiltinCallFactory::New(Value* callerEnv, SEXP builtin,
                                     const std::vector<Value*>& args,
                                     unsigned srcIdx) {
    bool noObj = true;
    bool unsafe = false;
    for (auto a : args) {
        if (auto mk = MkArg::Cast(a)) {
            if (mk->isEager())
                if (!mk->eagerArg()->type.maybeObj())
                    continue;
            noObj = false;
            continue;
        }
        if (a->type.maybeObj()) {
            noObj = false;
        }
        if (a->type.isA(RType::expandedDots)) {
            unsafe = true;
        }
    }

    if (!unsafe && (SafeBuiltinsList::always(builtin) ||
                    (noObj && SafeBuiltinsList::nonObject(builtin))))
        return new CallSafeBuiltin(builtin, args, srcIdx);
    else
        return new CallBuiltin(callerEnv, builtin, args, srcIdx);
}

VisibilityFlag CallBuiltin::visibilityFlag() const {
    switch (getFlag(builtinId)) {
    case 0:
        return VisibilityFlag::On;
    case 1:
        return VisibilityFlag::Off;
    default:
        return VisibilityFlag::Unknown;
    }
}

VisibilityFlag CallSafeBuiltin::visibilityFlag() const {
    switch (getFlag(builtinId)) {
    case 0:
        return VisibilityFlag::On;
    case 1:
        return VisibilityFlag::Off;
    default:
        return VisibilityFlag::Unknown;
    }
}

static void printCallArgs(std::ostream& out, const CallInstruction* call) {
    out << "(";

    size_t i = 0;
    size_t n = call->nCallArgs();
    call->eachNamedCallArg([&](SEXP name, Value* v) {
        if (name != R_NilValue)
            out << CHAR(PRINTNAME(name)) << "=";
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
    if (inPromise)
        out << "(pr)";
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

void Assume::printArgs(std::ostream& out, bool tty) const {
    InstructionImplementation::printArgs(out, tty);
    out << " (" << reason << ")";
}

Deopt::Deopt(FrameState* frameState)
    : FixedLenInstruction(
          PirType::voyd(),
          {{NativeType::frameState, NativeType::deoptReason, PirType::any()}},
          {{frameState, DeoptReasonWrapper::unknown(),
            UnknownDeoptTrigger::instance()}}) {}

void Deopt::printArgs(std::ostream& out, bool tty) const {
    if (hasDeoptReason())
        InstructionImplementation::printArgs(out, tty);
    else
        arg(0).val()->printRef(out);
    if (escapedEnv)
        out << "   !";
}

bool Deopt::hasDeoptReason() const {
    return deoptReason() != DeoptReasonWrapper::unknown();
}

MkCls::MkCls(Closure* cls, SEXP formals, SEXP srcRef,
             DispatchTable* originalBody, Value* lexicalEnv)
    : FixedLenInstructionWithEnvSlot(PirType::closure(), lexicalEnv), cls(cls),
      originalBody(originalBody), formals(formals), srcRef(srcRef) {}

void MkCls::printArgs(std::ostream& out, bool tty) const {
    if (cls)
        out << *cls;
    Instruction::printArgs(out, tty);
}

void Force::printArgs(std::ostream& out, bool tty) const {
    input()->printRef(out);
    out << ", ";
    if (frameState()) {
        frameState()->printRef(out);
        out << ", ";
    }
}

PirType Force::inferType(const GetType& getType) const {
    return type & getType(input()).forced();
}

ClosureVersion* CallInstruction::tryDispatch(Closure* cls) const {
    auto assumptions = inferAvailableAssumptions();

    if (!cls->matchesUserContext(assumptions)) {
#ifdef WARN_DISPATCH_FAIL
        std::cout << "DISPATCH FAILED! Closure's user context doesn't match "
                     "available assumptions \n";
#endif
        return nullptr;
    }

    auto res = cls->findCompatibleVersion(assumptions);
#ifdef WARN_DISPATCH_FAIL
    if (!res) {
        std::cout << "DISPATCH FAILED! Available versions: \n";
        cls->eachVersion([&](ClosureVersion* v) {
            std::cout << "* " << v->context() << "\n";
        });
        std::cout << "Available assumptions at callsite: \n ";
        std::cout << inferAvailableAssumptions() << "\n";
    }
#endif
    if (res) {
        if (res->context().includes(Assumption::NoExplicitlyMissingArgs))
            assert(res->effectiveNArgs() == nCallArgs());
        else
            assert(res->effectiveNArgs() >= nCallArgs());
    }
    return res;
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
    default: {
    }
    }
    return nullptr;
}

Context CallInstruction::inferAvailableAssumptions() const {
    auto callee = tryGetCls();
    rir::Function* localFun = nullptr;
    SEXP formals = nullptr;

    Context given;
    // If we know the callee, we can verify arg order and statically matching
    if (callee) {
        given.add(Assumption::StaticallyArgmatched);
        given.add(Assumption::CorrectOrderOfArguments);
        if (callee->nargs() >= nCallArgs()) {
            given.add(Assumption::NotTooManyArguments);
            auto missing = callee->nargs() - nCallArgs();
            given.numMissing(missing);
            given.add(Assumption::NoExplicitlyMissingArgs);
        }
    } else {
        if (auto clsArg = tryGetClsArg()) {
            if (auto mk = MkCls::Cast(clsArg)) {
                localFun = mk->originalBody->baseline();
                formals = mk->formals;
            } else if (auto ld = Const::Cast(clsArg)) {
                if (TYPEOF(ld->c()) == CLOSXP) {
                    if (auto dt = DispatchTable::check(BODY(ld->c()))) {
                        localFun = dt->baseline();
                        formals = FORMALS(ld->c());
                    }
                }
            }
        }
        if (localFun) {
            given.add(Assumption::StaticallyArgmatched);
            given.add(Assumption::CorrectOrderOfArguments);
            if (localFun->nargs() >= nCallArgs()) {
                given.add(Assumption::NotTooManyArguments);
                auto missing = localFun->nargs() - nCallArgs();
                given.numMissing(missing);
                given.add(Assumption::NoExplicitlyMissingArgs);
            }
        }
    }

    size_t i = 0;
    eachNamedCallArg([&](SEXP name, Value* arg) {
        if (arg->type.maybe(RType::expandedDots) || name != R_NilValue) {
            // who knows to how many args this expands...
            given.remove(Assumption::CorrectOrderOfArguments);
            given.remove(Assumption::StaticallyArgmatched);
            given.remove(Assumption::NotTooManyArguments);
            given.numMissing(0);
        } else {
            arg->callArgTypeToContext(given, i);
        }

        if (callee) {
            if (callee->formals().names().size() > i) {
                auto formal = callee->formals().names()[i];
                if (formal == R_DotsSymbol) {
                    // If the callee expects `...` then we can only statically
                    // statisfy that with an explicit (unexpanded) dots list!
                    if (!arg->type.isA(RType::dots)) {
                        given.remove(Assumption::CorrectOrderOfArguments);
                        given.remove(Assumption::StaticallyArgmatched);
                    }
                } else if (name != R_NilValue && formal != name) {
                    // we could be more clever here, but for now we just assume
                    // if any of the formal names does not match the passed name
                    // then it's not in the correct order.
                    given.remove(Assumption::CorrectOrderOfArguments);
                }
            }
        } else if (localFun && formals) {
            if (localFun->signature().numArguments > i) {
                if (TAG(formals) == R_DotsSymbol) {
                    // If the callee expects `...` then we can only statically
                    // statisfy that with an explicit (unexpanded) dots list!
                    if (!arg->type.isA(RType::dots)) {
                        given.remove(Assumption::CorrectOrderOfArguments);
                        given.remove(Assumption::StaticallyArgmatched);
                    }
                } else if (TAG(formals) != R_NilValue && TAG(formals) != name) {
                    // we could be more clever here, but for now we just assume
                    // if any of the formal names does not match the passed name
                    // then it's not in the correct order.
                    given.remove(Assumption::CorrectOrderOfArguments);
                }
                formals = CDR(formals);
            }
        }

        ++i;
    });

    return given;
}

Call::Call(Value* callerEnv, Value* fun, const std::vector<Value*>& args,
           Value* fs, unsigned srcIdx)
    : VarLenInstructionWithEnvSlot(PirType::val(), callerEnv, srcIdx) {
    assert(fs);
    pushArg(fs, NativeType::frameState);
    pushArg(fun, PirType::function());

    // Calling builtins with names or ... is not supported by callBuiltin,
    // that's why those calls go through the normal call BC.
    auto argtype = PirType(RType::prom) | RType::missing | RType::expandedDots |
                   PirType::val();
    if (auto con = Const::Cast(fun))
        if (TYPEOF(con->c()) == BUILTINSXP)
            argtype = argtype | PirType::val();

    for (unsigned i = 0; i < args.size(); ++i)
        pushArg(args[i], argtype);
}

NamedCall::NamedCall(Value* callerEnv, Value* fun,
                     const std::vector<Value*>& args,
                     const std::vector<SEXP>& names_, Value* fs,
                     unsigned srcIdx)
    : VarLenInstructionWithEnvSlot(PirType::val(), callerEnv, srcIdx) {
    assert(names_.size() == args.size());
    assert(fs);
    pushArg(fs, NativeType::frameState);
    pushArg(fun, PirType::function());

    // Calling builtins with names or ... is not supported by callBuiltin,
    // that's why those calls go through the normal call BC.
    auto argtype = PirType(RType::prom) | RType::missing | RType::expandedDots |
                   PirType::val();
    if (auto con = Const::Cast(fun))
        if (TYPEOF(con->c()) == BUILTINSXP)
            argtype = argtype | PirType::val();

    for (unsigned i = 0; i < args.size(); ++i) {
        pushArg(args[i], argtype);
        auto name = names_[i];
        assert(TYPEOF(name) == SYMSXP || name == R_NilValue);
        names.push_back(name);
    }
}

NamedCall::NamedCall(Value* callerEnv, Value* fun,
                     const std::vector<Value*>& args,
                     const std::vector<BC::PoolIdx>& names_, Value* fs,
                     unsigned srcIdx)
    : VarLenInstructionWithEnvSlot(PirType::val(), callerEnv, srcIdx) {
    assert(names_.size() == args.size());
    assert(fs);
    pushArg(fs, NativeType::frameState);
    pushArg(fun, PirType::function());

    // Calling builtins with names or ... is not supported by callBuiltin,
    // that's why those calls go through the normal call BC.
    auto argtype = PirType(RType::prom) | RType::missing | RType::expandedDots |
                   PirType::val();
    if (auto con = Const::Cast(fun))
        if (TYPEOF(con->c()) == BUILTINSXP)
            argtype = argtype | PirType::val();

    for (unsigned i = 0; i < args.size(); ++i) {
        pushArg(args[i], argtype);
        auto name = Pool::get(names_[i]);
        assert(TYPEOF(name) == SYMSXP || name == R_NilValue);
        names.push_back(name);
    }
}

StaticCall::StaticCall(Value* callerEnv, ClosureVersion* clsVersion,
                       Context givenContext, const std::vector<Value*>& args,
                       const ArglistOrder::CallArglistOrder& argOrderOrig,
                       Value* fs, unsigned srcIdx, Value* runtimeClosure)
    : VarLenInstructionWithEnvSlot(PirType::val(), callerEnv, srcIdx),
      cls_(clsVersion->owner()), argOrderOrig(argOrderOrig),
      givenContext(givenContext) {

    auto cls = clsVersion->owner();
    assert(cls->nargs() >= args.size());
    assert(fs);
    pushArg(fs, NativeType::frameState);
    pushArg(runtimeClosure, PirType::function());
    for (unsigned i = 0; i < args.size(); ++i) {
        assert(!ExpandDots::Cast(args[i]) &&
               "Static Call cannot accept dynamic number of arguments");
        if (cls->formals().names()[i] == R_DotsSymbol) {
            pushArg(args[i],
                    PirType() | RType::prom | RType::missing | RType::dots);
        } else {
            pushArg(args[i],
                    PirType() | RType::prom | RType::missing | PirType::val());
        }
    }

    assert(tryDispatch() == clsVersion);
}

PirType StaticCall::inferType(const GetType& getType) const {
    auto t = PirType::bottom();
    if (auto v = tryDispatch()) {
        Visitor::run(v->entry, [&](BB* bb) {
            if (bb->isExit()) {
                if (auto r = Return::Cast(bb->last())) {
                    t = t | r->arg(0).val()->type;
                } else {
                    t = t | PirType::any();
                }
            }
        });
        if (!(type & t).isVoid())
            return type & t;
    }
    return type;
}

Effects StaticCall::inferEffects(const GetType& getType) const {
    if (auto v = tryDispatch())
        if (v->properties.includes(ClosureVersion::Property::NoReflection))
            return effects & ~Effects(Effect::Reflection);
    return effects;
}

ClosureVersion* StaticCall::tryDispatch() const {
    return CallInstruction::tryDispatch(cls());
}

ClosureVersion* StaticCall::tryOptimisticDispatch() const {
    auto dispatch = CallInstruction::tryDispatch(cls());
    if (!hint)
        return dispatch;

    if (!dispatch)
        return nullptr;

    return (hint->context() < dispatch->context()) ? dispatch : hint;
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
    printCallArgs(out, this);
    if (frameState()) {
        frameState()->printRef(out);
        out << ", ";
    }
}

void StaticCall::printArgs(std::ostream& out, bool tty) const {
    if (auto trg = tryDispatch()) {
        out << trg->name();
    } else {
        out << cls()->name();
    }
    if (auto hint = tryOptimisticDispatch()) {
        if (hint != tryDispatch())
            out << "<hint: " << hint->nameSuffix() << ">";
    }
    printCallArgs(out, this);
    if (frameState()) {
        frameState()->printRef(out);
        out << ", ";
    }

    if (runtimeClosure() != Tombstone::closure()) {
        out << "from ";
        runtimeClosure()->printRef(out);
        out << " ";
    }

    if (!argOrderOrig.empty()) {
        out << "{ ";
        for (auto a : argOrderOrig)
            out << ArglistOrder::decodeArg(a)
                << (ArglistOrder::isArgNamed(a) ? "n " : " ");
        out << "} ";
    }
}

void Checkpoint::printArgs(std::ostream& out, bool tty) const {
    FixedLenInstruction::printArgs(out, tty);
    out << " -> BB" << bb()->trueBranch()->id << " (default) | BB"
        << bb()->falseBranch()->id << " (if assume failed)";
}

void Checkpoint::printGraphArgs(std::ostream& out, bool tty) const {
    FixedLenInstruction::printArgs(out, tty);
}

void Checkpoint::printGraphBranches(std::ostream& out, size_t bbId) const {
    auto trueBB = bb()->trueBranch();
    auto falseBB = bb()->falseBranch();
    out << "  BB" << bbId << " -> BB" << trueBB->uid() << ";  // -> BB"
        << trueBB->id << "\n"
        << "  BB" << bbId << " -> BB" << falseBB->uid()
        << " [color=red];  // -> BB" << falseBB->id << "\n";
}

BB* Checkpoint::deoptBranch() { return bb()->falseBranch(); }
BB* Checkpoint::nextBB() { return bb()->trueBranch(); }

Value* Assume::valueUnderTest() const {
    switch (reason.reason) {
    case DeoptReason::Typecheck: {
        if (auto t = IsType::Cast(condition()))
            return t->arg<0>().val();
        break;
    }
    case DeoptReason::CallTarget: {
        if (auto t = Identical::Cast(condition())) {
            auto value = t->arg<0>().val();
            if (Const::Cast(value))
                value = t->arg<1>().val();
            assert(!Const::Cast(value));
            return value;
        }
        break;
    }
    case DeoptReason::ForceAndCall: {
        if (auto t = Identical::Cast(condition())) {
            auto value = t->arg<0>().val();
            if (Const::Cast(value))
                value = t->arg<1>().val();
            assert(!Const::Cast(value));
            return value;
        } else if (auto t = IsType::Cast(condition())) {
            return t->arg<0>().val();
        }
        break;
    }
    case DeoptReason::EnvStubMaterialized: {
        if (auto t = IsEnvStub::Cast(condition()))
            return t->arg(0).val();
        break;
    }
    case DeoptReason::DeadBranchReached: {
        return condition();
    }
    // DeadCall is an unconditional deopt and never associated with an
    // assume.
    case DeoptReason::Unknown:
    case DeoptReason::DeadCall:
        assert(false);
    }

    if (Instruction::Cast(condition())) {
        printRecursive(std::cerr, 2);
        assert(false && "Unexpected condition for this kind of assumption");
    }

    return nullptr;
}

PirType Colon::inferType(const GetType& getType) const {
    auto convertsToInt = [](Value* a) {
        if (a->type.isA(RType::integer))
            return true;
        if (a->type.isA(RType::real)) {
            if (auto ld = Const::Cast(a)) {
                if (IS_SIMPLE_SCALAR(ld->c(), REALSXP)) {
                    auto v = *REAL(ld->c());
                    if ((double)(int)v == v) {
                        return true;
                    }
                }
            }
        }
        return false;
    };

    auto t = inferredTypeForArithmeticInstruction(getType);

    if (convertsToInt(lhs()) && convertsToInt(rhs())) {
        t = RType::integer;
    }

    if (t.maybe(PirType::num()))
        t = t | RType::integer;
    return t.orNotScalar();
}

} // namespace pir
} // namespace rir
