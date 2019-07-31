#include "lower.h"
#include "R/Funtab.h"
#include "R/Symbols.h"
#include "R/r.h"
#include "builtins.h"
#include "compiler/analysis/liveness.h"
#include "compiler/pir/pir_impl.h"
#include "compiler/util/visitor.h"
#include "interpreter/LazyEnvironment.h"
#include "interpreter/instance.h"
#include "jit/jit-dump.h"
#include "jit/jit-value.h"
#include "runtime/DispatchTable.h"
#include "utils/Pool.h"

#include <cassert>
#include <iostream>

namespace rir {
namespace pir {

static const auto cpOfs = (size_t) & ((InterpreterInstance*)0) -> cp.list;
static const auto stdVecDtptrOfs = sizeof(SEXPREC_ALIGN);
static const auto carOfs_ = (size_t) & ((SEXP)0) -> u.listsxp.carval;
static const auto cdrOfs_ = (size_t) & ((SEXP)0) -> u.listsxp.cdrval;
static const auto tagOfs_ = (size_t) & ((SEXP)0) -> u.listsxp.tagval;
static const auto prValueOfs = (size_t) & ((SEXP)0) -> u.promsxp.value;
static const auto stackCellValueOfs = (size_t) & ((R_bcstack_t*)0) -> u.sxpval;
static const auto sxpinfofOfs = (size_t) & ((SEXP)0) -> sxpinfo;
static const auto dataptrOfs = (size_t)STDVEC_DATAPTR((SEXP)0);
static const auto extsxpMagicOfs =
    (size_t) & ((rir_header*)STDVEC_DATAPTR((SEXP)0)) -> magic;

struct Representation {
    enum Type {
        Bottom,
        Integer,
        Real,
        Sexp,
    };
    Representation() : t(Bottom) {}
    // cppcheck-suppress noExplicitConstructor
    Representation(Type t) : t(t) {}
    explicit Representation(jit_type_t jt) {
        if (jt == jit_type_void)
            t = Bottom;
        else if (jt == jit_type_int)
            t = Integer;
        else if (jt == jit_type_float64)
            t = Real;
        else if (jt == sxp)
            t = Sexp;
        else
            assert(false);
    }
    Type t;
    operator jit_type_t() {
        switch (t) {
        case Representation::Bottom:
            return jit_type_void;
        case Representation::Integer:
            return jit_type_int;
        case Representation::Real:
            return jit_type_float64;
        case Representation::Sexp:
            return sxp;
        }
        assert(false);
        return nullptr;
    }
    bool merge(const Representation& other) {
        if (t < other.t) {
            t = other.t;
            return true;
        }
        return false;
    }
    bool operator==(const Representation& other) const { return t == other.t; }
    bool operator!=(const Representation& other) const {
        return !(*this == other);
    }
};

std::ostream& operator<<(std::ostream& out, const Representation& r) {
    switch (r.t) {
    case Representation::Bottom:
        out << "Bottom";
        break;
    case Representation::Integer:
        out << "Integer";
        break;
    case Representation::Real:
        out << "Real";
        break;
    case Representation::Sexp:
        out << "Sexp";
        break;
    }
    return out;
};

class PirCodeFunction : public jit_function {
  public:
    ClosureVersion* cls;
    Code* code;
    bool success = false;

    CFG cfg;
    LivenessIntervals liveness;
    size_t numLocals = liveness.maxLive;

    PirCodeFunction(jit_context& context, ClosureVersion* cls, Code* code,
                    const std::unordered_map<Promise*, unsigned>& promMap,
                    const std::unordered_set<Instruction*>& needsEnsureNamed);

    void build() override;
    jit_value argument(int i);
    jit_value constant(SEXP c, jit_type_t);
    jit_value sexptype(jit_value v);
    void ensureNamed(jit_value v);
    void incrementNamed(jit_value v);
    void writeBarrier(jit_value x, jit_value y, std::function<void()> no,
                      std::function<void()> yes);
    jit_value isObj(jit_value v);
    jit_value car(jit_value x);
    jit_value cdr(jit_value x);
    jit_value tag(jit_value x);
    void setCar(jit_value x, jit_value y);

    void externalsxpSetEntry(jit_value x, int i, jit_value y);
    jit_value externalsxpGetEntry(jit_value x, int i);

    void envStubSet(jit_value x, int i, jit_value y);
    jit_value envStubGet(jit_value x, int i);

    jit_value call(const NativeBuiltin&, const std::vector<jit_value>&);

    void setVisible(int i);
    jit_value force(Instruction*, jit_value);
    jit_value depromise(jit_value);

    void checkMissing(jit_value);
    void checkUnbound(jit_value);

    void gcSafepoint(Instruction*, size_t required, bool protectArgs);

    void incStack(int i, bool zero);
    void decStack(int i);
    void setStackHeight(jit_value);
    jit_value stack(int i);
    void stack(int i, jit_value);
    void stack(const std::vector<jit_value>&);

    jit_value getLocal(size_t i);
    void setLocal(size_t i, jit_value);

    jit_value load(Instruction* pos, Value* v, PirType t, Representation);

    jit_value load(Instruction* pos, Value* v, Representation r) {
        return load(pos, v, v->type, r);
    }

    jit_value load(Instruction* pos, Value* v) {
        return load(pos, v, v->type, representationOf(v));
    }
    jit_value loadSxp(Instruction* pos, Value* v) {
        return load(pos, v, Representation::Sexp);
    }
    jit_value loadSame(Instruction* pos, Value* v) {
        return load(pos, v, representationOf(pos));
    }

    jit_value paramCode() { return get_param(0); }
    jit_value paramArgs() { return get_param(1); }
    jit_value paramEnv() { return get_param(2); }
    jit_value paramClosure() { return get_param(3); }

    void insn_assert(jit_value v, const char* msg) {
        auto ok = jit_label();
        insn_branch_if(v, ok);
        call(NativeBuiltins::assertFail, {new_constant((void*)msg)});
        insn_label(ok);
    }

    jit_value unboxIntLgl(jit_value v) {
#ifdef ENABLE_SLOWASSERT
        auto type = sexptype(v);
        auto isLgl = type == new_constant(LGLSXP);
        auto isInt = type == new_constant(INTSXP);
        auto match = insn_or(isLgl, isInt);
        insn_assert(match, "unboxIntLgl expected INTSXP or LGLSXP");
#endif
        return insn_load_relative(v, stdVecDtptrOfs, jit_type_int);
    }
    jit_value unboxInt(jit_value v) {
#ifdef ENABLE_SLOWASSERT
        auto type = sexptype(v);
        insn_assert(type == new_constant(INTSXP), "unboxInt expected INTSXP");
#endif
        return insn_load_relative(v, stdVecDtptrOfs, jit_type_int);
    }
    jit_value unboxLgl(jit_value v) {
#ifdef ENABLE_SLOWASSERT
        auto type = sexptype(v);
        insn_assert(type == new_constant(LGLSXP), "unboxLgl expected LGLSXP");
#endif
        return insn_load_relative(v, stdVecDtptrOfs, jit_type_int);
    }
    jit_value unboxReal(jit_value v) {
#ifdef ENABLE_SLOWASSERT
        auto type = sexptype(v);
        insn_assert(type == new_constant(REALSXP),
                    "unboxReal expected REALSXP");
#endif
        return insn_load_relative(v, stdVecDtptrOfs, jit_type_float64);
    }
    jit_value unboxRealIntLgl(jit_value v) {
        auto isReal = jit_label();
        auto done = jit_label();

        auto res = jit_value_create(raw(), jit_type_float64);

        auto type = sexptype(v);
        auto tt = insn_eq(type, new_constant(REALSXP));
        insn_branch_if(tt, isReal);

        auto isNa = jit_label();
        auto intres = unboxIntLgl(v);
        insn_branch_if(intres == new_constant(NA_INTEGER), isNa);

        store(res, intres);
        insn_branch(done);

        insn_label(isNa);
        store(res, new_constant(NAN));
        insn_branch(done);

        insn_label(isReal);
        store(res, unboxReal(v));
        insn_label(done);

        return res;
    }

    jit_value box(Instruction* pos, jit_value v, PirType t) {
        if (t.isA(PirType(RType::integer).notObject()))
            return boxInt(pos, v);
        if (t.isA(PirType(RType::logical).notObject()))
            return boxLgl(pos, v);
        if (t.isA(PirType(RType::real).notObject()))
            return boxReal(pos, v);
        assert(false);
        return nullptr;
    }
    jit_value boxInt(Instruction* pos, jit_value v) {
        gcSafepoint(pos, 1, true);
        if (v.type() == jit_type_int)
            return call(NativeBuiltins::newInt, {v});
        assert(v.type() == jit_type_float64);
        return call(NativeBuiltins::newIntFromReal, {v});
    }
    jit_value boxReal(Instruction* pos, jit_value v) {
        gcSafepoint(pos, 1, true);
        if (v.type() == jit_type_float64)
            return call(NativeBuiltins::newReal, {v});
        assert(v.type() == jit_type_int);
        return call(NativeBuiltins::newRealFromInt, {v});
    }
    jit_value boxLgl(Instruction* pos, jit_value v) {
        gcSafepoint(pos, 1, true);
        if (v.type() == jit_type_int)
            return call(NativeBuiltins::newLgl, {v});
        assert(v.type() == jit_type_float64);
        return call(NativeBuiltins::newLglFromReal, {v});
    }
    jit_value withCallFrame(Instruction* i, const std::vector<Value*>& args,
                            const std::function<jit_value()>&);

  protected:
    const std::unordered_map<Promise*, unsigned>& promMap;
    const std::unordered_set<Instruction*>& needsEnsureNamed;

    std::unordered_map<Value*, jit_value> valueMap;

    Representation representationOf(Value* v);
    Representation representationOf(PirType t);

    void setVal(Instruction* i, jit_value val) {
        assert(!valueMap.count(i));
        if (val.type() == sxp && representationOf(i) == jit_type_int)
            val = unboxIntLgl(val);
        if (val.type() == sxp && representationOf(i) == jit_type_float64)
            val = unboxRealIntLgl(val);
        if (i->producesRirResult() && representationOf(i) != val.type()) {
            jit_dump_function(stdout, raw(), "test");
            i->print(std::cout);
            std::cout << "\nWanted a " << representationOf(i) << " but got a "
                      << Representation(val.type()) << "\n";
            std::cout << "\n";
            assert(false);
        }
        valueMap[i] = val;
    }

    jit_value cp;
    jit_value basepointer;
    jit_value nodestackPtrPtr;
    jit_value nodestackPtr() {
        return insn_load_relative(nodestackPtrPtr, 0, jit_type_void_ptr);
    }

    jit_type_t create_signature() override {
        return signature_helper(jit_type_void_ptr, jit_type_void_ptr, sxp,
                                jit_type_void_ptr, jit_type_void_ptr,
                                end_params);
    }
};

void PirCodeFunction::incStack(int i, bool zero) {
    if (i == 0)
        return;
    auto cur = insn_load_relative(nodestackPtrPtr, 0, jit_type_nuint);
    auto offset = new_constant(sizeof(R_bcstack_t) * i);
    if (zero)
        insn_memset(cur, new_constant(0), offset);
    auto up = insn_add(cur, offset);
    insn_store_relative(nodestackPtrPtr, 0, up);
}

void PirCodeFunction::decStack(int i) {
    if (i == 0)
        return;
    auto cur = insn_load_relative(nodestackPtrPtr, 0, jit_type_nuint);
    auto up = insn_sub(cur, new_constant(sizeof(R_bcstack_t) * i));
    insn_store_relative(nodestackPtrPtr, 0, up);
}

void PirCodeFunction::setStackHeight(jit_value pos) {
    insn_store_relative(nodestackPtrPtr, 0, pos);
}

void PirCodeFunction::setLocal(size_t i, jit_value v) {
    assert(i < numLocals);
    assert(v.type() == sxp);
    auto offset = i * sizeof(R_bcstack_t) + stackCellValueOfs;
    insn_store_relative(basepointer, offset, v);
}

jit_value PirCodeFunction::getLocal(size_t i) {
    assert(i < numLocals);
    auto offset = i * sizeof(R_bcstack_t);
    offset += stackCellValueOfs;
    return insn_load_relative(basepointer, offset, sxp);
}

jit_value PirCodeFunction::stack(int i) {
    auto offset = -(i + 1) * sizeof(R_bcstack_t);
    offset += stackCellValueOfs;
    return insn_load_relative(nodestackPtr(), offset, sxp);
}

void PirCodeFunction::stack(const std::vector<jit_value>& args) {
    auto offset = -args.size() * sizeof(R_bcstack_t);
    auto stackptr = nodestackPtr();
    for (auto& arg : args) {
        // set type tag to 0
        insn_store_relative(stackptr, offset, new_constant(0));
        offset += stackCellValueOfs;
        // store the value
        insn_store_relative(stackptr, offset, arg);
        offset += sizeof(R_bcstack_t) - stackCellValueOfs;
    }
}

void PirCodeFunction::stack(int i, jit_value v) {
    assert(v.type() == sxp);
    auto offset = -(i + 1) * sizeof(R_bcstack_t);
    auto stackptr = nodestackPtr();
    // set type tag to 0
    insn_store_relative(stackptr, offset, new_constant(0));
    offset += stackCellValueOfs;
    // store the value
    insn_store_relative(stackptr, offset, v);
}

jit_value PirCodeFunction::load(Instruction* pos, Value* val, PirType type,
                                Representation needed) {

    jit_value res;

    if (auto cast = CastType::Cast(val)) {
        auto arg = cast->arg(0).val();
        return load(pos, arg, cast->type, needed);
    }

    if (valueMap.count(val))
        res = valueMap.at(val);
    else if (val == Env::elided())
        res = constant(R_NilValue, needed);
    else if (auto e = Env::Cast(val)) {
        if (e == Env::notClosed()) {
            res = tag(paramClosure());
        } else if (e == Env::nil()) {
            res = constant(R_NilValue, needed);
        } else if (Env::isStaticEnv(e)) {
            res = constant(e->rho, sxp);
        } else {
            assert(false);
        }
    } else if (val == True::instance())
        res = constant(R_TrueValue, needed);
    else if (val == False::instance())
        res = constant(R_FalseValue, needed);
    else if (val == MissingArg::instance())
        res = constant(R_MissingArg, sxp);
    else if (val == UnboundValue::instance())
        res = constant(R_UnboundValue, sxp);
    else if (auto ld = LdConst::Cast(val))
        res = constant(ld->c(), needed);
    else if (val == NaLogical::instance())
        res = constant(R_LogicalNAValue, needed);
    else if (val == Nil::instance())
        res = constant(R_NilValue, needed);
    else {
        val->printRef(std::cerr);
        assert(false);
    }

    if (res.type() == sxp && needed != sxp) {
        if (type.isA(PirType(RType::integer).scalar().notObject())) {
            res = unboxInt(res);
        } else if (type.isA((PirType() | RType::integer | RType::logical)
                                .scalar()
                                .notObject())) {
            res = unboxIntLgl(res);
            assert(res.type() == jit_type_int);
        } else if (type.isA(PirType(RType::real).scalar().notObject())) {
            res = unboxReal(res);
            assert(res.type() == jit_type_float64);
        } else if (type.isA(
                       (PirType(RType::real) | RType::integer | RType::logical)
                           .scalar()
                           .notObject())) {
            res = unboxRealIntLgl(res);
            assert(res.type() == jit_type_float64);
        } else {
            code->printCode(std::cout, true, true);
            std::cout << "Don't know how to unbox a " << type << "\n";
            pos->print(std::cout);
            std::cout << "\n";
            val->printRef(std::cout);
            std::cout << "\n";
            assert(false);
        }
        // fall through, since more conversions might be needed after unboxing
    }

    if (res.type() == jit_type_int && needed == jit_type_float64) {
        // TODO should we deal with na here?
        res = insn_convert(res, jit_type_float64, false);
    } else if (res.type() == jit_type_float64 && needed == jit_type_int) {
        // TODO should we deal with na here?
        res = insn_convert(res, jit_type_int, false);
    } else if ((res.type() == jit_type_int || res.type() == jit_type_float64) &&
               needed == sxp) {
        if (type.isA(PirType() | RType::integer))
            res = boxInt(pos, res);
        else if (type.isA(PirType() | RType::logical))
            res = boxLgl(pos, res);
        else if (type.isA(NativeType::test))
            res = boxLgl(pos, res);
        else if (type.isA(RType::real))
            res = boxReal(pos, res);
        else {
            std::cout << "Failed to convert int/float to " << type << "\n";
            pos->print(std::cout);
            std::cout << "\n";
            Instruction::Cast(val)->print(std::cout);
            std::cout << "\n";
            code->printCode(std::cout, true, true);
            assert(false);
        }
    }

    if (res.type() != needed) {
        std::cout << "Failed to load ";
        if (auto i = Instruction::Cast(val))
            i->print(std::cout, true);
        else
            val->printRef(std::cout);
        std::cout << " for the instruction ";
        pos->print(std::cout, true);
        std::cout << " in the representation " << needed << "\n";
        assert(false);
    }

    return res;
}

extern "C" size_t R_NSize;
extern "C" size_t R_NodesInUse;

void PirCodeFunction::gcSafepoint(Instruction* i, size_t required,
                                  bool protectArgs) {
    auto ok = jit_label();

    if (required != (size_t)-1) {
        auto use_ptr = new_constant(&R_NodesInUse);
        auto size_ptr = new_constant(&R_NSize);
        auto use = insn_load_relative(use_ptr, 0, jit_type_ulong);
        auto size = insn_load_relative(size_ptr, 0, jit_type_ulong);
        auto req = insn_add(use, new_constant(required));
        auto t = insn_lt(req, size);
        insn_branch_if(t, ok);
    }

    // Store every live variable into a local variable slot from R
    size_t pos = 0;
    for (auto& v : valueMap) {
        auto test = Instruction::Cast(v.first);
        if (!test)
            continue;

        bool isArg = false;
        if (protectArgs) {
            i->eachArg([&](Value* a) { isArg = isArg || a == test; });
        }

        if (i != test && (isArg || liveness.live(i, v.first))) {
            if (v.second.type() == sxp)
                setLocal(pos++, v.second);
        }
    }

    insn_label(ok);
}

jit_value PirCodeFunction::isObj(jit_value v) {
    auto sxpinfo = insn_load_relative(v, sxpinfofOfs, jit_type_ulong);
    return insn_ne(
        new_constant(0),
        insn_and(sxpinfo,
                 new_constant((unsigned long)(1ul << (TYPE_BITS + 1)))));
};

jit_value PirCodeFunction::sexptype(jit_value v) {
    auto sxpinfo = insn_load_relative(v, sxpinfofOfs, jit_type_ulong);
    return insn_and(sxpinfo, new_constant(MAX_NUM_SEXPTYPE - 1));
};

void PirCodeFunction::ensureNamed(jit_value v) {
    auto sxpinfo = insn_load_relative(v, sxpinfofOfs, jit_type_ulong);
    // lsb of named count is the 33th bit
    auto named = insn_or(sxpinfo, new_constant(0x100000000ul));
    auto isNamed = jit_label();
    insn_branch_if(named == sxpinfo, isNamed);
    insn_store_relative(v, sxpinfofOfs, named);
    insn_label(isNamed);
};

void PirCodeFunction::incrementNamed(jit_value v) {
    auto sxpinfo = insn_load_relative(v, sxpinfofOfs, jit_type_ulong);
    // named count is NAMED_BITS, starting at the 33th bit
    static auto namedMask = ((unsigned long)pow(2, NAMED_BITS) - 1) << 32;
    static auto namedNegMask = ~namedMask;

    auto isNamedMax = jit_label();

    auto named = insn_and(sxpinfo, new_constant(namedMask));
    named = insn_shr(named, new_constant(32));
    insn_branch_if(named == new_constant(NAMEDMAX), isNamedMax);

    auto newNamed = insn_add(named, new_constant(1));
    newNamed = insn_shl(newNamed, new_constant(32));
    auto newSxpinfo = insn_and(sxpinfo, new_constant(namedNegMask));
    newSxpinfo = insn_or(newSxpinfo, newNamed);
    insn_store_relative(v, sxpinfofOfs, newSxpinfo);

    insn_label(isNamedMax);
};

void PirCodeFunction::writeBarrier(jit_value x, jit_value y,
                                   std::function<void()> no,
                                   std::function<void()> yes) {
    auto sxpinfoX = insn_load_relative(x, sxpinfofOfs, jit_type_ulong);

    auto markBitPos = new_constant((unsigned long)(1ul << (TYPE_BITS + 19)));
    auto genBitPos = new_constant((unsigned long)(1ul << (TYPE_BITS + 23)));

    auto done = jit_label();
    auto noBarrier = jit_label();
    auto maybeNeedsBarrier = jit_label();
    auto needsBarrier = jit_label();

    auto markBitX = insn_and(sxpinfoX, markBitPos);
    insn_branch_if(markBitX, maybeNeedsBarrier);
    insn_branch(noBarrier);

    // TODO: it seems with the right bit fiddling we could do this with just
    // one branch. Also, maybe we should share this code between write barriers?
    insn_label(maybeNeedsBarrier);
    auto sxpinfoY = insn_load_relative(y, sxpinfofOfs, jit_type_ulong);
    auto markBitY = insn_and(sxpinfoY, markBitPos);
    insn_branch_if_not(markBitY, needsBarrier);

    auto genBitX = insn_and(sxpinfoX, genBitPos);
    auto genBitY = insn_and(sxpinfoY, genBitPos);
    auto olderGen = insn_gt(genBitX, genBitY);
    insn_branch_if(olderGen, needsBarrier);

    insn_label(noBarrier);
    no();
    insn_branch(done);

    insn_label(needsBarrier);
    yes();

    insn_label(done);
};

void PirCodeFunction::setCar(jit_value x, jit_value y) {
    writeBarrier(x, y, [&]() { insn_store_relative(x, carOfs_, y); },
                 [&]() {
                     call(NativeBuiltins::setCar, {{x, y}});
                 });
}

void PirCodeFunction::externalsxpSetEntry(jit_value x, int i, jit_value y) {
#ifdef ENABLE_SLOWASSERT
    auto tp = sexptype(x);
    insn_assert(tp == new_constant(EXTERNALSXP),
                "externalsxpGetEntry on something which is not an EXTERNALSXP");
#endif
    writeBarrier(x, y,
                 [&]() {
                     auto ofset =
                         insn_load_relative(x, dataptrOfs, jit_type_uint);
                     auto pos = new_constant(dataptrOfs) + ofset +
                                new_constant(i * sizeof(SEXP));
                     insn_store_relative(x + pos, 0, y);
                 },
                 [&]() {
                     call(NativeBuiltins::externalsxpSetEntry,
                          {{x, new_constant(i), y}});
                 });
}

jit_value PirCodeFunction::externalsxpGetEntry(jit_value x, int i) {
#ifdef ENABLE_SLOWASSERT
    auto tp = sexptype(x);
    insn_assert(tp == new_constant(EXTERNALSXP),
                "externalsxpGetEntry on something which is not an EXTERNALSXP");
#endif
    auto ofset = insn_load_relative(x, dataptrOfs, jit_type_uint);
    auto pos =
        new_constant(dataptrOfs) + ofset + new_constant(i * sizeof(SEXP));
    return insn_load_relative(x + pos, 0, sxp);
}

jit_value PirCodeFunction::envStubGet(jit_value x, int i) {
    // We could use externalsxpGetEntry, but this is faster
#ifdef ENABLE_SLOWASSERT
    auto magic = insn_load_relative(x, extsxpMagicOfs, jit_type_uint);
    insn_assert(magic == new_constant(LAZY_ENVIRONMENT_MAGIC),
                "envStubGet on something which is not an env stub");
#endif
    auto offset = dataptrOfs + sizeof(LazyEnvironment) +
                  sizeof(SEXP) * LazyEnvironment::ArgOffset;
    offset += i * sizeof(SEXP);
    return insn_load_relative(x, offset, sxp);
}

void PirCodeFunction::envStubSet(jit_value x, int i, jit_value y) {
    // We could use externalsxpSetEntry, but this is faster
    writeBarrier(
        x, y,
        [&]() {
#ifdef ENABLE_SLOWASSERT
            auto magic = insn_load_relative(x, extsxpMagicOfs, jit_type_uint);
            insn_assert(magic == new_constant(LAZY_ENVIRONMENT_MAGIC),
                        "envStubSet on something which is not an env stub");
#endif
            auto offset = dataptrOfs + sizeof(LazyEnvironment) +
                          sizeof(SEXP) * LazyEnvironment::ArgOffset;
            offset += i * sizeof(SEXP);
            insn_store_relative(x, offset, y);
        },
        [&]() {
            call(NativeBuiltins::externalsxpSetEntry,
                 {{x, new_constant(i + LazyEnvironment::ArgOffset), y}});
        });
}

jit_value PirCodeFunction::car(jit_value x) {
    return insn_load_relative(x, carOfs_, sxp);
}
jit_value PirCodeFunction::cdr(jit_value x) {
    return insn_load_relative(x, cdrOfs_, sxp);
}
jit_value PirCodeFunction::tag(jit_value x) {
    return insn_load_relative(x, tagOfs_, sxp);
}

jit_value PirCodeFunction::constant(SEXP c, jit_type_t needed) {
    static std::unordered_set<SEXP> eternal = {R_TrueValue,  R_NilValue,
                                               R_FalseValue, R_UnboundValue,
                                               R_MissingArg, R_GlobalEnv};
    if (eternal.count(c) && needed == sxp) {
        return new_constant(c);
    }

    if (needed == jit_type_int) {
        assert(Rf_length(c) == 1);
        if (TYPEOF(c) == INTSXP)
            return new_constant(INTEGER(c)[0], jit_type_int);
        if (TYPEOF(c) == REALSXP) {
            return new_constant((int)REAL(c)[0], jit_type_int);
        }
        if (TYPEOF(c) == LGLSXP)
            return new_constant(LOGICAL(c)[0]);
    }

    if (needed == jit_type_float64) {
        assert(Rf_length(c) == 1);
        if (TYPEOF(c) == INTSXP)
            return new_constant((double)INTEGER(c)[0], jit_type_float64);
        if (TYPEOF(c) == REALSXP)
            return new_constant(REAL(c)[0], jit_type_float64);
    }

    assert(needed == sxp);

    auto i = Pool::insert(c);
    return insn_load_elem(cp, new_constant(i), sxp);
}

jit_value PirCodeFunction::argument(int i) {
    i *= sizeof(R_bcstack_t);
    i += stackCellValueOfs;
    auto res = insn_load_relative(paramArgs(), i, sxp);
    return res;
}

jit_value PirCodeFunction::call(const NativeBuiltin& builtin,
                                const std::vector<jit_value>& args_) {
    assert(args_.size() == builtin.nargs);
    std::vector<jit_value_t> args(args_.size());
    size_t i = 0;
    for (auto& a : args_)
        args[i++] = a.raw();

    return insn_call_native(builtin.name, builtin.fun, builtin.signature,
                            args.data(), builtin.nargs, 0);
}

void PirCodeFunction::setVisible(int i) {
    insn_store_relative(new_constant(&R_Visible), 0, new_constant(i));
}

jit_value PirCodeFunction::force(Instruction* i, jit_value arg) {
    auto ok = jit_label();
    auto type = sexptype(arg);
    auto tt = insn_eq(type, new_constant(PROMSXP));

    auto res = insn_dup(arg);
    insn_branch_if_not(tt, ok);

    auto val = insn_load_relative(arg, prValueOfs, sxp);
    store(res, val);
    auto tv = insn_eq(val, constant(R_UnboundValue, sxp));
    insn_branch_if_not(tv, ok);

    gcSafepoint(i, -1, false);
    auto evaled = call(NativeBuiltins::forcePromise, {arg});
    store(res, evaled);

    insn_label(ok);
    return res;
}

jit_value PirCodeFunction::depromise(jit_value arg) {
    if (arg.type() != sxp)
        return arg;

    auto ok = jit_label();
    auto type = sexptype(arg);
    auto tt = insn_eq(type, new_constant(PROMSXP));

    auto res = insn_dup(arg);
    insn_branch_if_not(tt, ok);

    auto val = insn_load_relative(arg, prValueOfs, sxp);
    store(res, val);

    insn_label(ok);
    return res;
}

void PirCodeFunction::checkMissing(jit_value v) {
    auto ok = jit_label();
    auto t = insn_eq(v, constant(R_MissingArg, sxp));
    insn_branch_if_not(t, ok);
    call(NativeBuiltins::error, {});
    insn_label(ok);
}

void PirCodeFunction::checkUnbound(jit_value v) {
    auto ok = jit_label();
    auto t = insn_eq(v, constant(R_UnboundValue, sxp));
    insn_branch_if_not(t, ok);
    call(NativeBuiltins::error, {});
    insn_label(ok);
}

jit_value
PirCodeFunction::withCallFrame(Instruction* i, const std::vector<Value*>& args,
                               const std::function<jit_value()>& theCall) {
    gcSafepoint(i, -1, false);
    auto nargs = args.size();
    incStack(nargs, false);
    std::vector<jit_value> jitArgs;
    for (auto& arg : args)
        jitArgs.push_back(load(i, arg, Representation::Sexp));
    stack(jitArgs);
    auto res = theCall();
    decStack(nargs);
    return res;
}

Representation PirCodeFunction::representationOf(Value* v) {
    return representationOf(v->type);
}

Representation PirCodeFunction::representationOf(PirType t) {
    // Combined types like integer|real cannot be unbox, since we do not know
    // how to re-box again.
    if (t.isA(NativeType::test))
        return Representation::Integer;
    if (t.isA(PirType(RType::logical).scalar().notObject()))
        return Representation::Integer;
    if (t.isA(PirType(RType::integer).scalar().notObject()))
        return Representation::Integer;
    if (t.isA(PirType(RType::real).scalar().notObject()))
        return Representation::Real;
    return Representation::Sexp;
}

void dummy() {}

PirCodeFunction::PirCodeFunction(
    jit_context& context, ClosureVersion* cls, Code* code,
    const std::unordered_map<Promise*, unsigned>& promMap,
    const std::unordered_set<Instruction*>& needsEnsureNamed)
    : jit_function(context), cls(cls), code(code), cfg(code),
      liveness(code->nextBBId, cfg), promMap(promMap),
      needsEnsureNamed(needsEnsureNamed) {
    create();

    auto cp_ = insn_load_relative(new_constant(globalContext()), cpOfs, sxp);
    cp = insn_add(cp_, new_constant(stdVecDtptrOfs));
    nodestackPtrPtr = new_constant(&R_BCNodeStackTop);
};

void PirCodeFunction::build() {
    success = true;

#if 0
    code->printCode(std::cout, true, false);
    for (auto& r : representation) {
        r.first->printRef(std::cout);
        std::cout << " = " << r.second << "\n";
    }
#endif

    basepointer = nodestackPtr();
    if (numLocals > 0)
        incStack(numLocals, true);

    std::unordered_map<BB*, jit_label> blockLabel;
    std::unordered_map<Instruction*, jit_value> phis;
    Visitor::run(code->entry, [&](BB* bb) {
        blockLabel[bb] = jit_label();
        for (auto i : *bb) {
            if (auto phi = Phi::Cast(i)) {
                auto val = jit_value_create(raw(), representationOf(i));
                phis[i] = val;
                phi->eachArg([&](BB*, Value* v) {
                    auto i = Instruction::Cast(v);
                    assert(i);
                    phis[i] = val;
                });
            }
        }
    });

    auto compileRelop = [&](
        Instruction* i, std::function<jit_value(jit_value, jit_value)> insert,
        BinopKind kind) {

        auto rep = representationOf(i);
        auto lhs = i->arg(0).val();
        auto rhs = i->arg(1).val();
        auto lhsRep = representationOf(lhs);
        auto rhsRep = representationOf(rhs);
        if (lhsRep == Representation::Sexp || rhsRep == Representation::Sexp) {
            auto a = loadSxp(i, lhs);
            auto b = loadSxp(i, rhs);

            jit_value res;
            gcSafepoint(i, -1, true);
            if (i->hasEnv()) {
                success = false;
                auto e = loadSxp(i, i->env());
                res = call(NativeBuiltins::binopEnv,
                           {a, b, e, new_constant(i->srcIdx),
                            new_constant((int)kind)});
            } else {
                res = call(NativeBuiltins::binop,
                           {a, b, new_constant((int)kind)});
            }
            if (rep == Representation::Integer)
                setVal(i, unboxIntLgl(res));
            else
                setVal(i, res);
            return;
        }

        jit_label done;
        jit_label isNa;

        auto checkNa = [&](jit_value v, Representation r) {
            if (r == Representation::Integer) {
                auto aIsNa = insn_eq(v, new_constant(NA_INTEGER));
                insn_branch_if(aIsNa, isNa);
            } else if (r == Representation::Real) {
                auto aIsNa = insn_ne(v, v);
                insn_branch_if(aIsNa, isNa);
            } else {
                assert(false);
            }
        };

        auto res = jit_value_create(raw(), jit_type_int);
        auto a = load(i, lhs, lhsRep);
        auto b = load(i, rhs, rhsRep);

        checkNa(a, lhsRep);
        checkNa(b, rhsRep);

        auto r = insert(a, b);
        store(res, r);
        insn_branch(done);

        insn_label(isNa);
        // TODO: fix bug when res is double
        store(res, new_constant(NA_INTEGER));

        insn_label(done);

        if (rep == Representation::Sexp)
            setVal(i, boxLgl(i, res));
        else
            setVal(i, res);
    };

    auto compileBinop = [&](
        Instruction* i, std::function<jit_value(jit_value, jit_value)> insert,
        BinopKind kind) {

        auto rep = representationOf(i);
        auto lhs = i->arg(0).val();
        auto rhs = i->arg(1).val();
        auto lhsRep = representationOf(lhs);
        auto rhsRep = representationOf(rhs);

        if (lhsRep == Representation::Sexp || rhsRep == Representation::Sexp) {
            auto a = loadSxp(i, i->arg(0).val());
            auto b = loadSxp(i, i->arg(1).val());

            jit_value res;
            gcSafepoint(i, -1, true);
            if (i->hasEnv()) {
                auto e = loadSxp(i, i->env());
                res = call(NativeBuiltins::binopEnv,
                           {a, b, e, new_constant(i->srcIdx),
                            new_constant((int)kind)});
            } else {
                res = call(NativeBuiltins::binop,
                           {a, b, new_constant((int)kind)});
            }

            if (rep == Representation::Integer)
                setVal(i, unboxIntLgl(res));
            else if (rep == Representation::Real)
                setVal(i, unboxRealIntLgl(res));
            else
                setVal(i, res);
            return;
        }

        jit_label done, isNa;
        auto res = jit_value_create(
            raw(),
            (lhsRep == Representation::Real || rhsRep == Representation::Real)
                ? jit_type_float64
                : jit_type_int);

        auto a = load(i, lhs, lhsRep);
        auto b = load(i, rhs, rhsRep);

        auto checkNa = [&](jit_value v, Representation r) {
            if (r == Representation::Integer) {
                auto aIsNa = insn_eq(v, new_constant(NA_INTEGER));
                insn_branch_if(aIsNa, isNa);
            }
        };
        checkNa(a, lhsRep);
        checkNa(b, rhsRep);

        store(res, insert(a, b));

        if (lhsRep == Representation::Integer ||
            rhsRep == Representation::Integer) {
            insn_branch(done);

            insn_label(isNa);
            store(res, new_constant(NA_INTEGER));

            insn_label(done);
        }

        if (rep == Representation::Sexp) {
            setVal(i, box(i, res, lhs->type.mergeWithConversion(rhs->type)));
        } else {
            setVal(i, res);
        }
    };

    std::unordered_map<Value*, std::unordered_map<SEXP, size_t>> bindingsCache;
    jit_value bindingsCacheBase;
    {
        SmallSet<std::pair<Value*, SEXP>> bindings;
        Visitor::run(code->entry, [&](Instruction* i) {
            SEXP varName = nullptr;
            if (auto l = LdVar::Cast(i))
                varName = l->varName;
            else if (auto l = StVar::Cast(i))
                varName = l->varName;

            if (varName) {
                auto e = MkEnv::Cast(i->env());
                if (e && !e->stub) {
                    bindings.insert(std::pair<Value*, SEXP>(i->env(), varName));
                }
            }
        });
        size_t idx = 0;
        for (auto& b : bindings) {
            bindingsCache[b.first][b.second] = idx * sizeof(SEXPREC);
            idx++;
        }
        auto size = new_constant(idx * sizeof(SEXPREC));
        bindingsCacheBase = insn_alloca(size);
    }

    LoweringVisitor::run(code->entry, [&](BB* bb) {
        insn_label(blockLabel.at(bb));

        for (auto it = bb->begin(); it != bb->end(); ++it) {
            auto i = *it;
            if (!success)
                return;

#if 0
        static std::vector<std::string> instrs;
        // Enable to emit all pir instructions as strings into the final code
        std::stringstream str;
        i->print(str, false);
        instrs.push_back(str.str());
        insn_call_native(instrs.back().c_str(), (void*)&dummy,
                         jit_type_create_signature(jit_abi_cdecl,
                                                   jit_type_void, {}, 0,
                                                   0), {}, 0, 0);
#endif

            switch (i->tag) {
            case Tag::PirCopy: {
                auto c = PirCopy::Cast(i);
                auto in = c->arg<0>().val();
                if (Phi::Cast(in))
                    setVal(i, loadSame(i, in));
                break;
            }

            case Tag::AsLogical: {
                auto arg = i->arg(0).val();

                auto r1 = representationOf(arg);
                auto r2 = representationOf(i);

                assert(r2 == Representation::Integer);

                jit_value res;
                if (r1 == Representation::Sexp) {
                    res = call(NativeBuiltins::asLogicalBlt, {loadSxp(i, arg)});
                } else if (r1 == Representation::Real) {
                    res = insn_dup(load(i, arg, Representation::Integer));

                    auto narg = load(i, arg, Representation::Real);
                    jit_label noNa;
                    auto notNa = insn_eq(narg, narg);
                    insn_branch_if(notNa, noNa);

                    store(res, new_constant(NA_INTEGER));

                    insn_label(noNa);
                } else {
                    assert(r1 == Representation::Integer);
                    res = load(i, arg, Representation::Integer);
                }

                setVal(i, res);
                break;
            }

            case Tag::CastType: {
                // Scheduled on use
                break;
            }

            case Tag::Missing: {
                assert(representationOf(i) == Representation::Integer);
                auto missing = Missing::Cast(i);
                setVal(i, call(NativeBuiltins::isMissing,
                               {constant(missing->varName, sxp),
                                loadSxp(i, i->env())}));
                break;
            }

            case Tag::ChkMissing: {
                auto arg = i->arg(0).val();
                if (representationOf(arg) == Representation::Sexp)
                    checkMissing(loadSxp(i, arg));
                setVal(i, loadSame(i, arg));
                break;
            }

            case Tag::IsType: {
                if (representationOf(i) != Representation::Integer) {
                    success = false;
                    break;
                }

                auto t = IsType::Cast(i);
                auto arg = i->arg(0).val();
                if (representationOf(arg) == Representation::Sexp) {
                    jit_value res;
                    auto a = loadSxp(i, arg);
                    if (t->typeTest.isA(RType::integer)) {
                        res = insn_eq(sexptype(a), new_constant(INTSXP));
                    } else if (t->typeTest.isA(PirType(RType::integer)
                                                   .orPromiseWrapped())) {
                        a = depromise(a);
                        res = insn_eq(sexptype(a), new_constant(INTSXP));
                    } else if (t->typeTest.isA(RType::real)) {
                        res = insn_eq(sexptype(a), new_constant(REALSXP));
                    } else if (t->typeTest.isA(
                                   PirType(RType::real).orPromiseWrapped())) {
                        a = depromise(a);
                        res = insn_eq(sexptype(a), new_constant(REALSXP));
                    } else {
                        t->print(std::cerr, true);
                        assert(false &&
                               "Unsupported IsType check in native backend");
                    }
                    if (t->typeTest.isScalar()) {
                        res = insn_and(
                            res, insn_eq(call(NativeBuiltins::length, {a}),
                                         new_constant(1)));
                    }
                    setVal(i, insn_ne(res, new_constant(0)));
                } else {
                    setVal(i, new_constant((int)1));
                }
                break;
            }

            case Tag::Is: {
                assert(representationOf(i) == Representation::Integer);
                auto is = Is::Cast(i);
                auto arg = i->arg(0).val();
                if (representationOf(arg) == Representation::Sexp) {
                    auto argNative = loadSxp(i, arg);
                    auto expectedTypeNative = new_constant(is->sexpTag);
                    jit_value res;
                    auto typeNative = sexptype(argNative);
                    switch (is->sexpTag) {
                    case NILSXP:
                    case LGLSXP:
                    case REALSXP:
                        res = typeNative == expectedTypeNative;
                        break;

                    case VECSXP: {
                        auto operandLhs = typeNative == new_constant(VECSXP);
                        auto operandRhs = typeNative == new_constant(LISTSXP);
                        res = insn_or(operandLhs, operandRhs);
                        break;
                    }

                    case LISTSXP: {
                        auto operandLhs = typeNative == new_constant(LISTSXP);
                        auto operandRhs = typeNative == new_constant(NILSXP);
                        res = insn_or(operandLhs, operandRhs);
                        break;
                    }

                    default:
                        assert(false);
                        success = false;
                        break;
                    }

                    setVal(i, res);

                } else {
                    // How do we implement the fast path? Because in native
                    // representations we may have lost the real representation
                    success = false;
                }
                break;
            }

            case Tag::IsObject: {
                assert(representationOf(i) == Representation::Integer);
                auto arg = i->arg(0).val();
                if (representationOf(arg) == Representation::Sexp)
                    setVal(i, isObj(loadSxp(i, arg)));
                else
                    setVal(i, new_constant((int)0));
                break;
            }

            case Tag::AsTest: {
                assert(representationOf(i) == Representation::Integer);

                auto arg = i->arg(0).val();
                if (auto lgl = AsLogical::Cast(arg))
                    arg = lgl->arg(0).val();

                if (representationOf(arg) == Representation::Sexp) {
                    gcSafepoint(i, -1, true);
                    setVal(i, call(NativeBuiltins::asTest, {loadSxp(i, arg)}));
                    break;
                }

                auto r = representationOf(arg);

                jit_label notNa;
                if (r == Representation::Real) {
                    auto narg = load(i, arg, r);
                    auto isNotNa = insn_eq(narg, narg);
                    narg = insn_convert(narg, jit_type_int, false);
                    setVal(i, narg);
                    insn_branch_if(isNotNa, notNa);
                } else {
                    auto narg = load(i, arg, Representation::Integer);
                    auto isNotNa = insn_ne(narg, new_constant(NA_INTEGER));
                    setVal(i, narg);
                    insn_branch_if(isNotNa, notNa);
                }

                call(NativeBuiltins::error, {});

                insn_label(notNa);
                break;
            }

            case Tag::Neq:
                compileRelop(
                    i, [&](jit_value a, jit_value b) { return insn_ne(a, b); },
                    BinopKind::NE);
                break;

            case Tag::Not: {
                auto resultRep = representationOf(i);
                auto argument = i->arg(0).val();
                auto argumentRep = representationOf(argument);
                if (argumentRep == Representation::Sexp) {
                    auto argumentNative = loadSxp(i, argument);

                    jit_value res;
                    gcSafepoint(i, -1, true);
                    if (i->hasEnv()) {
                        res = call(NativeBuiltins::notEnv,
                                   {argumentNative, loadSxp(i, i->env()),
                                    new_constant(i->srcIdx)});
                    } else {
                        res = call(NativeBuiltins::notOp, {argumentNative});
                    }
                    if (resultRep == Representation::Integer)
                        setVal(i, unboxIntLgl(res));
                    else
                        setVal(i, res);
                    break;
                }

                jit_label done;
                jit_label isNa;

                auto checkNa = [&](jit_value v, Representation r) {
                    if (r == Representation::Integer) {
                        auto aIsNa = insn_eq(v, new_constant(NA_INTEGER));
                        insn_branch_if(aIsNa, isNa);
                    } else if (r == Representation::Real) {
                        auto aIsNa = insn_ne(v, v);
                        insn_branch_if(aIsNa, isNa);
                    } else {
                        assert(false);
                    }
                };

                auto res = jit_value_create(raw(), jit_type_int);
                auto argumentNative = load(i, argument, argumentRep);

                checkNa(argumentNative, argumentRep);

                store(res, insn_to_not_bool(argumentNative));
                insn_branch(done);

                insn_label(isNa);
                // Maybe we need to model R_LogicalNAValue?
                store(res, new_constant(NA_INTEGER));

                insn_label(done);

                if (resultRep == Representation::Sexp)
                    setVal(i, boxLgl(i, res));
                else
                    setVal(i, res);
                break;
            }
            case Tag::Eq:
                compileRelop(
                    i, [&](jit_value a, jit_value b) { return insn_eq(a, b); },
                    BinopKind::EQ);
                break;

            case Tag::Gt:
                compileRelop(
                    i, [&](jit_value a, jit_value b) { return insn_gt(a, b); },
                    BinopKind::GT);
                break;

            case Tag::Gte:
                compileRelop(
                    i, [&](jit_value a, jit_value b) { return insn_ge(a, b); },
                    BinopKind::GTE);
                break;

            case Tag::Lt:
                compileRelop(
                    i, [&](jit_value a, jit_value b) { return insn_lt(a, b); },
                    BinopKind::LT);
                break;

            case Tag::Lte:
                compileRelop(
                    i, [&](jit_value a, jit_value b) { return insn_le(a, b); },
                    BinopKind::LTE);
                break;

            case Tag::LOr:
                compileRelop(
                    i, [&](jit_value a, jit_value b) { return insn_or(a, b); },
                    BinopKind::LOR);
                break;

            case Tag::LAnd:
                compileRelop(
                    i, [&](jit_value a, jit_value b) { return insn_and(a, b); },
                    BinopKind::LAND);
                break;

            case Tag::Add:
                compileBinop(i, [&](jit_value a, jit_value b) { return a + b; },
                             BinopKind::ADD);
                break;
            case Tag::Sub:
                compileBinop(i, [&](jit_value a, jit_value b) { return a - b; },
                             BinopKind::SUB);
                break;
            case Tag::Mul:
                compileBinop(i, [&](jit_value a, jit_value b) { return a * b; },
                             BinopKind::MUL);
                break;
            case Tag::Div:
                compileBinop(
                    i, [&](jit_value a, jit_value b) { return insn_div(a, b); },
                    BinopKind::DIV);
                break;

            case Tag::ScheduledDeopt: {
                // TODO, this is copied from pir_2_rir... rather ugly
                DeoptMetadata* m = nullptr;
                {
                    auto deopt = ScheduledDeopt::Cast(i);
                    size_t nframes = deopt->frames.size();
                    SEXP store = Rf_allocVector(
                        RAWSXP,
                        sizeof(DeoptMetadata) + nframes * sizeof(FrameInfo));
                    m = new (DATAPTR(store)) DeoptMetadata;
                    m->numFrames = nframes;

                    size_t i = 0;
                    // Frames in the ScheduledDeopt are in pir argument order
                    // (from left to right). On the other hand frames in the rir
                    // deopt_ instruction are in stack order, from tos down.
                    for (auto fi = deopt->frames.rbegin();
                         fi != deopt->frames.rend(); fi++)
                        m->frames[i++] = *fi;
                    Pool::insert(store);
                }

                std::vector<Value*> args;
                i->eachArg([&](Value* v) { args.push_back(v); });
                withCallFrame(i, args, [&]() -> jit_value {
                    return call(NativeBuiltins::deopt,
                                {paramCode(), paramClosure(), new_constant(m),
                                 paramArgs()});
                });
                break;
            }

            case Tag::Identical: {
                auto a = depromise(load(i, i->arg(0).val()));
                auto b = depromise(load(i, i->arg(1).val()));
                setVal(i, a == b);
                break;
            }

            case Tag::Branch: {
                auto condition =
                    load(i, i->arg(0).val(), Representation::Integer);
                insn_branch_if(condition, blockLabel.at(bb->trueBranch()));
                insn_branch(blockLabel.at(bb->falseBranch()));
                break;
            }

            case Tag::Phi:
                setVal(i, phis.at(i));
                break;

            case Tag::LdArg:
                setVal(i, argument(LdArg::Cast(i)->id));
                break;

            case Tag::LdFunctionEnv:
                setVal(i, paramEnv());
                break;

            case Tag::LdVar: {
                auto ld = LdVar::Cast(i);

                auto env = MkEnv::Cast(ld->env());
                if (env && env->stub) {
                    setVal(i, envStubGet(loadSxp(i, env),
                                         env->indexOf(ld->varName)));
                    break;
                }

                jit_value res;
                if (bindingsCache.count(i->env())) {
                    res = jit_value_create(raw(), sxp);
                    auto offset = bindingsCache.at(i->env()).at(ld->varName);

                    auto cache = insn_load_relative(bindingsCacheBase, offset,
                                                    jit_type_nuint);
                    jit_label done, miss;
                    insn_branch_if(insn_le(cache, new_constant((SEXP)1)), miss);
                    auto val = car(cache);
                    insn_branch_if(insn_eq(val, constant(R_UnboundValue, sxp)),
                                   miss);
                    store(res, val);
                    insn_branch(done);

                    insn_label(miss);
                    auto pos =
                        insn_add(bindingsCacheBase, new_constant(offset));
                    store(res, call(NativeBuiltins::ldvarCacheMiss,
                                    {constant(ld->varName, sxp),
                                     loadSxp(i, ld->env()), pos}));
                    insn_label(done);
                } else {
                    res = call(
                        NativeBuiltins::ldvar,
                        {constant(ld->varName, sxp), loadSxp(i, ld->env())});
                }
                checkMissing(res);
                checkUnbound(res);
                setVal(i, res);
                break;
            }

            case Tag::Extract1_1D: {
                auto extract = Extract1_1D::Cast(i);
                auto vector = loadSxp(i, extract->vec());
                auto idx = loadSxp(i, extract->idx());

                // We should implement the fast cases (known and primitive
                // types) speculatively here
                auto env = constant(R_NilValue, sxp);
                if (extract->hasEnv())
                    env = loadSxp(i, extract->env());

                gcSafepoint(i, -1, false);
                auto res =
                    call(NativeBuiltins::extract11,
                         {vector, idx, env, new_constant(extract->srcIdx)});
                setVal(i, res);
                break;
            }

            case Tag::Extract2_1D: {
                auto extract = Extract2_1D::Cast(i);
                auto vector = loadSxp(i, extract->vec());
                auto idx = loadSxp(i, extract->idx());

                auto env = constant(R_NilValue, sxp);
                if (extract->hasEnv())
                    env = loadSxp(i, extract->env());

                gcSafepoint(i, -1, false);
                auto res =
                    call(NativeBuiltins::extract21,
                         {vector, idx, env, new_constant(extract->srcIdx)});
                setVal(i, res);
                break;
            }

            case Tag::StVar: {
                auto st = StVar::Cast(i);
                auto environment = MkEnv::Cast(st->env());

                if (environment && environment->stub) {
                    envStubSet(loadSxp(i, environment),
                               environment->indexOf(st->varName),
                               loadSxp(i, st->val()));
                    break;
                }

                auto setter = NativeBuiltins::stvar;
                if (st->isStArg)
                    setter = NativeBuiltins::starg;

                if (bindingsCache.count(environment)) {
                    auto offset = bindingsCache.at(environment).at(st->varName);
                    auto cache = insn_load_relative(bindingsCacheBase, offset,
                                                    jit_type_nuint);
                    jit_label done, miss, identical;

                    auto newVal = loadSxp(i, st->arg<0>().val());

                    insn_branch_if(insn_le(cache, new_constant((SEXP)1)), miss);
                    auto val = car(cache);
                    insn_branch_if(insn_eq(val, constant(R_UnboundValue, sxp)),
                                   miss);

                    insn_branch_if(insn_eq(val, newVal), identical);

                    incrementNamed(newVal);
                    setCar(cache, newVal);

                    insn_branch(done);

                    insn_label(identical);
                    // In the fast case (where the value is not updated) we
                    // still need to ensure it is named.
                    ensureNamed(val);
                    insn_branch(done);

                    insn_label(miss);

                    gcSafepoint(i, 1, false);
                    call(setter,
                         {constant(st->varName, sxp), newVal,
                          loadSxp(i, st->env())});

                    insn_label(done);

                } else {
                    gcSafepoint(i, 1, false);
                    call(setter, {constant(st->varName, sxp),
                                  loadSxp(i, st->arg<0>().val()),
                                  loadSxp(i, st->env())});
                }
                break;
            }

            case Tag::StVarSuper: {
                auto st = StVarSuper::Cast(i);
                auto environment = MkEnv::Cast(st->env());
                if (environment) {
                    auto parent = MkEnv::Cast(environment->lexicalEnv());
                    if (environment->stub || (parent && parent->stub)) {
                        success = false;
                        break;
                    }
                }

                // In case we statically knew the parent PIR already converted
                // super assigns to standard stores
                gcSafepoint(i, 1, false);
                call(NativeBuiltins::defvar,
                     {constant(st->varName, sxp),
                      loadSxp(i, st->arg<0>().val()), loadSxp(i, st->env())});
                break;
            }

            case Tag::LdFun: {
                auto ld = LdFun::Cast(i);
                gcSafepoint(i, -1, false);
                auto res =
                    call(NativeBuiltins::ldfun,
                         {constant(ld->varName, sxp), loadSxp(i, ld->env())});
                // TODO..
                checkMissing(res);
                checkUnbound(res);
                setVal(i, res);
                setVisible(1);
                break;
            }

            case Tag::MkArg: {
                auto p = MkArg::Cast(i);
                gcSafepoint(i, 1, true);
                setVal(i,
                       call(NativeBuiltins::createPromise,
                            {paramCode(), new_constant(promMap.at(p->prom())),
                             loadSxp(i, p->env()), loadSxp(i, p->eagerArg())}));
                break;
            }

            case Tag::MkEnv: {
                // TODO support env stubs
                //  (creates normal env for stubs, aborts if any
                //  stvar/ldvar/isstubenv are present)
                auto mkenv = MkEnv::Cast(i);
                auto parent = loadSxp(i, mkenv->env());
                static std::vector<std::vector<Immediate>> mkEnvStubNames;
                mkEnvStubNames.push_back({});
                auto& namesBuffer = mkEnvStubNames.back();
                for (size_t i = 0; i < mkenv->nLocals(); ++i)
                    namesBuffer.push_back(Pool::insert(mkenv->varName[i]));

                if (mkenv->stub) {
                    gcSafepoint(i, 1, true);
                    auto env = call(NativeBuiltins::createStubEnvironment,
                                    {parent, new_constant(mkenv->nLocals()),
                                     new_constant(namesBuffer.data()),
                                     new_constant(mkenv->context)});
                    size_t pos = 0;
                    mkenv->eachLocalVar([&](SEXP name, Value* v) {
                        envStubSet(env, pos++, loadSxp(i, v));
                    });
                    setVal(i, env);
                    break;
                }

                gcSafepoint(i, mkenv->nargs() + 1, true);
                auto arglist = constant(R_NilValue, sxp);
                mkenv->eachLocalVarRev([&](SEXP name, Value* v) {
                    if (v == MissingArg::instance()) {
                        arglist = call(NativeBuiltins::createMissingBindingCell,
                                       {constant(name, sxp), arglist});
                    } else {
                        arglist =
                            call(NativeBuiltins::createBindingCell,
                                 {loadSxp(i, v), constant(name, sxp), arglist});
                    }
                });

                setVal(i,
                       call(NativeBuiltins::createEnvironment,
                            {parent, arglist, new_constant(mkenv->context)}));

                // Zero bindings cache
                if (bindingsCache.count(i))
                    for (auto b : bindingsCache.at(i))
                        insn_store_relative(bindingsCacheBase, b.second,
                                            new_constant(nullptr));
                break;
            }

            case Tag::IsEnvStub: {
                auto arg = loadSxp(i, i->arg(0).val());
                jit_label done;

                auto res = jit_value_create(raw(), representationOf(i));
                store(res, constant(R_FalseValue, representationOf(i)));

                auto magic =
                    insn_load_relative(arg, extsxpMagicOfs, jit_type_uint);
                insn_branch_if(magic != new_constant(LAZY_ENVIRONMENT_MAGIC),
                               done);

                auto materialized = envStubGet(arg, -1);
                insn_branch_if(materialized != new_constant(nullptr), done);

                store(res, constant(R_TrueValue, representationOf(i)));

                insn_label(done);

                setVal(i, res);
                break;
            }

            case Tag::MkFunCls: {
                auto mkFunction = MkFunCls::Cast(i);
                auto closure = mkFunction->cls;
                auto srcRef = constant(closure->srcRef(), sxp);
                auto formals = constant(closure->formals().original(), sxp);
                auto body =
                    constant(mkFunction->originalBody->container(), sxp);
                assert(DispatchTable::check(
                    mkFunction->originalBody->container()));
                gcSafepoint(i, 1, true);
                setVal(i, call(NativeBuiltins::createClosure,
                               {body, formals, loadSxp(i, mkFunction->env()),
                                srcRef}));
                break;
            }

            case Tag::Force: {
                auto f = Force::Cast(i);
                auto arg = loadSxp(i, f->arg<0>().val());
                if (!f->effects.includes(Effect::Force)) {
                    auto res = depromise(arg);
                    setVal(i, res);
#ifdef ENABLE_SLOWASSERT
                    insn_assert(res != constant(R_UnboundValue, sxp),
                                "Expected evaluated promise");
#endif
                } else {
                    setVal(i, force(i, arg));
                }
                break;
            }

            case Tag::Invisible:
                setVisible(0);
                break;

            case Tag::Visible:
                setVisible(1);
                break;

            case Tag::LdConst:
                // scheduled on use...
                break;

            case Tag::Return: {
                auto res = loadSxp(i, Return::Cast(i)->arg<0>().val());
                if (numLocals > 0) {
                    decStack(numLocals);
                }
                insn_return(res);
                break;
            }

            case Tag::CallSafeBuiltin: {
                auto b = CallSafeBuiltin::Cast(i);
                std::vector<Value*> args;
                b->eachCallArg([&](Value* v) { args.push_back(v); });

                // TODO: this should probably go somewhere else... This is an
                // inlined version of bitwise builtins
                if (representationOf(b) == Representation::Integer) {
                    if (b->nargs() == 2) {
                        auto x = b->arg(0).val();
                        auto y = b->arg(1).val();
                        auto xRep = representationOf(x);
                        auto yRep = representationOf(y);

                        static auto bitwise = {
                            findBuiltin("bitwiseShiftL"),
                            findBuiltin("bitwiseShiftR"),
                            findBuiltin("bitwiseAnd"),
                            findBuiltin("bitwiseOr"),
                            findBuiltin("bitwiseXor"),
                        };
                        auto found = std::find(bitwise.begin(), bitwise.end(),
                                               b->builtinId);
                        if (found != bitwise.end()) {
                            const static PirType num =
                                (PirType() | RType::integer | RType::logical |
                                 RType::real)
                                    .notObject()
                                    .scalar();

                            if (xRep == Representation::Sexp &&
                                x->type.isA(num))
                                xRep = Representation::Real;
                            if (yRep == Representation::Sexp &&
                                y->type.isA(num))
                                yRep = Representation::Real;

                            if (xRep != Representation::Sexp &&
                                yRep != Representation::Sexp) {
                                auto isNa = jit_label();
                                auto done = jit_label();
                                auto res =
                                    jit_value_create(raw(), jit_type_int);

                                auto xInt = load(i, x, Representation::Integer);
                                auto yInt = load(i, y, Representation::Integer);

                                auto naCheck = [&](Value* v, jit_value asInt,
                                                   Representation rep) {
                                    if (rep == Representation::Real) {
                                        auto vv = load(i, v, rep);
                                        insn_branch_if(vv != vv, isNa);
                                    } else {
                                        assert(rep == Representation::Integer);
                                        insn_branch_if(
                                            asInt == new_constant(NA_INTEGER),
                                            isNa);
                                    }
                                };
                                naCheck(x, xInt, xRep);
                                naCheck(y, yInt, yRep);

                                switch (found - bitwise.begin()) {
                                case 0:
                                    insn_branch_if(yInt < new_constant(0),
                                                   isNa);
                                    insn_branch_if(yInt > new_constant(31),
                                                   isNa);
                                    store(res, xInt << yInt);
                                    break;
                                case 1:
                                    insn_branch_if(yInt < new_constant(0),
                                                   isNa);
                                    insn_branch_if(yInt > new_constant(31),
                                                   isNa);
                                    store(res, xInt >> yInt);
                                    break;
                                case 2:
                                    store(res, xInt & yInt);
                                    break;
                                case 3:
                                    store(res, xInt | yInt);
                                    break;
                                case 4:
                                    store(res, xInt ^ yInt);
                                    break;
                                }

                                insn_branch(done);

                                insn_label(isNa);
                                store(res, new_constant(NA_INTEGER));

                                insn_label(done);
                                setVal(i, res);
                                break;
                            }
                        }
                    }
                }

                setVal(i, withCallFrame(i, args, [&]() -> jit_value {
                           return call(NativeBuiltins::callBuiltin,
                                       {
                                           paramCode(), new_constant(b->srcIdx),
                                           constant(b->blt, sxp),
                                           constant(symbol::delayedEnv, sxp),
                                           new_constant(b->nCallArgs()),
                                       });
                       }));
                break;
            }

            case Tag::CallBuiltin: {
                auto b = CallBuiltin::Cast(i);
                std::vector<Value*> args;
                b->eachCallArg([&](Value* v) { args.push_back(v); });
                setVal(i, withCallFrame(i, args, [&]() -> jit_value {
                           return call(NativeBuiltins::callBuiltin,
                                       {
                                           paramCode(),
                                           new_constant(b->srcIdx),
                                           constant(b->blt, sxp),
                                           loadSxp(i, b->env()),
                                           new_constant(b->nCallArgs()),
                                       });
                       }));
                break;
            }

            //            case Tag::StaticCall: {
            //                auto b = StaticCall::Cast(i);
            //                std::vector<Value*> args;
            //                b->eachCallArg([&](Value* v) { args.push_back(v);
            //                });
            //                setVal(i, withCallFrame(i, args, [&]() ->
            //                jit_value {
            //                           return call(
            //                               NativeBuiltins::call,
            //                               {
            //                                   paramCode(),
            //                                   new_constant(b->srcIdx),
            //                                   new_constant(b->cls()->rirClosure()),
            //                                   loadSxp(i, b->env()),
            //                                   new_constant(b->nCallArgs()),
            //                               });
            //                       }));
            //                break;
            //            }

            case Tag::Call: {
                auto b = Call::Cast(i);
                std::vector<Value*> args;
                b->eachCallArg([&](Value* v) { args.push_back(v); });
                setVal(i, withCallFrame(i, args, [&]() -> jit_value {
                           return call(NativeBuiltins::call,
                                       {
                                           paramCode(),
                                           new_constant(b->srcIdx),
                                           loadSxp(i, b->cls()),
                                           loadSxp(i, b->env()),
                                           new_constant(b->nCallArgs()),
                                       });
                       }));
                break;
            }

            case Tag::Nop:
                break;

            case Tag::AsInt: {
                auto arg = i->arg(0).val();
                auto asint = AsInt::Cast(i);
                jit_value res;
                if (representationOf(arg) == Representation::Integer) {
                    res = load(i, arg, Representation::Integer);
                } else if (representationOf(arg) == Representation::Real) {
                    if (asint->ceil) {
                        res = insn_ceil(load(i, arg, Representation::Real));
                    } else {
                        res = insn_floor(load(i, arg, Representation::Real));
                    }
                    res = insn_convert(res, jit_type_int);
                } else {
                    success = false;
                }
                setVal(i, res);
                break;
            }

            case Tag::Inc: {
                auto arg = i->arg(0).val();
                jit_value res;
                if (representationOf(arg) == Representation::Integer) {
                    res = load(i, arg, Representation::Integer);
                    res = insn_add(res, new_constant(1));
                } else {
                    success = false;
                }
                setVal(i, res);
                break;
            }

            case Tag::Dec: {
                auto arg = i->arg(0).val();
                jit_value res;
                if (representationOf(arg) == Representation::Integer) {
                    res = load(i, arg, Representation::Integer);
                    res = insn_sub(res, new_constant(1));
                } else {
                    success = false;
                }
                setVal(i, res);
                break;
            }

            default:
                success = false;
                break;
            }

#if 0
            if (!success) {
                std::cerr << "Can't compile ";
                i->print(std::cerr, true);
                std::cerr << "\n";
            }
#endif

            if (!success)
                return;

            if (phis.count(i)) {
                auto r = Representation(phis.at(i).type());
                if (PirCopy::Cast(i)) {
                    store(phis.at(i), load(i, i->arg(0).val(), r));
                } else {
                    store(phis.at(i), load(i, i, r));
                }
            }

            if (representationOf(i) == Representation::Sexp &&
                needsEnsureNamed.count(i)) {
                ensureNamed(loadSxp(i, i));
            }
        }

        if (bb->isJmp()) {
            insn_branch(blockLabel.at(bb->next()));
        }
    });

    // jit_dump_function(stdout, raw(), "test");
};

static jit_context context;

void* Lower::tryCompile(
    ClosureVersion* cls, Code* code,
    const std::unordered_map<Promise*, unsigned>& promMap,
    const std::unordered_set<Instruction*>& needsEnsureNamed) {
    PirCodeFunction function(context, cls, code, promMap, needsEnsureNamed);
    function.set_optimization_level(function.max_optimization_level());
    function.build_start();
    function.build();
    function.compile();
    function.build_end();

    if (cls->name().substr(0, 10) != "mandelbrot")
        return nullptr;
    if (function.success) {
        // auto ctx = globalContext();
        // void* args[1] = {&ctx};
        // SEXP result;
        // if (debug)
        //   jit_dump_function(stdout, function.raw(), "test");
        // function.apply(args, &result);
        // std::cout << "Returns: ";
        // Rf_PrintValue(result);
        // std::cout << "\n";

        return function.closure();
    }

    return nullptr;
}
}
}
