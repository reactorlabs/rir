#include "lower.h"
#include "R/r.h"
#include "builtins.h"
#include "compiler/pir/pir_impl.h"
#include "compiler/translations/pir_2_rir/liveness.h"
#include "compiler/util/visitor.h"
#include "interpreter/instance.h"
#include "jit/jit-dump.h"
#include "jit/jit-value.h"
#include "utils/Pool.h"

#include <cassert>
#include <iostream>

namespace rir {
namespace pir {

static const auto cpOfs = (size_t) & ((InterpreterInstance*)0) -> cp.list;
static const auto stdVecDtptrOfs = sizeof(SEXPREC_ALIGN);
static const auto carOfs = (size_t) & ((SEXP)0) -> u.listsxp.carval;
static const auto cdrOfs = (size_t) & ((SEXP)0) -> u.listsxp.cdrval;
static const auto tagOfs = (size_t) & ((SEXP)0) -> u.listsxp.tagval;
static const auto prValueOfs = (size_t) & ((SEXP)0) -> u.promsxp.value;
static const auto stackCellValueOfs = (size_t) & ((R_bcstack_t*)0) -> u.sxpval;
static const auto sxpinfofOfs = (size_t) & ((SEXP)0) -> sxpinfo;

struct Representation {
    enum Type {
        Bottom,
        Integer,
        Real,
        Sexp,
    };
    Representation() : t(Bottom) {}
    Representation(Type t) : t(t) {}
    explicit Representation(jit_type_t jt) {
        if (jt == jit_type_void)
            t = Bottom;
        else if (jt == jit_type_nint)
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
            return jit_type_nint;
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
    Code* code;
    bool success = false;

    CFG cfg;
    LivenessIntervals liveness;
    size_t numLocals = liveness.maxLive;

    PirCodeFunction(jit_context& context, Code* code,
                    const std::unordered_map<Promise*, unsigned>& promMap);

    void build() override;
    jit_value argument(int i);
    jit_value constant(SEXP c, jit_type_t);
    jit_value sexptype(jit_value v);

    jit_value call(const NativeBuiltin&, const std::vector<jit_value>&);

    void setVisible(int i);
    jit_value force(Instruction*, jit_value);

    void checkMissing(jit_value);
    void checkUnbound(jit_value);

    void gcSafepoint(Instruction*, size_t required, bool protectArgs);

    void incStack(int i, bool zero);
    void decStack(int i);
    void setStackHeight(jit_value);
    jit_value stack(int i);
    void stack(int i, jit_value);

    jit_value getLocal(size_t i);
    void setLocal(size_t i, jit_value);

    jit_value load(Instruction* pos, Value* v, Representation);
    jit_value loadSxp(Instruction* pos, Value* v) {
        return load(pos, v, Representation::Sexp);
    }
    jit_value loadSame(Instruction* pos, Value* v) {
        return load(pos, v, representation.at(pos));
    }

    jit_value paramCode() { return get_param(0); }
    jit_value paramCtx() { return get_param(1); }
    jit_value paramArgs() { return get_param(2); }
    jit_value paramEnv() { return get_param(3); }

    jit_value unboxInt(jit_value v) {
        return insn_load_relative(v, stdVecDtptrOfs, jit_type_nint);
    }
    jit_value unboxReal(jit_value v) {
        return insn_load_relative(v, stdVecDtptrOfs, jit_type_float64);
    }

    jit_value boxInt(Instruction* pos, jit_value v) {
        gcSafepoint(pos, 1, true);
        return call(NativeBuiltins::newInt, {v});
    }
    jit_value boxReal(Instruction* pos, jit_value v) {
        gcSafepoint(pos, 1, true);
        return call(NativeBuiltins::newReal, {v});
    }
    jit_value withCallFrame(Instruction* i, const std::vector<Value*>& args,
                            const std::function<jit_value()>&);

  protected:
    const std::unordered_map<Promise*, unsigned>& promMap;

    std::unordered_map<Value*, jit_value> valueMap;
    std::unordered_map<Instruction*, Representation> representation;

    Representation representationOf(Value* v);

    void setVal(Instruction* i, jit_value val) {
        assert(!valueMap.count(i));
        if (i->producesRirResult() && representation.at(i) != val.type()) {
            i->print(std::cout);
            jit_dump_function(stdout, raw(), "test");
            std::cout << "\nWanted a " << representation.at(i) << " but got a "
                      << Representation(val.type()) << "\n";
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
        return signature_helper(jit_type_void_ptr, jit_type_void_ptr,
                                jit_type_void_ptr, sxp, jit_type_void_ptr,
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
    auto offset = i * sizeof(R_bcstack_t);
    // set type tag to 0
    insn_store_relative(nodestackPtr(), offset, new_constant(0));
    offset += stackCellValueOfs;
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

void PirCodeFunction::stack(int i, jit_value v) {
    auto offset = -(i + 1) * sizeof(R_bcstack_t);
    // set type tag to 0
    insn_store_relative(nodestackPtr(), offset, new_constant(0));
    offset += stackCellValueOfs;
    // store the value
    insn_store_relative(nodestackPtr(), offset, v);
}

jit_value PirCodeFunction::load(Instruction* pos, Value* v,
                                Representation needed) {

    jit_value res;

    if (valueMap.count(v))
        res = valueMap.at(v);
    else if (auto e = Env::Cast(v))
        res = constant(e->rho, sxp);
    else if (v == True::instance())
        res = constant(R_TrueValue, needed);
    else if (v == False::instance())
        res = constant(R_FalseValue, needed);
    else if (v == MissingArg::instance())
        res = constant(R_MissingArg, sxp);
    else if (v == UnboundValue::instance())
        res = constant(R_UnboundValue, sxp);
    else if (auto ld = LdConst::Cast(v))
        res = constant(ld->c(), needed);
    else
        assert(false);

    if (res.type() == sxp && needed != sxp) {
        if (v->type.isA(RType::integer)) {
            res = unboxInt(res);
        } else if (v->type.isA(RType::real)) {
            res = unboxReal(res);
        } else {
            assert(false);
        }
        // fall through, since more conversions might be needed after unboxing
    }

    if (res.type() == jit_type_nint && needed == jit_type_float64)
        res = insn_convert(res, jit_type_float64, false);
    else if (res.type() == jit_type_nint && needed == sxp)
        res = boxInt(pos, res);
    else if (res.type() == jit_type_float64 && needed == sxp)
        res = boxReal(pos, res);

    assert(res.type() == needed);

    return res;
}

// template <SEXPTYPE type>
// jit_value PirCodeFunction::typedExtract2_1D() {
//     auto intOffset = stdVecDtptrOfs + sizeof(int) * constantIdx;
//     auto intRes = insn_load_relative(vec, intOffset, jit_type_int);
//     gcSafepoint(i, 1, false);
//     auto intResSexp = call(NativeBuiltins::newInt, {intRes});
//     store(res, intResSexp);
//     insn_branch(done);
// };

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

jit_value PirCodeFunction::sexptype(jit_value v) {
    auto sxpinfo = insn_load_relative(v, sxpinfofOfs, jit_type_ulong);
    return insn_and(sxpinfo, new_constant(MAX_NUM_SEXPTYPE - 1));
};

jit_value PirCodeFunction::constant(SEXP c, jit_type_t needed) {
    static std::unordered_set<SEXP> eternal = {
        R_TrueValue, R_NilValue, R_FalseValue, R_UnboundValue, R_MissingArg};
    if (eternal.count(c) && needed == sxp) {
        return new_constant(c);
    }

    if (needed == jit_type_nint) {
        assert(Rf_length(c) == 1);
        if (TYPEOF(c) == INTSXP)
            return new_constant(INTEGER(c)[0], jit_type_nint);
        if (TYPEOF(c) == REALSXP) {
            assert(REAL(c)[0] == (int)REAL(c)[0]);
            return new_constant((int)REAL(c)[0], jit_type_nint);
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
    if (!cp.is_valid()) {
        auto cp_ = insn_load_relative(paramCtx(), cpOfs, sxp);
        cp = insn_add(cp_, new_constant(stdVecDtptrOfs));
    }

    return insn_load_elem(cp, new_constant(i), sxp);
}

jit_value PirCodeFunction::argument(int i) {
    i *= sizeof(R_bcstack_t);
    i += stackCellValueOfs;
    return insn_load_relative(paramArgs(), i, sxp);
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
    int pos = nargs - 1;
    for (auto& arg : args)
        stack(pos--, load(i, arg, Representation::Sexp));
    auto res = theCall();
    decStack(nargs);
    return res;
}

static const bool debug = false;
static const bool debug2 = false;

Representation PirCodeFunction::representationOf(Value* v) {
    if (auto i = Instruction::Cast(v)) {
        if (representation.count(i))
            return representation.at(i);
    };
    static auto scalarInt = PirType(RType::integer).scalar().notObject();
    static auto scalarReal = PirType(RType::real).scalar().notObject();
    static auto scalarLogical = PirType(RType::logical).scalar().notObject();

    if (v->type.isA(scalarInt) || v->type.isA(scalarLogical))
        return Representation::Integer;
    if (v->type.isA(scalarReal))
        return Representation::Real;
    return Representation::Sexp;
}

PirCodeFunction::PirCodeFunction(
    jit_context& context, Code* code,
    const std::unordered_map<Promise*, unsigned>& promMap)
    : jit_function(context), code(code), cfg(code),
      liveness(code->nextBBId, cfg), promMap(promMap) {
    create();

    auto cp_ = insn_load_relative(paramCtx(), cpOfs, sxp);
    cp = insn_add(cp_, new_constant(stdVecDtptrOfs));
    nodestackPtrPtr = new_constant(&R_BCNodeStackTop);

    bool changed = true;
    auto update = [&](Instruction* i, Representation r) {
        if (representation[i] != r) {
            representation[i] = r;
            changed = true;
        }
    };
    auto apply = [&]() {
        Visitor::run(code->entry, [&](Instruction* i) {
            switch (i->tag) {
            case Tag::LdConst:
            case Tag::Gt:
            case Tag::Gte:
            case Tag::Lt:
            case Tag::Lte:
                update(i, representationOf(i));
                break;

            case Tag::Add:
            case Tag::Sub:
            case Tag::Mul:
            case Tag::Div:
            case Tag::PirCopy:
            case Tag::Phi: {
                Representation r = representationOf(i);
                i->eachArg([&](Value* v) {
                    if (i->mayHaveEnv() && i->env() == v)
                        return;
                    r.merge(representationOf(v));
                });
                update(i, r);
                break;
            }
            case Tag::AsTest: {
                update(i, Representation::Integer);
                break;
            }
            default:
                if (i->producesRirResult()) {
                    if (representation[i] != Representation::Sexp) {
                        representation[i] = Representation::Sexp;
                        changed = true;
                    }
                }
                break;
            }
        });
    };
    while (changed) {
        changed = false;
        apply();
    }
};

void PirCodeFunction::build() {
    success = true;

    if (debug) {
        std::cout
            << "===========================================================\n";
        code->printCode(std::cout, true, true);
    }

    if (debug2) {
        code->printCode(std::cout, true, false);
        for (auto& r : representation) {
            r.first->printRef(std::cout);
            std::cout << " = " << r.second << "\n";
        }
    }

    basepointer = nodestackPtr();
    if (numLocals > 0)
        incStack(numLocals, true);

    std::unordered_map<BB*, jit_label> blockLabel;
    std::unordered_map<Instruction*, jit_value> phis;
    Visitor::run(code->entry, [&](BB* bb) {
        blockLabel[bb] = jit_label();
        for (auto i : *bb) {
            if (auto phi = Phi::Cast(i)) {
                auto val = jit_value_create(raw(), representation.at(i));
                phis[i] = val;
                phi->eachArg([&](BB*, Value* v) {
                    if (auto c = PirCopy::Cast(v))
                        phis[c] = val;
                });
            }
        }
    });

    auto leastCommonArgRepresentation = [&](Instruction* i) {
        Representation r;
        i->eachArg([&](Value* v) {
            if (i->mayHaveEnv() && v == i->env())
                return;
            r.merge(representationOf(v));
        });
        return r;
    };

    LoweringVisitor::run(code->entry, [&](BB* bb) {
        insn_label(blockLabel.at(bb));

        for (auto it = bb->begin(); it != bb->end(); ++it) {
            auto i = *it;
            if (!success)
                return;

            switch (i->tag) {
            case Tag::PirCopy: {
                auto c = PirCopy::Cast(i);
                auto in = c->arg<0>().val();
                if (phis.count(c)) {
                    store(phis.at(c), loadSame(i, in));
                    setVal(i, load(i, in, representation.at(i)));
                } else {
                    setVal(i, loadSame(i, in));
                }
                break;
            }

            case Tag::AsTest: {
                if (representation.at(i) != Representation::Integer ||
                    representationOf(i->arg(0).val()) == Representation::Sexp) {
                    success = false;
                    break;
                }

                auto arg = loadSame(i, i->arg(0).val());
                jit_label notNa;
                auto isNa = insn_eq(arg, new_constant(NA_INTEGER));

                insn_branch_if_not(isNa, notNa);
                call(NativeBuiltins::error, {});

                insn_label(notNa);
                setVal(i, arg);
                break;
            }

            case Tag::Gte: {
                if (representation.at(i) != Representation::Integer) {
                    success = false;
                    break;
                }
                Representation argRep = leastCommonArgRepresentation(i);
                if (argRep == Representation::Integer ||
                    argRep == Representation::Real) {
                    auto res = jit_value_create(raw(), representation.at(i));
                    auto a = load(i, i->arg(0).val(), argRep);
                    auto b = load(i, i->arg(1).val(), argRep);

                    jit_label done;
                    jit_label isNa;
                    if (argRep == Representation::Integer) {
                        auto aIsNa = insn_eq(a, new_constant(NA_INTEGER));
                        insn_branch_if(aIsNa, isNa);
                        auto bIsNa = insn_eq(b, new_constant(NA_INTEGER));
                        insn_branch_if(bIsNa, isNa);
                    } else {
                        auto aIsNa = insn_ne(a, a);
                        insn_branch_if(aIsNa, isNa);
                        auto bIsNa = insn_ne(b, b);
                        insn_branch_if(bIsNa, isNa);
                    }

                    auto ge = insn_le(a, b);
                    store(res, ge);
                    insn_branch(done);

                    insn_label(isNa);
                    assert(representation.at(i) == Representation::Integer);
                    store(res, new_constant(NA_INTEGER));

                    insn_label(done);

                    setVal(i, res);
                } else {
                    success = false;
                }
                break;
            }

            case Tag::Add: {
                auto r = representation.at(i);
                Representation argRep = leastCommonArgRepresentation(i);

                assert(r == argRep);
                if (r != Representation::Integer && r != Representation::Real) {
                    success = false;
                    break;
                }

                auto a = load(i, i->arg(0).val(), argRep);
                auto b = load(i, i->arg(1).val(), argRep);
                setVal(i, insn_add(a, b));

                break;
            }

            case Tag::Branch: {
                auto condition =
                    load(i, i->arg(0).val(), Representation::Integer);
                auto isZero = insn_eq(condition, new_constant(0));
                insn_branch_if(isZero, blockLabel.at(bb->falseBranch()));
                insn_branch(blockLabel.at(bb->trueBranch()));
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
                auto res =
                    call(NativeBuiltins::ldvar,
                         {constant(ld->varName, sxp), loadSxp(i, ld->env())});
                checkMissing(res);
                checkUnbound(res);
                setVal(i, res);
                break;
            }

            //   case Tag::Extract2_1D: {
            //       auto ex = Extract2_1D::Cast(i);
            //       auto vec = load(ex->arg<0>().val());
            //       auto idx = ex->arg<1>().val();
            //       int constantIdx = -1;
            //       if (auto cidx = LdConst::Cast(idx)) {
            //           if (IS_SIMPLE_SCALAR(cidx->c(), REALSXP)) {
            //               constantIdx = REAL(cidx->c())[0] - 1;
            //           } else if (IS_SIMPLE_SCALAR(cidx->c(), INTSXP)) {
            //               constantIdx = INTEGER(cidx->c())[0] - 1;
            //           } else {
            //               success = false;
            //               break;
            //           }
            //       }

            //       if (constantIdx < 0 || constantIdx > 3) {
            //           success = false;
            //           break;
            //       }

            //       jit_value res = jit_value_create(raw(), sxp);
            //       auto type = sexptype(vec);
            //       auto isInt = jit_label();
            //       auto isReal = jit_label();
            //       auto isVec = jit_label();
            //       auto done = jit_label();
            //       auto isIntTest = insn_eq(type, new_constant(INTSXP));
            //       insn_branch_if(isIntTest, isInt);
            //       auto isRealTest = insn_eq(type, new_constant(REALSXP));
            //       insn_branch_if(isRealTest, isReal);
            //       auto isVecTest = insn_eq(type, new_constant(VECSXP));
            //       insn_branch_if(isVecTest, isVec);

            //       // TODO;
            //       call(NativeBuiltins::error, {});
            //       insn_branch(done);

            //       insn_label(isInt);
            //       // TODO check size
            //       auto intOffset = stdVecDtptrOfs + sizeof(int) *
            //       constantIdx;
            //       auto intRes = insn_load_relative(vec, intOffset,
            //       jit_type_int);
            //       gcSafepoint(i, 1, false);
            //       auto intResSexp = call(NativeBuiltins::newInt, {intRes});
            //       store(res, intResSexp);
            //       insn_branch(done);

            //       insn_label(isReal);
            //       // TODO check size
            //       auto realOffset = stdVecDtptrOfs + sizeof(double) *
            //       constantIdx;
            //       auto realRes =
            //           insn_load_relative(vec, realOffset,
            //           jit_type_float64);
            //       gcSafepoint(i, 1, false);
            //       auto realResSexp = call(NativeBuiltins::newReal,
            //       {realRes});
            //       store(res, realResSexp);
            //       insn_branch(done);

            //       insn_label(isVec);
            //       // TODO check size
            //       auto vecOffset = stdVecDtptrOfs + sizeof(SEXP) *
            //       constantIdx;
            //       auto vecRes = insn_load_relative(vec, vecOffset, sxp);
            //       store(res, vecRes);
            //       insn_branch(done);

            //       insn_label(done);
            //       vals[i] = res;
            //       break;
            //   }

            case Tag::StVar: {
                auto st = StVar::Cast(i);
                call(NativeBuiltins::stvar,
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
                auto mkenv = MkEnv::Cast(i);
                if (mkenv->stub) {
                    // TODO
                }

                gcSafepoint(i, mkenv->nargs() + 1, true);
                auto arglist = constant(R_NilValue, sxp);
                mkenv->eachLocalVarRev([&](SEXP name, Value* v) {
                    if (v == MissingArg::instance()) {
                        arglist = call(NativeBuiltins::consNrTaggedMissing,
                                       {constant(name, sxp), arglist});
                    } else {
                        arglist =
                            call(NativeBuiltins::consNrTagged,
                                 {loadSxp(i, v), constant(name, sxp), arglist});
                    }
                });
                auto parent = loadSxp(i, mkenv->env());

                setVal(i,
                       call(NativeBuiltins::createEnvironment,
                            {parent, arglist, new_constant(mkenv->context)}));
                break;
            }

            case Tag::Force: {
                auto arg = loadSxp(i, Force::Cast(i)->arg<0>().val());
                setVal(i, force(i, arg));
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
                setVal(i, withCallFrame(i, args, [&]() -> jit_value {
                           return call(
                               NativeBuiltins::callBuiltin,
                               {
                                   paramCode(), new_constant(b->srcIdx),
                                   constant(b->blt, sxp), new_constant(0),
                                   new_constant(b->nCallArgs()), paramCtx(),
                               });
                       }));
                break;
            }

            case Tag::CallBuiltin: {
                auto b = CallBuiltin::Cast(i);
                std::vector<Value*> args;
                b->eachCallArg([&](Value* v) { args.push_back(v); });
                setVal(i, withCallFrame(i, args, [&]() -> jit_value {
                           return call(
                               NativeBuiltins::callBuiltin,
                               {
                                   paramCode(), new_constant(b->srcIdx),
                                   constant(b->blt, sxp), loadSxp(i, b->env()),
                                   new_constant(b->nCallArgs()), paramCtx(),
                               });
                       }));
                break;
            }

            case Tag::Call: {
                auto b = Call::Cast(i);
                std::vector<Value*> args;
                b->eachCallArg([&](Value* v) { args.push_back(v); });
                setVal(i, withCallFrame(i, args, [&]() -> jit_value {
                           return call(
                               NativeBuiltins::call,
                               {
                                   paramCode(), new_constant(b->srcIdx),
                                   loadSxp(i, b->cls()), loadSxp(i, b->env()),
                                   new_constant(b->nCallArgs()), paramCtx(),
                               });
                       }));
                break;
            }

            case Tag::Nop:
                break;

            default:
                success = false;
                break;
            }

            if (!success && debug2) {
                std::cerr << "Can't compile ";
                i->print(std::cerr, true);
                std::cerr << "\n";
            }
        }

        if (bb->isJmp()) {
            insn_branch(blockLabel.at(bb->next()));
        }
    });

    if (success && debug2) {
        std::cout << "******************* SUCCESS ************************\n";
        jit_dump_function(stdout, raw(), "test");
    }

    if (!success && debug2) {
        std::cout << "****************** FAIL *************************\n";
        jit_dump_function(stdout, raw(), "test");
    }
};

static jit_context context;

void* Lower::tryCompile(Code* code,
                        const std::unordered_map<Promise*, unsigned>& promMap) {
    PirCodeFunction function(context, code, promMap);
    function.set_optimization_level(function.max_optimization_level());
    function.build_start();
    function.build();
    function.compile();
    function.build_end();

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
