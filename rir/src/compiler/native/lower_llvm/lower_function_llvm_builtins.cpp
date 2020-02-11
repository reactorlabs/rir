#include "lower_function_llvm.h"
#include "lower_llvm.h"
#include "nan_box.h"
#include "phi_builder.h"
#include "representation.h"
#include "variable.h"

#include "compiler/analysis/reference_count.h"
#include "compiler/native/builtins.h"
#include "compiler/native/jit_llvm.h"
#include "compiler/native/types_llvm.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/MDBuilder.h"

#include "R/BuiltinIds.h"
#include "R/Funtab.h"
#include "R/Symbols.h"
#include "R/r.h"
#include "compiler/analysis/liveness.h"
#include "compiler/pir/pir_impl.h"
#include "compiler/util/visitor.h"
#include "interpreter/LazyEnvironment.h"
#include "interpreter/builtins.h"
#include "interpreter/instance.h"
#include "runtime/DispatchTable.h"
#include "utils/Pool.h"

#include <algorithm>
#include <cassert>
#include <cstdlib>
#include <map>
#include <memory>
#include <string>
#include <vector>

namespace rir {
namespace pir {

using namespace llvm;

LLVMContext& C = rir::pir::JitLLVM::C;

bool LowerFunctionLLVM::tryInlineBuiltin(CallSafeBuiltin* b) {
    if (representationOf(b) == Representation::Integer) {
        if (b->nargs() == 2) {
            auto x = b->arg(0).val();
            auto y = b->arg(1).val();
            auto xRep = representationOf(x);
            auto yRep = representationOf(y);

            static auto bitwise = {
                blt("bitwiseShiftL"), blt("bitwiseShiftR"), blt("bitwiseAnd"),
                blt("bitwiseOr"),     blt("bitwiseXor"),
            };
            auto found =
                std::find(bitwise.begin(), bitwise.end(), b->builtinId);
            if (found != bitwise.end()) {
                const static PirType num =
                    (PirType() | RType::integer | RType::logical | RType::real)
                        .notObject()
                        .scalar();

                if (xRep == Representation::Sexp && x->type.isA(num))
                    xRep = Representation::Real;
                if (yRep == Representation::Sexp && y->type.isA(num))
                    yRep = Representation::Real;

                if (xRep != Representation::Sexp &&
                    yRep != Representation::Sexp) {

                    BasicBlock* isNaBr = nullptr;
                    auto done = BasicBlock::Create(C, "", fun);

                    auto res = phiBuilder(t::Int);

                    auto xInt = load(x, Representation::Integer);
                    auto yInt = load(y, Representation::Integer);

                    auto naCheck = [&](Value* v, llvm::Value* asInt,
                                       Representation rep) {
                        if (rep == Representation::Real) {
                            auto vv = load(v, rep);
                            if (!isNaBr)
                                isNaBr = BasicBlock::Create(C, "isNa", fun);
                            nacheck(vv, isNaBr);
                        } else {
                            assert(rep == Representation::Integer);
                            if (!isNaBr)
                                isNaBr = BasicBlock::Create(C, "isNa", fun);
                            nacheck(asInt, isNaBr);
                        }
                    };
                    naCheck(x, xInt, xRep);
                    naCheck(y, yInt, yRep);

                    switch (found - bitwise.begin()) {
                    case 0: {
                        if (!isNaBr)
                            isNaBr = BasicBlock::Create(C, "isNa", fun);
                        auto ok = BasicBlock::Create(C, "", fun);
                        auto ofl = builder.CreateICmpSLT(yInt, c(0));
                        builder.CreateCondBr(ofl, isNaBr, ok,
                                             branchMostlyFalse);
                        builder.SetInsertPoint(ok);

                        ok = BasicBlock::Create(C, "", fun);
                        ofl = builder.CreateICmpSGT(yInt, c(31));
                        builder.CreateCondBr(ofl, isNaBr, ok,
                                             branchMostlyFalse);
                        builder.SetInsertPoint(ok);

                        res.addInput(builder.CreateShl(xInt, yInt));
                        break;
                    }
                    case 1: {
                        if (!isNaBr)
                            isNaBr = BasicBlock::Create(C, "isNa", fun);
                        auto ok = BasicBlock::Create(C, "", fun);
                        auto ofl = builder.CreateICmpSLT(yInt, c(0));
                        builder.CreateCondBr(ofl, isNaBr, ok,
                                             branchMostlyFalse);
                        builder.SetInsertPoint(ok);

                        ok = BasicBlock::Create(C, "", fun);
                        ofl = builder.CreateICmpSGT(yInt, c(31));
                        builder.CreateCondBr(ofl, isNaBr, ok,
                                             branchMostlyFalse);
                        builder.SetInsertPoint(ok);

                        res.addInput(builder.CreateLShr(xInt, yInt));
                        break;
                    }
                    case 2: {
                        res.addInput(builder.CreateAnd(xInt, yInt));
                        break;
                    }
                    case 3: {
                        res.addInput(builder.CreateOr(xInt, yInt));
                        break;
                    }
                    case 4: {
                        res.addInput(builder.CreateXor(xInt, yInt));
                        break;
                    }
                    }

                    builder.CreateBr(done);

                    if (isNaBr) {
                        builder.SetInsertPoint(isNaBr);
                        res.addInput(c(NA_INTEGER));
                        builder.CreateBr(done);
                    }

                    builder.SetInsertPoint(done);
                    setVal(i, res());
                    fixVisibility();
                    return true;
                }
            }
        }
    }

    if (b->nargs() == 1) {
        auto a = load(b->callArg(0).val());
        auto irep = representationOf(b->arg(0).val());
        auto orep = representationOf(i);
        bool done = true;

        auto doTypetest = [&](int type) {
            if (irep == t::SEXP) {
                setVal(i, builder.CreateSelect(
                              builder.CreateICmpEQ(sexptype(a), c(type)),
                              constant(R_TrueValue, orep),
                              constant(R_FalseValue, orep)));
            } else {
                setVal(i, constant(R_FalseValue, orep));
            }
        };

        switch (b->builtinId) {
        case blt("length"):
            if (irep == t::SEXP) {
                llvm::Value* r = call(NativeBuiltins::length, {a});
                if (orep == t::SEXP) {
                    r = builder.CreateSelect(
                        builder.CreateICmpUGT(r, c(INT_MAX, 64)),
                        boxReal(builder.CreateUIToFP(r, t::Double)),
                        boxInt(builder.CreateTrunc(r, t::Int)));
                } else if (orep == t::Double) {
                    r = builder.CreateUIToFP(r, t::Double);
                } else {
                    assert(orep == Representation::Integer);
                    r = builder.CreateTrunc(r, t::Int);
                }
                setVal(i, r);
            } else {
                setVal(i, constant(ScalarInteger(1), orep));
            }
            break;
        case blt("names"): {
            auto itype = b->callArg(0).val()->type;
            if (itype.isA(PirType::vecs().orObject().orAttribs())) {
                if (!itype.maybeHasAttrs() && !itype.maybeObj()) {
                    setVal(i, constant(R_NilValue, t::SEXP));
                } else {
                    auto res = phiBuilder(t::SEXP);
                    auto done = BasicBlock::Create(C, "", fun);
                    auto hasAttr = BasicBlock::Create(C, "", fun);
                    auto noAttr = BasicBlock::Create(C, "", fun);
                    auto mightHaveNames = builder.CreateICmpNE(
                        attr(a), constant(R_NilValue, t::SEXP));
                    if (itype.maybeObj())
                        mightHaveNames =
                            builder.CreateOr(mightHaveNames, isObj(a));
                    builder.CreateCondBr(mightHaveNames, hasAttr, noAttr);

                    builder.SetInsertPoint(hasAttr);
                    res.addInput(callTheBuiltin());
                    builder.CreateBr(done);

                    builder.SetInsertPoint(noAttr);
                    res.addInput(constant(R_NilValue, t::SEXP));
                    builder.CreateBr(done);

                    builder.SetInsertPoint(done);
                    setVal(i, res());
                }
            } else {
                success = false;
            }
            break;
        }
        case blt("abs"): {
            if (irep == Representation::Integer) {
                assert(orep == irep);
                setVal(i, builder.CreateSelect(builder.CreateICmpSGE(a, c(0)),
                                               a, builder.CreateNeg(a)));
            } else if (irep == Representation::Real) {
                assert(orep == irep);
                setVal(i, builder.CreateSelect(builder.CreateFCmpOGE(a, c(0.0)),
                                               a, builder.CreateFNeg(a)));
            } else {
                done = false;
            }
            break;
        }
        case blt("sqrt"): {
            if (orep == Representation::Real &&
                irep == Representation::Integer) {
                a = convert(a, i->type);
                setVal(i, builder.CreateIntrinsic(Intrinsic::sqrt, {t::Double},
                                                  {a}));
            } else if (orep == irep && irep == Representation::Real) {
                setVal(i, builder.CreateIntrinsic(Intrinsic::sqrt, {t::Double},
                                                  {a}));
            } else {
                done = false;
            }
            break;
        }
        case blt("sum"):
        case blt("prod"): {
            if (irep == Representation::Integer ||
                irep == Representation::Real) {
                setVal(i, convert(a, i->type));
            } else if (orep == Representation::Real ||
                       orep == Representation::Integer) {
                assert(irep == Representation::Sexp);
                auto itype = b->callArg(0).val()->type;
                if (itype.isA(PirType::intReal())) {
                    auto trg = b->builtinId == blt("sum")
                                   ? NativeBuiltins::sumr
                                   : NativeBuiltins::prodr;
                    llvm::Value* res = call(trg, {a});
                    if (orep == Representation::Integer)
                        res = convert(res, i->type);
                    setVal(i, res);
                } else {
                    done = false;
                }
            } else {
                done = false;
            }
            break;
        }
        case blt("as.integer"):
            if (irep == Representation::Integer &&
                orep == Representation::Integer) {
                setVal(i, a);
            } else if (irep == Representation::Real &&
                       orep == Representation::Integer) {
                setVal(i, builder.CreateSelect(
                              builder.CreateFCmpUNE(a, a), c(NA_INTEGER),
                              builder.CreateFPToSI(a, t::Int)));
            } else if (irep == Representation::Real &&
                       orep == Representation::Real) {
                setVal(i, builder.CreateSelect(
                              builder.CreateFCmpUNE(a, a), a,
                              builder.CreateIntrinsic(Intrinsic::floor,
                                                      {a->getType()}, {a})));
            } else if (irep == t::SEXP) {
                auto isSimpleInt = builder.CreateAnd(
                    builder.CreateICmpEQ(attr(a),
                                         constant(R_NilValue, t::SEXP)),
                    builder.CreateICmpEQ(sexptype(a), c(INTSXP)));
                setVal(i, builder.CreateSelect(isSimpleInt, convert(a, i->type),
                                               callTheBuiltin()));
            } else {
                done = false;
            }
            break;
        case blt("is.logical"):
            if (b->arg(0).val()->type.isA(RType::logical)) {
                // ensure that logicals represented as ints are
                // handled.
                setVal(i, constant(R_TrueValue, orep));
            } else {
                doTypetest(LGLSXP);
            }
            break;
        case blt("is.complex"):
            doTypetest(CPLXSXP);
            break;
        case blt("is.character"):
            doTypetest(STRSXP);
            break;
        case blt("is.symbol"):
            doTypetest(SYMSXP);
            break;
        case blt("is.expression"):
            doTypetest(EXPRSXP);
            break;
        case blt("is.call"):
            doTypetest(LANGSXP);
            break;
        case blt("is.function"): {
            if (irep == Representation::Sexp) {
                auto t = sexptype(a);
                auto is = builder.CreateOr(
                    builder.CreateICmpEQ(t, c(CLOSXP)),
                    builder.CreateOr(builder.CreateICmpEQ(t, c(BUILTINSXP)),
                                     builder.CreateICmpEQ(t, c(SPECIALSXP))));
                setVal(i, builder.CreateSelect(is, constant(R_TrueValue, orep),
                                               constant(R_FalseValue, orep)));
            } else {
                setVal(i, constant(R_FalseValue, orep));
            }
            break;
        }
        case blt("is.na"):
            if (irep == Representation::Integer) {
                setVal(i, builder.CreateSelect(
                              builder.CreateICmpEQ(a, c(NA_INTEGER)),
                              constant(R_TrueValue, orep),
                              constant(R_FalseValue, orep)));
            } else if (irep == Representation::Real) {
                setVal(i, builder.CreateSelect(builder.CreateFCmpUNE(a, a),
                                               constant(R_TrueValue, orep),
                                               constant(R_FalseValue, orep)));
            } else {
                done = false;
            }
            break;
        case blt("bodyCode"): {
            assert(irep == Representation::Sexp && orep == irep);
            llvm::Value* res = nullptr;
            if (i->arg(0).val()->type.isA(RType::closure)) {
                res = cdr(a);
            } else {
                res = builder.CreateSelect(
                    builder.CreateICmpEQ(c(CLOSXP), sexptype(a)), cdr(a),
                    constant(R_NilValue, t::SEXP));
            }
            setVal(i, res);
            break;
        }
        case blt("environment"):
            if (!i->arg(0).val()->type.isA(RType::closure)) {
                success = false;
                break;
            }
            assert(irep == Representation::Sexp && orep == irep);
            setVal(i, tag(a));
            break;
        default:
            done = false;
        };
        if (done) {
            fixVisibility();
            break;
        }
    }

    if (b->nargs() == 2) {
        bool success = false;
        auto arep = representationOf(b->arg(0).val());
        auto brep = representationOf(b->arg(1).val());
        auto orep = representationOf(b);
        auto aval = load(b->arg(0).val());
        auto bval = load(b->arg(1).val());

        switch (b->builtinId) {
        case blt("vector"): {
            auto l = b->arg(1).val();
            if (l->type.isA(PirType::simpleScalarInt())) {
                if (auto con = LdConst::Cast(b->arg(0).val())) {
                    if (TYPEOF(con->c()) == STRSXP && XLENGTH(con->c()) == 1) {
                        SEXPTYPE type = str2type(CHAR(STRING_ELT(con->c(), 0)));
                        switch (type) {
                        case LGLSXP:
                        case INTSXP:
                        case REALSXP:
                        case CPLXSXP:
                        case STRSXP:
                        case EXPRSXP:
                        case VECSXP:
                        case RAWSXP:
                            setVal(i,
                                   call(NativeBuiltins::makeVector,
                                        {c(type),
                                         builder.CreateZExt(
                                             load(l, Representation::Integer),
                                             t::i64)}));
                            success = true;
                            break;
                        default: {
                        }
                        }
                    }
                }
            }
            break;
        }
        case blt("min"):
        case blt("max"): {
            bool isMin = b->builtinId == blt("min");
            if (arep == Representation::Integer &&
                brep == Representation::Integer &&
                orep != Representation::Real) {
                auto res = builder.CreateSelect(
                    isMin ? builder.CreateICmpSLT(bval, aval)
                          : builder.CreateICmpSLT(aval, bval),
                    bval, aval);
                if (orep == Representation::Integer) {
                    setVal(i, res);
                } else {
                    assert(orep == Representation::Sexp);
                    setVal(i, boxInt(res, false));
                }
                success = true;
            } else if (arep == Representation::Real &&
                       brep == Representation::Real &&
                       orep != Representation::Integer) {
                auto res = builder.CreateSelect(
                    isMin ? builder.CreateFCmpUGT(bval, aval)
                          : builder.CreateFCmpUGT(aval, bval),
                    aval, bval);
                if (orep == Representation::Real) {
                    setVal(i, res);
                } else {
                    assert(orep == Representation::Sexp);
                    setVal(i, boxReal(res, false));
                }
                success = true;
            }
            break;
        }
        }
        if (success) {
            fixVisibility();
            return true;
        }
    }

    if (b->builtinId == blt("c")) {
        bool allInt = true;
        bool allReal = true;
        b->eachCallArg([&](Value* v) {
            if (representationOf(v) != Representation::Real)
                allReal = false;
            if (representationOf(v) != Representation::Integer)
                allInt = false;
        });
        if (allInt || allReal) {
            auto res =
                call(NativeBuiltins::makeVector,
                     {allInt ? c(INTSXP) : c(REALSXP), c(b->nCallArgs(), 64)});
            auto pos = 0;
            auto resT =
                PirType(allInt ? RType::integer : RType::real).notObject();

            b->eachCallArg([&](Value* v) {
                assignVector(res, c(pos), load(v), resT);
                pos++;
            });
            setVal(i, res);
            fixVisibility();
            return true;
        }
    }

    if (b->builtinId == blt("list")) {
        auto res = call(NativeBuiltins::makeVector,
                        {c(VECSXP), c(b->nCallArgs(), 64)});
        protectTemp(res);
        auto pos = 0;
        auto resT = PirType(RType::vec).notObject();

        b->eachCallArg([&](Value* v) {
            assignVector(res, c(pos), loadSxp(v), resT);
            pos++;
        });
        setVal(i, res);
        fixVisibility();
        return true;
    }

    return false;
};

} // namespace pir
} // namespace rir