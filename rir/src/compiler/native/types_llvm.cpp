#include "types_llvm.h"
#include "builtins.h"

namespace rir {
namespace pir {

using namespace llvm;
int initializeTypes(LLVMContext& context) {
    std::vector<Type*> fields;
    t::i1 = IntegerType::get(context, 1);
    t::Int = IntegerType::get(context, 32);
    t::Double = Type::getDoubleTy(context);

    t::IntPtr = PointerType::get(t::Int, 0);
    t::DoublePtr = PointerType::get(t::Double, 0);

    t::i64 = IntegerType::get(context, 64);
    t::i32 = IntegerType::get(context, 32);
    t::i64ptr = PointerType::get(t::i64, 0);

    t::i8ptr = PointerType::get(IntegerType::get(context, 8), 0);
    t::voidPtr = t::i8ptr;
    t::charPtr = t::i8ptr;

    t::VectorLength = IntegerType::get(context, sizeof(ptrdiff_t) * 8);
    // for now, bool is the same as integer
    // t::Bool = t::Int;
    StructType* t_sxpinfo_struct =
        StructType::create(context, "struct.sxpinfo_struct");
    // sxpinfo_struct is just int64 in a structure, the bitmasking is not a
    // concern of the type
    fields = {t::i64};
    t_sxpinfo_struct->setBody(fields, false);
    // SEXPREC
    t::SEXPREC = StructType::create(context, "struct.SEXPREC");
    // SEXP
    // Addrspace == 1 -> GC managed pointer
    t::SEXP = PointerType::get(t::SEXPREC, 0);
    // SEXPREC, first the union
    t::SEXP_u1 = StructType::create(context, "union.SEXP_SEXP_SEXP");
    fields = {t::SEXP, t::SEXP, t::SEXP};
    t::SEXP_u1->setBody(fields, false);
    // now the real SEXPREC
    fields = {t_sxpinfo_struct, t::SEXP, t::SEXP, t::SEXP, t::SEXP_u1};
    t::SEXPREC->setBody(fields, false);

    t::SEXP_ptr = PointerType::get(t::SEXP, 0);

    StructType* t_vecsxp_struct =
        StructType::create(context, "struct.vecsxp_struct");
    fields = {t::i64, t::i64};
    t_vecsxp_struct->setBody(fields, false);

    fields = {t_sxpinfo_struct, t::SEXP, t::SEXP, t::SEXP, t_vecsxp_struct};
    t::VECTOR_SEXPREC = StructType::create(context, "struct.VECTOR_SEXPREC");
    t::VECTOR_SEXPREC->setBody(fields, false);

    t::VECTOR_SEXPREC_ptr = PointerType::get(t::VECTOR_SEXPREC, 0);

    t::LazyEnvironment = StructType::create(context, "LazyEnvironment");
    fields = {t::i32, t::i32, t::i32, t::i64, t::voidPtr};
    t::LazyEnvironment->setBody(fields);

    fields = {t::i32, t::i32, t::i32};
    t::RirRuntimeObject = StructType::create(context, "RirRuntimeObject");
    t::RirRuntimeObject->setBody(fields);

    t::stackCell = StructType::create(context, "R_bcstack_t");
    fields = {t::Int, t::SEXP};
    t::stackCell->setBody(fields, false);

    t::stackCellPtr = PointerType::get(t::stackCell, 0);

    // API function types
    Type* t_void = Type::getVoidTy(context);
    t::t_void = t_void;
    t::Void = t_void;

    t::setjmp_buf = StructType::create(context, "stjmp_buf");
#ifdef __APPLE__
    fields.clear();
    for (size_t i = 0; i < 38; ++i)
        fields.push_back(t::i32);
    t::setjmp_buf->setBody(fields);
#else
    auto jmp_buf_sigset_type = StructType::create(context, "stjmp_buf_sigset");
    fields.clear();
    for (size_t i = 0; i < 16; ++i)
        fields.push_back(t::i64);
    jmp_buf_sigset_type->setBody(fields);
    fields = {t::i64, t::i64, t::i64, t::i64, t::i64,
              t::i64, t::i64, t::i64, t::i32, jmp_buf_sigset_type};
    t::setjmp_buf->setBody(fields);
#endif

    t::setjmp_buf_ptr = PointerType::get(t::setjmp_buf, 0);

    t::RCNTXT = StructType::create(context, "struct.RCNTXT");
    t::RCNTXT_ptr = PointerType::get(t::RCNTXT, 0);
    fields = {
        t::RCNTXT_ptr, t::Int,     t::setjmp_buf,   t::Int,     t::Int,
        t::SEXP,       t::SEXP,    t::SEXP,         t::SEXP,    t::SEXP,
        t::SEXP,       t::voidPtr, t::voidPtr,      t::voidPtr, t::Int,
        t::Int,        t::Int,     t::SEXP,         t::voidPtr, t::SEXP,
        t::SEXP,       t::voidPtr, t::stackCellPtr, t::SEXP,    t::Int,
        t::SEXP,       t::voidPtr, t::Int,
    };
    t::RCNTXT->setBody(fields);

    t::DeoptReason = StructType::create(context, "DeoptReason");
    fields = {t::i32, t::voidPtr, t::i32};
    t::DeoptReason->setBody(fields, true);

#define DECLARE(name, ret, ...)                                                \
    fields = {__VA_ARGS__};                                                    \
    t::name = FunctionType::get(ret, fields, false)
    DECLARE(nativeFunction, t::SEXP, t::voidPtr, t::stackCellPtr, t::SEXP,
            t::SEXP);
    t::nativeFunctionPtr = PointerType::get(t::nativeFunction, 0);
    DECLARE(builtinFunction, t::SEXP, t::SEXP, t::SEXP, t::SEXP, t::SEXP);
    t::builtinFunctionPtr = PointerType::get(t::builtinFunction, 0);
    DECLARE(void_void, t_void);
    DECLARE(void_voidPtr, t_void, t::voidPtr);
    DECLARE(void_sexp, t_void, t::SEXP);
    DECLARE(void_sexpsexp, t_void, t::SEXP, t::SEXP);
    DECLARE(void_sexpsexpsexp, t_void, t::SEXP, t::SEXP, t::SEXP);
    DECLARE(sexp_sexp, t::SEXP, t::SEXP);
    DECLARE(sexp_sexpsexp, t::SEXP, t::SEXP, t::SEXP);
    DECLARE(sexp_sexpsexpsexp, t::SEXP, t::SEXP, t::SEXP, t::SEXP);
    DECLARE(sexp_sexpsexpsexpsexp, t::SEXP, t::SEXP, t::SEXP, t::SEXP, t::SEXP);
    DECLARE(sexp_sexpint, t::SEXP, t::SEXP, t::Int);
    DECLARE(sexp_sexpsexpint, t::SEXP, t::SEXP, t::SEXP, t::Int);
    DECLARE(int_sexp, t::Int, t::SEXP);
    DECLARE(int_sexpsexp, t::Int, t::SEXP, t::SEXP);
    DECLARE(int_sexpsexpsexp, t::Int, t::SEXP, t::SEXP, t::SEXP);
    DECLARE(int_sexpint, t::Int, t::SEXP, t::Int);
    DECLARE(sexp_sexp3int, t::SEXP, t::SEXP, t::SEXP, t::SEXP, t::Int);
    DECLARE(sexp_sexp3int2, t::SEXP, t::SEXP, t::SEXP, t::SEXP, t::Int, t::Int);
    DECLARE(sexp_sexp2int2, t::SEXP, t::SEXP, t::SEXP, t::Int, t::Int);
    DECLARE(sexp_double, t::SEXP, t::Double);
    DECLARE(sexp_int, t::SEXP, t::Int);
    DECLARE(int_sexp, t::Int, t::SEXP);
    DECLARE(double_sexp, t::Double, t::SEXP);
#undef DECLARE

    NativeBuiltins::forcePromise.llvmSignature = t::sexp_sexp;

    NativeBuiltins::consNr.llvmSignature = t::sexp_sexpsexp;
    NativeBuiltins::createBindingCell.llvmSignature = t::sexp_sexpsexpsexp;
    NativeBuiltins::createMissingBindingCell.llvmSignature =
        t::sexp_sexpsexpsexp;

    NativeBuiltins::ldvar.llvmSignature = t::sexp_sexpsexp;
    NativeBuiltins::ldvarForUpdate.llvmSignature = t::sexp_sexpsexp;
    NativeBuiltins::ldvarCacheMiss.llvmSignature = llvm::FunctionType::get(
        t::SEXP, {t::SEXP, t::SEXP, t::SEXP_ptr}, false);
    NativeBuiltins::stvar.llvmSignature = t::void_sexpsexpsexp;
    NativeBuiltins::stvari.llvmSignature =
        llvm::FunctionType::get(t::Void, {t::SEXP, t::Int, t::SEXP}, false);
    NativeBuiltins::defvar.llvmSignature = t::void_sexpsexpsexp;
    NativeBuiltins::starg.llvmSignature = t::void_sexpsexpsexp;
    NativeBuiltins::ldfun.llvmSignature = t::sexp_sexpsexp;
    NativeBuiltins::chkfun.llvmSignature = t::sexp_sexpsexp;

    NativeBuiltins::setCar.llvmSignature = t::void_sexpsexp;
    NativeBuiltins::setCdr.llvmSignature = t::void_sexpsexp;
    NativeBuiltins::setTag.llvmSignature = t::void_sexpsexp;

    NativeBuiltins::externalsxpSetEntry.llvmSignature =
        llvm::FunctionType::get(t::t_void, {t::SEXP, t::Int, t::SEXP}, false);

    NativeBuiltins::error.llvmSignature =
        llvm::FunctionType::get(t::t_void, {t::charPtr}, false);
    NativeBuiltins::warn.llvmSignature =
        llvm::FunctionType::get(t::t_void, {t::charPtr}, false);

    NativeBuiltins::createEnvironment.llvmSignature =
        llvm::FunctionType::get(t::SEXP, {t::SEXP, t::SEXP, t::Int}, false);
    NativeBuiltins::createStubEnvironment.llvmSignature =
        llvm::FunctionType::get(t::SEXP, {t::SEXP, t::Int, t::IntPtr, t::Int},
                                false);
    NativeBuiltins::materializeEnvironment.llvmSignature =
        llvm::FunctionType::get(t::SEXP, {t::SEXP}, false);

    NativeBuiltins::createPromise.llvmSignature =
        llvm::FunctionType::get(t::SEXP, {t::SEXP, t::SEXP}, false);
    NativeBuiltins::createPromiseNoEnvEager.llvmSignature =
        llvm::FunctionType::get(t::SEXP, {t::SEXP, t::SEXP}, false);
    NativeBuiltins::createPromiseNoEnv.llvmSignature =
        llvm::FunctionType::get(t::SEXP, {t::SEXP}, false);
    NativeBuiltins::createPromiseEager.llvmSignature =
        llvm::FunctionType::get(t::SEXP, {t::SEXP, t::SEXP, t::SEXP}, false);
    NativeBuiltins::createClosure.llvmSignature = llvm::FunctionType::get(
        t::SEXP, {t::SEXP, t::SEXP, t::SEXP, t::SEXP}, false);

    NativeBuiltins::newInt.llvmSignature =
        llvm::FunctionType::get(t::SEXP, {t::Int}, false);
    NativeBuiltins::newIntDebug.llvmSignature =
        llvm::FunctionType::get(t::SEXP, {t::Int, t::i64}, false);
    NativeBuiltins::newLgl.llvmSignature =
        llvm::FunctionType::get(t::SEXP, {t::Int}, false);
    NativeBuiltins::newReal.llvmSignature =
        llvm::FunctionType::get(t::SEXP, {t::Double}, false);
    NativeBuiltins::newIntFromReal.llvmSignature =
        llvm::FunctionType::get(t::SEXP, {t::Double}, false);
    NativeBuiltins::newLglFromReal.llvmSignature =
        llvm::FunctionType::get(t::SEXP, {t::Double}, false);
    NativeBuiltins::newRealFromInt.llvmSignature =
        llvm::FunctionType::get(t::SEXP, {t::Int}, false);

    NativeBuiltins::colon.llvmSignature =
        llvm::FunctionType::get(t::SEXP, {t::Int, t::Int}, false);

    NativeBuiltins::call.llvmSignature = llvm::FunctionType::get(
        t::SEXP, {t::voidPtr, t::Int, t::SEXP, t::SEXP, t::i64, t::i64}, false);
    NativeBuiltins::namedCall.llvmSignature = llvm::FunctionType::get(
        t::SEXP,
        {t::voidPtr, t::Int, t::SEXP, t::SEXP, t::i64, t::IntPtr, t::i64},
        false);
    NativeBuiltins::reorderedCall.llvmSignature =
        llvm::FunctionType::get(t::SEXP,
                                {t::voidPtr, t::Int, t::SEXP, t::SEXP, t::i64,
                                 t::i64, t::IntPtr, t::i64},
                                false);
    NativeBuiltins::dotsCall.llvmSignature = llvm::FunctionType::get(
        t::SEXP,
        {t::voidPtr, t::Int, t::SEXP, t::SEXP, t::i64, t::IntPtr, t::i64},
        false);
    NativeBuiltins::callBuiltin.llvmSignature = llvm::FunctionType::get(
        t::SEXP, {t::voidPtr, t::Int, t::SEXP, t::SEXP, t::i64}, false);

    NativeBuiltins::notEnv.llvmSignature = t::sexp_sexpsexpint;
    NativeBuiltins::notOp.llvmSignature = t::sexp_sexp;
    NativeBuiltins::binop.llvmSignature = t::sexp_sexpsexpint;
    NativeBuiltins::binopEnv.llvmSignature = t::sexp_sexp3int2;

    NativeBuiltins::isMissing.llvmSignature = t::int_sexpsexp;
    NativeBuiltins::asTest.llvmSignature = t::int_sexp;
    NativeBuiltins::asLogicalBlt.llvmSignature = t::int_sexp;

    NativeBuiltins::length.llvmSignature =
        llvm::FunctionType::get(t::i64, {t::SEXP}, false);
    NativeBuiltins::forSeqSize.llvmSignature = t::int_sexp;

    NativeBuiltins::makeVector.llvmSignature =
        llvm::FunctionType::get(t::SEXP, {t::Int, t::i64}, false);

    NativeBuiltins::deopt.llvmSignature = llvm::FunctionType::get(
        t::t_void, {t::voidPtr, t::SEXP, t::voidPtr, t::stackCellPtr}, false);

    NativeBuiltins::assertFail.llvmSignature = t::void_voidPtr;

    NativeBuiltins::printValue.llvmSignature = t::void_sexp;

    NativeBuiltins::matrixNcols.llvmSignature = t::int_sexp;
    NativeBuiltins::matrixNrows.llvmSignature = t::int_sexp;

    NativeBuiltins::extract11.llvmSignature = llvm::FunctionType::get(
        t::SEXP, {t::SEXP, t::SEXP, t::SEXP, t::Int}, false);
    NativeBuiltins::extract21.llvmSignature = llvm::FunctionType::get(
        t::SEXP, {t::SEXP, t::SEXP, t::SEXP, t::Int}, false);
    NativeBuiltins::extract21i.llvmSignature = llvm::FunctionType::get(
        t::SEXP, {t::SEXP, t::Int, t::SEXP, t::Int}, false);
    NativeBuiltins::extract21r.llvmSignature = llvm::FunctionType::get(
        t::SEXP, {t::SEXP, t::Double, t::SEXP, t::Int}, false);
    NativeBuiltins::extract12.llvmSignature = llvm::FunctionType::get(
        t::SEXP, {t::SEXP, t::SEXP, t::SEXP, t::SEXP, t::Int}, false);
    NativeBuiltins::extract22.llvmSignature = llvm::FunctionType::get(
        t::SEXP, {t::SEXP, t::SEXP, t::SEXP, t::SEXP, t::Int}, false);
    NativeBuiltins::extract22ii.llvmSignature = llvm::FunctionType::get(
        t::SEXP, {t::SEXP, t::Int, t::Int, t::SEXP, t::Int}, false);
    NativeBuiltins::extract22rr.llvmSignature = llvm::FunctionType::get(
        t::SEXP, {t::SEXP, t::Double, t::Double, t::SEXP, t::Int}, false);
    NativeBuiltins::extract13.llvmSignature = llvm::FunctionType::get(
        t::SEXP, {t::SEXP, t::SEXP, t::SEXP, t::SEXP, t::SEXP, t::Int}, false);

    NativeBuiltins::subassign11.llvmSignature = llvm::FunctionType::get(
        t::SEXP, {t::SEXP, t::SEXP, t::SEXP, t::SEXP, t::Int}, false);
    NativeBuiltins::subassign21.llvmSignature = llvm::FunctionType::get(
        t::SEXP, {t::SEXP, t::SEXP, t::SEXP, t::SEXP, t::Int}, false);
    NativeBuiltins::subassign21ii.llvmSignature = llvm::FunctionType::get(
        t::SEXP, {t::SEXP, t::Int, t::Int, t::SEXP, t::Int}, false);
    NativeBuiltins::subassign21rr.llvmSignature = llvm::FunctionType::get(
        t::SEXP, {t::SEXP, t::Double, t::Double, t::SEXP, t::Int}, false);
    NativeBuiltins::subassign21ir.llvmSignature = llvm::FunctionType::get(
        t::SEXP, {t::SEXP, t::Int, t::Double, t::SEXP, t::Int}, false);
    NativeBuiltins::subassign21ri.llvmSignature = llvm::FunctionType::get(
        t::SEXP, {t::SEXP, t::Double, t::Int, t::SEXP, t::Int}, false);
    NativeBuiltins::subassign12.llvmSignature = llvm::FunctionType::get(
        t::SEXP, {t::SEXP, t::SEXP, t::SEXP, t::SEXP, t::SEXP, t::Int}, false);
    NativeBuiltins::subassign22.llvmSignature = llvm::FunctionType::get(
        t::SEXP, {t::SEXP, t::SEXP, t::SEXP, t::SEXP, t::SEXP, t::Int}, false);
    NativeBuiltins::subassign22iii.llvmSignature = llvm::FunctionType::get(
        t::SEXP, {t::SEXP, t::Int, t::Int, t::Int, t::SEXP, t::Int}, false);
    NativeBuiltins::subassign22rrr.llvmSignature = llvm::FunctionType::get(
        t::SEXP, {t::SEXP, t::Double, t::Double, t::Double, t::SEXP, t::Int},
        false);
    NativeBuiltins::subassign22iir.llvmSignature = llvm::FunctionType::get(
        t::SEXP, {t::SEXP, t::Int, t::Int, t::Double, t::SEXP, t::Int}, false);
    NativeBuiltins::subassign22rri.llvmSignature = llvm::FunctionType::get(
        t::SEXP, {t::SEXP, t::Double, t::Double, t::Int, t::SEXP, t::Int},
        false);
    NativeBuiltins::subassign13.llvmSignature = llvm::FunctionType::get(
        t::SEXP, {t::SEXP, t::SEXP, t::SEXP, t::SEXP, t::SEXP, t::SEXP, t::Int},
        false);

    NativeBuiltins::nativeCallTrampoline.llvmSignature =
        llvm::FunctionType::get(t::SEXP,
                                {t::SEXP, t::Int, t::Int, t::SEXP, t::i64,
                                 t::i64, t::IntPtr, t::i64},
                                false);

    NativeBuiltins::unop.llvmSignature = t::sexp_sexpint;
    NativeBuiltins::unopEnv.llvmSignature = t::sexp_sexp2int2;

    NativeBuiltins::initClosureContext.llvmSignature = llvm::FunctionType::get(
        t::t_void, {t::SEXP, t::RCNTXT_ptr, t::SEXP, t::SEXP}, false);
    NativeBuiltins::endClosureContext.llvmSignature =
        llvm::FunctionType::get(t::t_void, {t::RCNTXT_ptr, t::SEXP}, false);

    NativeBuiltins::recordDeopt.llvmSignature = llvm::FunctionType::get(
        t::t_void, {t::SEXP, PointerType::get(t::DeoptReason, 0)}, false);

    NativeBuiltins::sumr.llvmSignature =
        llvm::FunctionType::get(t::Double, {t::SEXP}, false);
    NativeBuiltins::prodr.llvmSignature =
        llvm::FunctionType::get(t::Double, {t::SEXP}, false);

    NativeBuiltins::colonInputEffects.llvmSignature =
        llvm::FunctionType::get(t::Int, {t::SEXP, t::SEXP, t::Int}, false);
    NativeBuiltins::colonCastLhs.llvmSignature =
        llvm::FunctionType::get(t::SEXP, {t::SEXP}, false);
    NativeBuiltins::colonCastRhs.llvmSignature =
        llvm::FunctionType::get(t::SEXP, {t::SEXP, t::SEXP}, false);

    NativeBuiltins::names.llvmSignature =
        llvm::FunctionType::get(t::SEXP, {t::SEXP}, false);
    NativeBuiltins::setNames.llvmSignature =
        llvm::FunctionType::get(t::SEXP, {t::SEXP, t::SEXP}, false);
    NativeBuiltins::xlength_.llvmSignature =
        llvm::FunctionType::get(t::SEXP, {t::SEXP}, false);
    NativeBuiltins::getAttrb.llvmSignature =
        llvm::FunctionType::get(t::SEXP, {t::SEXP, t::SEXP}, false);

    NativeBuiltins::nonLocalReturn.llvmSignature =
        llvm::FunctionType::get(t_void, {t::SEXP, t::SEXP}, false);

    NativeBuiltins::clsEq.llvmSignature =
        llvm::FunctionType::get(t::i1, {t::SEXP, t::SEXP}, false);

    return 1;
}

namespace t {

Type* i1;
Type* Int;
Type* Double;
Type* Void;
Type* VectorLength;

PointerType* IntPtr;
PointerType* DoublePtr;

PointerType* SEXP;
PointerType* SEXP_ptr;

StructType* SEXP_u1;
StructType* SEXPREC;
StructType* VECTOR_SEXPREC;

StructType* LazyEnvironment;
StructType* RirRuntimeObject;

StructType* setjmp_buf;
PointerType* setjmp_buf_ptr;

PointerType* VECTOR_SEXPREC_ptr;

StructType* RCNTXT;
PointerType* RCNTXT_ptr;

StructType* DeoptReason;

Type* t_void;
Type* voidPtr;
Type* charPtr;
Type* i64;
Type* i32;
PointerType* i64ptr;
PointerType* i8ptr;

FunctionType* void_void;
FunctionType* void_voidPtr;
FunctionType* void_sexp;
FunctionType* void_sexpsexp;
FunctionType* void_sexpsexpsexp;
FunctionType* sexp_sexp;
FunctionType* sexp_sexpsexp;
FunctionType* sexp_sexpsexpsexp;
FunctionType* sexp_sexpsexpsexpsexp;
FunctionType* sexp_sexp3int;
FunctionType* sexp_sexp2int2;
FunctionType* sexp_sexp3int2;

FunctionType* sexp_sexpint;
FunctionType* sexp_sexpsexpint;
FunctionType* int_sexpsexp;
FunctionType* int_sexpsexpsexp;
FunctionType* int_sexpint;

FunctionType* sexp_double;
FunctionType* sexp_int;
FunctionType* int_sexp;
FunctionType* double_sexp;

FunctionType* void_argssexp;
FunctionType* void_argssexpsexp;
FunctionType* void_argssexpint;

FunctionType* nativeFunction;
Type* nativeFunctionPtr;
FunctionType* builtinFunction;
Type* builtinFunctionPtr;

StructType* stackCell;
PointerType* stackCellPtr;

} // namespace t
} // namespace pir
} // namespace rir
