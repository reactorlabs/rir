#include "Types.h"

#include "llvm/CodeGen/GCStrategy.h"
#include "llvm/CodeGen/GCs.h"

#include "RIntlns.h"

using namespace llvm;

namespace {
PointerType* initializeTypes() {
    using namespace rjit;
    LLVMContext& context = getGlobalContext();
    std::vector<Type*> fields;
    t::Int = IntegerType::get(context, 32);
    t::Double = Type::getDoubleTy(context);
    t::Bool = IntegerType::get(context, 1);
    t::VectorLength = IntegerType::get(context, sizeof(ptrdiff_t) * 8);
    // for now, bool is the same as integer
    // t::Bool = t::Int;
    StructType* t_sxpinfo_struct =
        StructType::create(context, "struct.sxpinfo_struct");
    // sxpinfo_struct is just int32 in a structure, the bitmasking is not a
    // concern of the type
    fields = {t::Int};
    t_sxpinfo_struct->setBody(fields, false);
    // SEXPREC
    t::SEXPREC = StructType::create(context, "struct.SEXPREC");
    // SEXP
    // Addrspace == 1 -> GC managed pointer
    t::SEXP = PointerType::get(t::SEXPREC, 1);
    // SEXPREC, first the union
    t::SEXP_u1 = StructType::create(context, "union.SEXP_SEXP_SEXP");
    fields = {t::SEXP, t::SEXP, t::SEXP};
    t::SEXP_u1->setBody(fields, false);
    // now the real SEXPREC
    fields = {t_sxpinfo_struct, t::SEXP, t::SEXP, t::SEXP, t::SEXP_u1};
    t::SEXPREC->setBody(fields, false);

    StructType* t_vecsxp_struct =
        StructType::create(context, "struct.vecsxp_struct");
    fields = {t::Int, t::Int};
    t_vecsxp_struct->setBody(fields, false);

    fields = {t_sxpinfo_struct, t_vecsxp_struct, t::SEXP, t::SEXP, t::SEXP};
    t::VECTOR_SEXPREC = StructType::create(context, "struct.VECTOR_SEXPREC");
    t::VECTOR_SEXPREC->setBody(fields, false);

    // API function types
    Type* t_void = Type::getVoidTy(context);
    t::t_void = t_void;
    t::Void = t_void;

    t::t_i64 = IntegerType::get(context, 64);

    // TODO: probably not the best idea...
    t::cntxt = StructType::create(context, "struct.RCNTXT");
    std::vector<Type*> cntxtbod(360 /* sizeof(RCNTXT) */,
                                IntegerType::get(context, 8));
    t::cntxt->setBody(cntxtbod);
    t::cntxtPtr = PointerType::get(t::cntxt, 0);

    t::i8ptr = PointerType::get(IntegerType::get(context, 8), 0);

    // FIXME
    t::voidPtr = PointerType::get(t::t_i64, 0);

#define DECLARE(name, ret, ...)                                                \
    fields = {__VA_ARGS__};                                                    \
    t::name = FunctionType::get(ret, fields, false)
    DECLARE(nativeFunction_t, t::SEXP, t::SEXP, t::SEXP, t::SEXP);
    t::nativeFunctionPtr_t = PointerType::get(t::nativeFunction_t, 0);
    DECLARE(void_void, t_void);
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
    DECLARE(void_cntxtsexpsexpsexpsexpsexp, t_void, t::cntxtPtr, t::SEXP,
            t::SEXP, t::SEXP, t::SEXP, t::SEXP);
    DECLARE(void_cntxtsexp, t_void, t::cntxtPtr, t::SEXP);
    DECLARE(void_cntxtsexpsexp, t_void, t::cntxtPtr, t::SEXP, t::SEXP);
    DECLARE(sexp_contxtsexpsexp, t::SEXP, t::cntxtPtr, t::SEXP, t::SEXP);
#undef DECLARE

    return t::SEXP;
}
}

namespace rjit {

namespace t {

Type* Int;
Type* Double;
Type* Void;
Type* Bool;
Type* VectorLength;

PointerType* SEXP = initializeTypes();

StructType* SEXP_u1;
StructType* SEXPREC;
StructType* VECTOR_SEXPREC;

StructType* cntxt;
PointerType* cntxtPtr;

Type* t_void;
Type* voidPtr;
Type* t_i64;
PointerType* i8ptr;

FunctionType* void_void;
FunctionType* void_sexp;
FunctionType* void_sexpsexp;
FunctionType* void_sexpsexpsexp;
FunctionType* sexp_sexp;
FunctionType* sexp_sexpsexp;
FunctionType* sexp_sexpsexpsexp;
FunctionType* sexp_sexpsexpsexpsexp;

FunctionType* sexp_sexpint;
FunctionType* sexp_sexpsexpint;
FunctionType* int_sexp;
FunctionType* int_sexpsexp;
FunctionType* int_sexpsexpsexp;
FunctionType* int_sexpint;

FunctionType* void_argssexp;
FunctionType* void_argssexpsexp;
FunctionType* void_argssexpint;

FunctionType* void_cntxtsexpsexpsexpsexpsexp;
FunctionType* void_cntxtsexp;
FunctionType* void_cntxtsexpsexp;
FunctionType* sexp_contxtsexpsexp;

FunctionType* nativeFunction_t;
Type* nativeFunctionPtr_t;

} // namespace t

} // namespace rjit
