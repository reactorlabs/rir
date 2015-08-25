#ifndef COMPILER_CPP
#define COMPILER_CPP

#include <cstdint>
#include <sstream>
#include <iostream>

#include <llvm/IR/Verifier.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/Support/raw_ostream.h>
#include "llvm/Analysis/Passes.h"

#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/MCJIT.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"


#include "compiler.h"
#include "stack_map.h"

using namespace llvm;

namespace {


namespace symbol {
#define DECLARE(name, txt) SEXP name = Rf_install(txt)

DECLARE(Block, "{");
DECLARE(Parenthesis, "(");
DECLARE(Assign, "<-");
DECLARE(Assign2, "=");
DECLARE(SuperAssign, "<<-");
DECLARE(If, "if");
DECLARE(Function, "function");
DECLARE(Return, "return");
DECLARE(For, "for");
DECLARE(While, "while");
DECLARE(Repeat, "repeat");
DECLARE(Break, "break");
DECLARE(Next, "next");
DECLARE(Switch, "switch");
DECLARE(Add, "+");
DECLARE(Sub, "-");
DECLARE(Mul, "*");
DECLARE(Div, "/");
DECLARE(Pow, "^");
DECLARE(Sqrt, "sqrt");
DECLARE(Exp, "exp");
DECLARE(Eq, "==");
DECLARE(Ne, "!=");
DECLARE(Lt, "<");
DECLARE(Le, "<=");
DECLARE(Ge, ">=");
DECLARE(Gt, ">");
DECLARE(BitAnd, "&");
DECLARE(BitOr, "|");
DECLARE(Not, "!");
DECLARE(Ellipsis, "...");


#undef DECLARE
}

PointerType * initializeTypes();


// Functions to call in the debugger:
static void printType(Type * t) {
    std::string type_str;
    llvm::raw_string_ostream rso(type_str);
    t->print(rso);
    std::cout << "Type: " << rso.str() << std::endl;
}
static void printTypeOf(Value * v) {
    Type * t = v->getType();
    printType(t);
}
static void disassNative(SEXP native) {
    ((Function*)TAG(native))->dump();
}


namespace t {

Type * Int;

PointerType * SEXP = initializeTypes();

StructType * SEXPREC;

StructType * cntxt;
PointerType * cntxtPtr;

StructType * CallArgs;
PointerType * CallArgsPtr;

PointerType * i8ptr;

FunctionType * void_void;
FunctionType * void_sexp;
FunctionType * void_sexpsexp;
FunctionType * void_sexpsexpsexp;
FunctionType * sexp_sexp;
FunctionType * sexp_sexpsexp;
FunctionType * sexp_sexpsexpsexp;
FunctionType * sexp_sexpsexpsexpsexp;

FunctionType * sexp_sexpint;
FunctionType * sexp_sexpsexpint;
FunctionType * int_sexp;
FunctionType * int_sexpsexp;
FunctionType * int_sexpsexpsexp;
FunctionType * int_sexpint;

FunctionType * void_argssexp;
FunctionType * void_argssexpsexp;
FunctionType * void_argssexpint;

FunctionType * void_cntxtsexpsexpsexpsexpsexp;
FunctionType * void_cntxtsexp;
FunctionType * sexp_contxtsexpsexp;
}

PointerType * initializeTypes() {
    LLVMContext & context = getGlobalContext();
    std::vector<Type*> fields;
    t::Int = IntegerType::get(context, 32);
    StructType * t_sxpinfo_struct = StructType::create(context, "struct.sxpinfo_struct");
    // sxpinfo_struct is just int32 in a structure, the bitmasking is not a concern of the type
    fields = { t::Int };
    t_sxpinfo_struct->setBody(fields, false);
    // SEXPREC
    t::SEXPREC = StructType::create(context, "struct.SEXPREC");
    // SEXP
    t::SEXP = PointerType::get(t::SEXPREC, 0);
    // SEXPREC, first the union
    StructType * u1 = StructType::create(context,"union.SEXP_SEXP_SEXP");
    fields = { t::SEXP, t::SEXP, t::SEXP };
    u1->setBody(fields, false);
    // now the real SEXPREC
    fields = { t_sxpinfo_struct, t::SEXP, t::SEXP, t::SEXP, u1 };
    t::SEXPREC->setBody(fields, false);
    // call header and pointer
    t::CallArgs = StructType::create(context, "struct.CallHeader");
    fields = { t::SEXP, t::SEXP };
    t::CallArgs->setBody(fields);
    t::CallArgsPtr = PointerType::get(t::CallArgs, 0);
    // API function types
    Type * t_void = Type::getVoidTy(context);

    // TODO: probably not the best idea...
    t::cntxt = StructType::create(context, "struct.RCNTXT");
    std::vector<Type*> cntxtbod(360 /* sizeof(RCNTXT) */, IntegerType::get(context, 8)); 
    t::cntxt->setBody(cntxtbod);
    t::cntxtPtr = PointerType::get(t::cntxt, 0);

    t::i8ptr = PointerType::get(IntegerType::get(context, 8), 0);
#define DECLARE(name, ret, ...) fields = { __VA_ARGS__ }; t::name = FunctionType::get(ret, fields, false)
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
    DECLARE(void_argssexp, t_void, t::CallArgsPtr, t::SEXP);
    DECLARE(void_argssexpsexp, t_void, t::CallArgsPtr, t::SEXP, t::SEXP);
    DECLARE(void_argssexpint, t_void, t::CallArgsPtr, t::SEXP, t::Int);
    DECLARE(void_cntxtsexpsexpsexpsexpsexp, t_void, t::cntxtPtr, t::SEXP, t::SEXP, t::SEXP, t::SEXP, t::SEXP);
    DECLARE(void_cntxtsexp, t_void, t::cntxtPtr, t::SEXP);
    DECLARE(sexp_contxtsexpsexp, t::SEXP, t::cntxtPtr, t::SEXP, t::SEXP);
#undef DECLARE

    // initialize LLVM backend
    LLVMInitializeNativeTarget();
    LLVMInitializeNativeAsmPrinter();
    LLVMInitializeNativeAsmParser();
    return t::SEXP;
}


std::vector<std::pair<uint8_t*, uintptr_t> > stackmaps;

class JITMemoryManager : public llvm::SectionMemoryManager {
private:
    JITMemoryManager(const JITMemoryManager&) LLVM_DELETED_FUNCTION;
    void operator=(const JITMemoryManager&) LLVM_DELETED_FUNCTION;

    struct MemoryGroup {
           SmallVector<sys::MemoryBlock, 16> AllocatedMem;
           SmallVector<sys::MemoryBlock, 16> FreeMem;
           sys::MemoryBlock Near;
    };

public:
    JITMemoryManager() {};

    uint64_t getSymbolAddress(const std::string &name) override {
        return SectionMemoryManager::getSymbolAddress(name);
    }

    uint8_t * allocateDataSection(
            uintptr_t size, unsigned alignment, unsigned sectionID,
            StringRef sectionName, bool readonly) override {

        auto res = SectionMemoryManager::allocateDataSection(
                size, alignment, sectionID, sectionName, readonly);

        if (sectionName.str() == ".llvm_stackmaps")
            stackmaps.push_back(std::pair<uint8_t*, uintptr_t>(res, size));

        return res;
    }

    uint8_t * allocateCodeSection(uintptr_t Size,
                                  unsigned Alignment,
                                  unsigned SectionID,
                                  StringRef SectionName) {
      return allocateSection(CodeMem, Size, Alignment);
    }
    
private:
    uint8_t * allocateSection(MemoryGroup &MemGroup,
                              uintptr_t Size,
                              unsigned Alignment) {

        if (!Alignment)
          Alignment = 16;

        assert(!(Alignment & (Alignment - 1)) && "Alignment must be a power of two.");

        uintptr_t RequiredSize = Alignment * ((Size + Alignment - 1)/Alignment + 1);
        uintptr_t Addr = 0;

        // Look in the list of free memory regions and use a block there if one
        // is available.
        for (int i = 0, e = MemGroup.FreeMem.size(); i != e; ++i) {
          sys::MemoryBlock &MB = MemGroup.FreeMem[i];
          if (MB.size() >= RequiredSize) {
            Addr = (uintptr_t)MB.base();
            uintptr_t EndOfBlock = Addr + MB.size();
            // Align the address.
            Addr = (Addr + Alignment - 1) & ~(uintptr_t)(Alignment - 1);
            // Store cutted free memory block.
            MemGroup.FreeMem[i] = sys::MemoryBlock((void*)(Addr + Size),
                                                   EndOfBlock - Addr - Size);
            return (uint8_t*)Addr;
          }
        }

        // No pre-allocated free block was large enough. Allocate a new memory region.
        // Note that all sections get allocated as read-write.  The permissions will
        // be updated later based on memory group.
        //
        // FIXME: It would be useful to define a default allocation size (or add
        // it as a constructor parameter) to minimize the number of allocations.
        //
        // FIXME: Initialize the Near member for each memory group to avoid
        // interleaving.
        std::error_code ec;
        sys::MemoryBlock MB = sys::Memory::allocateMappedMemory(RequiredSize,
                                                                &MemGroup.Near,
                                                                sys::Memory::MF_READ |
                                                                  sys::Memory::MF_WRITE,
                                                                ec);
        if (ec) {
          // FIXME: Add error propagation to the interface.
          return nullptr;
        }

        // Save this address as the basis for our next request
        MemGroup.Near = MB;

        MemGroup.AllocatedMem.push_back(MB);
        Addr = (uintptr_t)MB.base();
        uintptr_t EndOfBlock = Addr + MB.size();

        // Align the address.
        Addr = (Addr + Alignment - 1) & ~(uintptr_t)(Alignment - 1);

        // The allocateMappedMemory may allocate much more memory than we need. In
        // this case, we store the unused memory as a free memory block.
        unsigned FreeSize = EndOfBlock-Addr-Size;
        if (FreeSize > 16)
          MemGroup.FreeMem.push_back(sys::MemoryBlock((void*)(Addr + Size), FreeSize));

        // Return aligned address
        return (uint8_t*)Addr;
    }

    bool finalizeMemory(std::string *ErrMsg) override {
        if (SectionMemoryManager::finalizeMemory(ErrMsg))
            return true;

        for (int i = 0, e = CodeMem.AllocatedMem.size(); i != e; ++i) {
            std::error_code ec = 
                sys::Memory::protectMappedMemory(
                        CodeMem.AllocatedMem[i],
                        sys::Memory::MF_READ | sys::Memory::MF_EXEC |
                        sys::Memory::MF_WRITE);
            if (ec) {
                if (ErrMsg) {
                    *ErrMsg = ec.message();
                }
                return true;
            }
        }

        return false;
    }

    ~JITMemoryManager() {
      for (unsigned i = 0, e = CodeMem.AllocatedMem.size(); i != e; ++i)
        sys::Memory::releaseMappedMemory(CodeMem.AllocatedMem[i]);
    }
     
    MemoryGroup CodeMem;
};


#define DECLARE(name, type) Function * name = Function::Create(t::type, Function::ExternalLinkage, #name, m)


class JITModule {
public:
    Module * m;
    DECLARE(markVisible, void_void);
    DECLARE(markInvisible, void_void);
    DECLARE(userConstant, void_sexp);
    DECLARE(genericGetVar, sexp_sexpsexp);
    DECLARE(checkFunction, void_sexp);
    DECLARE(getFunction, sexp_sexpsexp);
    DECLARE(sexpType, int_sexp);
    DECLARE(call, sexp_sexpsexpsexpsexp);
    DECLARE(callNative, sexp_sexpsexp);
    DECLARE(createPromise, sexp_sexpsexp);
//    DECLARE(createArgument, sexp_sexp);
//    DECLARE(createKeywordArgument, sexp_sexpsexp);
//    DECLARE(addArgument, sexp_sexpsexp);
//    DECLARE(addKeywordArgument, sexp_sexpsexpsexp);
    DECLARE(genericUnaryMinus, sexp_sexpsexpsexp);
    DECLARE(genericUnaryPlus, sexp_sexpsexpsexp);
    DECLARE(genericAdd, sexp_sexpsexpsexpsexp);
    DECLARE(genericSub, sexp_sexpsexpsexpsexp);
    DECLARE(genericMul, sexp_sexpsexpsexpsexp);
    DECLARE(genericDiv, sexp_sexpsexpsexpsexp);
    DECLARE(genericPow, sexp_sexpsexpsexpsexp);
    DECLARE(genericSqrt, sexp_sexpsexpsexp);
    DECLARE(genericExp, sexp_sexpsexpsexp);
    DECLARE(genericEq, sexp_sexpsexpsexpsexp);
    DECLARE(genericNe, sexp_sexpsexpsexpsexp);
    DECLARE(genericLe, sexp_sexpsexpsexpsexp);
    DECLARE(genericLt, sexp_sexpsexpsexpsexp);
    DECLARE(genericGt, sexp_sexpsexpsexpsexp);
    DECLARE(genericGe, sexp_sexpsexpsexpsexp);
    DECLARE(genericBitAnd, sexp_sexpsexpsexpsexp);
    DECLARE(genericBitOr, sexp_sexpsexpsexpsexp);
    DECLARE(genericNot, sexp_sexpsexpsexp);
    DECLARE(genericSetVar, void_sexpsexpsexp);
    DECLARE(genericSetVarParent, void_sexpsexpsexp);
    DECLARE(convertToLogicalNoNA, int_sexpsexp);
    DECLARE(createClosure, sexp_sexpsexpsexp);
    DECLARE(returnJump, void_sexpsexp);
    DECLARE(startFor, sexp_sexpsexp);
    DECLARE(loopSequenceLength, int_sexpsexp);
    DECLARE(getForLoopValue, sexp_sexpint);
    DECLARE(checkSwitchControl, void_sexpsexp);
    DECLARE(switchControlInteger, int_sexpint);
    DECLARE(switchControlCharacter, int_sexpsexpsexp);
    DECLARE(addArgument, void_argssexp);
    DECLARE(addKeywordArgument, void_argssexpsexp);
    DECLARE(addEllipsisArgument, void_argssexpint);
    DECLARE(CONS_NR, sexp_sexpsexp);
    DECLARE(closureQuickArgumentAdaptor, sexp_sexpsexp);
    DECLARE(initClosureContext, void_cntxtsexpsexpsexpsexpsexp);
    DECLARE(endClosureContext, void_cntxtsexp);
    DECLARE(closureNativeCallTrampoline, sexp_contxtsexpsexp);

    Function * patchpoint;

    JITModule(std::string const & name): m(new Module(name, getGlobalContext())) {
        auto mod = m;
        {
            /* declare i64 @llvm.experimental.patchpoint.i64(i64, i32, i8*, i32, ...) */

            // Type Definitions
            std::vector<Type*>FuncTy_0_args;
            FuncTy_0_args.push_back(IntegerType::get(mod->getContext(), 64));
            FuncTy_0_args.push_back(IntegerType::get(mod->getContext(), 32));
            
            FuncTy_0_args.push_back(t::i8ptr);
            FuncTy_0_args.push_back(IntegerType::get(mod->getContext(), 32));
            FunctionType* FuncTy_0 = FunctionType::get(
             /*Result=*/IntegerType::get(mod->getContext(), 64),
             /*Params=*/FuncTy_0_args,
             /*isVarArg=*/true);
            
            // Function Declarations
            
            Function* func_llvm_experimental_patchpoint_i64 = mod->getFunction("llvm.experimental.patchpoint.i64");
            if (!func_llvm_experimental_patchpoint_i64) {
            func_llvm_experimental_patchpoint_i64 = Function::Create(
             /*Type=*/FuncTy_0,
             /*Linkage=*/GlobalValue::ExternalLinkage,
             /*Name=*/"llvm.experimental.patchpoint.i64", mod); // (external, no body)
            func_llvm_experimental_patchpoint_i64->setCallingConv(CallingConv::C);
            }
            AttributeSet func_llvm_experimental_patchpoint_i64_PAL;
            func_llvm_experimental_patchpoint_i64->setAttributes(func_llvm_experimental_patchpoint_i64_PAL);

            patchpoint = func_llvm_experimental_patchpoint_i64;
        }
    }

    operator Module * () {
        return m;
    }

    LLVMContext & getContext() {
        return m->getContext();
    }

};

#undef DECLARE


SEXP createNativeSXP(RFunctionPtr fptr, SEXP ast, std::vector<SEXP> const & objects, Function * f) {
    SEXP objs = allocVector(VECSXP, objects.size() + 1);
    PROTECT(objs);
    SET_VECTOR_ELT(objs, 0, ast);
    for (size_t i = 0; i < objects.size(); ++i)
        SET_VECTOR_ELT(objs, i +1, objects[i]);
    SEXP result = CONS(reinterpret_cast<SEXP>(fptr), objs);
    UNPROTECT(objects.size() + 1); // all objects in objects + objs itself which is now part of result
    SET_TAG(result, reinterpret_cast<SEXP>(f));
    SET_TYPEOF(result, NATIVESXP);
    return result;
}

static void * CallICStub0(SEXP, SEXP, SEXP, uintptr_t, uintptr_t);
static void * CallICStub1(SEXP, SEXP, SEXP, SEXP, uintptr_t, uintptr_t);
static void * CallICStub2(SEXP, SEXP, SEXP, SEXP, SEXP, uintptr_t, uintptr_t);

Value * insertICCallStub(
        std::vector<Value*> & callArgs, Value * function, Value * call, Value * rho, JITModule & m, BasicBlock * b, Value * f, uint32_t id) {

    /*  %result = call i64 (i64, i32, i8*, i32, ...)*
     *            @llvm.experimental.patchpoint.i64(i64 id, i32 15, i8* %target, i32 2, i64 %arg1, i64 %arg2)
     */

    ConstantInt* const_int_id       = ConstantInt::get(m.getContext(), APInt(64, (uint64_t)id, false));
    ConstantInt* const_int_bs       = ConstantInt::get(m.getContext(), APInt(32, StringRef("15"), 10));

    uint64_t targetAddr;
    switch(callArgs.size()) {
        case 0: targetAddr = (uint64_t)&CallICStub0; break;
        case 1: targetAddr = (uint64_t)&CallICStub1; break;
        case 2: targetAddr = (uint64_t)&CallICStub2; break;
        default: asm("int3"); //TODO Generate the icStubs!
    }
    ConstantInt* const_int64_target = ConstantInt::get(m.getContext(), APInt(64, targetAddr, false));
    CastInst* ptr_target            = new IntToPtrInst(const_int64_target, t::i8ptr, "target", b);

    ConstantInt* const_int32_numarg = ConstantInt::get(m.getContext(), APInt(32, callArgs.size() + 5, false));

    std::vector<Value*> int64_result_params;
    // Patchpoint argumenst (compiled away)
    int64_result_params.push_back(const_int_id);
    int64_result_params.push_back(const_int_bs);
    int64_result_params.push_back(ptr_target);
    int64_result_params.push_back(const_int32_numarg);

    // Closure arguments
    for (auto arg : callArgs) {
        int64_result_params.push_back(arg);
    }

    // Additional IC arguments
    int64_result_params.push_back(call);
    int64_result_params.push_back(function);
    int64_result_params.push_back(rho);
    int64_result_params.push_back(f);
    int64_result_params.push_back(const_int_id);

    CallInst* int64_result = CallInst::Create(m.patchpoint, int64_result_params, "", b);
    int64_result->setCallingConv(CallingConv::C);
    int64_result->setTailCall(false);
    AttributeSet int64_result_PAL;
    int64_result->setAttributes(int64_result_PAL);

    return new IntToPtrInst(int64_result, t::SEXP, "res", b);
}


#define ARGS(...) std::vector<Value *>({ __VA_ARGS__ })
#define INTRINSIC(name, ...) CallInst::Create(name, ARGS(__VA_ARGS__), "", context->b)
#define JUMP(block) BranchInst::Create(block, context->b);


class Compiler {
public:

    Compiler(std::string const & moduleName):
        m(moduleName) {}

    SEXP compile(std::string const & name, SEXP bytecode) {
        SEXP result = compileFunction(name, bytecode);

        auto memoryManager = new JITMemoryManager();
        // create execution engine and finalize the module
        std::string err;
        ExecutionEngine *engine = EngineBuilder(std::unique_ptr<Module>(m))
            .setErrorStr(&err)
            .setMCJITMemoryManager(
                      std::unique_ptr<RTDyldMemoryManager>(memoryManager))
            .setEngineKind(EngineKind::JIT)
            .create();

        if (!engine) {
          fprintf(stderr, "Could not create ExecutionEngine: %s\n", err.c_str());
          exit(1);
        }

        engine->finalizeObject();

        // perform all the relocations
        for (SEXP s : relocations) {
            auto f = reinterpret_cast<Function*>(TAG(s));
//            f->dump();
            SETCAR(s, reinterpret_cast<SEXP>(engine->getPointerToFunction(f)));
        }
        return result;
    }

private:
    class Context {
    public:

        Context(std::string const & name, Module * m) {
            f = Function::Create(t::sexp_sexpsexpint, Function::ExternalLinkage, name, m);
            Function::arg_iterator args = f->arg_begin();
            Value * body = args++;
            body->setName("body");
            rho = args++;
            rho->setName("rho");
            Value * useCache = args++;
            useCache->setName("useCache");
            b = BasicBlock::Create(getGlobalContext(), "start", f, nullptr);
            returnJump = false;
        }

        void addObject(SEXP object) {
            PROTECT(object);
            objects.push_back(object);
        }

        /** True if return jump is needed instead of return - this happens in promises
         */
        bool returnJump;

        /** True if result of the expression should be visible, false otherwise. Each expression resets the visibleResult to true.
         */
        bool visibleResult;

        Function * f;

        BasicBlock * b;

        /** Basic block to which break() statements should jump.
         */
        BasicBlock * breakBlock;

        /** Basic block to which next() statements should jump.
         */
        BasicBlock * nextBlock;

        Value * rho;

        std::vector<SEXP> objects;

    };

    SEXP compileFunction(std::string const & name, SEXP ast, bool isPromise = false) {
        Context * old = context;
        context = new Context(name, m);
        if (isPromise)
            context->returnJump = true;
        Value * last = compileExpression(ast);
        // since we are going to insert implicit return, which is a simple return even from a promise
        context->returnJump = false;
        if (last != nullptr)
            compileReturn(last, /*tail=*/ true);
        // now we create the NATIVESXP
        SEXP result = createNativeSXP(nullptr, ast, context->objects, context->f);
        // add the non-jitted SEXP to relocations
        relocations.push_back(result);
        delete context;
        context = old;
        return result;
    }

    /** Compiles an expression.

      The expression as a result is always visible by default, which can be changed in the respective compiling functions.

      An expression is either a constant, or symbol (variable read), or a function call.
      */
    Value * compileExpression(SEXP value) {
        context->visibleResult = true;
        switch (TYPEOF(value)) {
        case SYMSXP:
            return compileSymbol(value);
        case LANGSXP:
            return compileCall(value);
        case LGLSXP:
        case INTSXP:
        case REALSXP:
        case CPLXSXP:
        case STRSXP:
            return compileConstant(value);
        default:
            assert(false && "Unknown SEXP type in compiled ast.");
        }
    }

    /** Compiles user constant, which is SEXP constant marked with userConstant intrinsic.
      */
    Value * compileConstant(SEXP value) {
        Value * result = constant(value);
        INTRINSIC(m.userConstant, result);
        return result;
    }

    /** Compiles a symbol, which reads as variable read using genericGetVar intrinsic.
      */
    Value * compileSymbol(SEXP value) {
        return INTRINSIC(m.genericGetVar, constant(value), context->rho);
    }

    Value * compileCall(SEXP call) {
        Value * f;

        if (TYPEOF(CAR(call)) != SYMSXP) {
            // it is a complex function, first get the value of the function and then check it
            f = compileExpression(CAR(call));
            INTRINSIC(m.checkFunction, f);
        } else {
            // it is simple function - try compiling it with intrinsics
            f = compileIntrinsic(call);
            if (f != nullptr)
                return f;
            // otherwise just do get function
            f = INTRINSIC(m.getFunction, constant(CAR(call)), context->rho);
        }

        std::vector<Value*> args;
        compileArguments(CDR(call), args);

        return insertICCallStub(args, f, constant(call), context->rho, m, context->b, context->f, stackMapId++);
    }

    void compileArguments(SEXP argAsts, std::vector<Value*> & res) {
         while (argAsts != R_NilValue) {
             res.push_back(
                     compileArgument(CAR(argAsts), TAG(argAsts)));
             argAsts = CDR(argAsts);
         }
    }

    Value * compileArgument(SEXP arg, SEXP name) {
        Value * result;
        switch (TYPEOF(arg)) {
        case LGLSXP:
        case INTSXP:
        case REALSXP:
        case CPLXSXP:
        case STRSXP:
            // literals are self-evaluating
            return constant(arg);
            break;
        case SYMSXP:
            if (arg == R_DotsSymbol) {
                return constant(arg);
            }
        default: {
                SEXP code = compileFunction("promise", arg, /*isPromise=*/ true);
                context->addObject(code);
                return constant(code);
            }
        }
    }



#ifdef HAHA
    /** Compiles a call expressed in the AST.

      For simple calls checks if these can be compiled using intrinsics and does so, if possible. For others emits the function getting / checking code and then generates the arguments and calls the function.

      This differs depending on the function type. Special functions do not evaluate their arguments at all, builtin functions are always eager and normal functions are lazy.
     */
    Value * compileCall(SEXP call) {
        Value * f;
        if (TYPEOF(CAR(call)) != SYMSXP) {
            // it is a complex function, first get the value of the function and then check it
            f = compileExpression(CAR(call));
            INTRINSIC(m.checkFunction, f);
        } else {
            // it is simple function - try compiling it with intrinsics
            f = compileIntrinsic(call);
            if (f != nullptr)
                return f;
            // otherwise just do get function
            f = INTRINSIC(m.getFunction, constant(CAR(call)), context->rho);
        }
        // now we must compile the arguments, this depends on the actual type of the function - promises by default, eager for builtins and no evaluation for specials
        Value * ftype = INTRINSIC(m.sexpType, f);
        // switch the function call execution based on the function type
        BasicBlock * special = BasicBlock::Create(getGlobalContext(), "special", context->f, nullptr);
        BasicBlock * builtin = BasicBlock::Create(getGlobalContext(), "builtin", context->f, nullptr);

        BasicBlock * closure = BasicBlock::Create(getGlobalContext(), "closure", context->f, nullptr);
        BasicBlock * next = BasicBlock::Create(getGlobalContext(), "next", context->f, nullptr);
        SwitchInst * sw = SwitchInst::Create(ftype, closure, 2, context->b);
        sw->addCase(constant(SPECIALSXP), special);
        sw->addCase(constant(BUILTINSXP), builtin);
        // in special case, do not evaluate arguments and just call the function
        context->b = special;
        Value * specialResult = INTRINSIC(m.call, constant(call), f, constant(R_NilValue), context->rho);
        JUMP(next);
        // in builtin mode evaluate all arguments eagerly
        context->b = builtin;
        Value * args = compileArguments(CDR(call), /*eager=*/ true);
        Value * builtinResult = INTRINSIC(m.call, constant(call), f, args, context->rho);
        builtin = context->b; // bb might have changed during arg evaluation
        JUMP(next);
        // in general closure case the arguments will become promises
        context->b = closure;
        args = compileArguments(CDR(call), /*eager=*/ false);
        Value * closureResult = INTRINSIC(m.call, constant(call), f, args, context->rho);
        JUMP(next);
        // add a phi node for the call result
        context->b = next;
        PHINode * phi = PHINode::Create(t::SEXP, 3, "", context->b);
        phi->addIncoming(specialResult, special);
        phi->addIncoming(builtinResult, builtin);
        phi->addIncoming(closureResult, closure);
        return phi;
    }

#endif


    /** Many function calls may be compiled using intrinsics directly and not the R calling mechanism itself.

      This function determines based on the function symbol whether a compilation using intrinsics is possible and attempts it. It returns the result value of the compilation if successful, or nullptr if the function cannot be compiled using intrinsics.

      TODO this now uses even simpler approach than R bytecode compiler, I am simply assuming that these will never be overloaded. But we can change this when we want to.
      */
    Value * compileIntrinsic(SEXP call) {
#define CASE(sym) if (CAR(call) == sym)
        CASE(symbol::Block)
            return compileBlock(CDR(call));
        CASE(symbol::Parenthesis)
            return compileParenthesis(CDR(call));
        CASE(symbol::Function)
            return compileFunctionDefinition(CDR(call));
        CASE(symbol::Return) {
            return (CDR(call) == R_NilValue) ? compileReturn(constant(R_NilValue)) : compileReturn(compileExpression(CAR(CDR(call))));
        }
        CASE(symbol::Assign)
            return compileAssignment(call);
        CASE(symbol::Assign2)
            return compileAssignment(call);
        CASE(symbol::SuperAssign)
            return compileSuperAssignment(call);
        CASE(symbol::If)
            return compileCondition(call);
        CASE(symbol::Break)
            return compileBreak(call);
        CASE(symbol::Next)
            return compileNext(call);
        CASE(symbol::Repeat)
            return compileRepeatLoop(call);
        CASE(symbol::While)
            return compileWhileLoop(call);
        CASE(symbol::For)
            return compileForLoop(call);
        CASE(symbol::Switch)
            return compileSwitch(call);
        CASE(symbol::Add)
            return compileBinaryOrUnary(m.genericAdd, m.genericUnaryPlus, call);
        CASE(symbol::Sub)
            return compileBinaryOrUnary(m.genericSub, m.genericUnaryMinus, call);
        CASE(symbol::Mul)
            return compileBinary(m.genericMul, call);
        CASE(symbol::Div)
            return compileBinary(m.genericDiv, call);
        CASE(symbol::Pow)
            return compileBinary(m.genericPow, call);
        CASE(symbol::Sqrt)
            return compileUnary(m.genericSqrt, call);
        CASE(symbol::Exp)
            return compileUnary(m.genericExp, call);
        CASE(symbol::Eq)
            return compileBinary(m.genericEq, call);
        CASE(symbol::Ne)
            return compileBinary(m.genericNe, call);
        CASE(symbol::Lt)
            return compileBinary(m.genericLt, call);
        CASE(symbol::Le)
            return compileBinary(m.genericLe, call);
        CASE(symbol::Ge)
            return compileBinary(m.genericGe, call);
        CASE(symbol::Gt)
            return compileBinary(m.genericGt, call);
        CASE(symbol::BitAnd)
            return compileBinary(m.genericBitAnd, call);
        CASE(symbol::BitOr)
            return compileBinary(m.genericBitOr, call);
        CASE(symbol::Not)
            return compileUnary(m.genericNot, call);

        return nullptr;
#undef CASE
    }

    /** Block (a call to {) is compiled as a simple sequence of its statements with its return value being the result of the last statement. If a block is empty, a visible R_NilValue is returned.
      */
    Value * compileBlock(SEXP block) {
        Value * result = nullptr;
        while (block != R_NilValue) {
            result = compileExpression(CAR(block));
            block = CDR(block);
        }
        if (result == nullptr)
            result = constant(R_NilValue);
        return result;
    }

    /** Parenthesis expects a single argument only. Ellipsis is allowed, but not supported with the intrinsics at the moment so we default to R call.

      Otherwise markVisible intrinsic is applied to the result in accordance to the manual.
      */
    Value * compileParenthesis(SEXP arg) {\
        arg = CAR(arg);
        if (arg == symbol::Ellipsis)
            return nullptr; // we can't yet do this
        Value * result = compileExpression(arg);
        context->visibleResult = true;
        return result;
    }

    /** Similar to R bytecode compiler, only the body of the created function is compiled, the default arguments are left in their ast forms for now.

      TODO this should change.
     */
    Value * compileFunctionDefinition(SEXP fdef) {
        SEXP forms = CAR(fdef);
        SEXP body = compileFunction("function", CAR(CDR(fdef)));
        context->addObject(body);
        return INTRINSIC(m.createClosure, constant(forms), constant(body), context->rho);
    }

    /** Simple assignments (that is to a symbol) are compiled using the genericSetVar intrinsic.
      */
    Value * compileAssignment(SEXP e) {
        e = CDR(e);
        // intrinsic only handles simple assignments
        if (TYPEOF(CAR(e)) != SYMSXP)
            return nullptr;
        Value * v = compileExpression(CAR(CDR(e)));
        INTRINSIC(m.genericSetVar, constant(CAR(e)), v, context->rho);
        context->visibleResult = false;
        return v;
    }

    /** Super assignment is compiled as genericSetVarParentIntrinsic
     */
    Value * compileSuperAssignment(SEXP e) {
        e = CDR(e);
        // intrinsic only handles simple assignments
        if (TYPEOF(CAR(e)) != SYMSXP)
            return nullptr;
        Value * v = compileExpression(CAR(CDR(e)));
        INTRINSIC(m.genericSetVarParent, constant(CAR(e)), v, context->rho);
        context->visibleResult = false;
        return v;
    }

    /** Return calls or returns in general are compiled depending on the context. Usually a simple return instruction in bitcode is enough, but while in promises, we must use longjmp, which is done by calling returnJump intrinsic.
      */
    Value * compileReturn(Value * value, bool tail = false) {
        if (not context->visibleResult)
            INTRINSIC(m.markInvisible);
        if (context->returnJump) {
            INTRINSIC(m.returnJump, value, context->rho);
            // we need to have a return instruction as well to fool LLVM into believing the basic block has a terminating instruction
            ReturnInst::Create(getGlobalContext(), constant(R_NilValue), context->b);
        } else {
            ReturnInst::Create(getGlobalContext(), value, context->b);
        }
        // this is here to allow compilation of wrong code where statements are even after return
        if (not tail)
            context->b = BasicBlock::Create(getGlobalContext(), "deadcode", context->f);
        return nullptr;
    }

    /** Condition is compiled using the convertToLogicalNoNA intrinsic. True block has to be always present, but false block does not have to be present in which case an invisible R_NilValue should be returned.
      */
    Value * compileCondition(SEXP e) {
        e = CDR(e);
        SEXP condAst = CAR(e);
        e = CDR(e);
        SEXP trueAst = CAR(e);
        e = CDR(e);
        SEXP falseAst = (e != R_NilValue) ? CAR(e) : nullptr;
        Value * cond2 = compileExpression(condAst);
        Value * cond = INTRINSIC(m.convertToLogicalNoNA, cond2, constant(condAst));
        BasicBlock * ifTrue = BasicBlock::Create(getGlobalContext(), "ifTrue", context->f, nullptr);

        BasicBlock * ifFalse = BasicBlock::Create(getGlobalContext(), "ifFalse", context->f, nullptr);
        BasicBlock * next = BasicBlock::Create(getGlobalContext(), "next", context->f, nullptr);
        ICmpInst * test = new ICmpInst(*(context->b), ICmpInst::ICMP_EQ, cond, constant(TRUE), "condition");
        BranchInst::Create(ifTrue, ifFalse, test, context->b);
        // true case has to be always present
        context->b = ifTrue;
        Value * trueResult = compileExpression(trueAst);
        JUMP(next);
        ifTrue = context->b;
        // false case may not be present in which case invisible R_NilValue should be returned
        context->b = ifFalse;
        Value * falseResult;
        if (falseAst == nullptr) {
            falseResult = constant(R_NilValue);
            context->visibleResult = false;
        } else {
            falseResult = compileExpression(falseAst);
            ifFalse = context->b;
        }
        JUMP(next);
        // add a phi node for the result
        context->b = next;
        PHINode * phi = PHINode::Create(t::SEXP, 2, "", context->b);
        phi->addIncoming(trueResult, ifTrue);
        phi->addIncoming(falseResult, ifFalse);
        return phi;
    }

    /** Compiles break. Whenever we see break in the compiler, we know it is for a loop where context was skipped and therefore it must always be translated as direct jump in bitcode.

      TODO The error is probably not right.
       */
    Value * compileBreak(SEXP ast) {
        assert(context->breakBlock != nullptr and "Break outside loop");
        JUMP(context->breakBlock);
        // TODO this is really simple, but fine for us - dead code elimination will remove the block if required
        context->b = BasicBlock::Create(getGlobalContext(), "deadcode", context->f);
        return constant(R_NilValue);
    }

    /** Compiles next. Whenever we see next in the compiler, we know it is for a loop where context was skipped and therefore it must always be translated as direct jump in bitcode.

      TODO The error is probably not right.
       */
    Value * compileNext(SEXP ast) {
        assert(context->nextBlock != nullptr and "Next outside loop");
        JUMP(context->nextBlock);
        // TODO this is really simple, but fine for us - dead code elimination will remove the block if required
        context->b = BasicBlock::Create(getGlobalContext(), "deadcode", context->f);
        return constant(R_NilValue);
    }

    /** Compiles repeat loop. This is simple infinite loop. Only break can exit it.

      Return value of break loop is invisible R_NilValue.
     */
    Value * compileRepeatLoop(SEXP ast) {
        SEXP bodyAst = CAR(CDR(ast));
        if (not canSkipLoopContext(bodyAst))
            return nullptr;
        // save old loop pointers from the context
        BasicBlock * oldBreak = context->breakBlock;
        BasicBlock * oldNext = context->nextBlock;
        // create the body and next basic blocks
        context->nextBlock = BasicBlock::Create(getGlobalContext(), "repeatBody", context->f, nullptr);
        context->breakBlock = BasicBlock::Create(getGlobalContext(), "repeatBreak", context->f, nullptr);
        JUMP(context->nextBlock);
        context->b = context->nextBlock;
        compileExpression(bodyAst);
        JUMP(context->nextBlock);
        context->b = context->breakBlock;
        // restore the old loop pointers in the context
        context->breakBlock = oldBreak;
        context->nextBlock = oldNext;
        // return R_NilValue
        context->visibleResult = false;
        return constant(R_NilValue);
    }

    /** Compiles while loop.

      Return value of a while loop is invisible R_NilValue.
     */
    Value * compileWhileLoop(SEXP ast) {
        SEXP condAst = CAR(CDR(ast));
        SEXP bodyAst = CAR(CDR(CDR(ast)));
        if (not canSkipLoopContext(bodyAst))
            return nullptr;
        // save old loop pointers from the context
        BasicBlock * oldBreak = context->breakBlock;
        BasicBlock * oldNext = context->nextBlock;
        // create the body and next basic blocks
        context->nextBlock = BasicBlock::Create(getGlobalContext(), "whileCond", context->f, nullptr);
        context->breakBlock = BasicBlock::Create(getGlobalContext(), "whileBreak", context->f, nullptr);
        JUMP(context->nextBlock);
        context->b = context->nextBlock;
        // compile the condition
        Value * cond2 = compileExpression(condAst);
        Value * cond = INTRINSIC(m.convertToLogicalNoNA, cond2, constant(condAst));
        BasicBlock * whileBody = BasicBlock::Create(getGlobalContext(), "whileBody", context->f, nullptr);
        ICmpInst * test = new ICmpInst(*(context->b), ICmpInst::ICMP_EQ, cond, constant(TRUE), "condition");
        BranchInst::Create(whileBody, context->breakBlock, test, context->b);
        // compile the body
        context-> b = whileBody;
        compileExpression(bodyAst);
        JUMP(context->nextBlock);
        context->b = context->breakBlock;
        // restore the old loop pointers in the context
        context->breakBlock = oldBreak;
        context->nextBlock = oldNext;
        // return R_NilValue
        context->visibleResult = false;
        return constant(R_NilValue);
    }

    /** For loop is compiled into the following structure:

          get the sequence
          length = sequence length
          index = 0
          goto forCond
      forCond:
          goto (index < length) ? forBody : forBreak
      forBody:
          setVar(controlVar, getForLoopValue(seq, index)
          body of the loop
          goto forNext
      forNext:
          index += 1
          goto forCond
      forBreak:

      This uses a jump too many, but it simplifies the SSA considerations and will be optimized by LLVM anyhow when we go for LLVM optimizations.
      */
    Value * compileForLoop(SEXP ast) {
        SEXP controlAst = CAR(CDR(ast));
        assert(TYPEOF(controlAst) == SYMSXP and "Only symbols allowed as loop control variables");
        SEXP seqAst = CAR(CDR(CDR(ast)));
        SEXP bodyAst = CAR(CDR(CDR(CDR(ast))));
        if (not canSkipLoopContext(bodyAst))
            return nullptr;
        // save old loop pointers from the context
        BasicBlock * oldBreak = context->breakBlock;
        BasicBlock * oldNext = context->nextBlock;
        // create the body and next basic blocks
        context->nextBlock = BasicBlock::Create(getGlobalContext(), "forNext", context->f, nullptr);
        context->breakBlock = BasicBlock::Create(getGlobalContext(), "forBreak", context->f, nullptr);
        // This is a simple basic block to which all next's jump and which then jumps to forCond so that there is a simpler phi node at forCond.
        BasicBlock * forCond = BasicBlock::Create(getGlobalContext(), "forCond", context->f, nullptr);
        BasicBlock * forBody = BasicBlock::Create(getGlobalContext(), "forBody", context->f, nullptr);
        // now initialize the loop control structures
        Value * seq2 = compileExpression(seqAst);
        Value * seq = INTRINSIC(m.startFor, seq2, context->rho);
        Value * seqLength = INTRINSIC(m.loopSequenceLength, seq, constant(ast));
        BasicBlock * forStart = context->b;
        JUMP(forCond);
        context->b = forCond;
        PHINode * control = PHINode::Create(t::Int, 2, "loopControl", context->b);
        control->addIncoming(constant(0), forStart);
        // now check if control is smaller than length
        ICmpInst * test = new ICmpInst(*(context->b), ICmpInst::ICMP_ULT, control, seqLength, "condition");
        BranchInst::Create(forBody, context->breakBlock, test, context->b);
        // move to the for loop body, where we have to set the control variable properly
        context->b = forBody;
        Value * controlValue = INTRINSIC(m.getForLoopValue, seq, control);
        INTRINSIC(m.genericSetVar, constant(controlAst), controlValue, context->rho);
        // now compile the body of the loop
        compileExpression(bodyAst);
        JUMP(context->nextBlock);
        // in the next block, increment the internal control variable and jump to forCond
        context->b = context->nextBlock;
        Value * control1 = BinaryOperator::Create(Instruction::Add, control, constant(1), "", context->b);
        control->addIncoming(control1, context->nextBlock);
        JUMP(forCond);
        context->b = context->breakBlock;
        // restore the old loop pointers in the context
        context->breakBlock = oldBreak;
        context->nextBlock = oldNext;
        // return R_NilValue
        context->visibleResult = false;
        return constant(R_NilValue);
    }

    /** Determines whether we can skip creation of the loop context or not. The code is taken from Luke's bytecode compiler.
     */
    bool canSkipLoopContext(SEXP ast, bool breakOK = true) {
        if (TYPEOF(ast) == LANGSXP) {
            SEXP cs = CAR(ast);
            if (TYPEOF(cs) == SYMSXP)
                if (not breakOK and (cs == symbol::Break or cs == symbol::Next))
                    return false;
                else if (cs == symbol::Function or cs == symbol::For or cs == symbol::While or cs == symbol::Repeat)
                    return true;
                else if (cs == symbol::Parenthesis or cs == symbol::Block or cs == symbol::If)
                    return canSkipLoopContextList(CDR(ast), breakOK);
                else
                    return canSkipLoopContextList(CDR(ast), false);
            // this is change to Luke's code - I believe that whatever will return us the function to call might be compiled using intrinsics and therefore should follow the breakOK rules, not the rules for promises the arguments of user functions do
            return canSkipLoopContext(CAR(ast), breakOK) and canSkipLoopContextList(CDR(ast), false);
        } else {
            return true;
        }
    }

    bool canSkipLoopContextList(SEXP ast, bool breakOK) {
        while (ast != R_NilValue) {
            if (not canSkipLoopContext(CAR(ast), breakOK))
                return false;
            ast = CDR(ast);
        }
        return true;
    }

    /** Compiles the switch statement.

      There are two kinds of switch - integral and character one and they differ in what they are doing. The integral switch can be used always, and in its case the control variable is simple index to the cases. Contrary to the

          ctrl = evaluate switch control
          checkSwitchControl()
          goto sexptype() == STRSXP ? switchCharacter : switchIntegral
      switchIntegral:
          t = switchControlInteger(ctrl, numCases)
          switch (t):
      switchCharacter:
          t = switchControlCharacter(ctrl, call, caseStrings)
          switch (t):
      switchCase1:
          ...
          goto switchNext
      switchCaseN:
          ...
          goto switchNext
      switchNext:
          phi for the cases


     */
    Value * compileSwitch(SEXP call) {
        SEXP x = CDR(call);
        SEXP condAst = CAR(x);
        x = CDR(x);
        std::vector<SEXP> caseAsts;
        std::vector<SEXP> caseNames;
        int defaultIdx = -1;
        int i = 0;
        while (x != R_NilValue) {
            caseAsts.push_back(CAR(x));
            SEXP name = TAG(x);
            if (name == R_NilValue)
                if (defaultIdx == -1)
                    defaultIdx = i;
                else
                    defaultIdx = -2;
            else
                caseNames.push_back(name);
            x = CDR(x);
            ++i;
        }
        // actual switch compilation - get the control value and check it
        Value * control = compileExpression(condAst);
        INTRINSIC(m.checkSwitchControl, control, constant(call));
        Value * ctype = INTRINSIC(m.sexpType, control);
        ICmpInst* cond = new ICmpInst(*context->b, ICmpInst::ICMP_EQ, ctype, constant(STRSXP), "");
        BasicBlock * switchIntegral = BasicBlock::Create(getGlobalContext(), "switchIntegral", context->f, nullptr);
        BasicBlock * switchCharacter = BasicBlock::Create(getGlobalContext(), "switchCharacter", context->f, nullptr);
        BasicBlock * switchNext = BasicBlock::Create(getGlobalContext(), "switchNext", context->f, nullptr);
        BranchInst::Create(switchCharacter, switchIntegral, cond, context->b);
        // integral switch is simple
        context->b = switchIntegral;
        Value * caseIntegral = INTRINSIC(m.switchControlInteger, control, constant(caseAsts.size()));
        SwitchInst * swInt = SwitchInst::Create(caseIntegral, switchNext, caseAsts.size(), context->b);
        // for character switch we need to construct the vector,
        context->b = switchCharacter;
        SEXP cases;
        if (defaultIdx != -2 ) {
            cases = allocVector(STRSXP, caseNames.size());
            for (size_t i = 0; i < caseNames.size(); ++i)
                SET_STRING_ELT(cases, i, PRINTNAME(caseNames[i]));
        } else {
            cases = R_NilValue;
        }
        context->addObject(cases);
        Value * caseCharacter = INTRINSIC(m.switchControlCharacter, control, constant(call), constant(cases));
        SwitchInst * swChar = SwitchInst::Create(caseCharacter, switchNext, caseAsts.size(), context->b);
        // create the phi node at the end
        context->b = switchNext;
        PHINode * result = PHINode::Create(t::SEXP, caseAsts.size(), "", context->b);
        // walk the cases and create their blocks, add them to switches and their results to the phi node
        BasicBlock * last;
        for (int i = 0; i < caseAsts.size(); ++i) {
            context->b = last = BasicBlock::Create(getGlobalContext(), "switchCase", context->f, nullptr);
            swInt->addCase(constant(i), last);
            if (defaultIdx == -1 or defaultIdx > i) {
                swChar->addCase(constant(i), last);
            } else if (defaultIdx < i) {
                swChar->addCase(constant(i - 1), last);
            } else {
                swChar->addCase(constant(caseAsts.size() -1), last);
                swChar->setDefaultDest(last);
            }
            Value * caseResult = compileExpression(caseAsts[i]);
            JUMP(switchNext);
            result->addIncoming(caseResult, context->b);
        }
        if (swChar->getDefaultDest() == switchNext)
            swChar->setDefaultDest(last);
        swInt->setDefaultDest(last);
        context->b = switchNext;
        return result;
    }

    /** Compiles operators that can be either binary, or unary, based on the number of call arguments. Takes the binary and unary intrinsics to be used and the full call ast.
      */
    Value * compileBinaryOrUnary(Function * b, Function * u, SEXP call) {
        Value * lhs = compileExpression(CAR(CDR(call)));
        if (CDR(CDR(call)) != R_NilValue) {
            Value * rhs = compileExpression(CAR(CDR(CDR(call))));
            return INTRINSIC(b, lhs, rhs, constant(call), context->rho);
        } else {
            return INTRINSIC(u, lhs, constant(call), context->rho);
        }
    }

    /** Compiles binary operator using the given intrinsic and full call ast.
      */
    Value * compileBinary(Function * f, SEXP call) {
        Value * lhs = compileExpression(CAR(CDR(call)));
        Value * rhs = compileExpression(CAR(CDR(CDR(call))));
        return INTRINSIC(f, lhs, rhs, constant(call), context->rho);
    }

    /** Compiles unary operator using the given intrinsic and full call ast.
      */
    Value * compileUnary(Function * f, SEXP call) {
        Value * op = compileExpression(CAR(CDR(call)));
        return INTRINSIC(f, op, constant(call), context->rho);
    }

    /** Converts given SEXP to a bitcode constant. The SEXP address is taken as an integer constant into LLVM which is then converted to SEXP.

      NOTE that this approach assumes that any GC used is non-moving. We are using it because it removes one level of indirection when reading it from the constants vector as R bytecode compiler does.
      */
    Value * constant(SEXP value) {
        return ConstantExpr::getCast(Instruction::IntToPtr, ConstantInt::get(getGlobalContext(), APInt(64, (std::uint64_t)value)), t::SEXP);
    }

    /** Converts given integer to bitcode value. This is just a simple shorthand function, no magic here.
      */
    ConstantInt * constant(int value) {
        return ConstantInt::get(getGlobalContext(), APInt(32, value));
    }

    /** Current compilation module.

      The module contains the intrinsic function declarations as well as all compiled functions.
     */
    JITModule m;

    /** The context of current compilation.

      Each compiled function (including promises) has its own context. The context contains information about current return condition and visibility, break and next targets, R objects required and so on.
     */
    Context * context;

    /** List of relocations to be done when compiling.

      When a function is compiled, it is first translated to bitcode and a native SXP is created for it using nullptr for the native code. The function's SXP is added to the list of relocations here. When the compilation is done, the module is finalized and all SEXPs in the relocation lists are patched so that they point to correct native functions.
      */
    std::vector<SEXP> relocations;

    static uint32_t stackMapId;
};

uint32_t Compiler::stackMapId = 1;


#undef INTRINSIC

#define INTRINSIC(name, ...) CallInst::Create(name, ARGS(__VA_ARGS__), "", b)

// TODO: find target endianness
typedef StackMapV1Parser<llvm::support::little> StackMapParserT;

class ICCompiler  {
public:

    ICCompiler() : m("ic") {}

    void * compile(SEXP inCall, SEXP inFun, SEXP inRho,
            uintptr_t inCallee, uintptr_t inStackmapId,
            std::initializer_list<SEXP> inArgs) {

        uintptr_t patchAddr = 0;

        for (auto s : stackmaps) {
            ArrayRef<uint8_t> sm(s.first, s.second);
            StackMapParserT p(sm);

            for (const auto &r : p.records()) {
                if (inStackmapId == r.getID()) {
                    patchAddr = inCallee + r.getInstructionOffset();
                    break;
                }
            }
        }
        if (patchAddr == 0) {
            std::cout << "something went very wrong, "
                      << "cannot find stackmap for patchpoint " << inStackmapId
                      << std::endl;
            asm("int3");
        }

        // Set up a function type which corresponds to the ICStub signature
        std::vector<Type*> argT;
        for (int i = 0; i < inArgs.size() + 3; i++) {
            argT.push_back(t::SEXP);
        }
        argT.push_back(IntegerType::get(getGlobalContext(), 64));
        argT.push_back(IntegerType::get(getGlobalContext(), 64));
        auto funT = FunctionType::get(t::SEXP, argT, false);

        f = Function::Create(funT, Function::ExternalLinkage, "callIC", m);
        b = BasicBlock::Create(getGlobalContext(), "start", f, nullptr);

        // Load the args in the same order as the stub
        Function::arg_iterator argI = f->arg_begin();
        for (auto a : inArgs) {
            icArgs.push_back(argI++);
        }

        call = argI++;
        call->setName("call");
        fun = argI++;
        fun->setName("op");
        rho = argI++;
        rho->setName("rho");
        callee = argI++;
        callee->setName("callee");
        stackmapId = argI++;
        stackmapId->setName("stackmapId");

        if (!compileIc(inCall, inFun, inStackmapId))
            compileGenericIc(inCall, inFun);

        ExecutionEngine * engine = EngineBuilder(std::unique_ptr<Module>(m)).create();
        engine->finalizeObject();
        auto ic = engine->getPointerToFunction(f);

        // TODO: This directly patches the argument of the movabs,
        // clearly we need sth more stable
        *(void**)(patchAddr+2) = ic;

        return ic;
    }



private:
    bool compileIc(SEXP inCall, SEXP inFun, uint64_t inStackMapId) {
        std::vector<bool> promarg(icArgs.size(), false);

        // Check for named args or ...
        SEXP arg = CDR(inCall);
        int i = 0;
        while (arg != R_NilValue) {
            if (TAG(arg) != R_NilValue)
                return false;
            if (CAR(arg) == R_DotsSymbol)
                return false;

            switch (TYPEOF(CAR(arg))) {
                case LGLSXP:
                case INTSXP:
                case REALSXP:
                case CPLXSXP:
                case STRSXP:
                    break;
                default:
                    promarg[i] = true;
            }
            i++;
            arg = CDR(arg);
        }

        if (TYPEOF(inFun) == CLOSXP) {
            SEXP body = CDR(inFun);
            if (TYPEOF(body) == NATIVESXP) {
                
                BasicBlock * icMatch = BasicBlock::Create(
                        getGlobalContext(), "icMatch", f, nullptr);
                BasicBlock * icMiss = BasicBlock::Create(
                        getGlobalContext(), "icMiss", f, nullptr);
                BasicBlock * end = BasicBlock::Create(
                        getGlobalContext(), "end", f, nullptr);

                ICmpInst * test = new ICmpInst(*b,
                        ICmpInst::ICMP_EQ, fun, constant(inFun), "guard");
                BranchInst::Create(icMatch, icMiss, test, b);

                b = icMatch;

                Value * arglist = constant(R_NilValue);

                // This reverses the arglist, but quick argumentAdapter
                // reverses again
                for (int i = 0; i < icArgs.size(); ++i) {
                    Value * arg = icArgs[i];
                    if (promarg[i])
                        arg = INTRINSIC(m.createPromise, arg, rho);
                    arglist = INTRINSIC(m.CONS_NR, arg, arglist);
                }

                Value * newrho = INTRINSIC(m.closureQuickArgumentAdaptor,
                        fun, arglist);

                Value * cntxt = new AllocaInst(
                        t::cntxt, "", b);

                INTRINSIC(m.initClosureContext,
                        cntxt, call, newrho, rho, arglist, fun);

                Value * res = INTRINSIC(m.closureNativeCallTrampoline,
                        cntxt, constant(body), newrho);

                INTRINSIC(m.endClosureContext, cntxt, res);

                BranchInst::Create(end, b);
                b = icMiss;

                Value * missRes = insertICCallStub(
                        icArgs, fun, constant(inCall), rho, m, b, callee, inStackMapId);

                BranchInst::Create(end, b);
                b = end;

                PHINode * phi = PHINode::Create(t::SEXP, 2, "", b);
                phi->addIncoming(res, icMatch);
                phi->addIncoming(missRes, icMiss);
                ReturnInst::Create(getGlobalContext(), phi, b);

//                f->dump();
                return true;
            }
        }
        return false;
    }

    bool compileGenericIc(SEXP inCall, SEXP inFun) {
        Value * call = compileCall(inCall, inFun);
        ReturnInst::Create(getGlobalContext(), call, b);

        return true;
    }

    Value * compileCall(SEXP call, SEXP op) {
        // now create the CallArgs structure as local variable
        Value * callArgs = new AllocaInst(t::CallArgs, "callArgs", b);
        // set first to R_NilValue
        std::vector<Value*> callArgsIndices = { constant(0), constant(0) };
        Value * callArgsFirst = GetElementPtrInst::Create(callArgs, callArgsIndices, "", b);

        // now we must compile the arguments, this depends on the actual type of the function - promises by default, eager for builtins and no evaluation for specials
        Value * ftype = INTRINSIC(m.sexpType, fun);
        // switch the function call execution based on the function type
        BasicBlock * special = BasicBlock::Create(getGlobalContext(), "special", f, nullptr);
        BasicBlock * builtin = BasicBlock::Create(getGlobalContext(), "builtin", f, nullptr);

        BasicBlock * closure = BasicBlock::Create(getGlobalContext(), "closure", f, nullptr);
        BasicBlock * next = BasicBlock::Create(getGlobalContext(), "next", f, nullptr);
        SwitchInst * sw = SwitchInst::Create(ftype, closure, 2, b);
        sw->addCase(constant(SPECIALSXP), special);
        sw->addCase(constant(BUILTINSXP), builtin);
        // in special case, do not evaluate arguments and just call the function
        b = special;
        Value * specialResult = INTRINSIC(m.call, constant(call), fun, constant(R_NilValue), rho);
        BranchInst::Create(next, b);
        // in builtin mode evaluate all arguments eagerly
        b = builtin;
        Value * args = compileArguments(callArgs, callArgsFirst, CDR(call), /*eager=*/ true);
        Value * builtinResult = INTRINSIC(m.call, constant(call), fun, args, rho);
        builtin = b; // bb might have changed during arg evaluation
        BranchInst::Create(next, b);
        // in general closure case the arguments will become promises
        b = closure;
        args = compileArguments(callArgs, callArgsFirst, CDR(call), /*eager=*/ false);
        Value * closureResult = INTRINSIC(m.call, constant(call), fun, args, rho);
        BranchInst::Create(next, b);
        // add a phi node for the call result
        b = next;
        PHINode * phi = PHINode::Create(t::SEXP, 3, "", b);
        phi->addIncoming(specialResult, special);
        phi->addIncoming(builtinResult, builtin);
        phi->addIncoming(closureResult, closure);
        return phi;
    }

    /** Compiles arguments for given function.

      Creates the pairlist of arguments used in R from the arguments and their names.
      */
    Value * compileArguments(Value * args, Value * first, SEXP argAsts, bool eager) {
        // set first argument to R_NilValue
        new StoreInst(constant(R_NilValue), first, b);
        // if there are no arguments
        int argnum = 0;
        while (argAsts != R_NilValue) {
            compileArgument(args, argAsts, argnum++, eager);
            argAsts = CDR(argAsts);
        }
        return new LoadInst(first, "", b);
    }

    /** Compiles a single argument.

      Self evaluating literals are always returned as SEXP constants, anything else is either evaluated directly if eager is true, or they are compiled as new promises.
     */
    void compileArgument(Value * args, SEXP argAst, int argnum, bool eager) {
        SEXP arg = CAR(argAst);
        Value * result;
        switch (TYPEOF(arg)) {
        case LGLSXP:
        case INTSXP:
        case REALSXP:
        case CPLXSXP:
        case STRSXP:
            // literals are self-evaluating
            result = icArgs[argnum];
            break;
        case SYMSXP:
            if (arg == R_DotsSymbol) {
                INTRINSIC(m.addEllipsisArgument, args, rho, eager ? constant(TRUE) : constant(FALSE));
                return;
            }
        default:
            if (eager) {
                // TODO make this more efficient?
                result = INTRINSIC(m.callNative, icArgs[argnum], rho);
            } else {
                // we must create a promise out of the argument
                result = INTRINSIC(m.createPromise, icArgs[argnum], rho);
            }
        }
        SEXP name = TAG(argAst);
        if (name != R_NilValue)
            INTRINSIC(m.addKeywordArgument, args, result, constant(name));
        else
            INTRINSIC(m.addArgument, args, result);
    }

    // TODO: Pull up
    Value * constant(SEXP value) {
        return ConstantExpr::getCast(Instruction::IntToPtr, ConstantInt::get(getGlobalContext(), APInt(64, (std::uint64_t)value)), t::SEXP);
    }
    ConstantInt * constant(int value) {
        return ConstantInt::get(getGlobalContext(), APInt(32, value));
    }


    Function * f;
    BasicBlock * b;

    Value * rho;
    Value * fun;
    Value * callee;
    Value * stackmapId;
    Value * call;
    std::vector<Value*> icArgs;

    JITModule m;
};
#undef INTRINSIC

static void * CallICStub0(
        SEXP call, SEXP fun, SEXP rho, uintptr_t callee, uintptr_t stackmapId) {
    ICCompiler compiler;
    void * ic = compiler.compile(call, fun, rho, callee, stackmapId, {});
    return ((SEXP (*) (SEXP, SEXP, SEXP, uintptr_t, uintptr_t))ic)(
            call, fun, rho, callee, stackmapId);
}

static void * CallICStub1(SEXP a1,
        SEXP call, SEXP fun, SEXP rho, uintptr_t callee, uintptr_t stackmapId) {
    ICCompiler compiler;
    void * ic = compiler.compile(call, fun, rho, callee, stackmapId, {a1});
    return ((SEXP (*) (SEXP, SEXP, SEXP, SEXP, uintptr_t, uintptr_t))ic)(
            a1, call, fun, rho, callee, stackmapId);
}

static void * CallICStub2(SEXP a1, SEXP a2,
        SEXP call, SEXP fun, SEXP rho, uintptr_t callee, uintptr_t stackmapId) {
    ICCompiler compiler;
    void * ic = compiler.compile(call, fun, rho, callee, stackmapId, {a1, a2});
    return ((SEXP (*) (SEXP, SEXP, SEXP, SEXP, SEXP, uintptr_t, uintptr_t))ic)(
            a1, a2, call, fun, rho, callee, stackmapId);
}




} // namespace




REXPORT SEXP compile(SEXP ast) {
    return Compiler("module").compile("rfunction", ast);
}

REXPORT SEXP printBitcode(SEXP ast) {

}

REXPORT SEXP nativeAST(SEXP ast) {

}



#endif // COMPILER_CPP

