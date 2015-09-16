#ifndef COMPILER_CPP
#define COMPILER_CPP

#include <cstdint>
#include <sstream>
#include <iostream>

#include <llvm/IR/Verifier.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/Support/raw_ostream.h>
#include "llvm/Analysis/Passes.h"

#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/MCJIT.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/CodeGen/GCStrategy.h"
#include "llvm/CodeGen/GCs.h"

#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/Analysis/TargetTransformInfo.h"

#include "gc_pass.h"

#include "llvm/IR/Intrinsics.h"

#include "compiler.h"
#include "stack_map.h"

using namespace llvm;

namespace rjit {


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
extern void printType(Type * t);
void printType(Type * t) {
    std::string type_str;
    llvm::raw_string_ostream rso(type_str);
    t->print(rso);
    std::cout << "Type: " << rso.str() << std::endl;
}
extern void printTypeOf(Value * v);
void printTypeOf(Value * v) {
    Type * t = v->getType();
    printType(t);
}
extern void printAllTypeOf(std::vector<Value*> vs);
void printAllTypeOf(std::vector<Value*> vs) {
    for (auto v : vs) {
        Type * t = v->getType();
        printType(t);
    }
}
extern void disassNative(SEXP native);
void disassNative(SEXP native) {
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

Type * t_void;
Type * voidPtr;
Type * t_i64;
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
FunctionType * void_cntxtsexpsexp;
FunctionType * sexp_contxtsexpsexp;

FunctionType * patchIC_t;
FunctionType * compileIC_t;

FunctionType * nativeFunction_t;
Type * nativeFunctionPtr_t;
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
    // Addrspace == 1 -> GC managed pointer
    t::SEXP = PointerType::get(t::SEXPREC, 1);
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
    t::t_void = t_void;

    t::t_i64 = IntegerType::get(context, 64);

    // TODO: probably not the best idea...
    t::cntxt = StructType::create(context, "struct.RCNTXT");
    std::vector<Type*> cntxtbod(360 /* sizeof(RCNTXT) */, IntegerType::get(context, 8)); 
    t::cntxt->setBody(cntxtbod);
    t::cntxtPtr = PointerType::get(t::cntxt, 0);

    t::i8ptr = PointerType::get(IntegerType::get(context, 8), 0);

    // FIXME
    t::voidPtr = PointerType::get(t::t_i64, 0);


#define DECLARE(name, ret, ...) fields = { __VA_ARGS__ }; t::name = FunctionType::get(ret, fields, false)
    DECLARE(nativeFunction_t, t::SEXP, t::SEXP, t::SEXP, t::Int);
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
    DECLARE(void_argssexp, t_void, t::CallArgsPtr, t::SEXP);
    DECLARE(void_argssexpsexp, t_void, t::CallArgsPtr, t::SEXP, t::SEXP);
    DECLARE(void_argssexpint, t_void, t::CallArgsPtr, t::SEXP, t::Int);
    DECLARE(void_cntxtsexpsexpsexpsexpsexp, t_void, t::cntxtPtr, t::SEXP, t::SEXP, t::SEXP, t::SEXP, t::SEXP);
    DECLARE(void_cntxtsexp, t_void, t::cntxtPtr, t::SEXP);
    DECLARE(void_cntxtsexpsexp, t_void, t::cntxtPtr, t::SEXP, t::SEXP);
    DECLARE(sexp_contxtsexpsexp, t::SEXP, t::cntxtPtr, t::SEXP, t::SEXP);

    DECLARE(patchIC_t, t::t_void, t::voidPtr, t::t_i64, t::nativeFunctionPtr_t);
    DECLARE(compileIC_t, t::voidPtr, t::t_i64, t::SEXP, t::SEXP, t::SEXP, t::t_i64);
#undef DECLARE

    // initialize LLVM backend
    LLVMInitializeNativeTarget();
    LLVMInitializeNativeAsmPrinter();
    LLVMInitializeNativeAsmParser();
    linkStatepointExampleGC();

    registerGcCallback(&StackMap::stackScanner);

    return t::SEXP;
}

extern "C" void * compileIC(uint64_t numargs, SEXP call, SEXP fun, SEXP rho, uint64_t stackmapId);
extern "C" void patchIC(void * ic, uint64_t stackmapId, void * caller);

uint8_t* new_stackmap_addr = nullptr;
uintptr_t new_stackmap_size;

class JITMemoryManager : public llvm::SectionMemoryManager {
private:
    struct MemoryGroup {
           SmallVector<sys::MemoryBlock, 16> AllocatedMem;
           SmallVector<sys::MemoryBlock, 16> FreeMem;
           sys::MemoryBlock Near;
    };

public:
    JITMemoryManager() {};

    uint64_t getSymbolAddress(const std::string &name) override {
        auto res = SectionMemoryManager::getSymbolAddress(name);
        if (!res) {
            if (name == "compileIC") return (uint64_t)&compileIC;
            if (name == "patchIC") return (uint64_t)&patchIC;
        }
        return res;
    }

    uint8_t * allocateDataSection(
            uintptr_t size, unsigned alignment, unsigned sectionID,
            StringRef sectionName, bool readonly) override {

        auto res = SectionMemoryManager::allocateDataSection(
                size, alignment, sectionID, sectionName, readonly);

        if (sectionName.str() == ".llvm_stackmaps") {
            assert (!new_stackmap_addr);
            new_stackmap_addr = res;
            new_stackmap_size = size;
        }

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
    
    DECLARE(compileIC, compileIC_t);
    DECLARE(patchIC, patchIC_t);

    Function * stackmap;
    Function * patchpoint;

    JITModule(std::string const & name): m(new Module(name, getGlobalContext())) {
        FunctionType * stackmapTy = FunctionType::get(
                t::t_void,
                std::vector<Type*>({{
                    IntegerType::get(m->getContext(), 64),
                    IntegerType::get(m->getContext(), 32)}}),
                true);
        stackmap = Function::Create(
                stackmapTy,
                GlobalValue::ExternalLinkage,
                "llvm.experimental.stackmap",
                m);

        FunctionType * patchpointTy = FunctionType::get(
                t::t_void,
                std::vector<Type*>({{
                    IntegerType::get(m->getContext(), 64),
                    IntegerType::get(m->getContext(), 32),
                    t::i8ptr,
                    IntegerType::get(m->getContext(), 32)}}),
                true);
        patchpoint = Function::Create(
                patchpointTy,
                GlobalValue::ExternalLinkage,
                "llvm.experimental.patchpoint.void",
                m);
    }

    operator Module * () {
        return m;
    }

    LLVMContext & getContext() {
        return m->getContext();
    }

    void dump() {
        m->dump();
    }

    Module * getM() {
        return m;
    }

};

#undef DECLARE

static void setupFunction(Function & f) {
    f.setGC("statepoint-example");
    auto attrs = f.getAttributes();
    attrs = attrs.addAttribute(f.getContext(), AttributeSet::FunctionIndex,
                               "no-frame-pointer-elim", "true");
    f.setAttributes(attrs);
}

static uint64_t nextStackmapId = 3;

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

const static int patchpointSize = 10;

void emitStackmap(uint64_t id, std::vector<Value*> values, JITModule & m, BasicBlock * b) {
    ConstantInt* const_0 =
        ConstantInt::get(m.getContext(),  APInt(32, StringRef("0"), 10));
    Constant* const_null = ConstantExpr::getCast(
            Instruction::IntToPtr, const_0, t::i8ptr);
    ConstantInt* const_num_bytes =
        ConstantInt::get(m.getContext(),  APInt(32, patchpointSize, false));
    ConstantInt* const_id =
        ConstantInt::get(m.getContext(), APInt(64, id, false));

    std::vector<Value*> sm_args;

    // Args to the stackmap
    sm_args.push_back(const_id);
    sm_args.push_back(const_num_bytes);
    sm_args.push_back(const_null);
    sm_args.push_back(const_0);

    // Values to record
    for (auto arg : values) {
        sm_args.push_back(arg);
    }

    CallInst::Create(m.patchpoint, sm_args, "", b);
}

static Value * insertCall(Value * fun, std::vector<Value*> args,
      BasicBlock * b, JITModule & m, uint64_t function_id) {

    auto res = CallInst::Create(fun, args, "", b);

    if (function_id != (uint64_t)-1) {
        assert(function_id > 1);
        assert(function_id < nextStackmapId);

        AttributeSet PAL;
        {
            SmallVector<AttributeSet, 4> Attrs;
            AttributeSet PAS;
            {
                AttrBuilder B;
                B.addAttribute("statepoint-id",
                               std::to_string(function_id));
                PAS = AttributeSet::get(m.getContext(), ~0U, B);
            }
            Attrs.push_back(PAS);
            PAL = AttributeSet::get(m.getContext(), Attrs);
        }
        res->setAttributes(PAL);
    }

    return res;
}

// record stackmaps will parse the stackmap section of the current module and
// index all entries.
void recordStackmaps(std::vector<uint64_t> functionIds) {
    if (new_stackmap_addr) {
        int i = 0;

        ArrayRef<uint8_t> sm(new_stackmap_addr, new_stackmap_size);
        StackMapParserT p(sm);

        for (const auto &r : p.records()) {
            assert(r.getID() != (uint64_t)-1 && r.getID() != statepointID);

            auto function_id = std::find(functionIds.begin(), functionIds.end(), r.getID());

            if (function_id == functionIds.end()) {
                // No such function id -> must be patchpoint
                StackMap::registerPatchpoint(r.getID(), sm, i);
            } else {
                auto pos = function_id - functionIds.begin();
                // We have a statepoint entry, lets find the corresponding
                // function entry. The assumption here is, that the order
                // of functionIds recorded by the compiler has to be the
                // same as the order of function entries in the stackmap seciton
                uintptr_t function = p.getFunction(pos).getFunctionAddress();
                StackMap::registerStatepoint(
                        function, r.getInstructionOffset(), sm, i);

            }
            i++;
        }
    }

}

/** Converts given SEXP to a bitcode constant.
 * The SEXP address is taken as an integer constant into LLVM which is then
 * converted to SEXP.
 * NOTE that this approach assumes that any GC used is non-moving.
 * We are using it because it removes one level of indirection when reading
 * it from the constants vector as R bytecode compiler does.
 *
 * FIXME: loading the const is wrapped in a call to hide the const -- otherwise
 * rewriteStatepointsForGC pass will fail, since it cannot create relocation
 * for a constant. The underlying problem is, that the pass assumes all
 * values of type SEXP to be moving GC pointers and there is no other more
 * fine grained method of specifying which values to spill.
 */
static Value * loadConstant(SEXP value, Module * m, BasicBlock * b) {
    auto f = Function::Create(
            FunctionType::get(
                t::SEXP,
                std::vector<Type*>(),
                false),
            Function::ExternalLinkage, "ldConst", m);
    auto bb = BasicBlock::Create(getGlobalContext(), "start", f, nullptr);

    auto con = ConstantExpr::getCast(
            Instruction::IntToPtr,
            ConstantInt::get(
                getGlobalContext(), APInt(64, (std::uint64_t)value)),
            t::SEXP);

    ReturnInst::Create(getGlobalContext(), con, bb);

    return CallInst::Create(f, {}, "const", b); 
}

static ExecutionEngine * jitModule(Module * m) {

    auto memoryManager = new JITMemoryManager();

    legacy::PassManager pm;

    pm.add(createTargetTransformInfoWrapperPass(TargetIRAnalysis()));

    pm.add(createPlaceRJITSafepointsPass());

    PassManagerBuilder PMBuilder;
    PMBuilder.OptLevel = 0;  // Set optimization level to -O0
    PMBuilder.SizeLevel = 0; // so that no additional phases are run.
    PMBuilder.populateModulePassManager(pm);

    // TODO: maybe have our own version which is not relocating?
    pm.add(createRewriteStatepointsForGCPass());
    pm.run(*m);

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

    return engine;
}

#define JUMP(block) BranchInst::Create(block, context->b)

class Compiler {
public:

    Compiler(std::string const & moduleName):
        m(moduleName) {}

    SEXP compile(std::string const & name, SEXP bytecode) {
        SEXP result = compileFunction(name, bytecode);
        return result;
    }

    SEXP compileFunction(std::string const & name, SEXP ast, bool isPromise = false) {
        Context * old = context;
        context = new Context(name, m);
        context->function_id = nextStackmapId++;
        functionIds.push_back(context->function_id);
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

    void jitAll() {

        ExecutionEngine * engine = jitModule(m.getM());

        // perform all the relocations
        for (SEXP s : relocations) {
            auto f = reinterpret_cast<Function*>(TAG(s));
            auto fp = engine->getPointerToFunction(f);
            SETCAR(s, reinterpret_cast<SEXP>(fp));
        }
        recordStackmaps(functionIds);
        new_stackmap_addr = nullptr;
        functionIds.clear();
    }

private:
    class Context {
    public:

        Context(std::string const & name, Module * m) {
            f = Function::Create(t::sexp_sexpsexpint, Function::ExternalLinkage, name, m);
            setupFunction(*f);
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

        unsigned function_id;
    };

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
        case NILSXP:
        case CLOSXP:
            return compileConstant(value);
        case BCODESXP:
        //TODO: reuse the compiled fun
        case NATIVESXP:
            return compileExpression(VECTOR_ELT(CDR(value), 0));
        default:
            assert(false && "Unknown SEXP type in compiled ast.");
        }
    }

    /** Compiles user constant, which constant marked with userConstant intrinsic.
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

    Value * compileICCallStub(Value * call, Value * op, std::vector<Value*> & callArgs);

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

        return compileICCallStub(constant(call), f, args);
    }

    void compileArguments(SEXP argAsts, std::vector<Value*> & res) {
         while (argAsts != R_NilValue) {
             res.push_back(
                     compileArgument(CAR(argAsts), TAG(argAsts)));
             argAsts = CDR(argAsts);
         }
    }

    Value * compileArgument(SEXP arg, SEXP name) {
        switch (TYPEOF(arg)) {
        case LGLSXP:
        case INTSXP:
        case REALSXP:
        case CPLXSXP:
        case STRSXP:
        case NILSXP:
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
            if (TYPEOF(cs) == SYMSXP) {
                if (not breakOK and (cs == symbol::Break or cs == symbol::Next)) {
                    return false;
                } else if (cs == symbol::Function or cs == symbol::For or cs == symbol::While or cs == symbol::Repeat) {
                    return true;
                } else if (cs == symbol::Parenthesis or cs == symbol::Block or cs == symbol::If) {
                    return canSkipLoopContextList(CDR(ast), breakOK);
                } else {
                    return canSkipLoopContextList(CDR(ast), false);
                }
            }
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
        for (unsigned i = 0; i < caseAsts.size(); ++i) {
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
        Value * res;
        if (CDR(CDR(call)) != R_NilValue) {
            Value * rhs = compileExpression(CAR(CDR(CDR(call))));
            res = INTRINSIC(b, lhs, rhs, constant(call), context->rho);
        } else {
            res = INTRINSIC(u, lhs, constant(call), context->rho);
        }
        return res;
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

    Value * constant(SEXP value) {
        return loadConstant(value, m.getM(), context->b);
    }

    /** Converts given integer to bitcode value. This is just a simple shorthand function, no magic here.
      */
    static ConstantInt * constant(int value) {
        return ConstantInt::get(getGlobalContext(), APInt(32, value));
    }

    template <typename ...Values>
    Value * INTRINSIC(Value * fun, Values... args) {
        return INTRINSIC(fun, std::vector<Value*>({args...}));
    }

    Value * INTRINSIC(Value * fun, std::vector<Value*> args) {
        return insertCall(fun, args, context->b, m, context->function_id);
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

    std::vector<uint64_t> functionIds;
};

class ICCompiler  {
public:
    ICCompiler(uint64_t stackmapIdC, int size, JITModule & m, unsigned fid) :
            m(m), size(size), functionId(fid) {
        // Set up a function type which corresponds to the ICStub signature
        std::vector<Type*> argT;
        for (int i = 0; i < size + 3; i++) {
            argT.push_back(t::SEXP);
        }
        argT.push_back(t::nativeFunctionPtr_t);

        auto funT = FunctionType::get(t::SEXP, argT, false);
        ic_t = funT;

        f = Function::Create(funT, Function::ExternalLinkage, "callIC", m);
        setupFunction(*f);
        b = BasicBlock::Create(getGlobalContext(), "start", f, nullptr);

        // Load the args in the same order as the stub
        Function::arg_iterator argI = f->arg_begin();
        for (int i = 0; i < size; i++) {
            icArgs.push_back(argI++);
        }

        call = argI++;
        call->setName("call");
        fun = argI++;
        fun->setName("op");
        rho = argI++;
        rho->setName("rho");
        caller = argI++;
        caller->setName("caller");

        stackmapId = ConstantInt::get(m.getContext(), APInt(64, stackmapIdC, false));
    }

    Function * compileStub() {
        Value * res = compileCallStub();

        ReturnInst::Create(getGlobalContext(), res, b);

        return f;
    }

    void * compile(SEXP inCall, SEXP inFun, SEXP inRho) {

        if (!compileIc(inCall, inFun))
            compileGenericIc(inCall, inFun);

        return finalize();
    }

private:
    void * finalize() {
        // FIXME: Allocate a NATIVESXP, or link it to the caller??

        ExecutionEngine * engine = jitModule(m.getM());
        // m.dump();
        void * ic = engine->getPointerToFunction(f);

        recordStackmaps({functionId});
        new_stackmap_addr = nullptr;

        return ic;
    }

    Value * compileCallStub() {
        Value * icAddr = INTRINSIC(m.compileIC,
                ConstantInt::get(getGlobalContext(), APInt(64, size)),
                call, fun, rho, stackmapId);

        INTRINSIC(m.patchIC, icAddr, stackmapId, caller);

        Value * ic = new BitCastInst(icAddr, PointerType::get(ic_t, 0), "", b);

        std::vector<Value*> allArgs;
        allArgs.insert(allArgs.end(), icArgs.begin(), icArgs.end());
        allArgs.push_back(call);
        allArgs.push_back(fun);
        allArgs.push_back(rho);
        allArgs.push_back(caller);

        return INTRINSIC_NO_SAFEPOINT(ic, allArgs);
    }

    bool compileIc(SEXP inCall, SEXP inFun) {
        if (TYPEOF(inFun) == CLOSXP) {
            std::vector<bool> promarg(icArgs.size(), false);

            // Check for named args or ...
            SEXP arg = CDR(inCall);
            SEXP form = FORMALS(inFun);
            int i = 0;
            while (arg != R_NilValue && form != R_NilValue) {
                // We do not yet do the static version of match.c, thus cannot
                // support named args
                if (TAG(arg) != R_NilValue)
                    return false;

                // We cannot inline ellipsis
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
                form = CDR(form);
            }

            // number of args != number of formal args, fallback to generic
            if (form != R_NilValue || i != icArgs.size())
                return false;

            SEXP body = CDR(inFun);
            // TODO: If the body is not native we could jit it here
            if (TYPEOF(body) == NATIVESXP) {

                BasicBlock * icMatch = BasicBlock::Create(
                        getGlobalContext(), "icMatch", f, nullptr);
                BasicBlock * icMiss = BasicBlock::Create(
                        getGlobalContext(), "icMiss", f, nullptr);
                BasicBlock * end = BasicBlock::Create(
                        getGlobalContext(), "end", f, nullptr);

                // Insert a guard to check if the incomming function matches
                // the one we got this time
                ICmpInst * test = new ICmpInst(*b,
                        ICmpInst::ICMP_EQ, fun, constant(inFun), "guard");
                BranchInst::Create(icMatch, icMiss, test, b);

                b = icMatch;

                // This is an inlined version of applyNativeClosure
                Value * arglist = constant(R_NilValue);

                // This reverses the arglist, but quickArgumentAdapter
                // reverses again
                // TODO: construct the environment in one go,
                // without using quickArgumentAdapter
                for (int i = 0; i < icArgs.size(); ++i) {
                    Value * arg = icArgs[i];
                    if (promarg[i])
                        arg = INTRINSIC(m.createPromise, arg, rho);
                    arglist = INTRINSIC(m.CONS_NR, arg, arglist);
                }

                Value * newrho = INTRINSIC(m.closureQuickArgumentAdaptor,
                        fun, arglist);

                Value * cntxt = new AllocaInst(t::cntxt, "", b);

                INTRINSIC(m.initClosureContext,
                        cntxt, call, newrho, rho, arglist, fun);

                Value * res = INTRINSIC_NO_SAFEPOINT(
                        m.closureNativeCallTrampoline,
                        cntxt, constant(body), newrho);

                INTRINSIC(m.endClosureContext, cntxt, res);

                BranchInst::Create(end, b);
                b = icMiss;

                Value * missRes = compileCallStub();

                BranchInst::Create(end, b);
                b = end;

                PHINode * phi = PHINode::Create(t::SEXP, 2, "", b);
                phi->addIncoming(res, icMatch);
                phi->addIncoming(missRes, icMiss);
                ReturnInst::Create(getGlobalContext(), phi, b);

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
        // TODO: only emit one branch depending on the type we currently see

        // now create the CallArgs structure as local variable
        Value * callArgs = new AllocaInst(t::CallArgs, "callArgs", b);
        // set first to R_NilValue
        std::vector<Value*> callArgsIndices = { constant(0), constant(0) };
        Value * callArgsFirst = GetElementPtrInst::Create(t::CallArgs, callArgs, callArgsIndices, "", b);

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
        Value * specialResult = INTRINSIC(m.call,
                constant(call), fun, constant(R_NilValue), rho);
        BranchInst::Create(next, b);
        // in builtin mode evaluate all arguments eagerly
        b = builtin;
        Value * args = compileArguments(callArgs, callArgsFirst, CDR(call), /*eager=*/ true);
        Value * builtinResult = INTRINSIC(m.call,
                constant(call), fun, args, rho);
        builtin = b; // bb might have changed during arg evaluation
        BranchInst::Create(next, b);
        // in general closure case the arguments will become promises
        b = closure;
        args = compileArguments(callArgs, callArgsFirst, CDR(call), /*eager=*/ false);
        Value * closureResult = INTRINSIC(m.call,
                constant(call), fun, args, rho);
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
        // This list has to stay in sync with Compiler::compileArgument
        // note: typeof(arg) does not correspond to the runtime type of the ic
        // arg, since the caller already converts non self evaluating arguments
        // to promises or native code.
        switch (TYPEOF(arg)) {
        case LGLSXP:
        case INTSXP:
        case REALSXP:
        case CPLXSXP:
        case STRSXP:
        case NILSXP:
            // literals are self-evaluating
            result = icArgs[argnum];
            break;
        case SYMSXP:
            if (arg == R_DotsSymbol) {
                INTRINSIC(m.addEllipsisArgument, args, rho, eager ? constant(TRUE) : constant(FALSE));
                return;
            }
            // Fall through:
        default:
            if (eager) {
                // TODO make this more efficient?
                result = INTRINSIC(m.callNative, icArgs[argnum], rho);
            } else {
                // we must create a promise out of the argument
                result = INTRINSIC(m.createPromise, icArgs[argnum], rho);
            }
            break;
        }
        SEXP name = TAG(argAst);
        if (name != R_NilValue)
            INTRINSIC(m.addKeywordArgument, args, result, constant(name));
        else
            INTRINSIC(m.addArgument, args, result);
    }

    /** Converts given integer to bitcode value. This is just a simple shorthand function, no magic here.
      */
    static ConstantInt * constant(int value) {
        return ConstantInt::get(getGlobalContext(), APInt(32, value));
    }

    Value * constant(SEXP value) {
        return loadConstant(value, m.getM(), b);
    }

    template <typename ...Values>
    Value * INTRINSIC_NO_SAFEPOINT(Value * fun, Values... args) {
        return INTRINSIC_NO_SAFEPOINT(fun, std::vector<Value*>({args...}));
    }

    Value * INTRINSIC_NO_SAFEPOINT(Value * fun, std::vector<Value*> args) {
        return insertCall(fun, args, b, m, -1);
    }

    template <typename ...Values>
    Value * INTRINSIC(Value * fun, Values... args) {
        return INTRINSIC(fun, std::vector<Value*>({args...}));
    }

    Value * INTRINSIC(Value * fun, std::vector<Value*> args) {
        return insertCall(fun, args, b, m, functionId);
    }

    Type * ic_t;

    Function * f;
    BasicBlock * b;

    Value * rho;
    Value * fun;
    Value * caller;
    Value * stackmapId;
    Value * call;
    std::vector<Value*> icArgs;

    JITModule & m;
    unsigned size;
    unsigned functionId;
};



Value * Compiler::compileICCallStub(Value * call, Value * op, std::vector<Value*> & callArgs) {
    uint64_t smid = nextStackmapId++;

    auto ic_function_id = nextStackmapId++;
    functionIds.push_back(ic_function_id);
    ICCompiler ic(smid, callArgs.size(), m, ic_function_id);
    auto ic_stub = ic.compileStub();

    std::vector<Value*> ic_args;
    // Closure arguments
    for (auto arg : callArgs) {
        ic_args.push_back(arg);
    }

    // Additional IC arguments
    ic_args.push_back(call);
    ic_args.push_back(op);
    ic_args.push_back(context->rho);
    ic_args.push_back(context->f);

    // Record a patch point
    emitStackmap(smid, {{ic_stub}}, m, context->b);

    return INTRINSIC(ic_stub, ic_args);
}



void patchIC(void * ic, uint64_t stackmapId, void * caller) {
    auto r = StackMap::getPatchpoint(stackmapId);
    assert(r.getNumLocations() == 1);

    uint8_t * patchAddr = (uint8_t*) ((uintptr_t)caller + r.getInstructionOffset());

    int reg = r.getLocation(0).getDwarfRegNum();

    uint8_t prefix = reg > 7 ? 0x49 : 0x48;
    uint8_t movinst = 0xb8 + (reg%8);

    static_assert(patchpointSize == 10, "requre 10 bytes to patch call");

    *patchAddr++ = prefix;
    *patchAddr++ = movinst;

    *(void**)(patchAddr) = ic;
}

void * compileIC(uint64_t numargs, SEXP call, SEXP fun, SEXP rho, uint64_t stackmapId) {
    JITModule m("ic");

    ICCompiler compiler(stackmapId, numargs, m, nextStackmapId++);

    return compiler.compile(call, fun, rho);
}

/** More complex compilation method that compiles multiple functions into a specified module name.

  The module name is expected to be a STRSXP and the functions is expected to be a pairlist. If pairlist has tags associated with the elements, they will be used as function names.
 */
SEXP compileFunctions(SEXP moduleName, SEXP functions) {
    char const * mName = CHAR(STRING_ELT(moduleName, 0));
    Compiler c(mName);
    while (functions != R_NilValue) {
        SEXP f = CAR(functions);
        // get the function ast
        SEXP body = BODY(f);
        SEXP name = TAG(functions);
        char const * fName = (name == R_NilValue) ? "unnamed function" : CHAR(PRINTNAME(name));
        if (TYPEOF(body) == BCODESXP)
            std::cout << "Ignoring " << fName << " because it is in bytecode" << std::endl;
        else if (TYPEOF(body) == NATIVESXP)
            std::cout << "Ignoring " << fName << " because it is already compiled" << std::endl;
        else
            SET_BODY(f, c.compileFunction(fName, body));
        // move to next function
        functions = CDR(functions);
    }
    c.jitAll();
    return moduleName;
}

SEXP compile(SEXP ast) {
    Compiler c("module");
    SEXP result = c.compile("rfunction", ast);
    c.jitAll();
    return result;
}

} // namespace

#endif // COMPILER_CPP

