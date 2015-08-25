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

#include <sstream>

#include "codegen.h"

#include "rbc.h"

#define STR(WHAT)                                                              \
  ((static_cast<std::ostringstream &>(                                         \
        std::ostringstream().seekp(0, std::ios_base::cur) << WHAT)).str())


using namespace llvm;

namespace {

StructType * initializeTypes();

StructType * t_SEXPREC = initializeTypes();
PointerType * t_SEXP;
IntegerType * t_Rboolean;
StructType * t_InterpreterContext;
PointerType * p_InterpreterContext;
FunctionType * t_InterpreterLoop;

FunctionType * t_InitializeInterpreter;
FunctionType * t_FinalizeInterpreter;

FunctionType * t_voidInstruction0;
FunctionType * t_voidInstruction1;
FunctionType * t_voidInstruction2;
FunctionType * t_voidInstruction3;
FunctionType * t_voidInstruction4;

FunctionType * t_intInstruction0;
FunctionType * t_intInstruction1;
FunctionType * t_intInstruction2;
FunctionType * t_intInstruction3;
FunctionType * t_intInstruction4;


/** Creates the types for the codegen.

  Since the types do not depend on modules, but on the context, we can pregenerate them and use in all modules that will be created by the jit.
  */
StructType * initializeTypes() {
    LLVMContext & context = getGlobalContext();
    std::vector<Type*> fields;
    StructType * t_sxpinfo_struct = StructType::create(context, "struct.sxpinfo_struct");
    t_SEXPREC = StructType::create(context, "struct.SEXPREC");
    // SEXP
    t_SEXP = PointerType::get(t_SEXPREC, 0);
    // sxpinfo_struct is just int32 in a structure, the bitmasking is not a concern of the type
    fields.push_back(IntegerType::get(context, 32));
    t_sxpinfo_struct->setBody(fields, false);
    // primsxp
    fields.clear();
    // SEXPREC, first the union
    fields.clear();
    StructType * u1 = StructType::create(context,"union.SEXP_SEXP_SEXP");
    fields.push_back(t_SEXP);
    fields.push_back(t_SEXP);
    fields.push_back(t_SEXP);
    u1->setBody(fields, false);
    // now the real SEXPREC
    fields.clear();
    fields.push_back(t_sxpinfo_struct);
    fields.push_back(t_SEXP);
    fields.push_back(t_SEXP);
    fields.push_back(t_SEXP);
    fields.push_back(u1);
    t_SEXPREC->setBody(fields, false);
    //  Pointer to R_bcStack_t, first the union
    StructType * u2 = StructType::create(context, "union.INT_DOUBLE_SEXP");
    fields.clear();
    fields.push_back(Type::getDoubleTy(context));
    u2->setBody(fields, false);
    // then the type
    StructType * t_R_bcstack_t = StructType::create(context, "struct.R_bcstack_t");
    fields.clear();
    fields.push_back(IntegerType::get(context, 32));
    fields.push_back(u2);
    t_R_bcstack_t->setBody(fields);
    // and now the pointer
    PointerType * bcStackPtr = PointerType::get(t_R_bcstack_t, 0);
    // RBoolean
    t_Rboolean = IntegerType::get(context, 32);
    // InterpreterContext
    t_InterpreterContext = StructType::create(context, "struct.InterpreterContext");
    fields.clear();
    fields.push_back(t_SEXP);
    fields.push_back(t_SEXP);
    fields.push_back(t_Rboolean);
    fields.push_back(t_SEXP);
    fields.push_back(t_SEXP);
    fields.push_back(bcStackPtr);
    fields.push_back(bcStackPtr);
    fields.push_back(t_Rboolean);
    t_InterpreterContext->setBody(fields);
    p_InterpreterContext = PointerType::get(t_InterpreterContext, 0);
    // Interpreter function
    fields.clear();
    fields.push_back(t_SEXP);
    fields.push_back(t_SEXP);
    fields.push_back(t_Rboolean);
    t_InterpreterLoop = FunctionType::get(t_SEXP, fields, false);
    // instruction types
    std::vector<Type*> args;
    // interpreter initializer
    args.clear();
    args.push_back(p_InterpreterContext);
    args.push_back(t_SEXP);
    args.push_back(t_SEXP);
    args.push_back(t_Rboolean);
    args.push_back(IntegerType::get(context, 32));
    t_InitializeInterpreter = FunctionType::get(Type::getVoidTy(context), args, false);
    // interpreter finalizer
    args.clear();
    args.push_back(p_InterpreterContext);
    t_FinalizeInterpreter = FunctionType::get(t_SEXP, args, false);
    // instruction types
    args.clear();
    args.push_back(p_InterpreterContext);
    t_voidInstruction0 = FunctionType::get(Type::getVoidTy(context), args, false);
    args.clear();
    args.push_back(p_InterpreterContext);
    args.push_back(IntegerType::get(context, 32));
    t_voidInstruction1 = FunctionType::get(Type::getVoidTy(context), args, false);
    args.clear();
    args.push_back(p_InterpreterContext);
    args.push_back(IntegerType::get(context, 32));
    args.push_back(IntegerType::get(context, 32));
    t_voidInstruction2 = FunctionType::get(Type::getVoidTy(context), args, false);
    args.clear();
    args.push_back(p_InterpreterContext);
    args.push_back(IntegerType::get(context, 32));
    args.push_back(IntegerType::get(context, 32));
    args.push_back(IntegerType::get(context, 32));
    t_voidInstruction3 = FunctionType::get(Type::getVoidTy(context), args, false);
    args.clear();
    args.push_back(p_InterpreterContext);
    args.push_back(IntegerType::get(context, 32));
    args.push_back(IntegerType::get(context, 32));
    args.push_back(IntegerType::get(context, 32));
    args.push_back(IntegerType::get(context, 32));
    t_voidInstruction4 = FunctionType::get(Type::getVoidTy(context), args, false);
    args.clear();
    args.push_back(p_InterpreterContext);
    t_intInstruction0 = FunctionType::get(IntegerType::get(context, 32), args, false);
    args.clear();
    args.push_back(p_InterpreterContext);
    args.push_back(IntegerType::get(context, 32));
    t_intInstruction1 = FunctionType::get(IntegerType::get(context, 32), args, false);
    args.clear();
    args.push_back(p_InterpreterContext);
    args.push_back(IntegerType::get(context, 32));
    args.push_back(IntegerType::get(context, 32));
    t_intInstruction2 = FunctionType::get(IntegerType::get(context, 32), args, false);
    args.clear();
    args.push_back(p_InterpreterContext);
    args.push_back(IntegerType::get(context, 32));
    args.push_back(IntegerType::get(context, 32));
    args.push_back(IntegerType::get(context, 32));
    t_intInstruction3 = FunctionType::get(IntegerType::get(context, 32), args, false);
    args.clear();
    args.push_back(p_InterpreterContext);
    args.push_back(IntegerType::get(context, 32));
    args.push_back(IntegerType::get(context, 32));
    args.push_back(IntegerType::get(context, 32));
    args.push_back(IntegerType::get(context, 32));
    t_intInstruction4 = FunctionType::get(IntegerType::get(context, 32), args, false);
}

/** Simple class that encapsulates a LLVM module used to compile R's function. Contains the module itself and all declarations of functions that the JIT may use - the R bytecode opcodes and evaluation helpers.
 */
class JITModule {
public:

    operator Module * () {
        return module;
    }

    ConstantInt * constant(int value) {
        return ConstantInt::get(getGlobalContext(), APInt(32, value));
    }

    /** Creates new LLVM module and populates it with declarations of the helper and opcode functions.
      */
    JITModule() {
        // create new module
        module = new Module("", getGlobalContext());
        // generate the function declarations
        // interpreter initialization & finalization
        initializeInterpreter = Function::Create(t_InitializeInterpreter, Function::ExternalLinkage, "initializeInterpreter", module);
        finalizeInterpreter = Function::Create(t_FinalizeInterpreter, Function::ExternalLinkage, "finalizeInterpreter", module);
        // switch special instructions
        SWITCH_OP_start = Function::Create(t_intInstruction4, Function::ExternalLinkage, "instructionSWITCH_OP_start", module);
        SWITCH_OP_character = Function::Create(t_intInstruction4, Function::ExternalLinkage, "instructionSWITCH_OP_character", module);
        SWITCH_OP_integral = Function::Create(t_intInstruction4, Function::ExternalLinkage, "instructionSWITCH_OP_integral", module);
        // handle the normal instructions
        #define SCONCAT(a) #a
        #define INSTRUCTION0(name, opcode) name = Function::Create(t_voidInstruction0, Function::ExternalLinkage, SCONCAT(instruction ## name), module);
        #define INSTRUCTION1(name, opcode) name = Function::Create(t_voidInstruction1, Function::ExternalLinkage, SCONCAT(instruction ## name), module);
        #define INSTRUCTION2(name, opcode) name = Function::Create(t_voidInstruction2, Function::ExternalLinkage, SCONCAT(instruction ## name), module);
        #define INSTRUCTION3(name, opcode) name = Function::Create(t_voidInstruction3, Function::ExternalLinkage, SCONCAT(instruction ## name), module);
        #define SPECIAL0(name, opcode) name = Function::Create(t_voidInstruction0, Function::ExternalLinkage, SCONCAT(instruction ## name), module);
        #define SPECIAL1(name, opcode) name = Function::Create(t_voidInstruction1, Function::ExternalLinkage, SCONCAT(instruction ## name), module);
        #define SPECIAL2(name, opcode) name = Function::Create(t_intInstruction2, Function::ExternalLinkage, SCONCAT(instruction ## name), module);
        #define SPECIAL3(name, opcode) name = Function::Create(t_voidInstruction3, Function::ExternalLinkage, SCONCAT(instruction ## name), module);
        #define SPECIAL4(name, opcode)
        RBC
        #undef INSTRUCTION0
        #undef INSTRUCTION1
        #undef INSTRUCTION2
        #undef INSTRUCTION3
        #undef SPECIAL0
        #undef SPECIAL1
        #undef SPECIAL2
        #undef SPECIAL3
        #undef SPECIAL4
    }

    Module * module;
    Function * initializeInterpreter;
    Function * finalizeInterpreter;
    // pregenerated instruction functions
    #define INSTRUCTION0(name, opcode) Function * name;
    #define INSTRUCTION1(name, opcode) Function * name;
    #define INSTRUCTION2(name, opcode) Function * name;
    #define INSTRUCTION3(name, opcode) Function * name;
    #define SPECIAL0(name, opcode) Function * name;
    #define SPECIAL1(name, opcode) Function * name;
    #define SPECIAL2(name, opcode) Function * name;
    #define SPECIAL3(name, opcode) Function * name;
    #define SPECIAL4(name, opcode) Function * name;
        RBC
    #undef INSTRUCTION0
    #undef INSTRUCTION1
    #undef INSTRUCTION2
    #undef INSTRUCTION3
    #undef SPECIAL0
    #undef SPECIAL1
    #undef SPECIAL2
    #undef SPECIAL3
    #undef SPECIAL4

};


/** Given R bytecode and LLVM function, creates basic blocks for it appropriately.
 */
class BasicBlockAnalysis {
public:
    BasicBlockAnalysis() {}

    /** Analyzes the given code, finds all basic blocks and creates LLVM basic blocks for them.

      The BB discovery is not really optimal at all, but as soon as we move from RBC and translate directly from ASTs it will be dead code anyways. So I do not really care now.
     */
    void analyze(SEXP code, SEXP consts,  Function * f) {
        using namespace rjit;
        int pc = 1;
        int const length = length(code);
        int * cp = INTEGER(code);
        blocks_.resize(length);
        BasicBlock * last = BasicBlock::Create(getGlobalContext(), "pc1", f, nullptr);
        while (pc < length) {
            // propagate last basic block to current instruction, if it has not been marked as basic block start already
            if (blocks_[pc] == nullptr)
                blocks_[pc] = last;
            else
                last = blocks_[pc];
            // based on the opcode of the instruction, mark necessary basic block beginnings
            switch (static_cast<Opcode>(cp[pc])) {
            case Opcode::RETURN_OP:
            case Opcode::RETURNJMP_OP:
                markAsNewBasicBlock(pc + 1, pc, f); // next instruction
                ++pc;
                break;
            case Opcode::GOTO_OP:
                blocks_[pc + 1] = blocks_[pc];
                markAsNewBasicBlock(pc + 2, pc, f); // next instruction
                markAsNewBasicBlock(cp[pc + 1], pc, f); // target
                pc +=2;
                break;
            case Opcode::STARTFOR_OP:
                blocks_[pc + 3] = blocks_[pc + 2] = blocks_[pc + 1] = blocks_[pc];
                markAsNewBasicBlock(pc + 2, pc, f);
                markAsNewBasicBlock(cp[pc + 3], pc, f);
                pc += 4;
                break;
            case Opcode::SWITCH_OP_start:
                blocks_[pc + 4] = blocks_[pc + 3] = blocks_[pc + 2] = blocks_[pc + 1] = blocks_[pc];
                markAsNewBasicBlock(VECTOR_ELT(consts, cp[pc+3]), pc, f);
                markAsNewBasicBlock(VECTOR_ELT(consts, cp[pc+4]), pc, f);
                pc += 5;
                break;
#define INSTRUCTION0(name, opcode) case Opcode::name: ++pc; break;
#define INSTRUCTION1(name, opcode) case Opcode::name: blocks_[pc + 1] = blocks_[pc]; pc += 2; break;
#define INSTRUCTION2(name, opcode) case Opcode::name: blocks_[pc + 2] = blocks_[pc + 1] = blocks_[pc]; pc += 3; break;
#define INSTRUCTION3(name, opcode) case Opcode::name: blocks_[pc + 3] = blocks_[pc + 2] = blocks_[pc + 1] = blocks_[pc]; pc += 4; break;
#define SPECIAL0(name, opcode)
#define SPECIAL1(name, opcode)
#define SPECIAL2(name, opcode) case Opcode::name: blocks_[pc + 2] = blocks_[pc + 1] = blocks_[pc]; markAsNewBasicBlock(pc + 3, pc, f); markAsNewBasicBlock(cp[pc + 2], pc, f); pc += 3; break;
#define SPECIAL3(name, opcode)
#define SPECIAL4(name, opcode)
                RBC
        #undef INSTRUCTION0
        #undef INSTRUCTION1
        #undef INSTRUCTION2
        #undef INSTRUCTION3
        #undef SPECIAL0
        #undef SPECIAL1
        #undef SPECIAL2
        #undef SPECIAL3
        #undef SPECIAL4
            default:
                assert(false and "Unknown opcode");
            }
        }
    }

    BasicBlock * blockForPc(int pc) {
        return blocks_[pc];
    }

private:

    /** Marks given target pc as beginning of a basic block and creates a basic block for it, if it has not been marked as basic block beginning yet.
     */
    void markAsNewBasicBlock(int target, int current, Function * f) {
        // do nothing if the instruction is after the code, that is fine
        if (target >= blocks_.size())
            return;
        // if we haven't yet seen the target pc, create a basic block for it
        if (blocks_[target] == nullptr) {
            blocks_[target] = BasicBlock::Create(getGlobalContext(), STR("pc" << target), f, nullptr);
        // otherwise, if the target is after current index, it has already been marked as bb start
        } else if (current > target) {
            // if the target is already a beginning of a basic block, do nothing, else:
            if (blocks_[target - 1] == blocks_[target]) {
                BasicBlock * old = blocks_[target];
                blocks_[target] = BasicBlock::Create(getGlobalContext(), STR("pc" << target), f, nullptr);
                while (++target < blocks_.size())
                    if (blocks_[target] == old)
                        blocks_[target] = blocks_[target - 1];
            }
        }
    }

    /** Marks all addresses in given vector as beginnings of basic blocks.
     */
    void markAsNewBasicBlock(SEXP vector, int current, Function * f) {
        for (int i = 0; i < length(vector); ++i)
            markAsNewBasicBlock(INTEGER(vector)[i], current, f);
    }

    std::vector<BasicBlock *> blocks_;

};



class Compiler {
public:
    Function * compile(SEXP bytecode, Twine const & name) {
        assert(TYPEOF(bytecode) == BCODESXP and "Only bytecode allowed here");
        body = R_bcDecode(BCODE_CODE(bytecode));
        code = INTEGER(body);
        consts = BCODE_CONSTS(bytecode);
        // create the function
        f = Function::Create(t_InterpreterLoop, Function::ExternalLinkage, name, module);
        // initialize the evaluation context
        initialize();
        // compile the bytecode into its own
        compileBytecode();
        // call the finalizer in last basic block
        finalize();
        // verify the function we have
        //f->dump();
        verifyFunction(*f);
        return f;
    }

    rjit::RFunctionPtr jit() {
        assert(f != nullptr and "compile must be called before");
        ExecutionEngine * engine = EngineBuilder(std::unique_ptr<Module>(module)).create();
        engine->finalizeObject();
        return reinterpret_cast<rjit::RFunctionPtr>(engine->getPointerToFunction(f));
    }



private:

    void initialize() {
        current = BasicBlock::Create(getGlobalContext(), "start", f, nullptr);
        // split the bytecode into basic blocks
        bbs.analyze(body, consts, f);
        // create context struct on the stack
        context = new AllocaInst(t_InterpreterContext, "context", current);
        // call the initializer
        Function::arg_iterator args = f->arg_begin();
        Value * body = args++;
        body->setName("body");
        Value * rho = args++;
        rho->setName("rho");
        Value * useCache = args++;
        useCache->setName("useCache");
        std::vector<Value *> params({context, body, rho, useCache, module.constant(code[0])});
        CallInst::Create(module.initializeInterpreter, params, "", current);
        // create last basic block
        lastBB = BasicBlock::Create(getGlobalContext(), "end", f, nullptr);
        // jump to pc1 basic block from the initial basic block
        BranchInst::Create(bbs.blockForPc(1), current);
    }

    void finalize() {
        Value * result = CallInst::Create(module.finalizeInterpreter, context, "result", lastBB);
        ReturnInst::Create(getGlobalContext(), result, lastBB);
    }

    void insertSwitch(Function * oracle, SEXP targets) {
        int * t = INTEGER(targets);
        int l = length(targets);
        SwitchInst * si = SwitchInst::Create(instruction4(oracle), bbs.blockForPc(t[l - 1 ]), l, current);
        for (int i = 0; i < l; ++i)
            si->addCase(module.constant(i), bbs.blockForPc(t[i]));
    }

#define INSTRUCTION0(name) case Opcode::name: instruction0(module.name); pc += 1; break;
#define INSTRUCTION1(name) case Opcode::name: instruction1(module.name); pc += 2; break;
#define INSTRUCTION2(name) case Opcode::name: instruction2(module.name); pc += 3; break;
#define INSTRUCTION3(name) case Opcode::name: instruction3(module.name); pc += 4; break;
#define CONDITIONAL_JUMP(name) case Opcode::name: { \
    Value * v = new ICmpInst(*current, ICmpInst::ICMP_EQ, instruction2(module.name), module.constant(1)); \
    BranchInst::Create(bbs.blockForPc(code[pc + 2]), bbs.blockForPc(pc + 3), v, current); \
    pc += 3; \
    break; }

    /** Compiles R bytecode into LLVM IR representation.
     */
    void compileBytecode() {
        using namespace rjit;
        pc = 1;
        int l = length(body);
        while (pc < l) {
            current = bbs.blockForPc(pc);
            switch (static_cast<Opcode>(code[pc])) {
            default:
            INSTRUCTION0(BCMISMATCH_OP);
            case Opcode::RETURN_OP:
                instruction0(module.RETURN_OP);
                BranchInst::Create(lastBB, current);
                pc += 1;
                break;
            case Opcode::GOTO_OP:
                instruction1(module.GOTO_OP);
                BranchInst::Create(bbs.blockForPc(code[pc + 1]), current);
                pc += 2;
                break;
            CONDITIONAL_JUMP(BRIFNOT_OP);
            INSTRUCTION0(POP_OP);
            INSTRUCTION0(DUP_OP);
            INSTRUCTION0(PRINTVALUE_OP);
            INSTRUCTION1(STARTLOOPCNTXT_OP);
            INSTRUCTION0(ENDLOOPCNTXT_OP);
            INSTRUCTION0(DOLOOPNEXT_OP);
            INSTRUCTION0(DOLOOPBREAK_OP);
            case Opcode::STARTFOR_OP:
                instruction3(module.STARTFOR_OP);
                BranchInst::Create(bbs.blockForPc(code[pc + 3]), current);
                pc += 4;
                break;
            CONDITIONAL_JUMP(STEPFOR_OP);
            INSTRUCTION0(ENDFOR_OP);
            INSTRUCTION0(SETLOOPVAL_OP);
            INSTRUCTION0(INVISIBLE_OP);
            INSTRUCTION1(LDCONST_OP);
            INSTRUCTION0(LDNULL_OP);
            INSTRUCTION0(LDTRUE_OP);
            INSTRUCTION0(LDFALSE_OP);
            INSTRUCTION1(GETVAR_OP);
            INSTRUCTION1(DDVAL_OP);
            INSTRUCTION1(SETVAR_OP);
            INSTRUCTION1(GETFUN_OP);
            INSTRUCTION1(GETGLOBFUN_OP);
            INSTRUCTION1(GETSYMFUN_OP);
            INSTRUCTION1(GETBUILTIN_OP);
            INSTRUCTION1(GETINTLBUILTIN_OP);
            INSTRUCTION0(CHECKFUN_OP);
            INSTRUCTION1(MAKEPROM_OP);
            INSTRUCTION0(DOMISSING_OP);
            INSTRUCTION1(SETTAG_OP);
            INSTRUCTION0(DODOTS_OP);
            INSTRUCTION0(PUSHARG_OP);
            INSTRUCTION1(PUSHCONSTARG_OP);
            INSTRUCTION0(PUSHNULLARG_OP);
            INSTRUCTION0(PUSHTRUEARG_OP);
            INSTRUCTION0(PUSHFALSEARG_OP);
            INSTRUCTION1(CALL_OP);
            INSTRUCTION1(CALLBUILTIN_OP);
            INSTRUCTION1(CALLSPECIAL_OP);
            INSTRUCTION1(MAKECLOSURE_OP);
            INSTRUCTION1(UMINUS_OP);
            INSTRUCTION1(UPLUS_OP);
            INSTRUCTION1(ADD_OP);
            INSTRUCTION1(SUB_OP);
            INSTRUCTION1(MUL_OP);
            INSTRUCTION1(DIV_OP);
            INSTRUCTION1(EXPT_OP);
            INSTRUCTION1(SQRT_OP);
            INSTRUCTION1(EXP_OP);
            INSTRUCTION1(EQ_OP);
            INSTRUCTION1(NE_OP);
            INSTRUCTION1(LT_OP);
            INSTRUCTION1(LE_OP);
            INSTRUCTION1(GE_OP);
            INSTRUCTION1(GT_OP);
            INSTRUCTION1(AND_OP);
            INSTRUCTION1(OR_OP);
            INSTRUCTION1(NOT_OP);
            INSTRUCTION0(DOTSERR_OP);
            INSTRUCTION1(STARTASSIGN_OP);
            INSTRUCTION1(ENDASSIGN_OP);
            CONDITIONAL_JUMP(STARTSUBSET_OP);
            INSTRUCTION0(DFLTSUBSET_OP);
            CONDITIONAL_JUMP(STARTSUBASSIGN_OP);
            INSTRUCTION0(DFLTSUBASSIGN_OP);
            CONDITIONAL_JUMP(STARTC_OP);
            INSTRUCTION0(DFLTC_OP);
            CONDITIONAL_JUMP(STARTSUBSET2_OP);
            INSTRUCTION0(DFLTSUBSET2_OP);
            CONDITIONAL_JUMP(STARTSUBASSIGN2_OP);
            INSTRUCTION0(DFLTSUBASSIGN2_OP);
            INSTRUCTION2(DOLLAR_OP);
            INSTRUCTION2(DOLLARGETS_OP);
            INSTRUCTION0(ISNULL_OP);
            INSTRUCTION0(ISLOGICAL_OP);
            INSTRUCTION0(ISINTEGER_OP);
            INSTRUCTION0(ISDOUBLE_OP);
            INSTRUCTION0(ISCOMPLEX_OP);
            INSTRUCTION0(ISCHARACTER_OP);
            INSTRUCTION0(ISSYMBOL_OP);
            INSTRUCTION0(ISOBJECT_OP);
            INSTRUCTION0(ISNUMERIC_OP);
            INSTRUCTION1(VECSUBSET_OP);
            INSTRUCTION1(MATSUBSET_OP);
            INSTRUCTION1(VECSUBASSIGN_OP);
            INSTRUCTION1(MATSUBASSIGN_OP);
            CONDITIONAL_JUMP(AND1ST_OP);
            INSTRUCTION1(AND2ND_OP);
            CONDITIONAL_JUMP(OR1ST_OP);
            INSTRUCTION1(OR2ND_OP);
            INSTRUCTION1(GETVAR_MISSOK_OP);
            INSTRUCTION1(DDVAL_MISSOK_OP);
            INSTRUCTION0(VISIBLE_OP);
            INSTRUCTION1(SETVAR2_OP);
            INSTRUCTION1(STARTASSIGN2_OP);
            INSTRUCTION1(ENDASSIGN2_OP);
            INSTRUCTION2(SETTER_CALL_OP);
            INSTRUCTION1(GETTER_CALL_OP);
            INSTRUCTION0(SWAP_OP);
            INSTRUCTION0(DUP2ND_OP);
            case Opcode::SWITCH_OP_start: {
                Value * v = new ICmpInst(*current, ICmpInst::ICMP_EQ, instruction4(module.SWITCH_OP_start), module.constant(1));
                BasicBlock * character = BasicBlock::Create(getGlobalContext(), STR("switch" << pc << "character"), f, nullptr);
                BasicBlock * integral = BasicBlock::Create(getGlobalContext(), STR("switch" << pc << "integral"), f, nullptr);
                BranchInst::Create(integral, character, v, current);
                current = integral;
                insertSwitch(module.SWITCH_OP_integral, VECTOR_ELT(consts, code[pc + 4]));
                current = character;
                insertSwitch(module.SWITCH_OP_character, VECTOR_ELT(consts, code[pc + 4]));
                pc += 5;
                break;
            }
            case Opcode::RETURNJMP_OP:
                instruction0(module.RETURNJMP_OP);
                BranchInst::Create(lastBB, current);
                pc += 1;
                break;
            CONDITIONAL_JUMP(STARTSUBSET_N_OP);
            CONDITIONAL_JUMP(STARTSUBASSIGN_N_OP);
            INSTRUCTION1(VECSUBSET2_OP);
            INSTRUCTION1(MATSUBSET2_OP);
            INSTRUCTION1(VECSUBASSIGN2_OP);
            INSTRUCTION1(MATSUBASSIGN2_OP);
            CONDITIONAL_JUMP(STARTSUBSET2_N_OP);
            CONDITIONAL_JUMP(STARTSUBASSIGN2_N_OP);
            INSTRUCTION2(SUBSET_N_OP);
            INSTRUCTION2(SUBSET2_N_OP);
            INSTRUCTION2(SUBASSIGN_N_OP);
            INSTRUCTION2(SUBASSIGN2_N_OP);
            INSTRUCTION1(LOG_OP);
            INSTRUCTION1(LOGBASE_OP);
            INSTRUCTION2(MATH1_OP);
            INSTRUCTION2(DOTCALL_OP);
            INSTRUCTION1(COLON_OP);
            INSTRUCTION1(SEQALONG_OP);
            INSTRUCTION1(SEQLEN_OP);
            }
        }
    }

#undef INSTRUCTION0
#undef INSTRUCTION1
#undef INSTRUCTION2
#undef INSTRUCTION3
#undef CONDITIONAL_JUMP


    Value * instruction0(Function * opcode) {
        std::vector<Value*> args;
        args.push_back(context);
        return CallInst::Create(opcode, args, "", current);
    }

    Value * instruction1(Function * opcode) {
        std::vector<Value*> args;
        args.push_back(context);
        args.push_back(module.constant(code[pc + 1]));
        return CallInst::Create(opcode, args, "", current);
    }

    Value * instruction2(Function * opcode) {
        std::vector<Value*> args;
        args.push_back(context);
        args.push_back(module.constant(code[pc + 1]));
        args.push_back(module.constant(code[pc + 2]));
        return CallInst::Create(opcode, args, "", current);
    }

    Value * instruction3(Function * opcode) {
        std::vector<Value*> args;
        args.push_back(context);
        args.push_back(module.constant(code[pc + 1]));
        args.push_back(module.constant(code[pc + 2]));
        args.push_back(module.constant(code[pc + 3]));
        return CallInst::Create(opcode, args, "", current);
    }

    Value * instruction4(Function * opcode) {
        std::vector<Value*> args;
        args.push_back(context);
        args.push_back(module.constant(code[pc + 1]));
        args.push_back(module.constant(code[pc + 2]));
        args.push_back(module.constant(code[pc + 3]));
        args.push_back(module.constant(code[pc + 4]));
        return CallInst::Create(opcode, args, "", current);
    }


    BasicBlockAnalysis bbs;
    JITModule module;
    Function * f;
    int * code;
    SEXP body;
    SEXP consts;
    BasicBlock * current;
    BasicBlock * lastBB;
    Value * context;
    int pc;




};


} // namespace



namespace rjit {


SEXP compile(SEXP bytecode) {
    Compiler c;
    Function * f = c.compile(bytecode, "rjitf");
    RFunctionPtr fptr = c.jit();
    SEXP result = CONS(reinterpret_cast<SEXP>(fptr), BCODE_CONSTS(bytecode));
    assert(TAG(result) == R_NilValue);
    SET_TAG(result, reinterpret_cast<SEXP>(f));
    SET_TYPEOF(result, NATIVESXP);
    return result;
}

} // namespace rjit
