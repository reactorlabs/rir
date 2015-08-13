#include <sstream>
#include <iostream>

#include "llvm_includes.h"
#include "codegen.h"

#include "rbc.h"

#include "runtime_helper.h"
#include "jit_module.h"
#include "jit_types.h"

#define STR(WHAT)                                                              \
  ((static_cast<std::ostringstream &>(                                         \
        std::ostringstream().seekp(0, std::ios_base::cur) << WHAT)).str())


using namespace llvm;

namespace {




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
    Compiler() : runtime(RuntimeHelper::helper) {
        assert(initialized and
               "Call initializeJIT before instantiating compiler");
    }

    static bool initialized;
    static void initializeJIT() {
        LLVMInitializeNativeTarget();
        LLVMInitializeNativeAsmPrinter();
        LLVMInitializeNativeAsmParser();
        T::initialize(RuntimeHelper::helper);
        initialized = true;
    }


    Function * compile(SEXP bytecode, Twine const & name) {
        assert(TYPEOF(bytecode) == BCODESXP and "Only bytecode allowed here");
        body = R_bcDecode(BCODE_CODE(bytecode));
        code = INTEGER(body);
        consts = BCODE_CONSTS(bytecode);
        // create the function
        f = Function::Create(
                runtime.t->t_applyClosure,
                Function::ExternalLinkage,
                name,
                module);
        // initialize the evaluation context
        initialize();
        // compile the bytecode into its own
        compileBytecode();
        // call the finalizer in last basic block
        finalize();
        // verify the function we have
        f->dump();
        verifyFunction(*f);
        return f;
    }

    rjit::RFunctionPtr jit() {
        assert(f != nullptr and "compile must be called before");
        std::string err;
        ExecutionEngine * engine = EngineBuilder(std::unique_ptr<Module>(module))
          .setErrorStr(&err)
          .setEngineKind(EngineKind::JIT)
          .create();
        if (!engine) {
          fprintf(stderr, "Could not create ExecutionEngine: %s\n", err.c_str());
          DIE;
        }
        engine->finalizeObject();
        return reinterpret_cast<rjit::RFunctionPtr>(engine->getPointerToFunction(f));
    }



private:

    void initialize() {
        current = BasicBlock::Create(getGlobalContext(), "start", f, nullptr);

        Function::arg_iterator args = f->arg_begin();
        Value * call = args++;
        call->setName("call");
        Value * op = args++;
        op->setName("op");
        Value * arglist = args++;
        arglist->setName("arglist");
        Value * sysparent = args++;
        sysparent->setName("sysparent");
        Value * suppliedvars = args++;
        suppliedvars->setName("suppliedvars");

        // Get consts out of closure
        // TODO: directly access struct

        Value * cons = CallInst::Create(
                module.getFunction("getConstsFromClosure"),
                std::vector<Value *>({{op}}),
                "consts",
                current);

        Value * rho = CallInst::Create(
                module.getFunction("closureArgumentAdaptor"),
                std::vector<Value *>({{call, op, arglist, sysparent, suppliedvars}}),
                "rho",
                current);

        cntxt = new AllocaInst(runtime.t->t_RCNTXT, "cntxt", current);
        CallInst::Create(
                module.getFunction("initClosureContext"),
                std::vector<Value *>({{
                    cntxt, call, rho, sysparent, arglist, op}}),
                "",
                current);
        
        /* todo setjmp/longjmp */

        // split the bytecode into basic blocks
        bbs.analyze(body, consts, f);

        // create context struct on the stack
        context = new AllocaInst(
                runtime.t->t_InterpreterContext, "context", current);

        // call the initializer
        CallInst::Create(
                module.getFunction("initializeInterpreter"),
                std::vector<Value *>({{
                    context, cons, rho,
                    module.constant(1), module.constant(code[0])}}),
                "",
                current);

        // create last basic block
        lastBB = BasicBlock::Create(getGlobalContext(), "end", f, nullptr);
        // jump to pc1 basic block from the initial basic block
        BranchInst::Create(bbs.blockForPc(1), current);
    }

    void finalize() {
        Value * result = CallInst::Create(
                module.getFunction("finalizeInterpreter"), context, "result", lastBB);
        CallInst::Create(
                module.getFunction("endClosureContext"),
                std::vector<Value *>({{cntxt, result}}),
                "",
                lastBB);
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
    Value * cntxt;
    int pc;
    RuntimeHelper & runtime;
};
bool Compiler::initialized = false;


} // namespace



namespace rjit {

void initializeJIT() {
    Compiler::initializeJIT();
}

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
