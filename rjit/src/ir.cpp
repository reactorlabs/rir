#include "ir.h"

using namespace llvm;


namespace {

void initializeTypes() {
    LLVMContext & context = getGlobalContext();
    std::vector<Type*> fields;
    StructType * t_sxpinfo_struct = StructType::create(context, "struct.sxpinfo_struct");
    ir::t_SEXPREC = StructType::create(context, "struct.SEXPREC");
    // SEXP
    ir::t_SEXP = PointerType::get(ir::t_SEXPREC, 0);
    // sxpinfo_struct is just int32 in a structure, the bitmasking is not a concern of the type
    fields.push_back(IntegerType::get(context, 32));
    t_sxpinfo_struct->setBody(fields, false);
    // primsxp
    fields.clear();
    // SEXPREC, first the union
    fields.clear();
    StructType * u1 = StructType::create(context,"union.SEXP_SEXP_SEXP");
    fields.push_back(ir::t_SEXP);
    fields.push_back(ir::t_SEXP);
    fields.push_back(ir::t_SEXP);
    u1->setBody(fields, false);
    // now the real SEXPREC
    fields.clear();
    fields.push_back(t_sxpinfo_struct);
    fields.push_back(ir::t_SEXP);
    fields.push_back(ir::t_SEXP);
    fields.push_back(ir::t_SEXP);
    fields.push_back(u1);
    ir::t_SEXPREC->setBody(fields, false);
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
    ir::t_RBoolean = IntegerType::get(context, 32);
    // InterpreterContext
    ir::t_InterpreterContext = StructType::create(context, "struct.InterpreterContext");
    fields.clear();
    fields.push_back(ir::t_SEXP);
    fields.push_back(ir::t_SEXP);
    fields.push_back(ir::t_RBoolean);
    fields.push_back(ir::t_SEXP);
    fields.push_back(ir::t_SEXP);
    fields.push_back(bcStackPtr);
    fields.push_back(bcStackPtr);
    fields.push_back(ir::t_RBoolean);
    ir::t_InterpreterContext->setBody(fields);
    ir::p_InterpreterContext = PointerType::get(ir::t_InterpreterContext, 0);
    // Interpreter function
    fields.clear();
    fields.push_back(ir::t_SEXP);
    fields.push_back(ir::t_SEXP);
    fields.push_back(ir::t_RBoolean);
    ir::t_interpreterLoop = FunctionType::get(ir::t_SEXP, fields, false);
}

void initializeFunctions() {
    LLVMContext & context = getGlobalContext();
    // define the instruction types
    std::vector<Type*> args;
    // interpreter initializer
    args.clear();
    args.push_back(ir::p_InterpreterContext);
    args.push_back(ir::t_SEXP);
    args.push_back(ir::t_SEXP);
    args.push_back(ir::t_RBoolean);
    args.push_back(IntegerType::get(context, 32));
    FunctionType * t_initializeInterpreter = FunctionType::get(Type::getVoidTy(context), args, false);
    // interpreter finalizer
    args.clear();
    args.push_back(ir::p_InterpreterContext);
    FunctionType * t_finalizeInterpreter = FunctionType::get(ir::t_SEXP, args, false);
    // instruction types
    args.clear();
    args.push_back(ir::p_InterpreterContext);
    FunctionType * t_instruction0 = FunctionType::get(Type::getVoidTy(context), args, false);
    args.clear();
    args.push_back(ir::p_InterpreterContext);
    args.push_back(IntegerType::get(context, 32));
    FunctionType * t_instruction1 = FunctionType::get(Type::getVoidTy(context), args, false);
    args.clear();
    args.push_back(ir::p_InterpreterContext);
    args.push_back(IntegerType::get(context, 32));
    args.push_back(IntegerType::get(context, 32));
    FunctionType * t_instruction2 = FunctionType::get(Type::getVoidTy(context), args, false);
    args.clear();
    args.push_back(ir::p_InterpreterContext);
    args.push_back(IntegerType::get(context, 32));
    args.push_back(IntegerType::get(context, 32));
    FunctionType * t_cinstruction2 = FunctionType::get(IntegerType::get(context, 32), args, false);
    args.clear();
    args.push_back(ir::p_InterpreterContext);
    args.push_back(IntegerType::get(context, 32));
    args.push_back(IntegerType::get(context, 32));
    args.push_back(IntegerType::get(context, 32));
    FunctionType * t_instruction3 = FunctionType::get(Type::getVoidTy(context), args, false);
    args.clear();
    args.push_back(ir::p_InterpreterContext);
    args.push_back(IntegerType::get(context, 32));
    args.push_back(IntegerType::get(context, 32));
    args.push_back(IntegerType::get(context, 32));
    args.push_back(IntegerType::get(context, 32));
    FunctionType * t_instruction4 = FunctionType::get(Type::getVoidTy(context), args, false);
    args.clear();
    args.push_back(ir::p_InterpreterContext);
    args.push_back(IntegerType::get(context, 32));
    args.push_back(IntegerType::get(context, 32));
    args.push_back(IntegerType::get(context, 32));
    args.push_back(IntegerType::get(context, 32));
    FunctionType * t_cinstruction4 = FunctionType::get(IntegerType::get(context, 32), args, false);

    /*

    // interpreter initialization & finalization
    ir::initializeInterpreter = Function::Create(t_initializeInterpreter, Function::ExternalLinkage, "initializeInterpreter", module);
    ir::finalizeInterpreter = Function::Create(t_finalizeInterpreter, Function::ExternalLinkage, "finalizeInterpreter", module);
    // switch special instructions
    ir::SWITCH_OP_start = Function::Create(t_cinstruction4, Function::ExternalLinkage, "instructionSWITCH_OP_start", module);
    ir::SWITCH_OP_character = Function::Create(t_cinstruction4, Function::ExternalLinkage, "instructionSWITCH_OP_character", module);
    ir::SWITCH_OP_integral = Function::Create(t_cinstruction4, Function::ExternalLinkage, "instructionSWITCH_OP_integral", module);
    // handle the normal instructions
#define SCONCAT(a) #a
#define INSTRUCTION0(name, opcode) ir::name = Function::Create(t_instruction0, Function::ExternalLinkage, SCONCAT(instruction ## name), module);
#define INSTRUCTION1(name, opcode) ir::name = Function::Create(t_instruction1, Function::ExternalLinkage, SCONCAT(instruction ## name), module);
#define INSTRUCTION2(name, opcode) ir::name = Function::Create(t_instruction2, Function::ExternalLinkage, SCONCAT(instruction ## name), module);
#define INSTRUCTION3(name, opcode) ir::name = Function::Create(t_instruction3, Function::ExternalLinkage, SCONCAT(instruction ## name), module);
#define SPECIAL0(name, opcode) ir::name = Function::Create(t_instruction0, Function::ExternalLinkage, SCONCAT(instruction ## name), module);
#define SPECIAL1(name, opcode) ir::name = Function::Create(t_instruction1, Function::ExternalLinkage, SCONCAT(instruction ## name), module);
#define SPECIAL2(name, opcode) ir::name = Function::Create(t_cinstruction2, Function::ExternalLinkage, SCONCAT(instruction ## name), module);
#define SPECIAL3(name, opcode) ir::name = Function::Create(t_instruction3, Function::ExternalLinkage, SCONCAT(instruction ## name), module);
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
 */

}

StructType * initialize() {
    initializeTypes();
    initializeFunctions();
    return ir::t_SEXPREC;
}

} // namespace

namespace ir {
    StructType * t_SEXPREC;
    PointerType * t_SEXP;
    IntegerType * t_RBoolean;
    StructType * t_InterpreterContext;
    PointerType * p_InterpreterContext;

    FunctionType * t_interpreterLoop;

    Function * initializeInterpreter;
    Function * finalizeInterpreter;

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
}


