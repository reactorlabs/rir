#include "jit_types.h"
#include "runtime_helper.h"

using namespace llvm;

FunctionType * T::t_voidInstruction0;
FunctionType * T::t_voidInstruction1;
FunctionType * T::t_voidInstruction2;
FunctionType * T::t_voidInstruction3;
FunctionType * T::t_voidInstruction4;

FunctionType * T::t_intInstruction0;
FunctionType * T::t_intInstruction1;
FunctionType * T::t_intInstruction2;
FunctionType * T::t_intInstruction3;
FunctionType * T::t_intInstruction4;

FunctionType * T::t_jitFun;


void T::initialize(RuntimeHelper & helper) {
    LLVMContext & context = getGlobalContext();
    std::vector<Type*> fields;

    // instruction types
    std::vector<Type*> args;
    args.clear();
    args.push_back(helper.t->p_InterpreterContext);
    t_voidInstruction0 = FunctionType::get(Type::getVoidTy(context), args, false);
    args.clear();
    args.push_back(helper.t->p_InterpreterContext);
    args.push_back(IntegerType::get(context, 32));
    t_voidInstruction1 = FunctionType::get(Type::getVoidTy(context), args, false);
    args.clear();
    args.push_back(helper.t->p_InterpreterContext);
    args.push_back(IntegerType::get(context, 32));
    args.push_back(IntegerType::get(context, 32));
    t_voidInstruction2 = FunctionType::get(Type::getVoidTy(context), args, false);
    args.clear();
    args.push_back(helper.t->p_InterpreterContext);
    args.push_back(IntegerType::get(context, 32));
    args.push_back(IntegerType::get(context, 32));
    args.push_back(IntegerType::get(context, 32));
    t_voidInstruction3 = FunctionType::get(Type::getVoidTy(context), args, false);
    args.clear();
    args.push_back(helper.t->p_InterpreterContext);
    args.push_back(IntegerType::get(context, 32));
    args.push_back(IntegerType::get(context, 32));
    args.push_back(IntegerType::get(context, 32));
    args.push_back(IntegerType::get(context, 32));
    t_voidInstruction4 = FunctionType::get(Type::getVoidTy(context), args, false);
    args.clear();
    args.push_back(helper.t->p_InterpreterContext);
    t_intInstruction0 = FunctionType::get(IntegerType::get(context, 32), args, false);
    args.clear();
    args.push_back(helper.t->p_InterpreterContext);
    args.push_back(IntegerType::get(context, 32));
    t_intInstruction1 = FunctionType::get(IntegerType::get(context, 32), args, false);
    args.clear();
    args.push_back(helper.t->p_InterpreterContext);
    args.push_back(IntegerType::get(context, 32));
    args.push_back(IntegerType::get(context, 32));
    t_intInstruction2 = FunctionType::get(IntegerType::get(context, 32), args, false);
    args.clear();
    args.push_back(helper.t->p_InterpreterContext);
    args.push_back(IntegerType::get(context, 32));
    args.push_back(IntegerType::get(context, 32));
    args.push_back(IntegerType::get(context, 32));
    t_intInstruction3 = FunctionType::get(IntegerType::get(context, 32), args, false);
    args.clear();
    args.push_back(helper.t->p_InterpreterContext);
    args.push_back(IntegerType::get(context, 32));
    args.push_back(IntegerType::get(context, 32));
    args.push_back(IntegerType::get(context, 32));
    args.push_back(IntegerType::get(context, 32));
    t_intInstruction4 = FunctionType::get(IntegerType::get(context, 32), args, false);

    args.clear();
    args.push_back(helper.t->t_SEXP);
    args.push_back(helper.t->t_SEXP);
    t_jitFun = FunctionType::get(
            helper.t->t_SEXP, args, false);
}

