#include "runtime_helper.h"

#include <iostream>

using namespace llvm;

RuntimeHelper::RuntimeHelper() : context(getGlobalContext()) {
    std::string err;
    // TODO: hardcoded path is not the best idea...
    DataStreamer *streamer = getDataFileStreamer("rjit/eval.bc", &err);
    if (!streamer) {
        std::cout << err << std::endl;
        DIE;
    }
    ErrorOr<std::unique_ptr<Module>> m =
        getStreamedBitcodeModule("eval", streamer, context);
    evalM = std::move(*m);
    evalM->materializeAllPermanently();

    t = std::unique_ptr<T>(new T(evalM.get(), context));
}

Function * RuntimeHelper::getFunction(const std::string name) {
    return evalM->getFunction(name);
}

RuntimeHelper::T::T(Module * m, LLVMContext & context) {
    t_SEXPREC     = m->getTypeByName("struct.SEXPREC");
    t_SEXP        = PointerType::get(t_SEXPREC, 0);
    t_R_bcstack_t = m->getTypeByName("struct.R_bcstack_t");
    bcStackPtr    = PointerType::get(t_R_bcstack_t, 0);
    t_Rboolean    = IntegerType::get(context, 32);
    t_InterpreterContext = m->getTypeByName("struct.InterpreterContext");
    p_InterpreterContext = PointerType::get(t_InterpreterContext, 0);
    t_RCNTXT = m->getTypeByName("struct.RCNTXT");
    p_RCNTXT = PointerType::get(t_RCNTXT, 0);
    t_execClosure = m->getFunction("R_execClosure")->getFunctionType();
    t_listsxp = m->getTypeByName("struct.listsxp_struct");
}

RuntimeHelper RuntimeHelper::helper;
