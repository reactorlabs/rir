#include "JITModule.h"

#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/Host.h"

using namespace llvm;

namespace rjit {

JITModule::JITModule(std::string const& name)
    : m(new Module(name, getGlobalContext())) {
    FunctionType* stackmapTy = FunctionType::get(
        t::t_void,
        std::vector<Type*>({{IntegerType::get(m->getContext(), 64),
                             IntegerType::get(m->getContext(), 32)}}),
        true);
    stackmap = Function::Create(stackmapTy, GlobalValue::ExternalLinkage,
                                "llvm.experimental.stackmap", m);

    FunctionType* patchpointTy = FunctionType::get(
        t::t_void,
        std::vector<Type*>({{IntegerType::get(m->getContext(), 64),
                             IntegerType::get(m->getContext(), 32), t::i8ptr,
                             IntegerType::get(m->getContext(), 32)}}),
        true);
    patchpoint = Function::Create(patchpointTy, GlobalValue::ExternalLinkage,
                                  "llvm.experimental.patchpoint.void", m);
    m->setDataLayout(*EngineBuilder().selectTarget()->getDataLayout());
}

} // namespace rjit
