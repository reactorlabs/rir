#include "jit_module.h"
#include "runtime_helper.h"
#include "jit_types.h"
#include "llvm_includes.h"

#include <iostream>

using namespace llvm;

JITModule::operator Module * () {
    return module;
}

ConstantInt * JITModule::constant(int value) {
    return ConstantInt::get(getGlobalContext(), APInt(32, value));
}

Function * JITModule::getFunction(const std::string name, Function * foreign) {
    auto known = module->getFunction(name);
    if (known) return known;

    return Function::Create(
            foreign->getFunctionType(),
            Function::ExternalLinkage,
            name,
            module);
}

Function * JITModule::getFunction(const std::string name) {
    auto known = module->getFunction(name);
    if (known) return known;

    auto lib = RuntimeHelper::helper.getFunction(name);
    if (!lib) {
        std::cout << "I can't find the function " << name << std::endl;
        DIE;
    }

    return getFunction(name, lib);
}

/** Creates new LLVM module and populates it with declarations of the helper and opcode functions.
  */

JITModule::JITModule() {
    // create new module
    module = new Module("jit", getGlobalContext());

    // This is a hack to make SEXPREC known in the new module to make sure that
    // linking in r_instrinsics.bc does not duplicte the type. 
    module->getOrInsertGlobal("foo", RuntimeHelper::helper.t->t_SEXPREC);

    // TODO: load only once the ModuleCopy -- but this segv atm...
    //
    std::string err;
    // TODO: hardcoded path is not the best idea...
    DataStreamer *streamer = getDataFileStreamer("rjit/intrinsics.bc", &err);
    if (!streamer) {
        std::cout << err << std::endl;
        DIE;
    }
    ErrorOr<std::unique_ptr<Module>> m =
        getStreamedBitcodeModule("intrinsics", streamer, getGlobalContext());
    Module * intr = std::move(*m).get();
    intr->materializeAllPermanently();

    Linker::LinkModules(module, intr);

    // switch special instructions
    SWITCH_OP_start = Function::Create(
            T::t_intInstruction4,
            Function::ExternalLinkage,
            "instructionSWITCH_OP_start",
            module);
    SWITCH_OP_character = Function::Create(
            T::t_intInstruction4,
            Function::ExternalLinkage,
            "instructionSWITCH_OP_character",
            module);
    SWITCH_OP_integral = Function::Create(T::t_intInstruction4,
            Function::ExternalLinkage,
            "instructionSWITCH_OP_integral",
            module);

    // handle the normal instructions
    #define SCONCAT(a) #a
    #define INSTRUCTION0(name, opcode)          \
        name = Function::Create(                \
                T::t_voidInstruction0,          \
                Function::ExternalLinkage,      \
                SCONCAT(instruction ## name),   \
                module);
    #define INSTRUCTION1(name, opcode)          \
        name = Function::Create(                \
                T::t_voidInstruction1,          \
                Function::ExternalLinkage,      \
                SCONCAT(instruction ## name),   \
                module);
    #define INSTRUCTION2(name, opcode)          \
        name = Function::Create(                \
                T::t_voidInstruction2,          \
                Function::ExternalLinkage,      \
                SCONCAT(instruction ## name),   \
                module);
    #define INSTRUCTION3(name, opcode)          \
        name = Function::Create(                \
                T::t_voidInstruction3,          \
                Function::ExternalLinkage,      \
                SCONCAT(instruction ## name),   \
                module);
    #define SPECIAL0(name, opcode)              \
        name = Function::Create(                \
                T::t_intInstruction0,          \
                Function::ExternalLinkage,      \
                SCONCAT(instruction ## name),   \
                module);
    #define SPECIAL1(name, opcode)              \
        name = Function::Create(                \
                T::t_intInstruction1,          \
                Function::ExternalLinkage,      \
                SCONCAT(instruction ## name),   \
                module);
    #define SPECIAL2(name, opcode)              \
        name = Function::Create(                \
                T::t_intInstruction2,          \
                Function::ExternalLinkage,      \
                SCONCAT(instruction ## name),   \
                module);
    #define SPECIAL3(name, opcode)              \
        name = Function::Create(                \
                T::t_intInstruction3,          \
                Function::ExternalLinkage,      \
                SCONCAT(instruction ## name),   \
                module);
    #define SPECIAL4(name, opcode)              \
        name = Function::Create(                \
                T::t_intInstruction4,          \
                Function::ExternalLinkage,      \
                SCONCAT(instruction ## name),   \
                module);
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

