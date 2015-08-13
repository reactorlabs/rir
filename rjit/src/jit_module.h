#ifndef JIT_MODULE_H
#define JIT_MODULE_H

#include "llvm_includes.h"
#include "rbc.h"

/** Simple class that encapsulates a LLVM module used to compile R's function. Contains the module itself and all declarations of functions that the JIT may use - the R bytecode opcodes and evaluation runtimes.
 */
class JITModule {
public:

    operator llvm::Module * ();
    llvm::ConstantInt * constant(int value);
    llvm::Function * getFunction(const std::string name);
    llvm::Function * getFunction(const std::string, llvm::Function *);

    /** Creates new LLVM module and populates it with declarations of the helper and opcode functions.
      */
    JITModule();

    llvm::Module * module;

    // pregenerated instruction functions
    #define INSTRUCTION0(name, opcode) llvm::Function * name;
    #define INSTRUCTION1(name, opcode) llvm::Function * name;
    #define INSTRUCTION2(name, opcode) llvm::Function * name;
    #define INSTRUCTION3(name, opcode) llvm::Function * name;
    #define SPECIAL0(name, opcode) llvm::Function * name;
    #define SPECIAL1(name, opcode) llvm::Function * name;
    #define SPECIAL2(name, opcode) llvm::Function * name;
    #define SPECIAL3(name, opcode) llvm::Function * name;
    #define SPECIAL4(name, opcode) llvm::Function * name;
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




#endif
