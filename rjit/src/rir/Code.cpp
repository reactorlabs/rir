

#include "Code.h"
#include "interp.h"
#include "CodeVerifier.h"
#include "interpreter_context.h"

namespace rjit {
namespace rir {


#define DEF_INSTR(name, imm, ...) case name : pc += sizeof(ArgT) * imm; break;
#include "insns.h"

/** Serializes the rir::Code object into a Function.
 */

// TODO Bytecodes and indices should be unified

SEXP Code::toFunction() {
    // calculate the overall size required (in bytes)
    unsigned totalSize = 0;
    // size of the function header
    totalSize += sizeof(Function);
    totalSize += calcSize(totalSize);

    // create the vector
    assert(totalSize % sizeof (int) == 0 and "Are code objects properly padded?");
    SEXP result = Rf_allocVector(INTSXP, totalSize / sizeof (int));
    PROTECT(result);
    OpcodeT * code = reinterpret_cast<OpcodeT*>(INTEGER(result));

    // fill in the header
    ::Function * fHdr = reinterpret_cast<::Function *>(code);
    fHdr->magic = 0xCAFEBABE;
    fHdr->origin = nullptr; // this will be ptr to unoptimized version, but we do not optimize yet
    fHdr->size = totalSize;
    size_t tempSize = 0;
    fHdr->codeLength = codeSize(tempSize) + 1;

    // now serialize the codes, one by one, first itself, then all the children
    // keep in mind to update the instructions which deal push promise indices to update 
    // them so that they push offsets from the beginning

    ::Code * codeObject = reinterpret_cast<::Code *>(code)
    codeObject++;
    for (){

        // Create the code

        // Pad the code
        pad4

    codeObject++;
        // Create the AST
    }

    // how would the new bytecode be added into the result vector?
    ::Code * newCode;
    //  = reinterpret_cast<::Code *>(code);
    // unsigned header; /// offset to Function object
    newCode->header = sizeof(::Code);
    // unsigned src; /// AST of the function (or promise) represented by the code
    newCode->src = ast;
    // unsigned codeSize; /// bytes of code (not padded)
    newCode->codeSize = size;
    // unsigned srcLength; /// number of instructions
    newCode->srcLength = tempSize + 1;
    // separate the bc block into individual instructions and store them into data[]
    // Store the index of the src/ast constant pool for each pc that is in the astMap of the code object.
    verifyStack(newCode);

    size_t * astSrcLoc;
    // the new src context
    Context * context = create_context((size_t) 4096);
    size_t counter = 0;
    for (unsigned i = 0; i < size; ++i){
        newCode->data[i] = (uint8_t) bc[i];
        if (astMap->ind[i] && astMap->ind[i] != 0){
            astSrcLoc[counter] = src_pool_add(context, astMap->ast[i]);
            counter++;
        }
    }



    // assigning to the result vector is achieved by using the memcpy function. 
    memcpy(dest, src, totalSize);
    UNPROTECT(1);
    return nullptr;
}

    // Create the Code object as defined in interp.h
    // from the code object generated from the CodeStream
::Code * Code::createCode(::Code * code, size_t astLength){

    // unsigned header; /// offset to Function object
    code->header = sizeof(::Code);
    // unsigned src; /// AST of the function (or promise) represented by the code
    code->src = ast;
    // unsigned codeSize; /// bytes of code (not padded)
    code->codeSize = size;
    // unsigned srcLength; /// number of instructions
    code->srcLength = astLength + 1;
    // separate the bc block into individual instructions and store them into data[]
    // Store the index of the src/ast constant pool for each pc that is in the astMap of the code object.
    verifyStack(code);

    return code;
}


    // Create the ast map 
    // the map 
size_t Code::createAst(){



}

size_t Code:codeSize(size_t count){

    count += children.size();

    for (Code * c: children){
        codeSize(c, count);
    }
    return count;
}

unsigned Code::calcSize(unsigned totalSize){
    totalSize += sizeof(::Code);
    totalSize += size;
    totalSize += pad4(sizeof(::Code)+size);
    totalSize += astMap.size();
    
    for (Code * c : code->children){
        totalSize += c->calcSize(size);
    }

    return size;
}

} // namespace rir
} // namespace rjit
