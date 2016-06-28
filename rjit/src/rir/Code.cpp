
#include "Code.h"
#include "interp.h"
#include "CodeVerifier.h"
#include "interp_context.h"
#include "Pool.h"

extern void print(::Code * c);

namespace rjit {
namespace rir {


::Code * Code::linearizeTo(uint8_t *& stream, ::Function * start, Context * ctx) {

    print();

    // increase the number of code objects
    ++start->codeLength;
    // get the header and fill it in
    ::Code * c = reinterpret_cast<::Code *>(stream);
    Rprintf("Code object created at %u\n", c);
    c->magic = CODE_MAGIC;
    c->header = (uintptr_t)c - (uintptr_t)start;
    c->src = src_pool_add(ctx, ast);
    c->codeSize = size;
    c->srcLength = sources.size();
    // copy the code
    stream += sizeof(::Code);
    memcpy(c->data, bc, size);
    // copy the source objects
    stream += pad4(size);
    unsigned * srcs = reinterpret_cast<unsigned*>(stream);
    for (size_t i = 0, e = sources.size(); i != e; ++i)
        *(srcs++) = sources[i] == nullptr ? 0 : src_pool_add(ctx, sources[i]);
    // get pointer to the very end
    stream = reinterpret_cast<uint8_t*>(srcs);
    // fill in missing information about stack sizes
    CodeVerifier::verifyStack(c);

    // linearize childdren and remember their addresses for relocation
    std::vector<unsigned> reloc;
    for (Code * child : children)
        reloc.push_back(child->linearizeTo(stream, start, ctx)->header);

    // relocate promise numbers in the old code vector to their offsets
    // TODO this needs more thinking - we shouldn't really change stuff in constant pool vectors as these may be shared by others, so for now, I am adding a new one...
    BC_t * pc  = reinterpret_cast<BC_t * >(c->data);
    BC_t * end = pc + c->codeSize;
    while (pc < end) {
        switch (*pc) {
        case BC_t::call_: {
            SEXP indices = Pool::get(* reinterpret_cast<unsigned*>(pc + 1));
            SEXP offsets = Rf_allocVector(INTSXP, Rf_length(indices));
            for (size_t i = 0, e = Rf_length(indices) / sizeof(int); i != e; ++i)
                INTEGER(offsets)[i] = reloc[INTEGER(indices)[i]];
            * reinterpret_cast<unsigned*>(pc + 1) = Pool::insert(offsets);
            break;
        }
        default:
            break;
        }
        pc += BC::size(*pc);
    }
    ::print(c);
    return c;
}

SEXP Code::linearize(Context * ctx) {
    // get the size
    size_t lsize = sizeof(::Function) + linearizedSize();
    SEXP result = Rf_allocVector(INTSXP, lsize);
    PROTECT(result);
    // fill in the header
    ::Function * f = reinterpret_cast<::Function *>(INTEGER(result));
    Rprintf("Function object created at %u\n", f);
    f->magic = FUNCTION_MAGIC;
    f->size = lsize;
    f->origin = nullptr;
    f->codeLength = 0; // will be updated as we serialize the code objects
    // linearize the code object and its childrens
    uint8_t * stream = reinterpret_cast<uint8_t*>(f + 1);
    linearizeTo(stream, f, ctx);

    // verify the function layout
    CodeVerifier::vefifyFunctionLayout(result, ctx);

    UNPROTECT(1);
    return result;
}


#ifdef HAHA


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


#endif

} // namespace rir
} // namespace rjit

