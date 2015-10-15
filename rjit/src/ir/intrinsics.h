#ifndef INTRINSICS_H_
#define INTRINSICS_H_

#include "ir.h"
#include "Builder.h"

namespace rjit {
namespace ir {

/** Given a body object (assumed to be nativeSXP, returns its constant pool.
*/
class ExtractConstantPool: public Intrinsic {
public:
    llvm::Value * f() { return getValue(0); }

    ExtractConstantPool(llvm::Instruction * ins):
        Intrinsic(ins) {
    }

    static ExtractConstantPool create(Builder & b, llvm::Value * f) {
        std::vector<llvm::Value *> args_;
        args_.push_back(f);
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<ExtractConstantPool>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::ExtractConstantPool);
        return ins;
    }

    static char const * intrinsicName() {
        return "extractConstantPool";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP }, false);
    }

};
/** Replacement for GETSTACK_LOGICAL_NO_NA_PTR
The call is used only for error reporting.
*/
class ConvertToLogicalNoNA_: public Intrinsic {
public:
    llvm::Value * what() { return getValue(0); }

    llvm::Value * constantPool() { return getValue(1); }

    
int call() { return getValueInt(2); }
SEXP call(SEXP constantPool) { return VECTOR_ELT(constantPool, call()); }
SEXP call(Builder const & b) { return b.constantPool(call()); }
        

    ConvertToLogicalNoNA_(llvm::Instruction * ins):
        Intrinsic(ins) {
    }

    static ConvertToLogicalNoNA_ create(Builder & b, llvm::Value * what, SEXP call) {
        std::vector<llvm::Value *> args_;
        args_.push_back(what);
        args_.push_back(b.consts());
        args_.push_back(Builder::integer(b.constantPoolIndex(call)));
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<ConvertToLogicalNoNA_>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::ConvertToLogicalNoNA_);
        return ins;
    }

    static char const * intrinsicName() {
        return "convertToLogicalNoNA_";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::Int, { t::SEXP, t::SEXP, t::Int }, false);
    }

};

class PrintValue_: public Intrinsic {
public:
    llvm::Value * value() { return getValue(0); }

    PrintValue_(llvm::Instruction * ins):
        Intrinsic(ins) {
    }

    static PrintValue_ create(Builder & b, llvm::Value * value) {
        std::vector<llvm::Value *> args_;
        args_.push_back(value);
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<PrintValue_>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::PrintValue_);
        return ins;
    }

    static char const * intrinsicName() {
        return "printValue_";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::Void, { t::SEXP }, false);
    }

};
/** startFor returns the sequence over which the loop will iterate.
No need for all the other things here because we do not support other than generic variable loads and stores.
*/
class StartFor_: public Intrinsic {
public:
    llvm::Value * seq() { return getValue(0); }

    llvm::Value * rho() { return getValue(1); }

    StartFor_(llvm::Instruction * ins):
        Intrinsic(ins) {
    }

    static StartFor_ create(Builder & b, llvm::Value * seq, llvm::Value * rho) {
        std::vector<llvm::Value *> args_;
        args_.push_back(seq);
        args_.push_back(rho);
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<StartFor_>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::StartFor_);
        return ins;
    }

    static char const * intrinsicName() {
        return "startFor_";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP }, false);
    }

};
/** Loop sequence length returns the length of the sequence the loop will iterate over and errors if the sequence is of wrong type.
*/
class LoopSequenceLength_: public Intrinsic {
public:
    llvm::Value * seq() { return getValue(0); }

    llvm::Value * constantPool() { return getValue(1); }

    
int call() { return getValueInt(2); }
SEXP call(SEXP constantPool) { return VECTOR_ELT(constantPool, call()); }
SEXP call(Builder const & b) { return b.constantPool(call()); }
        

    LoopSequenceLength_(llvm::Instruction * ins):
        Intrinsic(ins) {
    }

    static LoopSequenceLength_ create(Builder & b, llvm::Value * seq, SEXP call) {
        std::vector<llvm::Value *> args_;
        args_.push_back(seq);
        args_.push_back(b.consts());
        args_.push_back(Builder::integer(b.constantPoolIndex(call)));
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<LoopSequenceLength_>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::LoopSequenceLength_);
        return ins;
    }

    static char const * intrinsicName() {
        return "loopSequenceLength_";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::Int, { t::SEXP, t::SEXP, t::Int }, false);
    }

};
/** Given the for loop sequence, and index, returns the index-th value of the sequence.
TODO Note that this always allocates for vectors.
*/
class GetForLoopValue_: public Intrinsic {
public:
    llvm::Value * seq() { return getValue(0); }

    int index() { return getValueInt(1); }

    GetForLoopValue_(llvm::Instruction * ins):
        Intrinsic(ins) {
    }

    static GetForLoopValue_ create(Builder & b, llvm::Value * seq, int index) {
        std::vector<llvm::Value *> args_;
        args_.push_back(seq);
        args_.push_back(Builder::integer(index));
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GetForLoopValue_>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GetForLoopValue_);
        return ins;
    }

    static char const * intrinsicName() {
        return "getForLoopValue_";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::Int }, false);
    }

};

class MarkVisible_: public Intrinsic {
public:
    MarkVisible_(llvm::Instruction * ins):
        Intrinsic(ins) {
    }

    static MarkVisible_ create(Builder & b) {
        std::vector<llvm::Value *> args_;
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<MarkVisible_>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::MarkVisible_);
        return ins;
    }

    static char const * intrinsicName() {
        return "markVisible_";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::Void, {  }, false);
    }

};

class MarkInvisible_: public Intrinsic {
public:
    MarkInvisible_(llvm::Instruction * ins):
        Intrinsic(ins) {
    }

    static MarkInvisible_ create(Builder & b) {
        std::vector<llvm::Value *> args_;
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<MarkInvisible_>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::MarkInvisible_);
        return ins;
    }

    static char const * intrinsicName() {
        return "markInvisible_";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::Void, {  }, false);
    }

};
/** When LLVM IR creates user visible constant, this function contains all the code required to make the constant.
Currently this means marking it as not mutable only.
*/
class UserConstant_: public Intrinsic {
public:
    llvm::Value * value() { return getValue(0); }

    UserConstant_(llvm::Instruction * ins):
        Intrinsic(ins) {
    }

    static UserConstant_ create(Builder & b, llvm::Value * value) {
        std::vector<llvm::Value *> args_;
        args_.push_back(value);
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<UserConstant_>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::UserConstant_);
        return ins;
    }

    static char const * intrinsicName() {
        return "userConstant_";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::Void, { t::SEXP }, false);
    }

};
/** Generic getvar does not use any caches whatsoever.
TODO this means we can get rid of the checks in getvar(), and reduce its code to this. We definitely want faster versions.
*/
class GenericGetVar_: public Intrinsic {
public:
    llvm::Value * rho() { return getValue(0); }

    llvm::Value * constantPool() { return getValue(1); }

    
int symbol() { return getValueInt(2); }
SEXP symbol(SEXP constantPool) { return VECTOR_ELT(constantPool, symbol()); }
SEXP symbol(Builder const & b) { return b.constantPool(symbol()); }
        

    GenericGetVar_(llvm::Instruction * ins):
        Intrinsic(ins) {
    }

    static GenericGetVar_ create(Builder & b, llvm::Value * rho, SEXP symbol) {
        std::vector<llvm::Value *> args_;
        args_.push_back(rho);
        args_.push_back(b.consts());
        args_.push_back(Builder::integer(b.constantPoolIndex(symbol)));
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GenericGetVar_>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GenericGetVar_);
        return ins;
    }

    static char const * intrinsicName() {
        return "genericGetVar_";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP, t::Int }, false);
    }

};

class GenericGetEllipsisArg_: public Intrinsic {
public:
    llvm::Value * rho() { return getValue(0); }

    llvm::Value * constantPool() { return getValue(1); }

    
int symbol() { return getValueInt(2); }
SEXP symbol(SEXP constantPool) { return VECTOR_ELT(constantPool, symbol()); }
SEXP symbol(Builder const & b) { return b.constantPool(symbol()); }
        

    GenericGetEllipsisArg_(llvm::Instruction * ins):
        Intrinsic(ins) {
    }

    static GenericGetEllipsisArg_ create(Builder & b, llvm::Value * rho, SEXP symbol) {
        std::vector<llvm::Value *> args_;
        args_.push_back(rho);
        args_.push_back(b.consts());
        args_.push_back(Builder::integer(b.constantPoolIndex(symbol)));
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GenericGetEllipsisArg_>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GenericGetEllipsisArg_);
        return ins;
    }

    static char const * intrinsicName() {
        return "genericGetEllipsisArg_";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP, t::Int }, false);
    }

};

class GenericSetVar_: public Intrinsic {
public:
    llvm::Value * value() { return getValue(0); }

    llvm::Value * rho() { return getValue(1); }

    llvm::Value * constantPool() { return getValue(2); }

    
int symbol() { return getValueInt(3); }
SEXP symbol(SEXP constantPool) { return VECTOR_ELT(constantPool, symbol()); }
SEXP symbol(Builder const & b) { return b.constantPool(symbol()); }
        

    GenericSetVar_(llvm::Instruction * ins):
        Intrinsic(ins) {
    }

    static GenericSetVar_ create(Builder & b, llvm::Value * value, llvm::Value * rho, SEXP symbol) {
        std::vector<llvm::Value *> args_;
        args_.push_back(value);
        args_.push_back(rho);
        args_.push_back(b.consts());
        args_.push_back(Builder::integer(b.constantPoolIndex(symbol)));
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GenericSetVar_>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GenericSetVar_);
        return ins;
    }

    static char const * intrinsicName() {
        return "genericSetVar_";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::Void, { t::SEXP, t::SEXP, t::SEXP, t::Int }, false);
    }

};

class GenericSetVarParent_: public Intrinsic {
public:
    llvm::Value * value() { return getValue(0); }

    llvm::Value * rho() { return getValue(1); }

    llvm::Value * constantPool() { return getValue(2); }

    
int symbol() { return getValueInt(3); }
SEXP symbol(SEXP constantPool) { return VECTOR_ELT(constantPool, symbol()); }
SEXP symbol(Builder const & b) { return b.constantPool(symbol()); }
        

    GenericSetVarParent_(llvm::Instruction * ins):
        Intrinsic(ins) {
    }

    static GenericSetVarParent_ create(Builder & b, llvm::Value * value, llvm::Value * rho, SEXP symbol) {
        std::vector<llvm::Value *> args_;
        args_.push_back(value);
        args_.push_back(rho);
        args_.push_back(b.consts());
        args_.push_back(Builder::integer(b.constantPoolIndex(symbol)));
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GenericSetVarParent_>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GenericSetVarParent_);
        return ins;
    }

    static char const * intrinsicName() {
        return "genericSetVarParent_";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::Void, { t::SEXP, t::SEXP, t::SEXP, t::Int }, false);
    }

};

class GetFunction_: public Intrinsic {
public:
    llvm::Value * rho() { return getValue(0); }

    llvm::Value * constantPool() { return getValue(1); }

    
int symbol() { return getValueInt(2); }
SEXP symbol(SEXP constantPool) { return VECTOR_ELT(constantPool, symbol()); }
SEXP symbol(Builder const & b) { return b.constantPool(symbol()); }
        

    GetFunction_(llvm::Instruction * ins):
        Intrinsic(ins) {
    }

    static GetFunction_ create(Builder & b, llvm::Value * rho, SEXP symbol) {
        std::vector<llvm::Value *> args_;
        args_.push_back(rho);
        args_.push_back(b.consts());
        args_.push_back(Builder::integer(b.constantPoolIndex(symbol)));
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GetFunction_>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GetFunction_);
        return ins;
    }

    static char const * intrinsicName() {
        return "getFunction_";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP, t::Int }, false);
    }

};

class GetGlobalFunction_: public Intrinsic {
public:
    llvm::Value * constantPool() { return getValue(0); }

    
int symbol() { return getValueInt(1); }
SEXP symbol(SEXP constantPool) { return VECTOR_ELT(constantPool, symbol()); }
SEXP symbol(Builder const & b) { return b.constantPool(symbol()); }
        

    GetGlobalFunction_(llvm::Instruction * ins):
        Intrinsic(ins) {
    }

    static GetGlobalFunction_ create(Builder & b, SEXP symbol) {
        std::vector<llvm::Value *> args_;
        args_.push_back(b.consts());
        args_.push_back(Builder::integer(b.constantPoolIndex(symbol)));
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GetGlobalFunction_>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GetGlobalFunction_);
        return ins;
    }

    static char const * intrinsicName() {
        return "getGlobalFunction_";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::Int }, false);
    }

};

class GetSymFunction_: public Intrinsic {
public:
    llvm::Value * constantPool() { return getValue(0); }

    
int name() { return getValueInt(1); }
SEXP name(SEXP constantPool) { return VECTOR_ELT(constantPool, name()); }
SEXP name(Builder const & b) { return b.constantPool(name()); }
        

    GetSymFunction_(llvm::Instruction * ins):
        Intrinsic(ins) {
    }

    static GetSymFunction_ create(Builder & b, SEXP name) {
        std::vector<llvm::Value *> args_;
        args_.push_back(b.consts());
        args_.push_back(Builder::integer(b.constantPoolIndex(name)));
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GetSymFunction_>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GetSymFunction_);
        return ins;
    }

    static char const * intrinsicName() {
        return "getSymFunction_";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::Int }, false);
    }

};

class GetBuiltinFunction_: public Intrinsic {
public:
    llvm::Value * constantPool() { return getValue(0); }

    
int name() { return getValueInt(1); }
SEXP name(SEXP constantPool) { return VECTOR_ELT(constantPool, name()); }
SEXP name(Builder const & b) { return b.constantPool(name()); }
        

    GetBuiltinFunction_(llvm::Instruction * ins):
        Intrinsic(ins) {
    }

    static GetBuiltinFunction_ create(Builder & b, SEXP name) {
        std::vector<llvm::Value *> args_;
        args_.push_back(b.consts());
        args_.push_back(Builder::integer(b.constantPoolIndex(name)));
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GetBuiltinFunction_>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GetBuiltinFunction_);
        return ins;
    }

    static char const * intrinsicName() {
        return "getBuiltinFunction_";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::Int }, false);
    }

};

class GetInternalBuiltinFunction_: public Intrinsic {
public:
    llvm::Value * constantPool() { return getValue(0); }

    
int name() { return getValueInt(1); }
SEXP name(SEXP constantPool) { return VECTOR_ELT(constantPool, name()); }
SEXP name(Builder const & b) { return b.constantPool(name()); }
        

    GetInternalBuiltinFunction_(llvm::Instruction * ins):
        Intrinsic(ins) {
    }

    static GetInternalBuiltinFunction_ create(Builder & b, SEXP name) {
        std::vector<llvm::Value *> args_;
        args_.push_back(b.consts());
        args_.push_back(Builder::integer(b.constantPoolIndex(name)));
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GetInternalBuiltinFunction_>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GetInternalBuiltinFunction_);
        return ins;
    }

    static char const * intrinsicName() {
        return "getInternalBuiltinFunction_";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::Int }, false);
    }

};

class CheckFunction_: public Intrinsic {
public:
    llvm::Value * f() { return getValue(0); }

    CheckFunction_(llvm::Instruction * ins):
        Intrinsic(ins) {
    }

    static CheckFunction_ create(Builder & b, llvm::Value * f) {
        std::vector<llvm::Value *> args_;
        args_.push_back(f);
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<CheckFunction_>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::CheckFunction_);
        return ins;
    }

    static char const * intrinsicName() {
        return "checkFunction_";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::Void, { t::SEXP }, false);
    }

};
/** Creates a promise out of the given code and environment and returns its value.
*/
class CreatePromise_: public Intrinsic {
public:
    llvm::Value * rho() { return getValue(0); }

    llvm::Value * constantPool() { return getValue(1); }

    
int code() { return getValueInt(2); }
SEXP code(SEXP constantPool) { return VECTOR_ELT(constantPool, code()); }
SEXP code(Builder const & b) { return b.constantPool(code()); }
        

    CreatePromise_(llvm::Instruction * ins):
        Intrinsic(ins) {
    }

    static CreatePromise_ create(Builder & b, llvm::Value * rho, SEXP code) {
        std::vector<llvm::Value *> args_;
        args_.push_back(rho);
        args_.push_back(b.consts());
        args_.push_back(Builder::integer(b.constantPoolIndex(code)));
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<CreatePromise_>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::CreatePromise_);
        return ins;
    }

    static char const * intrinsicName() {
        return "createPromise_";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP, t::Int }, false);
    }

};
/** Given a SEXP, returns its type.
We can perfectly do this in LLVM, but having an function for it simplifies the analysis on our end.
*/
class SexpType_: public Intrinsic {
public:
    llvm::Value * value() { return getValue(0); }

    SexpType_(llvm::Instruction * ins):
        Intrinsic(ins) {
    }

    static SexpType_ create(Builder & b, llvm::Value * value) {
        std::vector<llvm::Value *> args_;
        args_.push_back(value);
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<SexpType_>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::SexpType_);
        return ins;
    }

    static char const * intrinsicName() {
        return "sexpType_";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::Int, { t::SEXP }, false);
    }

};

class AddArgument_: public Intrinsic {
public:
    llvm::Value * args() { return getValue(0); }

    llvm::Value * arg() { return getValue(1); }

    AddArgument_(llvm::Instruction * ins):
        Intrinsic(ins) {
    }

    static AddArgument_ create(Builder & b, llvm::Value * args, llvm::Value * arg) {
        std::vector<llvm::Value *> args_;
        args_.push_back(args);
        args_.push_back(arg);
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<AddArgument_>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::AddArgument_);
        return ins;
    }

    static char const * intrinsicName() {
        return "addArgument_";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP }, false);
    }

};

class AddKeywordArgument_: public Intrinsic {
public:
    llvm::Value * args() { return getValue(0); }

    llvm::Value * arg() { return getValue(1); }

    llvm::Value * name() { return getValue(2); }

    AddKeywordArgument_(llvm::Instruction * ins):
        Intrinsic(ins) {
    }

    static AddKeywordArgument_ create(Builder & b, llvm::Value * args, llvm::Value * arg, llvm::Value * name) {
        std::vector<llvm::Value *> args_;
        args_.push_back(args);
        args_.push_back(arg);
        args_.push_back(name);
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<AddKeywordArgument_>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::AddKeywordArgument_);
        return ins;
    }

    static char const * intrinsicName() {
        return "addKeywordArgument_";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP, t::SEXP }, false);
    }

};

class AddEllipsisArgumentHead_: public Intrinsic {
public:
    llvm::Value * args() { return getValue(0); }

    llvm::Value * rho() { return getValue(1); }

    llvm::Value * eager() { return getValue(2); }

    AddEllipsisArgumentHead_(llvm::Instruction * ins):
        Intrinsic(ins) {
    }

    static AddEllipsisArgumentHead_ create(Builder & b, llvm::Value * args, llvm::Value * rho, llvm::Value * eager) {
        std::vector<llvm::Value *> args_;
        args_.push_back(args);
        args_.push_back(rho);
        args_.push_back(eager);
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<AddEllipsisArgumentHead_>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::AddEllipsisArgumentHead_);
        return ins;
    }

    static char const * intrinsicName() {
        return "addEllipsisArgumentHead_";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP, t::Bool }, false);
    }

};

class AddEllipsisArgumentTail_: public Intrinsic {
public:
    llvm::Value * args() { return getValue(0); }

    llvm::Value * rho() { return getValue(1); }

    llvm::Value * eager() { return getValue(2); }

    AddEllipsisArgumentTail_(llvm::Instruction * ins):
        Intrinsic(ins) {
    }

    static AddEllipsisArgumentTail_ create(Builder & b, llvm::Value * args, llvm::Value * rho, llvm::Value * eager) {
        std::vector<llvm::Value *> args_;
        args_.push_back(args);
        args_.push_back(rho);
        args_.push_back(eager);
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<AddEllipsisArgumentTail_>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::AddEllipsisArgumentTail_);
        return ins;
    }

    static char const * intrinsicName() {
        return "addEllipsisArgumentTail_";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP, t::Bool }, false);
    }

};

class CallBuiltin_: public Intrinsic {
public:
    llvm::Value * call() { return getValue(0); }

    llvm::Value * closure() { return getValue(1); }

    llvm::Value * arguments() { return getValue(2); }

    llvm::Value * rho() { return getValue(3); }

    CallBuiltin_(llvm::Instruction * ins):
        Intrinsic(ins) {
    }

    static CallBuiltin_ create(Builder & b, llvm::Value * call, llvm::Value * closure, llvm::Value * arguments, llvm::Value * rho) {
        std::vector<llvm::Value *> args_;
        args_.push_back(call);
        args_.push_back(closure);
        args_.push_back(arguments);
        args_.push_back(rho);
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<CallBuiltin_>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::CallBuiltin_);
        return ins;
    }

    static char const * intrinsicName() {
        return "callBuiltin_";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP, t::SEXP, t::SEXP }, false);
    }

};

class CallSpecial_: public Intrinsic {
public:
    llvm::Value * call() { return getValue(0); }

    llvm::Value * closure() { return getValue(1); }

    llvm::Value * arguments() { return getValue(2); }

    llvm::Value * rho() { return getValue(3); }

    CallSpecial_(llvm::Instruction * ins):
        Intrinsic(ins) {
    }

    static CallSpecial_ create(Builder & b, llvm::Value * call, llvm::Value * closure, llvm::Value * arguments, llvm::Value * rho) {
        std::vector<llvm::Value *> args_;
        args_.push_back(call);
        args_.push_back(closure);
        args_.push_back(arguments);
        args_.push_back(rho);
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<CallSpecial_>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::CallSpecial_);
        return ins;
    }

    static char const * intrinsicName() {
        return "callSpecial_";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP, t::SEXP, t::SEXP }, false);
    }

};

class CallClosure_: public Intrinsic {
public:
    llvm::Value * call() { return getValue(0); }

    llvm::Value * closure() { return getValue(1); }

    llvm::Value * arguments() { return getValue(2); }

    llvm::Value * rho() { return getValue(3); }

    CallClosure_(llvm::Instruction * ins):
        Intrinsic(ins) {
    }

    static CallClosure_ create(Builder & b, llvm::Value * call, llvm::Value * closure, llvm::Value * arguments, llvm::Value * rho) {
        std::vector<llvm::Value *> args_;
        args_.push_back(call);
        args_.push_back(closure);
        args_.push_back(arguments);
        args_.push_back(rho);
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<CallClosure_>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::CallClosure_);
        return ins;
    }

    static char const * intrinsicName() {
        return "callClosure_";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP, t::SEXP, t::SEXP }, false);
    }

};

class CreateClosure_: public Intrinsic {
public:
    llvm::Value * forms() { return getValue(0); }

    llvm::Value * body() { return getValue(1); }

    llvm::Value * rho() { return getValue(2); }

    CreateClosure_(llvm::Instruction * ins):
        Intrinsic(ins) {
    }

    static CreateClosure_ create(Builder & b, llvm::Value * forms, llvm::Value * body, llvm::Value * rho) {
        std::vector<llvm::Value *> args_;
        args_.push_back(forms);
        args_.push_back(body);
        args_.push_back(rho);
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<CreateClosure_>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::CreateClosure_);
        return ins;
    }

    static char const * intrinsicName() {
        return "createClosure_";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP, t::SEXP }, false);
    }

};

class GenericUnaryMinus_: public Intrinsic {
public:
    llvm::Value * op() { return getValue(0); }

    llvm::Value * rho() { return getValue(1); }

    llvm::Value * constantPool() { return getValue(2); }

    
int call() { return getValueInt(3); }
SEXP call(SEXP constantPool) { return VECTOR_ELT(constantPool, call()); }
SEXP call(Builder const & b) { return b.constantPool(call()); }
        

    GenericUnaryMinus_(llvm::Instruction * ins):
        Intrinsic(ins) {
    }

    static GenericUnaryMinus_ create(Builder & b, llvm::Value * op, llvm::Value * rho, SEXP call) {
        std::vector<llvm::Value *> args_;
        args_.push_back(op);
        args_.push_back(rho);
        args_.push_back(b.consts());
        args_.push_back(Builder::integer(b.constantPoolIndex(call)));
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GenericUnaryMinus_>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GenericUnaryMinus_);
        return ins;
    }

    static char const * intrinsicName() {
        return "genericUnaryMinus_";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP, t::SEXP, t::Int }, false);
    }

};

class GenericUnaryPlus_: public Intrinsic {
public:
    llvm::Value * op() { return getValue(0); }

    llvm::Value * rho() { return getValue(1); }

    llvm::Value * constantPool() { return getValue(2); }

    
int call() { return getValueInt(3); }
SEXP call(SEXP constantPool) { return VECTOR_ELT(constantPool, call()); }
SEXP call(Builder const & b) { return b.constantPool(call()); }
        

    GenericUnaryPlus_(llvm::Instruction * ins):
        Intrinsic(ins) {
    }

    static GenericUnaryPlus_ create(Builder & b, llvm::Value * op, llvm::Value * rho, SEXP call) {
        std::vector<llvm::Value *> args_;
        args_.push_back(op);
        args_.push_back(rho);
        args_.push_back(b.consts());
        args_.push_back(Builder::integer(b.constantPoolIndex(call)));
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GenericUnaryPlus_>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GenericUnaryPlus_);
        return ins;
    }

    static char const * intrinsicName() {
        return "genericUnaryPlus_";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP, t::SEXP, t::Int }, false);
    }

};

class GenericAdd_: public Intrinsic {
public:
    llvm::Value * lhs() { return getValue(0); }

    llvm::Value * rhs() { return getValue(1); }

    llvm::Value * rho() { return getValue(2); }

    llvm::Value * constantPool() { return getValue(3); }

    
int call() { return getValueInt(4); }
SEXP call(SEXP constantPool) { return VECTOR_ELT(constantPool, call()); }
SEXP call(Builder const & b) { return b.constantPool(call()); }
        

    GenericAdd_(llvm::Instruction * ins):
        Intrinsic(ins) {
    }

    static GenericAdd_ create(Builder & b, llvm::Value * lhs, llvm::Value * rhs, llvm::Value * rho, SEXP call) {
        std::vector<llvm::Value *> args_;
        args_.push_back(lhs);
        args_.push_back(rhs);
        args_.push_back(rho);
        args_.push_back(b.consts());
        args_.push_back(Builder::integer(b.constantPoolIndex(call)));
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GenericAdd_>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GenericAdd_);
        return ins;
    }

    static char const * intrinsicName() {
        return "genericAdd_";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP, t::SEXP, t::SEXP, t::Int }, false);
    }

};

class GenericSub_: public Intrinsic {
public:
    llvm::Value * lhs() { return getValue(0); }

    llvm::Value * rhs() { return getValue(1); }

    llvm::Value * rho() { return getValue(2); }

    llvm::Value * constantPool() { return getValue(3); }

    
int call() { return getValueInt(4); }
SEXP call(SEXP constantPool) { return VECTOR_ELT(constantPool, call()); }
SEXP call(Builder const & b) { return b.constantPool(call()); }
        

    GenericSub_(llvm::Instruction * ins):
        Intrinsic(ins) {
    }

    static GenericSub_ create(Builder & b, llvm::Value * lhs, llvm::Value * rhs, llvm::Value * rho, SEXP call) {
        std::vector<llvm::Value *> args_;
        args_.push_back(lhs);
        args_.push_back(rhs);
        args_.push_back(rho);
        args_.push_back(b.consts());
        args_.push_back(Builder::integer(b.constantPoolIndex(call)));
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GenericSub_>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GenericSub_);
        return ins;
    }

    static char const * intrinsicName() {
        return "genericSub_";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP, t::SEXP, t::SEXP, t::Int }, false);
    }

};

class GenericMul_: public Intrinsic {
public:
    llvm::Value * lhs() { return getValue(0); }

    llvm::Value * rhs() { return getValue(1); }

    llvm::Value * rho() { return getValue(2); }

    llvm::Value * constantPool() { return getValue(3); }

    
int call() { return getValueInt(4); }
SEXP call(SEXP constantPool) { return VECTOR_ELT(constantPool, call()); }
SEXP call(Builder const & b) { return b.constantPool(call()); }
        

    GenericMul_(llvm::Instruction * ins):
        Intrinsic(ins) {
    }

    static GenericMul_ create(Builder & b, llvm::Value * lhs, llvm::Value * rhs, llvm::Value * rho, SEXP call) {
        std::vector<llvm::Value *> args_;
        args_.push_back(lhs);
        args_.push_back(rhs);
        args_.push_back(rho);
        args_.push_back(b.consts());
        args_.push_back(Builder::integer(b.constantPoolIndex(call)));
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GenericMul_>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GenericMul_);
        return ins;
    }

    static char const * intrinsicName() {
        return "genericMul_";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP, t::SEXP, t::SEXP, t::Int }, false);
    }

};

class GenericDiv_: public Intrinsic {
public:
    llvm::Value * lhs() { return getValue(0); }

    llvm::Value * rhs() { return getValue(1); }

    llvm::Value * rho() { return getValue(2); }

    llvm::Value * constantPool() { return getValue(3); }

    
int call() { return getValueInt(4); }
SEXP call(SEXP constantPool) { return VECTOR_ELT(constantPool, call()); }
SEXP call(Builder const & b) { return b.constantPool(call()); }
        

    GenericDiv_(llvm::Instruction * ins):
        Intrinsic(ins) {
    }

    static GenericDiv_ create(Builder & b, llvm::Value * lhs, llvm::Value * rhs, llvm::Value * rho, SEXP call) {
        std::vector<llvm::Value *> args_;
        args_.push_back(lhs);
        args_.push_back(rhs);
        args_.push_back(rho);
        args_.push_back(b.consts());
        args_.push_back(Builder::integer(b.constantPoolIndex(call)));
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GenericDiv_>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GenericDiv_);
        return ins;
    }

    static char const * intrinsicName() {
        return "genericDiv_";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP, t::SEXP, t::SEXP, t::Int }, false);
    }

};

class GenericPow_: public Intrinsic {
public:
    llvm::Value * lhs() { return getValue(0); }

    llvm::Value * rhs() { return getValue(1); }

    llvm::Value * rho() { return getValue(2); }

    llvm::Value * constantPool() { return getValue(3); }

    
int call() { return getValueInt(4); }
SEXP call(SEXP constantPool) { return VECTOR_ELT(constantPool, call()); }
SEXP call(Builder const & b) { return b.constantPool(call()); }
        

    GenericPow_(llvm::Instruction * ins):
        Intrinsic(ins) {
    }

    static GenericPow_ create(Builder & b, llvm::Value * lhs, llvm::Value * rhs, llvm::Value * rho, SEXP call) {
        std::vector<llvm::Value *> args_;
        args_.push_back(lhs);
        args_.push_back(rhs);
        args_.push_back(rho);
        args_.push_back(b.consts());
        args_.push_back(Builder::integer(b.constantPoolIndex(call)));
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GenericPow_>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GenericPow_);
        return ins;
    }

    static char const * intrinsicName() {
        return "genericPow_";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP, t::SEXP, t::SEXP, t::Int }, false);
    }

};

class GenericSqrt_: public Intrinsic {
public:
    llvm::Value * op() { return getValue(0); }

    llvm::Value * rho() { return getValue(1); }

    llvm::Value * constantPool() { return getValue(2); }

    
int call() { return getValueInt(3); }
SEXP call(SEXP constantPool) { return VECTOR_ELT(constantPool, call()); }
SEXP call(Builder const & b) { return b.constantPool(call()); }
        

    GenericSqrt_(llvm::Instruction * ins):
        Intrinsic(ins) {
    }

    static GenericSqrt_ create(Builder & b, llvm::Value * op, llvm::Value * rho, SEXP call) {
        std::vector<llvm::Value *> args_;
        args_.push_back(op);
        args_.push_back(rho);
        args_.push_back(b.consts());
        args_.push_back(Builder::integer(b.constantPoolIndex(call)));
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GenericSqrt_>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GenericSqrt_);
        return ins;
    }

    static char const * intrinsicName() {
        return "genericSqrt_";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP, t::SEXP, t::Int }, false);
    }

};

class GenericExp_: public Intrinsic {
public:
    llvm::Value * op() { return getValue(0); }

    llvm::Value * rho() { return getValue(1); }

    llvm::Value * constantPool() { return getValue(2); }

    
int call() { return getValueInt(3); }
SEXP call(SEXP constantPool) { return VECTOR_ELT(constantPool, call()); }
SEXP call(Builder const & b) { return b.constantPool(call()); }
        

    GenericExp_(llvm::Instruction * ins):
        Intrinsic(ins) {
    }

    static GenericExp_ create(Builder & b, llvm::Value * op, llvm::Value * rho, SEXP call) {
        std::vector<llvm::Value *> args_;
        args_.push_back(op);
        args_.push_back(rho);
        args_.push_back(b.consts());
        args_.push_back(Builder::integer(b.constantPoolIndex(call)));
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GenericExp_>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GenericExp_);
        return ins;
    }

    static char const * intrinsicName() {
        return "genericExp_";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP, t::SEXP, t::Int }, false);
    }

};

class GenericEq_: public Intrinsic {
public:
    llvm::Value * lhs() { return getValue(0); }

    llvm::Value * rhs() { return getValue(1); }

    llvm::Value * rho() { return getValue(2); }

    llvm::Value * constantPool() { return getValue(3); }

    
int call() { return getValueInt(4); }
SEXP call(SEXP constantPool) { return VECTOR_ELT(constantPool, call()); }
SEXP call(Builder const & b) { return b.constantPool(call()); }
        

    GenericEq_(llvm::Instruction * ins):
        Intrinsic(ins) {
    }

    static GenericEq_ create(Builder & b, llvm::Value * lhs, llvm::Value * rhs, llvm::Value * rho, SEXP call) {
        std::vector<llvm::Value *> args_;
        args_.push_back(lhs);
        args_.push_back(rhs);
        args_.push_back(rho);
        args_.push_back(b.consts());
        args_.push_back(Builder::integer(b.constantPoolIndex(call)));
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GenericEq_>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GenericEq_);
        return ins;
    }

    static char const * intrinsicName() {
        return "genericEq_";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP, t::SEXP, t::SEXP, t::Int }, false);
    }

};

class GenericNe_: public Intrinsic {
public:
    llvm::Value * lhs() { return getValue(0); }

    llvm::Value * rhs() { return getValue(1); }

    llvm::Value * rho() { return getValue(2); }

    llvm::Value * constantPool() { return getValue(3); }

    
int call() { return getValueInt(4); }
SEXP call(SEXP constantPool) { return VECTOR_ELT(constantPool, call()); }
SEXP call(Builder const & b) { return b.constantPool(call()); }
        

    GenericNe_(llvm::Instruction * ins):
        Intrinsic(ins) {
    }

    static GenericNe_ create(Builder & b, llvm::Value * lhs, llvm::Value * rhs, llvm::Value * rho, SEXP call) {
        std::vector<llvm::Value *> args_;
        args_.push_back(lhs);
        args_.push_back(rhs);
        args_.push_back(rho);
        args_.push_back(b.consts());
        args_.push_back(Builder::integer(b.constantPoolIndex(call)));
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GenericNe_>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GenericNe_);
        return ins;
    }

    static char const * intrinsicName() {
        return "genericNe_";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP, t::SEXP, t::SEXP, t::Int }, false);
    }

};

class GenericLt_: public Intrinsic {
public:
    llvm::Value * lhs() { return getValue(0); }

    llvm::Value * rhs() { return getValue(1); }

    llvm::Value * rho() { return getValue(2); }

    llvm::Value * constantPool() { return getValue(3); }

    
int call() { return getValueInt(4); }
SEXP call(SEXP constantPool) { return VECTOR_ELT(constantPool, call()); }
SEXP call(Builder const & b) { return b.constantPool(call()); }
        

    GenericLt_(llvm::Instruction * ins):
        Intrinsic(ins) {
    }

    static GenericLt_ create(Builder & b, llvm::Value * lhs, llvm::Value * rhs, llvm::Value * rho, SEXP call) {
        std::vector<llvm::Value *> args_;
        args_.push_back(lhs);
        args_.push_back(rhs);
        args_.push_back(rho);
        args_.push_back(b.consts());
        args_.push_back(Builder::integer(b.constantPoolIndex(call)));
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GenericLt_>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GenericLt_);
        return ins;
    }

    static char const * intrinsicName() {
        return "genericLt_";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP, t::SEXP, t::SEXP, t::Int }, false);
    }

};

class GenericLe_: public Intrinsic {
public:
    llvm::Value * lhs() { return getValue(0); }

    llvm::Value * rhs() { return getValue(1); }

    llvm::Value * rho() { return getValue(2); }

    llvm::Value * constantPool() { return getValue(3); }

    
int call() { return getValueInt(4); }
SEXP call(SEXP constantPool) { return VECTOR_ELT(constantPool, call()); }
SEXP call(Builder const & b) { return b.constantPool(call()); }
        

    GenericLe_(llvm::Instruction * ins):
        Intrinsic(ins) {
    }

    static GenericLe_ create(Builder & b, llvm::Value * lhs, llvm::Value * rhs, llvm::Value * rho, SEXP call) {
        std::vector<llvm::Value *> args_;
        args_.push_back(lhs);
        args_.push_back(rhs);
        args_.push_back(rho);
        args_.push_back(b.consts());
        args_.push_back(Builder::integer(b.constantPoolIndex(call)));
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GenericLe_>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GenericLe_);
        return ins;
    }

    static char const * intrinsicName() {
        return "genericLe_";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP, t::SEXP, t::SEXP, t::Int }, false);
    }

};

class GenericGe_: public Intrinsic {
public:
    llvm::Value * lhs() { return getValue(0); }

    llvm::Value * rhs() { return getValue(1); }

    llvm::Value * rho() { return getValue(2); }

    llvm::Value * constantPool() { return getValue(3); }

    
int call() { return getValueInt(4); }
SEXP call(SEXP constantPool) { return VECTOR_ELT(constantPool, call()); }
SEXP call(Builder const & b) { return b.constantPool(call()); }
        

    GenericGe_(llvm::Instruction * ins):
        Intrinsic(ins) {
    }

    static GenericGe_ create(Builder & b, llvm::Value * lhs, llvm::Value * rhs, llvm::Value * rho, SEXP call) {
        std::vector<llvm::Value *> args_;
        args_.push_back(lhs);
        args_.push_back(rhs);
        args_.push_back(rho);
        args_.push_back(b.consts());
        args_.push_back(Builder::integer(b.constantPoolIndex(call)));
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GenericGe_>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GenericGe_);
        return ins;
    }

    static char const * intrinsicName() {
        return "genericGe_";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP, t::SEXP, t::SEXP, t::Int }, false);
    }

};

class GenericGt_: public Intrinsic {
public:
    llvm::Value * lhs() { return getValue(0); }

    llvm::Value * rhs() { return getValue(1); }

    llvm::Value * rho() { return getValue(2); }

    llvm::Value * constantPool() { return getValue(3); }

    
int call() { return getValueInt(4); }
SEXP call(SEXP constantPool) { return VECTOR_ELT(constantPool, call()); }
SEXP call(Builder const & b) { return b.constantPool(call()); }
        

    GenericGt_(llvm::Instruction * ins):
        Intrinsic(ins) {
    }

    static GenericGt_ create(Builder & b, llvm::Value * lhs, llvm::Value * rhs, llvm::Value * rho, SEXP call) {
        std::vector<llvm::Value *> args_;
        args_.push_back(lhs);
        args_.push_back(rhs);
        args_.push_back(rho);
        args_.push_back(b.consts());
        args_.push_back(Builder::integer(b.constantPoolIndex(call)));
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GenericGt_>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GenericGt_);
        return ins;
    }

    static char const * intrinsicName() {
        return "genericGt_";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP, t::SEXP, t::SEXP, t::Int }, false);
    }

};

class GenericBitAnd_: public Intrinsic {
public:
    llvm::Value * lhs() { return getValue(0); }

    llvm::Value * rhs() { return getValue(1); }

    llvm::Value * rho() { return getValue(2); }

    llvm::Value * constantPool() { return getValue(3); }

    
int call() { return getValueInt(4); }
SEXP call(SEXP constantPool) { return VECTOR_ELT(constantPool, call()); }
SEXP call(Builder const & b) { return b.constantPool(call()); }
        

    GenericBitAnd_(llvm::Instruction * ins):
        Intrinsic(ins) {
    }

    static GenericBitAnd_ create(Builder & b, llvm::Value * lhs, llvm::Value * rhs, llvm::Value * rho, SEXP call) {
        std::vector<llvm::Value *> args_;
        args_.push_back(lhs);
        args_.push_back(rhs);
        args_.push_back(rho);
        args_.push_back(b.consts());
        args_.push_back(Builder::integer(b.constantPoolIndex(call)));
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GenericBitAnd_>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GenericBitAnd_);
        return ins;
    }

    static char const * intrinsicName() {
        return "genericBitAnd_";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP, t::SEXP, t::SEXP, t::Int }, false);
    }

};

class GenericBitOr_: public Intrinsic {
public:
    llvm::Value * lhs() { return getValue(0); }

    llvm::Value * rhs() { return getValue(1); }

    llvm::Value * rho() { return getValue(2); }

    llvm::Value * constantPool() { return getValue(3); }

    
int call() { return getValueInt(4); }
SEXP call(SEXP constantPool) { return VECTOR_ELT(constantPool, call()); }
SEXP call(Builder const & b) { return b.constantPool(call()); }
        

    GenericBitOr_(llvm::Instruction * ins):
        Intrinsic(ins) {
    }

    static GenericBitOr_ create(Builder & b, llvm::Value * lhs, llvm::Value * rhs, llvm::Value * rho, SEXP call) {
        std::vector<llvm::Value *> args_;
        args_.push_back(lhs);
        args_.push_back(rhs);
        args_.push_back(rho);
        args_.push_back(b.consts());
        args_.push_back(Builder::integer(b.constantPoolIndex(call)));
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GenericBitOr_>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GenericBitOr_);
        return ins;
    }

    static char const * intrinsicName() {
        return "genericBitOr_";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP, t::SEXP, t::SEXP, t::Int }, false);
    }

};

class GenericNot_: public Intrinsic {
public:
    llvm::Value * op() { return getValue(0); }

    llvm::Value * rho() { return getValue(1); }

    llvm::Value * constantPool() { return getValue(2); }

    
int call() { return getValueInt(3); }
SEXP call(SEXP constantPool) { return VECTOR_ELT(constantPool, call()); }
SEXP call(Builder const & b) { return b.constantPool(call()); }
        

    GenericNot_(llvm::Instruction * ins):
        Intrinsic(ins) {
    }

    static GenericNot_ create(Builder & b, llvm::Value * op, llvm::Value * rho, SEXP call) {
        std::vector<llvm::Value *> args_;
        args_.push_back(op);
        args_.push_back(rho);
        args_.push_back(b.consts());
        args_.push_back(Builder::integer(b.constantPoolIndex(call)));
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GenericNot_>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GenericNot_);
        return ins;
    }

    static char const * intrinsicName() {
        return "genericNot_";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP, t::SEXP, t::Int }, false);
    }

};

class GenericGetVarMissOK_: public Intrinsic {
public:
    llvm::Value * symbol() { return getValue(0); }

    llvm::Value * rho() { return getValue(1); }

    GenericGetVarMissOK_(llvm::Instruction * ins):
        Intrinsic(ins) {
    }

    static GenericGetVarMissOK_ create(Builder & b, llvm::Value * symbol, llvm::Value * rho) {
        std::vector<llvm::Value *> args_;
        args_.push_back(symbol);
        args_.push_back(rho);
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GenericGetVarMissOK_>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GenericGetVarMissOK_);
        return ins;
    }

    static char const * intrinsicName() {
        return "genericGetVarMissOK_";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP }, false);
    }

};

class GenericGetEllipsisValueMissOK_: public Intrinsic {
public:
    llvm::Value * symbol() { return getValue(0); }

    llvm::Value * rho() { return getValue(1); }

    GenericGetEllipsisValueMissOK_(llvm::Instruction * ins):
        Intrinsic(ins) {
    }

    static GenericGetEllipsisValueMissOK_ create(Builder & b, llvm::Value * symbol, llvm::Value * rho) {
        std::vector<llvm::Value *> args_;
        args_.push_back(symbol);
        args_.push_back(rho);
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GenericGetEllipsisValueMissOK_>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GenericGetEllipsisValueMissOK_);
        return ins;
    }

    static char const * intrinsicName() {
        return "genericGetEllipsisValueMissOK_";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP }, false);
    }

};

class CheckSwitchControl_: public Intrinsic {
public:
    llvm::Value * ctrl() { return getValue(0); }

    llvm::Value * constantPool() { return getValue(1); }

    
int call() { return getValueInt(2); }
SEXP call(SEXP constantPool) { return VECTOR_ELT(constantPool, call()); }
SEXP call(Builder const & b) { return b.constantPool(call()); }
        

    CheckSwitchControl_(llvm::Instruction * ins):
        Intrinsic(ins) {
    }

    static CheckSwitchControl_ create(Builder & b, llvm::Value * ctrl, SEXP call) {
        std::vector<llvm::Value *> args_;
        args_.push_back(ctrl);
        args_.push_back(b.consts());
        args_.push_back(Builder::integer(b.constantPoolIndex(call)));
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<CheckSwitchControl_>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::CheckSwitchControl_);
        return ins;
    }

    static char const * intrinsicName() {
        return "checkSwitchControl_";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::Void, { t::SEXP, t::SEXP, t::Int }, false);
    }

};

class SwitchControlCharacter_: public Intrinsic {
public:
    llvm::Value * ctrl() { return getValue(0); }

    llvm::Value * consts() { return getValue(1); }

    
int call() { return getValueInt(2); }
SEXP call(SEXP constantPool) { return VECTOR_ELT(constantPool, call()); }
SEXP call(Builder const & b) { return b.constantPool(call()); }
        

    
int cases() { return getValueInt(3); }
SEXP cases(SEXP constantPool) { return VECTOR_ELT(constantPool, cases()); }
SEXP cases(Builder const & b) { return b.constantPool(cases()); }
        

    SwitchControlCharacter_(llvm::Instruction * ins):
        Intrinsic(ins) {
    }

    static SwitchControlCharacter_ create(Builder & b, llvm::Value * ctrl, llvm::Value * consts, SEXP call, SEXP cases) {
        std::vector<llvm::Value *> args_;
        args_.push_back(ctrl);
        args_.push_back(consts);
        args_.push_back(Builder::integer(b.constantPoolIndex(call)));
        args_.push_back(Builder::integer(b.constantPoolIndex(cases)));
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<SwitchControlCharacter_>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::SwitchControlCharacter_);
        return ins;
    }

    static char const * intrinsicName() {
        return "switchControlCharacter_";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::Int, { t::SEXP, t::SEXP, t::Int, t::Int }, false);
    }

};

class SwitchControlInteger_: public Intrinsic {
public:
    llvm::Value * ctrl() { return getValue(0); }

    int numCases() { return getValueInt(1); }

    SwitchControlInteger_(llvm::Instruction * ins):
        Intrinsic(ins) {
    }

    static SwitchControlInteger_ create(Builder & b, llvm::Value * ctrl, int numCases) {
        std::vector<llvm::Value *> args_;
        args_.push_back(ctrl);
        args_.push_back(Builder::integer(numCases));
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<SwitchControlInteger_>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::SwitchControlInteger_);
        return ins;
    }

    static char const * intrinsicName() {
        return "switchControlInteger_";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::Int, { t::SEXP, t::Int }, false);
    }

};

class ReturnJump_: public Intrinsic {
public:
    llvm::Value * value() { return getValue(0); }

    llvm::Value * rho() { return getValue(1); }

    ReturnJump_(llvm::Instruction * ins):
        Intrinsic(ins) {
    }

    static ReturnJump_ create(Builder & b, llvm::Value * value, llvm::Value * rho) {
        std::vector<llvm::Value *> args_;
        args_.push_back(value);
        args_.push_back(rho);
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<ReturnJump_>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::ReturnJump_);
        return ins;
    }

    static char const * intrinsicName() {
        return "returnJump_";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::Void, { t::SEXP, t::SEXP }, false);
    }

};
} // namespace ir
} // namespace rjit
#endif // INTRINSICS_H_

