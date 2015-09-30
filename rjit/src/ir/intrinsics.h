#ifndef INTRINSICS_H_
#define INTRINSICS_H_

#include "ir.h"
#include "Builder.h"

namespace rjit {
namespace ir {

/** Replacement for GETSTACK_LOGICAL_NO_NA_PTR
The call is used only for error reporting.
*/
class ConvertToLogicalNoNA: public Intrinsic {
public:
    llvm::Value * what() { return getValue(0); }

    SEXP call() { return getValueSEXP(1); }

    ConvertToLogicalNoNA(llvm::CallInst * ins):
        Intrinsic(ins) {
    }

    static ConvertToLogicalNoNA create(Builder & b, llvm::Value * what, SEXP call) {
        std::vector<llvm::Value *> args_;
        args_.push_back(what);
        args_.push_back(Builder::constantPoolSexp(call));
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<ConvertToLogicalNoNA>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::ConvertToLogicalNoNA);
        return ins;
    }

    static char const * intrinsicName() {
        return "convertToLogicalNoNA";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::Int, { t::SEXP, t::SEXP }, false);
    }

};

class PrintValue: public Intrinsic {
public:
    llvm::Value * value() { return getValue(0); }

    PrintValue(llvm::CallInst * ins):
        Intrinsic(ins) {
    }

    static PrintValue create(Builder & b, llvm::Value * value) {
        std::vector<llvm::Value *> args_;
        args_.push_back(value);
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<PrintValue>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::PrintValue);
        return ins;
    }

    static char const * intrinsicName() {
        return "printValue";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::Void, { t::SEXP }, false);
    }

};
/** startFor returns the sequence over which the loop will iterate.
No need for all the other things here because we do not support other than generic variable loads and stores.
*/
class StartFor: public Intrinsic {
public:
    llvm::Value * seq() { return getValue(0); }

    llvm::Value * rho() { return getValue(1); }

    StartFor(llvm::CallInst * ins):
        Intrinsic(ins) {
    }

    static StartFor create(Builder & b, llvm::Value * seq, llvm::Value * rho) {
        std::vector<llvm::Value *> args_;
        args_.push_back(seq);
        args_.push_back(rho);
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<StartFor>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::StartFor);
        return ins;
    }

    static char const * intrinsicName() {
        return "startFor";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP }, false);
    }

};
/** Loop sequence length returns the length of the sequence the loop will iterate over and errors if the sequence is of wrong type.
*/
class LoopSequenceLength: public Intrinsic {
public:
    llvm::Value * seq() { return getValue(0); }

    SEXP call() { return getValueSEXP(1); }

    LoopSequenceLength(llvm::CallInst * ins):
        Intrinsic(ins) {
    }

    static LoopSequenceLength create(Builder & b, llvm::Value * seq, SEXP call) {
        std::vector<llvm::Value *> args_;
        args_.push_back(seq);
        args_.push_back(Builder::constantPoolSexp(call));
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<LoopSequenceLength>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::LoopSequenceLength);
        return ins;
    }

    static char const * intrinsicName() {
        return "loopSequenceLength";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::Int, { t::SEXP, t::SEXP }, false);
    }

};
/** Given the for loop sequence, and index, returns the index-th value of the sequence.
TODO Note that this always allocates for vectors.
*/
class GetForLoopValue: public Intrinsic {
public:
    llvm::Value * seq() { return getValue(0); }

    int index() { return getValueInt(1); }

    GetForLoopValue(llvm::CallInst * ins):
        Intrinsic(ins) {
    }

    static GetForLoopValue create(Builder & b, llvm::Value * seq, int index) {
        std::vector<llvm::Value *> args_;
        args_.push_back(seq);
        args_.push_back(Builder::integer(index));
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GetForLoopValue>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GetForLoopValue);
        return ins;
    }

    static char const * intrinsicName() {
        return "getForLoopValue";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::Int }, false);
    }

};

class MarkVisible: public Intrinsic {
public:
    MarkVisible(llvm::CallInst * ins):
        Intrinsic(ins) {
    }

    static MarkVisible create(Builder & b) {
        std::vector<llvm::Value *> args_;
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<MarkVisible>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::MarkVisible);
        return ins;
    }

    static char const * intrinsicName() {
        return "markVisible";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::Void, {  }, false);
    }

};

class MarkInvisible: public Intrinsic {
public:
    MarkInvisible(llvm::CallInst * ins):
        Intrinsic(ins) {
    }

    static MarkInvisible create(Builder & b) {
        std::vector<llvm::Value *> args_;
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<MarkInvisible>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::MarkInvisible);
        return ins;
    }

    static char const * intrinsicName() {
        return "markInvisible";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::Void, {  }, false);
    }

};
/** When LLVM IR creates user visible constant, this function contains all the code required to make the constant.
Currently this means marking it as not mutable only.
*/
class UserConstant: public Intrinsic {
public:
    llvm::Value * value() { return getValue(0); }

    UserConstant(llvm::CallInst * ins):
        Intrinsic(ins) {
    }

    static UserConstant create(Builder & b, llvm::Value * value) {
        std::vector<llvm::Value *> args_;
        args_.push_back(value);
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<UserConstant>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::UserConstant);
        return ins;
    }

    static char const * intrinsicName() {
        return "userConstant";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::Void, { t::SEXP }, false);
    }

};
/** Generic getvar does not use any caches whatsoever.
TODO this means we can get rid of the checks in getvar(), and reduce its code to this. We definitely want faster versions.
*/
class GenericGetVar: public Intrinsic {
public:
    SEXP symbol() { return getValueSEXP(0); }

    llvm::Value * rho() { return getValue(1); }

    GenericGetVar(llvm::CallInst * ins):
        Intrinsic(ins) {
    }

    static GenericGetVar create(Builder & b, SEXP symbol, llvm::Value * rho) {
        std::vector<llvm::Value *> args_;
        args_.push_back(Builder::constantPoolSexp(symbol));
        args_.push_back(rho);
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GenericGetVar>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GenericGetVar);
        return ins;
    }

    static char const * intrinsicName() {
        return "genericGetVar";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP }, false);
    }

};

class GenericGetEllipsisArg: public Intrinsic {
public:
    SEXP symbol() { return getValueSEXP(0); }

    llvm::Value * rho() { return getValue(1); }

    GenericGetEllipsisArg(llvm::CallInst * ins):
        Intrinsic(ins) {
    }

    static GenericGetEllipsisArg create(Builder & b, SEXP symbol, llvm::Value * rho) {
        std::vector<llvm::Value *> args_;
        args_.push_back(Builder::constantPoolSexp(symbol));
        args_.push_back(rho);
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GenericGetEllipsisArg>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GenericGetEllipsisArg);
        return ins;
    }

    static char const * intrinsicName() {
        return "genericGetEllipsisArg";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP }, false);
    }

};

class GenericSetVar: public Intrinsic {
public:
    SEXP symbol() { return getValueSEXP(0); }

    llvm::Value * value() { return getValue(1); }

    llvm::Value * rho() { return getValue(2); }

    GenericSetVar(llvm::CallInst * ins):
        Intrinsic(ins) {
    }

    static GenericSetVar create(Builder & b, SEXP symbol, llvm::Value * value, llvm::Value * rho) {
        std::vector<llvm::Value *> args_;
        args_.push_back(Builder::constantPoolSexp(symbol));
        args_.push_back(value);
        args_.push_back(rho);
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GenericSetVar>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GenericSetVar);
        return ins;
    }

    static char const * intrinsicName() {
        return "genericSetVar";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::Void, { t::SEXP, t::SEXP, t::SEXP }, false);
    }

};

class GenericSetVarParent: public Intrinsic {
public:
    SEXP symbol() { return getValueSEXP(0); }

    llvm::Value * value() { return getValue(1); }

    llvm::Value * rho() { return getValue(2); }

    GenericSetVarParent(llvm::CallInst * ins):
        Intrinsic(ins) {
    }

    static GenericSetVarParent create(Builder & b, SEXP symbol, llvm::Value * value, llvm::Value * rho) {
        std::vector<llvm::Value *> args_;
        args_.push_back(Builder::constantPoolSexp(symbol));
        args_.push_back(value);
        args_.push_back(rho);
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GenericSetVarParent>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GenericSetVarParent);
        return ins;
    }

    static char const * intrinsicName() {
        return "genericSetVarParent";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::Void, { t::SEXP, t::SEXP, t::SEXP }, false);
    }

};

class GetFunction: public Intrinsic {
public:
    SEXP symbol() { return getValueSEXP(0); }

    llvm::Value * rho() { return getValue(1); }

    GetFunction(llvm::CallInst * ins):
        Intrinsic(ins) {
    }

    static GetFunction create(Builder & b, SEXP symbol, llvm::Value * rho) {
        std::vector<llvm::Value *> args_;
        args_.push_back(Builder::constantPoolSexp(symbol));
        args_.push_back(rho);
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GetFunction>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GetFunction);
        return ins;
    }

    static char const * intrinsicName() {
        return "getFunction";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP }, false);
    }

};

class GetGlobalFunction: public Intrinsic {
public:
    SEXP symbol() { return getValueSEXP(0); }

    GetGlobalFunction(llvm::CallInst * ins):
        Intrinsic(ins) {
    }

    static GetGlobalFunction create(Builder & b, SEXP symbol) {
        std::vector<llvm::Value *> args_;
        args_.push_back(Builder::constantPoolSexp(symbol));
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GetGlobalFunction>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GetGlobalFunction);
        return ins;
    }

    static char const * intrinsicName() {
        return "getGlobalFunction";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP }, false);
    }

};

class GetSymFunction: public Intrinsic {
public:
    SEXP name() { return getValueSEXP(0); }

    GetSymFunction(llvm::CallInst * ins):
        Intrinsic(ins) {
    }

    static GetSymFunction create(Builder & b, SEXP name) {
        std::vector<llvm::Value *> args_;
        args_.push_back(Builder::constantPoolSexp(name));
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GetSymFunction>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GetSymFunction);
        return ins;
    }

    static char const * intrinsicName() {
        return "getSymFunction";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP }, false);
    }

};

class GetBuiltinFunction: public Intrinsic {
public:
    SEXP name() { return getValueSEXP(0); }

    GetBuiltinFunction(llvm::CallInst * ins):
        Intrinsic(ins) {
    }

    static GetBuiltinFunction create(Builder & b, SEXP name) {
        std::vector<llvm::Value *> args_;
        args_.push_back(Builder::constantPoolSexp(name));
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GetBuiltinFunction>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GetBuiltinFunction);
        return ins;
    }

    static char const * intrinsicName() {
        return "getBuiltinFunction";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP }, false);
    }

};

class GetInternalBuiltinFunction: public Intrinsic {
public:
    SEXP name() { return getValueSEXP(0); }

    GetInternalBuiltinFunction(llvm::CallInst * ins):
        Intrinsic(ins) {
    }

    static GetInternalBuiltinFunction create(Builder & b, SEXP name) {
        std::vector<llvm::Value *> args_;
        args_.push_back(Builder::constantPoolSexp(name));
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GetInternalBuiltinFunction>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GetInternalBuiltinFunction);
        return ins;
    }

    static char const * intrinsicName() {
        return "getInternalBuiltinFunction";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP }, false);
    }

};

class CheckFunction: public Intrinsic {
public:
    llvm::Value * f() { return getValue(0); }

    CheckFunction(llvm::CallInst * ins):
        Intrinsic(ins) {
    }

    static CheckFunction create(Builder & b, llvm::Value * f) {
        std::vector<llvm::Value *> args_;
        args_.push_back(f);
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<CheckFunction>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::CheckFunction);
        return ins;
    }

    static char const * intrinsicName() {
        return "checkFunction";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::Void, { t::SEXP }, false);
    }

};
/** Creates a promise out of the given code and environment and returns its value.
*/
class CreatePromise: public Intrinsic {
public:
    SEXP code() { return getValueSEXP(0); }

    llvm::Value * rho() { return getValue(1); }

    CreatePromise(llvm::CallInst * ins):
        Intrinsic(ins) {
    }

    static CreatePromise create(Builder & b, SEXP code, llvm::Value * rho) {
        std::vector<llvm::Value *> args_;
        args_.push_back(Builder::constantPoolSexp(code));
        args_.push_back(rho);
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<CreatePromise>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::CreatePromise);
        return ins;
    }

    static char const * intrinsicName() {
        return "createPromise";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP }, false);
    }

};
/** Given a SEXP, returns its type.
We can perfectly do this in LLVM, but having an function for it simplifies the analysis on our end.
*/
class SexpType: public Intrinsic {
public:
    llvm::Value * value() { return getValue(0); }

    SexpType(llvm::CallInst * ins):
        Intrinsic(ins) {
    }

    static SexpType create(Builder & b, llvm::Value * value) {
        std::vector<llvm::Value *> args_;
        args_.push_back(value);
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<SexpType>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::SexpType);
        return ins;
    }

    static char const * intrinsicName() {
        return "sexpType";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::Int, { t::SEXP }, false);
    }

};

class AddArgument: public Intrinsic {
public:
    llvm::Value * args() { return getValue(0); }

    llvm::Value * arg() { return getValue(1); }

    AddArgument(llvm::CallInst * ins):
        Intrinsic(ins) {
    }

    static AddArgument create(Builder & b, llvm::Value * args, llvm::Value * arg) {
        std::vector<llvm::Value *> args_;
        args_.push_back(args);
        args_.push_back(arg);
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<AddArgument>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::AddArgument);
        return ins;
    }

    static char const * intrinsicName() {
        return "addArgument";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP }, false);
    }

};

class AddKeywordArgument: public Intrinsic {
public:
    llvm::Value * args() { return getValue(0); }

    llvm::Value * arg() { return getValue(1); }

    llvm::Value * name() { return getValue(2); }

    AddKeywordArgument(llvm::CallInst * ins):
        Intrinsic(ins) {
    }

    static AddKeywordArgument create(Builder & b, llvm::Value * args, llvm::Value * arg, llvm::Value * name) {
        std::vector<llvm::Value *> args_;
        args_.push_back(args);
        args_.push_back(arg);
        args_.push_back(name);
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<AddKeywordArgument>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::AddKeywordArgument);
        return ins;
    }

    static char const * intrinsicName() {
        return "addKeywordArgument";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP, t::SEXP }, false);
    }

};

class AddEllipsisArgument: public Intrinsic {
public:
    llvm::Value * args() { return getValue(0); }

    llvm::Value * rho() { return getValue(1); }

    llvm::Value * eager() { return getValue(2); }

    AddEllipsisArgument(llvm::CallInst * ins):
        Intrinsic(ins) {
    }

    static AddEllipsisArgument create(Builder & b, llvm::Value * args, llvm::Value * rho, llvm::Value * eager) {
        std::vector<llvm::Value *> args_;
        args_.push_back(args);
        args_.push_back(rho);
        args_.push_back(eager);
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<AddEllipsisArgument>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::AddEllipsisArgument);
        return ins;
    }

    static char const * intrinsicName() {
        return "addEllipsisArgument";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP, t::Bool }, false);
    }

};

class CallBuiltin: public Intrinsic {
public:
    llvm::Value * call() { return getValue(0); }

    llvm::Value * closure() { return getValue(1); }

    llvm::Value * arguments() { return getValue(2); }

    llvm::Value * rho() { return getValue(3); }

    CallBuiltin(llvm::CallInst * ins):
        Intrinsic(ins) {
    }

    static CallBuiltin create(Builder & b, llvm::Value * call, llvm::Value * closure, llvm::Value * arguments, llvm::Value * rho) {
        std::vector<llvm::Value *> args_;
        args_.push_back(call);
        args_.push_back(closure);
        args_.push_back(arguments);
        args_.push_back(rho);
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<CallBuiltin>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::CallBuiltin);
        return ins;
    }

    static char const * intrinsicName() {
        return "callBuiltin";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP, t::SEXP, t::SEXP }, false);
    }

};

class CallSpecial: public Intrinsic {
public:
    llvm::Value * call() { return getValue(0); }

    llvm::Value * closure() { return getValue(1); }

    llvm::Value * arguments() { return getValue(2); }

    llvm::Value * rho() { return getValue(3); }

    CallSpecial(llvm::CallInst * ins):
        Intrinsic(ins) {
    }

    static CallSpecial create(Builder & b, llvm::Value * call, llvm::Value * closure, llvm::Value * arguments, llvm::Value * rho) {
        std::vector<llvm::Value *> args_;
        args_.push_back(call);
        args_.push_back(closure);
        args_.push_back(arguments);
        args_.push_back(rho);
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<CallSpecial>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::CallSpecial);
        return ins;
    }

    static char const * intrinsicName() {
        return "callSpecial";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP, t::SEXP, t::SEXP }, false);
    }

};

class CallClosure: public Intrinsic {
public:
    llvm::Value * call() { return getValue(0); }

    llvm::Value * closure() { return getValue(1); }

    llvm::Value * arguments() { return getValue(2); }

    llvm::Value * rho() { return getValue(3); }

    CallClosure(llvm::CallInst * ins):
        Intrinsic(ins) {
    }

    static CallClosure create(Builder & b, llvm::Value * call, llvm::Value * closure, llvm::Value * arguments, llvm::Value * rho) {
        std::vector<llvm::Value *> args_;
        args_.push_back(call);
        args_.push_back(closure);
        args_.push_back(arguments);
        args_.push_back(rho);
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<CallClosure>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::CallClosure);
        return ins;
    }

    static char const * intrinsicName() {
        return "callClosure";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP, t::SEXP, t::SEXP }, false);
    }

};

class CreateClosure: public Intrinsic {
public:
    llvm::Value * forms() { return getValue(0); }

    llvm::Value * body() { return getValue(1); }

    llvm::Value * rho() { return getValue(2); }

    CreateClosure(llvm::CallInst * ins):
        Intrinsic(ins) {
    }

    static CreateClosure create(Builder & b, llvm::Value * forms, llvm::Value * body, llvm::Value * rho) {
        std::vector<llvm::Value *> args_;
        args_.push_back(forms);
        args_.push_back(body);
        args_.push_back(rho);
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<CreateClosure>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::CreateClosure);
        return ins;
    }

    static char const * intrinsicName() {
        return "createClosure";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP, t::SEXP }, false);
    }

};

class GenericUnaryMinus: public Intrinsic {
public:
    llvm::Value * op() { return getValue(0); }

    SEXP call() { return getValueSEXP(1); }

    llvm::Value * rho() { return getValue(2); }

    GenericUnaryMinus(llvm::CallInst * ins):
        Intrinsic(ins) {
    }

    static GenericUnaryMinus create(Builder & b, llvm::Value * op, SEXP call, llvm::Value * rho) {
        std::vector<llvm::Value *> args_;
        args_.push_back(op);
        args_.push_back(Builder::constantPoolSexp(call));
        args_.push_back(rho);
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GenericUnaryMinus>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GenericUnaryMinus);
        return ins;
    }

    static char const * intrinsicName() {
        return "genericUnaryMinus";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP, t::SEXP }, false);
    }

};

class GenericUnaryPlus: public Intrinsic {
public:
    llvm::Value * op() { return getValue(0); }

    SEXP call() { return getValueSEXP(1); }

    llvm::Value * rho() { return getValue(2); }

    GenericUnaryPlus(llvm::CallInst * ins):
        Intrinsic(ins) {
    }

    static GenericUnaryPlus create(Builder & b, llvm::Value * op, SEXP call, llvm::Value * rho) {
        std::vector<llvm::Value *> args_;
        args_.push_back(op);
        args_.push_back(Builder::constantPoolSexp(call));
        args_.push_back(rho);
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GenericUnaryPlus>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GenericUnaryPlus);
        return ins;
    }

    static char const * intrinsicName() {
        return "genericUnaryPlus";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP, t::SEXP }, false);
    }

};

class GenericAdd: public Intrinsic {
public:
    llvm::Value * lhs() { return getValue(0); }

    llvm::Value * rhs() { return getValue(1); }

    SEXP call() { return getValueSEXP(2); }

    llvm::Value * rho() { return getValue(3); }

    GenericAdd(llvm::CallInst * ins):
        Intrinsic(ins) {
    }

    static GenericAdd create(Builder & b, llvm::Value * lhs, llvm::Value * rhs, SEXP call, llvm::Value * rho) {
        std::vector<llvm::Value *> args_;
        args_.push_back(lhs);
        args_.push_back(rhs);
        args_.push_back(Builder::constantPoolSexp(call));
        args_.push_back(rho);
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GenericAdd>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GenericAdd);
        return ins;
    }

    static char const * intrinsicName() {
        return "genericAdd";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP, t::SEXP, t::SEXP }, false);
    }

};

class GenericSub: public Intrinsic {
public:
    llvm::Value * lhs() { return getValue(0); }

    llvm::Value * rhs() { return getValue(1); }

    SEXP call() { return getValueSEXP(2); }

    llvm::Value * rho() { return getValue(3); }

    GenericSub(llvm::CallInst * ins):
        Intrinsic(ins) {
    }

    static GenericSub create(Builder & b, llvm::Value * lhs, llvm::Value * rhs, SEXP call, llvm::Value * rho) {
        std::vector<llvm::Value *> args_;
        args_.push_back(lhs);
        args_.push_back(rhs);
        args_.push_back(Builder::constantPoolSexp(call));
        args_.push_back(rho);
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GenericSub>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GenericSub);
        return ins;
    }

    static char const * intrinsicName() {
        return "genericSub";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP, t::SEXP, t::SEXP }, false);
    }

};

class GenericMul: public Intrinsic {
public:
    llvm::Value * lhs() { return getValue(0); }

    llvm::Value * rhs() { return getValue(1); }

    SEXP call() { return getValueSEXP(2); }

    llvm::Value * rho() { return getValue(3); }

    GenericMul(llvm::CallInst * ins):
        Intrinsic(ins) {
    }

    static GenericMul create(Builder & b, llvm::Value * lhs, llvm::Value * rhs, SEXP call, llvm::Value * rho) {
        std::vector<llvm::Value *> args_;
        args_.push_back(lhs);
        args_.push_back(rhs);
        args_.push_back(Builder::constantPoolSexp(call));
        args_.push_back(rho);
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GenericMul>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GenericMul);
        return ins;
    }

    static char const * intrinsicName() {
        return "genericMul";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP, t::SEXP, t::SEXP }, false);
    }

};

class GenericDiv: public Intrinsic {
public:
    llvm::Value * lhs() { return getValue(0); }

    llvm::Value * rhs() { return getValue(1); }

    SEXP call() { return getValueSEXP(2); }

    llvm::Value * rho() { return getValue(3); }

    GenericDiv(llvm::CallInst * ins):
        Intrinsic(ins) {
    }

    static GenericDiv create(Builder & b, llvm::Value * lhs, llvm::Value * rhs, SEXP call, llvm::Value * rho) {
        std::vector<llvm::Value *> args_;
        args_.push_back(lhs);
        args_.push_back(rhs);
        args_.push_back(Builder::constantPoolSexp(call));
        args_.push_back(rho);
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GenericDiv>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GenericDiv);
        return ins;
    }

    static char const * intrinsicName() {
        return "genericDiv";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP, t::SEXP, t::SEXP }, false);
    }

};

class GenericPow: public Intrinsic {
public:
    llvm::Value * lhs() { return getValue(0); }

    llvm::Value * rhs() { return getValue(1); }

    SEXP call() { return getValueSEXP(2); }

    llvm::Value * rho() { return getValue(3); }

    GenericPow(llvm::CallInst * ins):
        Intrinsic(ins) {
    }

    static GenericPow create(Builder & b, llvm::Value * lhs, llvm::Value * rhs, SEXP call, llvm::Value * rho) {
        std::vector<llvm::Value *> args_;
        args_.push_back(lhs);
        args_.push_back(rhs);
        args_.push_back(Builder::constantPoolSexp(call));
        args_.push_back(rho);
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GenericPow>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GenericPow);
        return ins;
    }

    static char const * intrinsicName() {
        return "genericPow";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP, t::SEXP, t::SEXP }, false);
    }

};

class GenericSqrt: public Intrinsic {
public:
    llvm::Value * op() { return getValue(0); }

    SEXP call() { return getValueSEXP(1); }

    llvm::Value * rho() { return getValue(2); }

    GenericSqrt(llvm::CallInst * ins):
        Intrinsic(ins) {
    }

    static GenericSqrt create(Builder & b, llvm::Value * op, SEXP call, llvm::Value * rho) {
        std::vector<llvm::Value *> args_;
        args_.push_back(op);
        args_.push_back(Builder::constantPoolSexp(call));
        args_.push_back(rho);
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GenericSqrt>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GenericSqrt);
        return ins;
    }

    static char const * intrinsicName() {
        return "genericSqrt";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP, t::SEXP }, false);
    }

};

class GenericExp: public Intrinsic {
public:
    llvm::Value * op() { return getValue(0); }

    SEXP call() { return getValueSEXP(1); }

    llvm::Value * rho() { return getValue(2); }

    GenericExp(llvm::CallInst * ins):
        Intrinsic(ins) {
    }

    static GenericExp create(Builder & b, llvm::Value * op, SEXP call, llvm::Value * rho) {
        std::vector<llvm::Value *> args_;
        args_.push_back(op);
        args_.push_back(Builder::constantPoolSexp(call));
        args_.push_back(rho);
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GenericExp>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GenericExp);
        return ins;
    }

    static char const * intrinsicName() {
        return "genericExp";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP, t::SEXP }, false);
    }

};

class GenericEq: public Intrinsic {
public:
    llvm::Value * lhs() { return getValue(0); }

    llvm::Value * rhs() { return getValue(1); }

    SEXP call() { return getValueSEXP(2); }

    llvm::Value * rho() { return getValue(3); }

    GenericEq(llvm::CallInst * ins):
        Intrinsic(ins) {
    }

    static GenericEq create(Builder & b, llvm::Value * lhs, llvm::Value * rhs, SEXP call, llvm::Value * rho) {
        std::vector<llvm::Value *> args_;
        args_.push_back(lhs);
        args_.push_back(rhs);
        args_.push_back(Builder::constantPoolSexp(call));
        args_.push_back(rho);
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GenericEq>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GenericEq);
        return ins;
    }

    static char const * intrinsicName() {
        return "genericEq";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP, t::SEXP, t::SEXP }, false);
    }

};

class GenericNe: public Intrinsic {
public:
    llvm::Value * lhs() { return getValue(0); }

    llvm::Value * rhs() { return getValue(1); }

    SEXP call() { return getValueSEXP(2); }

    llvm::Value * rho() { return getValue(3); }

    GenericNe(llvm::CallInst * ins):
        Intrinsic(ins) {
    }

    static GenericNe create(Builder & b, llvm::Value * lhs, llvm::Value * rhs, SEXP call, llvm::Value * rho) {
        std::vector<llvm::Value *> args_;
        args_.push_back(lhs);
        args_.push_back(rhs);
        args_.push_back(Builder::constantPoolSexp(call));
        args_.push_back(rho);
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GenericNe>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GenericNe);
        return ins;
    }

    static char const * intrinsicName() {
        return "genericNe";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP, t::SEXP, t::SEXP }, false);
    }

};

class GenericLt: public Intrinsic {
public:
    llvm::Value * lhs() { return getValue(0); }

    llvm::Value * rhs() { return getValue(1); }

    SEXP call() { return getValueSEXP(2); }

    llvm::Value * rho() { return getValue(3); }

    GenericLt(llvm::CallInst * ins):
        Intrinsic(ins) {
    }

    static GenericLt create(Builder & b, llvm::Value * lhs, llvm::Value * rhs, SEXP call, llvm::Value * rho) {
        std::vector<llvm::Value *> args_;
        args_.push_back(lhs);
        args_.push_back(rhs);
        args_.push_back(Builder::constantPoolSexp(call));
        args_.push_back(rho);
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GenericLt>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GenericLt);
        return ins;
    }

    static char const * intrinsicName() {
        return "genericLt";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP, t::SEXP, t::SEXP }, false);
    }

};

class GenericLe: public Intrinsic {
public:
    llvm::Value * lhs() { return getValue(0); }

    llvm::Value * rhs() { return getValue(1); }

    SEXP call() { return getValueSEXP(2); }

    llvm::Value * rho() { return getValue(3); }

    GenericLe(llvm::CallInst * ins):
        Intrinsic(ins) {
    }

    static GenericLe create(Builder & b, llvm::Value * lhs, llvm::Value * rhs, SEXP call, llvm::Value * rho) {
        std::vector<llvm::Value *> args_;
        args_.push_back(lhs);
        args_.push_back(rhs);
        args_.push_back(Builder::constantPoolSexp(call));
        args_.push_back(rho);
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GenericLe>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GenericLe);
        return ins;
    }

    static char const * intrinsicName() {
        return "genericLe";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP, t::SEXP, t::SEXP }, false);
    }

};

class GenericGe: public Intrinsic {
public:
    llvm::Value * lhs() { return getValue(0); }

    llvm::Value * rhs() { return getValue(1); }

    SEXP call() { return getValueSEXP(2); }

    llvm::Value * rho() { return getValue(3); }

    GenericGe(llvm::CallInst * ins):
        Intrinsic(ins) {
    }

    static GenericGe create(Builder & b, llvm::Value * lhs, llvm::Value * rhs, SEXP call, llvm::Value * rho) {
        std::vector<llvm::Value *> args_;
        args_.push_back(lhs);
        args_.push_back(rhs);
        args_.push_back(Builder::constantPoolSexp(call));
        args_.push_back(rho);
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GenericGe>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GenericGe);
        return ins;
    }

    static char const * intrinsicName() {
        return "genericGe";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP, t::SEXP, t::SEXP }, false);
    }

};

class GenericGt: public Intrinsic {
public:
    llvm::Value * lhs() { return getValue(0); }

    llvm::Value * rhs() { return getValue(1); }

    SEXP call() { return getValueSEXP(2); }

    llvm::Value * rho() { return getValue(3); }

    GenericGt(llvm::CallInst * ins):
        Intrinsic(ins) {
    }

    static GenericGt create(Builder & b, llvm::Value * lhs, llvm::Value * rhs, SEXP call, llvm::Value * rho) {
        std::vector<llvm::Value *> args_;
        args_.push_back(lhs);
        args_.push_back(rhs);
        args_.push_back(Builder::constantPoolSexp(call));
        args_.push_back(rho);
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GenericGt>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GenericGt);
        return ins;
    }

    static char const * intrinsicName() {
        return "genericGt";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP, t::SEXP, t::SEXP }, false);
    }

};

class GenericBitAnd: public Intrinsic {
public:
    llvm::Value * lhs() { return getValue(0); }

    llvm::Value * rhs() { return getValue(1); }

    SEXP call() { return getValueSEXP(2); }

    llvm::Value * rho() { return getValue(3); }

    GenericBitAnd(llvm::CallInst * ins):
        Intrinsic(ins) {
    }

    static GenericBitAnd create(Builder & b, llvm::Value * lhs, llvm::Value * rhs, SEXP call, llvm::Value * rho) {
        std::vector<llvm::Value *> args_;
        args_.push_back(lhs);
        args_.push_back(rhs);
        args_.push_back(Builder::constantPoolSexp(call));
        args_.push_back(rho);
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GenericBitAnd>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GenericBitAnd);
        return ins;
    }

    static char const * intrinsicName() {
        return "genericBitAnd";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP, t::SEXP, t::SEXP }, false);
    }

};

class GenericBitOr: public Intrinsic {
public:
    llvm::Value * lhs() { return getValue(0); }

    llvm::Value * rhs() { return getValue(1); }

    SEXP call() { return getValueSEXP(2); }

    llvm::Value * rho() { return getValue(3); }

    GenericBitOr(llvm::CallInst * ins):
        Intrinsic(ins) {
    }

    static GenericBitOr create(Builder & b, llvm::Value * lhs, llvm::Value * rhs, SEXP call, llvm::Value * rho) {
        std::vector<llvm::Value *> args_;
        args_.push_back(lhs);
        args_.push_back(rhs);
        args_.push_back(Builder::constantPoolSexp(call));
        args_.push_back(rho);
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GenericBitOr>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GenericBitOr);
        return ins;
    }

    static char const * intrinsicName() {
        return "genericBitOr";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP, t::SEXP, t::SEXP }, false);
    }

};

class GenericNot: public Intrinsic {
public:
    llvm::Value * op() { return getValue(0); }

    SEXP call() { return getValueSEXP(1); }

    llvm::Value * rho() { return getValue(2); }

    GenericNot(llvm::CallInst * ins):
        Intrinsic(ins) {
    }

    static GenericNot create(Builder & b, llvm::Value * op, SEXP call, llvm::Value * rho) {
        std::vector<llvm::Value *> args_;
        args_.push_back(op);
        args_.push_back(Builder::constantPoolSexp(call));
        args_.push_back(rho);
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GenericNot>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GenericNot);
        return ins;
    }

    static char const * intrinsicName() {
        return "genericNot";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP, t::SEXP }, false);
    }

};

class GenericGetVarMissOK: public Intrinsic {
public:
    llvm::Value * symbol() { return getValue(0); }

    llvm::Value * rho() { return getValue(1); }

    GenericGetVarMissOK(llvm::CallInst * ins):
        Intrinsic(ins) {
    }

    static GenericGetVarMissOK create(Builder & b, llvm::Value * symbol, llvm::Value * rho) {
        std::vector<llvm::Value *> args_;
        args_.push_back(symbol);
        args_.push_back(rho);
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GenericGetVarMissOK>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GenericGetVarMissOK);
        return ins;
    }

    static char const * intrinsicName() {
        return "genericGetVarMissOK";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP }, false);
    }

};

class GenericGetEllipsisValueMissOK: public Intrinsic {
public:
    llvm::Value * symbol() { return getValue(0); }

    llvm::Value * rho() { return getValue(1); }

    GenericGetEllipsisValueMissOK(llvm::CallInst * ins):
        Intrinsic(ins) {
    }

    static GenericGetEllipsisValueMissOK create(Builder & b, llvm::Value * symbol, llvm::Value * rho) {
        std::vector<llvm::Value *> args_;
        args_.push_back(symbol);
        args_.push_back(rho);
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<GenericGetEllipsisValueMissOK>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::GenericGetEllipsisValueMissOK);
        return ins;
    }

    static char const * intrinsicName() {
        return "genericGetEllipsisValueMissOK";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::SEXP, { t::SEXP, t::SEXP }, false);
    }

};

class CheckSwitchControl: public Intrinsic {
public:
    llvm::Value * ctrl() { return getValue(0); }

    SEXP call() { return getValueSEXP(1); }

    CheckSwitchControl(llvm::CallInst * ins):
        Intrinsic(ins) {
    }

    static CheckSwitchControl create(Builder & b, llvm::Value * ctrl, SEXP call) {
        std::vector<llvm::Value *> args_;
        args_.push_back(ctrl);
        args_.push_back(Builder::constantPoolSexp(call));
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<CheckSwitchControl>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::CheckSwitchControl);
        return ins;
    }

    static char const * intrinsicName() {
        return "checkSwitchControl";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::Void, { t::SEXP, t::SEXP }, false);
    }

};

class SwitchControlCharacter: public Intrinsic {
public:
    llvm::Value * ctrl() { return getValue(0); }

    SEXP call() { return getValueSEXP(1); }

    SEXP cases() { return getValueSEXP(2); }

    SwitchControlCharacter(llvm::CallInst * ins):
        Intrinsic(ins) {
    }

    static SwitchControlCharacter create(Builder & b, llvm::Value * ctrl, SEXP call, SEXP cases) {
        std::vector<llvm::Value *> args_;
        args_.push_back(ctrl);
        args_.push_back(Builder::constantPoolSexp(call));
        args_.push_back(Builder::constantPoolSexp(cases));
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<SwitchControlCharacter>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::SwitchControlCharacter);
        return ins;
    }

    static char const * intrinsicName() {
        return "switchControlCharacter";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::Int, { t::SEXP, t::SEXP, t::SEXP }, false);
    }

};

class SwitchControlInteger: public Intrinsic {
public:
    llvm::Value * ctrl() { return getValue(0); }

    int numCases() { return getValueInt(1); }

    SwitchControlInteger(llvm::CallInst * ins):
        Intrinsic(ins) {
    }

    static SwitchControlInteger create(Builder & b, llvm::Value * ctrl, int numCases) {
        std::vector<llvm::Value *> args_;
        args_.push_back(ctrl);
        args_.push_back(Builder::integer(numCases));
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<SwitchControlInteger>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::SwitchControlInteger);
        return ins;
    }

    static char const * intrinsicName() {
        return "switchControlInteger";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::Int, { t::SEXP, t::Int }, false);
    }

};

class ReturnJump: public Intrinsic {
public:
    llvm::Value * value() { return getValue(0); }

    llvm::Value * rho() { return getValue(1); }

    ReturnJump(llvm::CallInst * ins):
        Intrinsic(ins) {
    }

    static ReturnJump create(Builder & b, llvm::Value * value, llvm::Value * rho) {
        std::vector<llvm::Value *> args_;
        args_.push_back(value);
        args_.push_back(rho);
        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<ReturnJump>(), args_, "", b);
        b.insertCall(ins);
        setIRType(ins, ::rjit::ir::Type::ReturnJump);
        return ins;
    }

    static char const * intrinsicName() {
        return "returnJump";
    }

    static llvm::FunctionType * intrinsicType() { 
        return llvm::FunctionType::get(t::Void, { t::SEXP, t::SEXP }, false);
    }

};
} // namespace ir
} // namespace rjit
#endif // INTRINSICS_H_

