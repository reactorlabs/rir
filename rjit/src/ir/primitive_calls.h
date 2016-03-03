#ifndef INTRINSICS_H_
#define INTRINSICS_H_

#include <iostream>

#include "Ir.h"
#include "Builder.h"
#include "Properties.h"

#include "ir/IrScalars.h"

namespace rjit {
namespace ir {

class EndClosureContext : public PrimitiveCall {
  public:
    llvm::Value* cntxt() { return getValue(0); }
    llvm::Value* resul() { return getValue(1); }

    EndClosureContext(llvm::Instruction* ins)
        : PrimitiveCall(ins, Kind::EndClosureContext) {}

    static EndClosureContext* create(Builder& b, ir::Value cntxt,
                                     ir::Value result) {
        Sentinel s(b);
        return insertBefore(s, cntxt, result);
    }

    static EndClosureContext* insertBefore(llvm::Instruction* ins,
                                           ir::Value cntxt, ir::Value result) {
        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<EndClosureContext>(ins->getModule()),
            arguments(cntxt, result), "", ins);
        Builder::markSafepoint(i);
        return new EndClosureContext(i);
    }

    static EndClosureContext* insertBefore(Pattern* p, ir::Value context,
                                           ir::Value result) {
        return insertBefore(p->first(), context, result);
    }

    static char const* intrinsicName() { return "endClosureContext"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(t::Void, {t::cntxtPtr, t::SEXP}, false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::EndClosureContext;
    }

  private:
    static std::vector<llvm::Value*> arguments(llvm::Value* cntxt,
                                               llvm::Value* result) {
        std::vector<llvm::Value*> args_;
        args_.push_back(cntxt);
        args_.push_back(result);
        return args_;
    }
};

class ClosureQuickArgumentAdaptor : public PrimitiveCall {
  public:
    llvm::Value* op() { return getValue(0); }
    llvm::Value* arglis() { return getValue(1); }

    ClosureQuickArgumentAdaptor(llvm::Instruction* ins)
        : PrimitiveCall(ins, Kind::ClosureQuickArgumentAdaptor) {}

    static ClosureQuickArgumentAdaptor* create(Builder& b, ir::Value op,
                                               ir::Value arglist) {
        Sentinel s(b);
        return insertBefore(s, op, arglist);
    }

    static ClosureQuickArgumentAdaptor*
    insertBefore(llvm::Instruction* ins, ir::Value op, ir::Value arglis) {

        std::vector<llvm::Value*> args_;
        args_.push_back(op);
        args_.push_back(arglis);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<ClosureQuickArgumentAdaptor>(ins->getModule()),
            args_, "", ins);

        Builder::markSafepoint(i);
        ClosureQuickArgumentAdaptor* result =
            new ClosureQuickArgumentAdaptor(i);
        return result;
    }

    static char const* intrinsicName() { return "closureQuickArgumentAdaptor"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(t::SEXP, {t::SEXP, t::SEXP}, false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::ClosureQuickArgumentAdaptor;
    }
};

class CallNative : public PrimitiveCall {
  public:
    llvm::Value* native() { return getValue(0); }
    llvm::Value* rho() { return getValue(1); }

    CallNative(llvm::Instruction* ins) : PrimitiveCall(ins, Kind::CallNative) {}

    static CallNative* create(Builder& b, ir::Value native, ir::Value rho,
                              ir::Value closure) {
        Sentinel s(b);
        return insertBefore(s, native, rho, closure);
    }

    static CallNative* insertBefore(llvm::Instruction* ins, ir::Value native,
                                    ir::Value rho, ir::Value closure) {

        std::vector<llvm::Value*> args_;
        args_.push_back(native);
        args_.push_back(rho);
        args_.push_back(closure);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<CallNative>(ins->getModule()), args_, "", ins);

        Builder::markSafepoint(i);
        CallNative* result = new CallNative(i);
        return result;
    }

    static char const* intrinsicName() { return "callNative"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(t::SEXP, {t::SEXP, t::SEXP, t::SEXP},
                                       false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::CallNative;
    }
};

class ClosureNativeCallTrampoline : public PrimitiveCall {
  public:
    llvm::Value* cntxt() { return getValue(0); }
    llvm::Value* native() { return getValue(1); }
    llvm::Value* rh() { return getValue(2); }
    llvm::Value* closure() { return getValue(3); }

    ClosureNativeCallTrampoline(llvm::Instruction* ins)
        : PrimitiveCall(ins, Kind::ClosureNativeCallTrampoline) {}

    static ClosureNativeCallTrampoline* create(Builder& b, ir::Value cntxt,
                                               ir::Value native, ir::Value rh,
                                               ir::Value closure) {

        Sentinel s(b);
        return insertBefore(s, cntxt, native, rh, closure);
    }

    static ClosureNativeCallTrampoline*
    insertBefore(llvm::Instruction* ins, ir::Value cntxt, ir::Value native,
                 ir::Value rh, ir::Value closure) {

        std::vector<llvm::Value*> args_;
        args_.push_back(cntxt);
        args_.push_back(native);
        args_.push_back(rh);
        args_.push_back(closure);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<ClosureNativeCallTrampoline>(ins->getModule()),
            args_, "", ins);

        return new ClosureNativeCallTrampoline(i);
    }

    static char const* intrinsicName() { return "closureNativeCallTrampoline"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(
            t::SEXP, {t::cntxtPtr, t::SEXP, t::SEXP, t::SEXP}, false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::ClosureNativeCallTrampoline;
    }
};

// Replacement for GETSTACK_LOGICAL_NO_NA_PTR The call is used only for
// error reporting.
class ConvertToLogicalNoNA : public PrimitiveCall {
  public:
    llvm::Value* what() { return getValue(0); }
    llvm::Value* constantPool() { return getValue(1); }

    int call() { return getValueInt(2); }
    SEXP callValue() {
        llvm::Function* f = ins()->getParent()->getParent();
        JITModule* m = static_cast<JITModule*>(f->getParent());
        return VECTOR_ELT(m->constPool(f), call());
    }
    SEXP call(Builder const& b) { return b.constantPool(call()); }

    ConvertToLogicalNoNA(llvm::Instruction* ins)
        : PrimitiveCall(ins, Kind::ConvertToLogicalNoNA) {}

    static ConvertToLogicalNoNA* create(Builder& b, ir::Value what, SEXP call) {
        Sentinel s(b);
        return insertBefore(s, what, b.consts(),
                            Builder::integer(b.constantPoolIndex(call)));
    }

    static ConvertToLogicalNoNA* insertBefore(llvm::Instruction* ins,
                                              ir::Value what,
                                              ir::Value constantPool,
                                              ir::Value call) {

        std::vector<llvm::Value*> args_;
        args_.push_back(what);
        args_.push_back(constantPool);
        args_.push_back(call);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<ConvertToLogicalNoNA>(ins->getModule()), args_,
            "", ins);

        Builder::markSafepoint(i);
        return new ConvertToLogicalNoNA(i);
    }

    static ConvertToLogicalNoNA* insertBefore(Pattern* p, ir::Value what,
                                              ir::Value constantPool,
                                              ir::Value call) {
        return insertBefore(p->first(), what, constantPool, call);
    }

    static char const* intrinsicName() { return "convertToLogicalNoNA"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(t::Int, {t::SEXP, t::SEXP, t::Int},
                                       false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::ConvertToLogicalNoNA;
    }
};

class PrintValue : public PrimitiveCall {
  public:
    llvm::Value* value() { return getValue(0); }

    PrintValue(llvm::Instruction* ins) : PrimitiveCall(ins, Kind::PrintValue) {}

    static PrintValue* create(Builder& b, ir::Value value) {
        Sentinel s(b);
        return insertBefore(s, value);
    }

    static PrintValue* insertBefore(llvm::Instruction* ins, ir::Value value) {

        std::vector<llvm::Value*> args_;
        args_.push_back(value);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<PrintValue>(ins->getModule()), args_, "", ins);

        Builder::markSafepoint(i);
        return new PrintValue(i);
    }

    static PrintValue* insertBefore(Pattern* p, ir::Value value) {
        return insertBefore(p->first(), value);
    }

    static char const* intrinsicName() { return "printValue"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(t::Void, {t::SEXP}, false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::PrintValue;
    }
};

// startFor returns the sequence over which the loop will iterate. No
// need for all the other things here because we do not support other
// than generic variable loads and stores.
class StartFor : public PrimitiveCall {
  public:
    llvm::Value* seq() { return getValue(0); }
    llvm::Value* rho() { return getValue(1); }

    StartFor(llvm::Instruction* ins) : PrimitiveCall(ins, Kind::StartFor) {}

    static StartFor* create(Builder& b, ir::Value seq, ir::Value rho) {
        Sentinel s(b);
        return insertBefore(s, seq, rho);
    }

    static StartFor* insertBefore(llvm::Instruction* ins, ir::Value seq,
                                  ir::Value rho) {

        std::vector<llvm::Value*> args_;
        args_.push_back(seq);
        args_.push_back(rho);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<StartFor>(ins->getModule()), args_, "", ins);

        Builder::markSafepoint(i);
        return new StartFor(i);
    }

    static StartFor* insertBefore(Pattern* p, ir::Value seq, ir::Value rho) {
        return insertBefore(p->first(), seq, rho);
    }

    static char const* intrinsicName() { return "startFor"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(t::SEXP, {t::SEXP, t::SEXP}, false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::StartFor;
    }
};

// Loop sequence length returns the length of the sequence the loop will
// iterate over and errors if the sequence is of wrong type.
class LoopSequenceLength : public PrimitiveCall {
  public:
    llvm::Value* seq() { return getValue(0); }
    llvm::Value* constantPool() { return getValue(1); }

    int call() { return getValueInt(2); }
    SEXP callValue() {
        llvm::Function* f = ins()->getParent()->getParent();
        JITModule* m = static_cast<JITModule*>(f->getParent());
        return VECTOR_ELT(m->constPool(f), call());
    }
    SEXP call(Builder const& b) { return b.constantPool(call()); }

    LoopSequenceLength(llvm::Instruction* ins)
        : PrimitiveCall(ins, Kind::LoopSequenceLength) {}

    static LoopSequenceLength* create(Builder& b, ir::Value seq, SEXP call) {
        Sentinel s(b);
        return insertBefore(s, seq, b.consts(),
                            Builder::integer(b.constantPoolIndex(call)));
    }

    static LoopSequenceLength* insertBefore(llvm::Instruction* ins,
                                            ir::Value seq,
                                            ir::Value constantPool,
                                            ir::Value call) {

        std::vector<llvm::Value*> args_;
        args_.push_back(seq);
        args_.push_back(constantPool);
        args_.push_back(call);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<LoopSequenceLength>(ins->getModule()), args_, "",
            ins);

        Builder::markSafepoint(i);
        return new LoopSequenceLength(i);
    }

    static LoopSequenceLength* insertBefore(Pattern* p, ir::Value seq,
                                            ir::Value constantPool,
                                            ir::Value call) {
        return insertBefore(p->first(), seq, constantPool, call);
    }

    static char const* intrinsicName() { return "loopSequenceLength"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(t::Int, {t::SEXP, t::SEXP, t::Int},
                                       false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::LoopSequenceLength;
    }
};

// Given the for loop sequence, and index, returns the index-th value of
// the sequence.
class GetForLoopValue : public PrimitiveCall {
  public:
    llvm::Value* seq() { return getValue(0); }
    llvm::Value* index() { return getValue(1); }
    llvm::Value* store() { return getValue(2); }

    GetForLoopValue(llvm::Instruction* ins)
        : PrimitiveCall(ins, Kind::GetForLoopValue) {}

    static GetForLoopValue* create(Builder& b, ir::Value seq, ir::Value index,
                                   ir::Value store) {
        Sentinel s(b);
        return insertBefore(s, seq, index, store);
    }

    static GetForLoopValue* insertBefore(llvm::Instruction* ins, ir::Value seq,
                                         ir::Value index, ir::Value store) {

        std::vector<llvm::Value*> args_;
        args_.push_back(seq);
        args_.push_back(index);
        args_.push_back(store);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<GetForLoopValue>(ins->getModule()), args_, "",
            ins);

        Builder::markSafepoint(i);
        return new GetForLoopValue(i);
    }

    static GetForLoopValue* insertBefore(Pattern* p, ir::Value seq,
                                         ir::Value index, ir::Value store) {
        return insertBefore(p->first(), seq, index, store);
    }

    static char const* intrinsicName() { return "getForLoopValue"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(t::SEXP, {t::SEXP, t::Int, t::SEXP},
                                       false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::GetForLoopValue;
    }
};

/** Read and retrieves the value of a vector index for single bracket.
*/
class GetDispatchValue : public PrimitiveCall {
  public:
    llvm::Value* vec() { return getValue(0); }
    llvm::Value* index() { return getValue(1); }
    llvm::Value* rho() { return getValue(2); }
    llvm::Value* constantPool() { return getValue(3); }

    int call() { return getValueInt(4); }
    SEXP callValue() {
        llvm::Function* f = ins()->getParent()->getParent();
        JITModule* m = static_cast<JITModule*>(f->getParent());
        return VECTOR_ELT(m->constPool(f), call());
    }
    SEXP call(Builder const& b) { return b.constantPool(call()); }

    GetDispatchValue(llvm::Instruction* ins)
        : PrimitiveCall(ins, Kind::GetDispatchValue) {}

    static GetDispatchValue* create(Builder& b, ir::Value vec, ir::Value index,
                                    ir::Value rho, SEXP call) {
        Sentinel s(b);
        return insertBefore(s, vec, index, rho, b.consts(),
                            Builder::integer(b.constantPoolIndex(call)));
    }

    static GetDispatchValue* insertBefore(llvm::Instruction* ins, ir::Value vec,
                                          ir::Value index, ir::Value rho,
                                          ir::Value constantPool,
                                          ir::Value call) {

        std::vector<llvm::Value*> args_;
        args_.push_back(vec);
        args_.push_back(index);
        args_.push_back(rho);
        args_.push_back(constantPool);
        args_.push_back(call);
        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<GetDispatchValue>(ins->getModule()), args_, "",
            ins);

        Builder::markSafepoint(i);
        return new GetDispatchValue(i);
    }

    static GetDispatchValue* insertBefore(Pattern* p, ir::Value vec,
                                          ir::Value index, ir::Value rho,
                                          ir::Value constantPool,
                                          ir::Value call) {

        return insertBefore(p->first(), vec, index, rho, constantPool, call);
    }

    static char const* intrinsicName() { return "getDispatchValue"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(
            t::SEXP, {t::SEXP, t::SEXP, t::SEXP, t::SEXP, t::Int}, false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::GetDispatchValue;
    }
};

/** Read and retrieves the value of a vector index for double bracket.
 */

class GetDispatchValue2 : public PrimitiveCall {
  public:
    llvm::Value* vec() { return getValue(0); }
    llvm::Value* index() { return getValue(1); }
    llvm::Value* rho() { return getValue(2); }
    llvm::Value* constantPool() { return getValue(3); }

    int call() { return getValueInt(4); }
    SEXP callValue() {
        llvm::Function* f = ins()->getParent()->getParent();
        JITModule* m = static_cast<JITModule*>(f->getParent());
        return VECTOR_ELT(m->constPool(f), call());
    }
    SEXP call(Builder const& b) { return b.constantPool(call()); }

    GetDispatchValue2(llvm::Instruction* ins)
        : PrimitiveCall(ins, Kind::GetDispatchValue2) {}

    static GetDispatchValue2* create(Builder& b, ir::Value vec, ir::Value index,
                                     ir::Value rho, SEXP call) {
        Sentinel s(b);
        return insertBefore(s, vec, index, rho, b.consts(),
                            Builder::integer(b.constantPoolIndex(call)));
    }

    static GetDispatchValue2*
    insertBefore(llvm::Instruction* ins, ir::Value vec, ir::Value index,
                 ir::Value rho, ir::Value constantPool, ir::Value call) {

        std::vector<llvm::Value*> args_;
        args_.push_back(vec);
        args_.push_back(index);
        args_.push_back(rho);
        args_.push_back(constantPool);
        args_.push_back(call);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<GetDispatchValue2>(ins->getModule()), args_, "",
            ins);

        Builder::markSafepoint(i);
        return new GetDispatchValue2(i);
    }

    static GetDispatchValue2* insertBefore(Pattern* p, ir::Value vec,
                                           ir::Value index, ir::Value rho,
                                           ir::Value constantPool,
                                           ir::Value call) {

        return insertBefore(p->first(), vec, index, rho, constantPool, call);
    }

    static char const* intrinsicName() { return "getDispatchValue2"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(
            t::SEXP, {t::SEXP, t::SEXP, t::SEXP, t::SEXP, t::Int}, false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::GetDispatchValue2;
    }
};

/** Assign a value into the vector for single bracket.
 *
 */

class AssignDispatchValue : public PrimitiveCall {
  public:
    // llvm::Value* lhs() { return getValue(0); }
    llvm::Value* vector() { return getValue(0); }
    llvm::Value* index() { return getValue(1); }
    llvm::Value* val() { return getValue(2); }

    llvm::Value* rho() { return getValue(3); }
    llvm::Value* constantPool() { return getValue(4); }

    int call() { return getValueInt(5); }
    SEXP callValue() {
        llvm::Function* f = ins()->getParent()->getParent();
        JITModule* m = static_cast<JITModule*>(f->getParent());
        return VECTOR_ELT(m->constPool(f), call());
    }
    SEXP call(Builder const& b) { return b.constantPool(call()); }

    AssignDispatchValue(llvm::Instruction* ins)
        : PrimitiveCall(ins, Kind::AssignDispatchValue) {}

    static AssignDispatchValue* create(Builder& b, ir::Value vector,
                                       ir::Value index, ir::Value val,
                                       ir::Value rho, SEXP call) {
        Sentinel s(b);
        return insertBefore(s, vector, index, val, rho, b.consts(),
                            Builder::integer(b.constantPoolIndex(call)));
    }

    static AssignDispatchValue* insertBefore(llvm::Instruction* ins,
                                             ir::Value vector, ir::Value index,
                                             ir::Value val, ir::Value rho,
                                             ir::Value constantPool,
                                             ir::Value call) {

        std::vector<llvm::Value*> args_;
        // args_.push_back(lhs);
        args_.push_back(vector);
        args_.push_back(index);
        args_.push_back(val);
        args_.push_back(rho);
        args_.push_back(constantPool);
        args_.push_back(call);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<AssignDispatchValue>(ins->getModule()), args_, "",
            ins);

        Builder::markSafepoint(i);
        return new AssignDispatchValue(i);
    }

    static AssignDispatchValue*
    insertBefore(Pattern* p, ir::Value vector, ir::Value index, ir::Value val,
                 ir::Value rho, ir::Value constantPool, ir::Value call) {

        return insertBefore(p->first(), vector, index, val, rho, constantPool,
                            call);
    }

    static char const* intrinsicName() { return "assignDispatchValue"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(
            t::SEXP, {t::SEXP, t::SEXP, t::SEXP, t::SEXP, t::SEXP, t::Int},
            false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::AssignDispatchValue;
    }
};

/** Assign a value into the vector for single bracket.
 *
 */

class AssignDispatchValue2 : public PrimitiveCall {
  public:
    // llvm::Value* lhs() { return getValue(0); }
    llvm::Value* vector() { return getValue(0); }
    llvm::Value* index() { return getValue(1); }
    llvm::Value* val() { return getValue(2); }

    llvm::Value* rho() { return getValue(3); }
    llvm::Value* constantPool() { return getValue(4); }

    int call() { return getValueInt(5); }
    SEXP callValue() {
        llvm::Function* f = ins()->getParent()->getParent();
        JITModule* m = static_cast<JITModule*>(f->getParent());
        return VECTOR_ELT(m->constPool(f), call());
    }
    SEXP call(Builder const& b) { return b.constantPool(call()); }

    AssignDispatchValue2(llvm::Instruction* ins)
        : PrimitiveCall(ins, Kind::AssignDispatchValue2) {}

    static AssignDispatchValue2* create(Builder& b, ir::Value vector,
                                        ir::Value index, ir::Value val,
                                        ir::Value rho, SEXP call) {
        Sentinel s(b);
        return insertBefore(s, vector, index, val, rho, b.consts(),
                            Builder::integer(b.constantPoolIndex(call)));
    }

    static AssignDispatchValue2* insertBefore(llvm::Instruction* ins,
                                              ir::Value vector, ir::Value index,
                                              ir::Value val, ir::Value rho,
                                              ir::Value constantPool,
                                              ir::Value call) {

        std::vector<llvm::Value*> args_;
        // args_.push_back(lhs);
        args_.push_back(vector);
        args_.push_back(index);
        args_.push_back(val);
        args_.push_back(rho);
        args_.push_back(constantPool);
        args_.push_back(call);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<AssignDispatchValue2>(ins->getModule()), args_,
            "", ins);

        Builder::markSafepoint(i);
        return new AssignDispatchValue2(i);
    }

    static AssignDispatchValue2*
    insertBefore(Pattern* p, ir::Value vector, ir::Value index, ir::Value val,
                 ir::Value rho, ir::Value constantPool, ir::Value call) {

        return insertBefore(p->first(), vector, index, val, rho, constantPool,
                            call);
    }

    static char const* intrinsicName() { return "assignDispatchValue2"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(
            t::SEXP, {t::SEXP, t::SEXP, t::SEXP, t::SEXP, t::SEXP, t::Int},
            false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::AssignDispatchValue2;
    }
};

class SuperAssignDispatch : public PrimitiveCall {
  public:
    // llvm::Value* lhs() { return getValue(0); }
    llvm::Value* vector() { return getValue(0); }
    llvm::Value* index() { return getValue(1); }
    llvm::Value* val() { return getValue(2); }

    llvm::Value* rho() { return getValue(3); }
    llvm::Value* constantPool() { return getValue(4); }

    int call() { return getValueInt(5); }
    SEXP callValue() {
        llvm::Function* f = ins()->getParent()->getParent();
        JITModule* m = static_cast<JITModule*>(f->getParent());
        return VECTOR_ELT(m->constPool(f), call());
    }
    SEXP call(Builder const& b) { return b.constantPool(call()); }

    SuperAssignDispatch(llvm::Instruction* ins)
        : PrimitiveCall(ins, Kind::SuperAssignDispatch) {}

    static SuperAssignDispatch* create(Builder& b, ir::Value vector,
                                       ir::Value index, ir::Value val,
                                       ir::Value rho, SEXP call) {
        Sentinel s(b);
        return insertBefore(s, vector, index, val, rho, b.consts(),
                            Builder::integer(b.constantPoolIndex(call)));
    }

    static SuperAssignDispatch* insertBefore(llvm::Instruction* ins,
                                             ir::Value vector, ir::Value index,
                                             ir::Value val, ir::Value rho,
                                             ir::Value constantPool,
                                             ir::Value call) {

        std::vector<llvm::Value*> args_;
        // args_.push_back(lhs);
        args_.push_back(vector);
        args_.push_back(index);
        args_.push_back(val);
        args_.push_back(rho);
        args_.push_back(constantPool);
        args_.push_back(call);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<SuperAssignDispatch>(ins->getModule()), args_, "",
            ins);

        Builder::markSafepoint(i);
        return new SuperAssignDispatch(i);
    }

    static SuperAssignDispatch*
    insertBefore(Pattern* p, ir::Value vector, ir::Value index, ir::Value val,
                 ir::Value rho, ir::Value constantPool, ir::Value call) {

        return insertBefore(p->first(), vector, index, val, rho, constantPool,
                            call);
    }

    static char const* intrinsicName() { return "superAssignDispatch"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(
            t::SEXP, {t::SEXP, t::SEXP, t::SEXP, t::SEXP, t::SEXP, t::Int},
            false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::SuperAssignDispatch;
    }
};

class SuperAssignDispatch2 : public PrimitiveCall {
  public:
    // llvm::Value* lhs() { return getValue(0); }
    llvm::Value* vector() { return getValue(0); }
    llvm::Value* index() { return getValue(1); }
    llvm::Value* val() { return getValue(2); }

    llvm::Value* rho() { return getValue(3); }
    llvm::Value* constantPool() { return getValue(4); }

    int call() { return getValueInt(5); }
    SEXP callValue() {
        llvm::Function* f = ins()->getParent()->getParent();
        JITModule* m = static_cast<JITModule*>(f->getParent());
        return VECTOR_ELT(m->constPool(f), call());
    }
    SEXP call(Builder const& b) { return b.constantPool(call()); }

    SuperAssignDispatch2(llvm::Instruction* ins)
        : PrimitiveCall(ins, Kind::SuperAssignDispatch2) {}

    static SuperAssignDispatch2* create(Builder& b, ir::Value vector,
                                        ir::Value index, ir::Value val,
                                        ir::Value rho, SEXP call) {
        Sentinel s(b);
        return insertBefore(s, vector, index, val, rho, b.consts(),
                            Builder::integer(b.constantPoolIndex(call)));
    }

    static SuperAssignDispatch2* insertBefore(llvm::Instruction* ins,
                                              ir::Value vector, ir::Value index,
                                              ir::Value val, ir::Value rho,
                                              ir::Value constantPool,
                                              ir::Value call) {

        std::vector<llvm::Value*> args_;
        // args_.push_back(lhs);
        args_.push_back(vector);
        args_.push_back(index);
        args_.push_back(val);
        args_.push_back(rho);
        args_.push_back(constantPool);
        args_.push_back(call);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<SuperAssignDispatch2>(ins->getModule()), args_,
            "", ins);

        Builder::markSafepoint(i);
        return new SuperAssignDispatch2(i);
    }

    static SuperAssignDispatch2*
    insertBefore(Pattern* p, ir::Value vector, ir::Value index, ir::Value val,
                 ir::Value rho, ir::Value constantPool, ir::Value call) {

        return insertBefore(p->first(), vector, index, val, rho, constantPool,
                            call);
    }

    static char const* intrinsicName() { return "superAssignDispatch2"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(
            t::SEXP, {t::SEXP, t::SEXP, t::SEXP, t::SEXP, t::SEXP, t::Int},
            false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::SuperAssignDispatch2;
    }
};

class MarkVisible : public PrimitiveCall {
  public:
    MarkVisible(llvm::Instruction* ins)
        : PrimitiveCall(ins, Kind::MarkVisible) {}

    static MarkVisible* create(Builder& b) {
        Sentinel s(b);
        return insertBefore(s);
    }

    static MarkVisible* insertBefore(llvm::Instruction* ins) {

        std::vector<llvm::Value*> args_;

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<MarkVisible>(ins->getModule()), args_, "", ins);

        Builder::markSafepoint(i);
        return new MarkVisible(i);
    }

    static MarkVisible* insertBefore(Pattern* p) {
        return insertBefore(p->first());
    }

    static char const* intrinsicName() { return "markVisible"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(t::Void, {

                                                }, false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::MarkVisible;
    }
};

class MarkInvisible : public PrimitiveCall {
  public:
    MarkInvisible(llvm::Instruction* ins)
        : PrimitiveCall(ins, Kind::MarkInvisible) {}

    static MarkInvisible* create(Builder& b) {
        Sentinel s(b);
        return insertBefore(s);
    }

    static MarkInvisible* insertBefore(llvm::Instruction* ins) {

        std::vector<llvm::Value*> args_;

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<MarkInvisible>(ins->getModule()), args_, "", ins);

        Builder::markSafepoint(i);
        return new MarkInvisible(i);
    }

    static MarkInvisible* insertBefore(Pattern* p) {
        return insertBefore(p->first());
    }

    static char const* intrinsicName() { return "markInvisible"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(t::Void, {

                                                }, false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::MarkInvisible;
    }
};

// When LLVM IR creates user visible constant, this function contains all
// the code required to make the constant. Currently this means taking
// the value from the constant pool and marking it as not mutable.
class UserLiteral : public PrimitiveCall {
  public:
    llvm::Value* constantPool() { return getValue(0); }

    int index() { return getValueInt(1); }

    SEXP indexValue() {
        llvm::Function* f = ins()->getParent()->getParent();
        JITModule* m = static_cast<JITModule*>(f->getParent());
        return VECTOR_ELT(m->constPool(f), index());
    }

    SEXP index(Builder const& b) { return b.constantPool(index()); }

    UserLiteral(llvm::Instruction* ins)
        : PrimitiveCall(ins, Kind::UserLiteral) {}

    static UserLiteral* create(Builder& b, SEXP index) {
        Sentinel s(b);
        return insertBefore(s, b.consts(),
                            Builder::integer(b.constantPoolIndex(index)));
    }

    static UserLiteral* insertBefore(llvm::Instruction* ins,
                                     ir::Value constantPool, ir::Value index) {

        std::vector<llvm::Value*> args_;
        args_.push_back(constantPool);
        args_.push_back(index);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<UserLiteral>(ins->getModule()), args_, "", ins);

        Builder::markSafepoint(i);
        return new UserLiteral(i);
    }

    static UserLiteral* insertBefore(Pattern* p, ir::Value constantPool,
                                     ir::Value index) {
        return insertBefore(p->first(), constantPool, index);
    }

    static char const* intrinsicName() { return "userLiteral"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(t::SEXP, {t::SEXP, t::Int}, false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::UserLiteral;
    }
};

class PatchIC : public PrimitiveCall {
  public:
    PatchIC(llvm::Instruction* ins) : PrimitiveCall(ins, Kind::PatchIC) {}

    static PatchIC* create(Builder& b, ir::Value addr, ir::Value stackmapId,
                           ir::Value caller) {
        Sentinel s(b);
        return insertBefore(s, addr, stackmapId, caller);
    }

    static PatchIC* insertBefore(llvm::Instruction* ins, ir::Value addr,
                                 ir::Value stackmapId, ir::Value caller) {

        std::vector<llvm::Value*> args_;
        args_.push_back(addr);
        args_.push_back(stackmapId);
        args_.push_back(caller);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<PatchIC>(ins->getModule()), args_, "", ins);

        Builder::markSafepoint(i);
        return new PatchIC(i);
    }

    static char const* intrinsicName() { return "patchIC"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(
            t::Void, {t::voidPtr, t::t_i64, t::nativeFunctionPtr_t}, false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::PatchIC;
    }
};

class CompileIC : public PrimitiveCall {
  public:
    CompileIC(llvm::Instruction* ins) : PrimitiveCall(ins, Kind::CompileIC) {}

    static CompileIC* create(Builder& b, ir::Value size, ir::Value call,
                             ir::Value fun, ir::Value rho,
                             ir::Value stackmapId) {
        Sentinel s(b);
        return insertBefore(s, size, call, fun, rho, stackmapId);
    }

    static CompileIC* insertBefore(llvm::Instruction* ins, ir::Value size,
                                   ir::Value call, ir::Value fun, ir::Value rho,
                                   ir::Value stackmapId) {
        std::vector<llvm::Value*> args_;
        args_.push_back(size);
        args_.push_back(call);
        args_.push_back(fun);
        args_.push_back(rho);
        args_.push_back(stackmapId);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<CompileIC>(ins->getModule()), args_, "", ins);

        Builder::markSafepoint(i);
        return new CompileIC(i);
    }

    static char const* intrinsicName() { return "compileIC"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(
            t::voidPtr, {t::t_i64, t::SEXP, t::SEXP, t::SEXP, t::t_i64}, false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::CompileIC;
    }
};

class InitClosureContext : public PrimitiveCall {
  public:
    InitClosureContext(llvm::Instruction* ins)
        : PrimitiveCall(ins, Kind::InitClosureContext) {}

    static InitClosureContext* create(Builder& b, ir::Value context,
                                      ir::Value call, ir::Value newrho,
                                      ir::Value rho, ir::Value actuals,
                                      ir::Value fun) {
        Sentinel s(b);
        return insertBefore(s, context, call, newrho, rho, actuals, fun);
    }

    static InitClosureContext* insertBefore(llvm::Instruction* ins,
                                            ir::Value context, ir::Value call,
                                            ir::Value newrho, ir::Value rho,
                                            ir::Value actuals, ir::Value fun) {

        std::vector<llvm::Value*> args_;
        args_.push_back(context);
        args_.push_back(call);
        args_.push_back(newrho);
        args_.push_back(rho);
        args_.push_back(actuals);
        args_.push_back(fun);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<InitClosureContext>(ins->getModule()), args_, "",
            ins);

        Builder::markSafepoint(i);
        return new InitClosureContext(i);
    }

    static char const* intrinsicName() { return "initClosureContext"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(
            t::Void, {t::cntxtPtr, t::SEXP, t::SEXP, t::SEXP, t::SEXP, t::SEXP},
            false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::InitClosureContext;
    }
};

// Call NewEnvironment
class NewEnv : public PrimitiveCall {
  public:
    NewEnv(llvm::Instruction* ins) : PrimitiveCall(ins, Kind::NewEnv) {}

    static NewEnv* create(Builder& b, ir::Value names, ir::Value values,
                          ir::Value parent) {
        Sentinel s(b);
        return insertBefore(s, names, values, parent);
    }

    static NewEnv* insertBefore(llvm::Instruction* ins, ir::Value names,
                                ir::Value values, ir::Value parent) {

        std::vector<llvm::Value*> args_;
        args_.push_back(names);
        args_.push_back(values);
        args_.push_back(parent);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<NewEnv>(ins->getModule()), args_, "", ins);

        Builder::markSafepoint(i);
        return new NewEnv(i);
    }

    static char const* intrinsicName() { return "Rf_NewEnvironment"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(t::SEXP, {t::SEXP, t::SEXP, t::SEXP},
                                       false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::NewEnv;
    }
};

// Call CONS_NR
class ConsNr : public PrimitiveCall {
  public:
    ConsNr(llvm::Instruction* ins) : PrimitiveCall(ins, Kind::ConsNr) {}

    static ConsNr* create(Builder& b, ir::Value car, ir::Value cdr) {
        Sentinel s(b);
        return insertBefore(s, car, cdr);
    }

    static ConsNr* insertBefore(llvm::Instruction* ins, ir::Value car,
                                ir::Value cdr) {
        std::vector<llvm::Value*> args_;
        args_.push_back(car);
        args_.push_back(cdr);
        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<ConsNr>(ins->getModule()), args_, "", ins);

        Builder::markSafepoint(i);
        return new ConsNr(i);
    }

    static char const* intrinsicName() { return "CONS_NR"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(t::SEXP, {t::SEXP, t::SEXP}, false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::ConsNr;
    }
};

// Just returns the index-th constant from the constant pool.
class Constant : public PrimitiveCall {
  public:
    llvm::Value* constantPool() { return getValue(0); }

    int index() { return getValueInt(1); }
    SEXP indexValue() {
        llvm::Function* f = ins()->getParent()->getParent();
        JITModule* m = static_cast<JITModule*>(f->getParent());
        return VECTOR_ELT(m->constPool(f), index());
    }
    SEXP index(Builder const& b) { return b.constantPool(index()); }

    Constant(llvm::Instruction* ins) : PrimitiveCall(ins, Kind::Constant) {}

    static Constant* create(Builder& b, SEXP index) {
        Sentinel s(b);
        return insertBefore(s, b.consts(),
                            Builder::integer(b.constantPoolIndex(index)));
    }

    static Constant* insertBefore(llvm::Instruction* ins,
                                  ir::Value constantPool, ir::Value index) {

        std::vector<llvm::Value*> args_;
        args_.push_back(constantPool);
        args_.push_back(index);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<Constant>(ins->getModule()), args_, "", ins);

        Builder::markSafepoint(i);
        return new Constant(i);
    }

    static Constant* insertBefore(Pattern* p, ir::Value constantPool,
                                  ir::Value index) {
        return insertBefore(p->first(), constantPool, index);
    }

    static char const* intrinsicName() { return "constant"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(t::SEXP, {t::SEXP, t::Int}, false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::Constant;
    }
};

// Generic getvar does not use any caches whatsoever. TODO this means we
// can get rid of the checks in getvar(), and reduce its code to this. We
// definitely want faster versions.
class GenericGetVar : public PrimitiveCall {
  public:
    llvm::Value* rho() { return getValue(0); }
    llvm::Value* constantPool() { return getValue(1); }

    int symbol() { return getValueInt(2); }
    SEXP symbolValue() {
        llvm::Function* f = ins()->getParent()->getParent();
        JITModule* m = static_cast<JITModule*>(f->getParent());
        return VECTOR_ELT(m->constPool(f), symbol());
    }
    SEXP symbol(Builder const& b) { return b.constantPool(symbol()); }

    GenericGetVar(llvm::Instruction* ins)
        : PrimitiveCall(ins, Kind::GenericGetVar) {}

    static GenericGetVar* create(Builder& b, ir::Value rho, SEXP symbol) {
        Sentinel s(b);
        return insertBefore(s, rho, b.consts(),
                            Builder::integer(b.constantPoolIndex(symbol)));
    }

    static GenericGetVar* insertBefore(llvm::Instruction* ins, ir::Value rho,
                                       ir::Value constantPool,
                                       ir::Value symbol) {

        std::vector<llvm::Value*> args_;
        args_.push_back(rho);
        args_.push_back(constantPool);
        args_.push_back(symbol);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<GenericGetVar>(ins->getModule()), args_, "", ins);

        Builder::markSafepoint(i);
        return new GenericGetVar(i);
    }

    static GenericGetVar* insertBefore(Pattern* p, ir::Value rho,
                                       ir::Value constantPool,
                                       ir::Value symbol) {
        return insertBefore(p->first(), rho, constantPool, symbol);
    }

    static char const* intrinsicName() { return "genericGetVar"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(t::SEXP, {t::SEXP, t::SEXP, t::Int},
                                       false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::GenericGetVar;
    }
};

class GenericGetEllipsisArg : public PrimitiveCall {
  public:
    llvm::Value* rho() { return getValue(0); }
    llvm::Value* constantPool() { return getValue(1); }

    int symbol() { return getValueInt(2); }
    SEXP symbolValue() {
        llvm::Function* f = ins()->getParent()->getParent();
        JITModule* m = static_cast<JITModule*>(f->getParent());
        return VECTOR_ELT(m->constPool(f), symbol());
    }
    SEXP symbol(Builder const& b) { return b.constantPool(symbol()); }

    GenericGetEllipsisArg(llvm::Instruction* ins)
        : PrimitiveCall(ins, Kind::GenericGetEllipsisArg) {}

    static GenericGetEllipsisArg* create(Builder& b, ir::Value rho,
                                         SEXP symbol) {
        Sentinel s(b);
        return insertBefore(s, rho, b.consts(),
                            Builder::integer(b.constantPoolIndex(symbol)));
    }

    static GenericGetEllipsisArg* insertBefore(llvm::Instruction* ins,
                                               ir::Value rho,
                                               ir::Value constantPool,
                                               ir::Value symbol) {

        std::vector<llvm::Value*> args_;
        args_.push_back(rho);
        args_.push_back(constantPool);
        args_.push_back(symbol);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<GenericGetEllipsisArg>(ins->getModule()), args_,
            "", ins);

        Builder::markSafepoint(i);
        return new GenericGetEllipsisArg(i);
    }

    static GenericGetEllipsisArg* insertBefore(Pattern* p, ir::Value rho,
                                               ir::Value constantPool,
                                               ir::Value symbol) {
        return insertBefore(p->first(), rho, constantPool, symbol);
    }

    static char const* intrinsicName() { return "genericGetEllipsisArg"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(t::SEXP, {t::SEXP, t::SEXP, t::Int},
                                       false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::GenericGetEllipsisArg;
    }
};

class GenericSetVar : public PrimitiveCall {
  public:
    llvm::Value* value() { return getValue(0); }
    llvm::Value* rho() { return getValue(1); }
    llvm::Value* constantPool() { return getValue(2); }

    int symbol() { return getValueInt(3); }
    SEXP symbolValue() {
        llvm::Function* f = ins()->getParent()->getParent();
        JITModule* m = static_cast<JITModule*>(f->getParent());
        return VECTOR_ELT(m->constPool(f), symbol());
    }
    SEXP symbol(Builder const& b) { return b.constantPool(symbol()); }

    GenericSetVar(llvm::Instruction* ins)
        : PrimitiveCall(ins, Kind::GenericSetVar) {}

    static GenericSetVar* create(Builder& b, ir::Value value, llvm::Value* rho,
                                 SEXP symbol) {
        Sentinel s(b);
        return insertBefore(s, value, rho, b.consts(),
                            Builder::integer(b.constantPoolIndex(symbol)));
    }

    static GenericSetVar* insertBefore(llvm::Instruction* ins, ir::Value value,
                                       ir::Value rho, ir::Value constantPool,
                                       ir::Value symbol) {

        std::vector<llvm::Value*> args_;
        args_.push_back(value);
        args_.push_back(rho);
        args_.push_back(constantPool);
        args_.push_back(symbol);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<GenericSetVar>(ins->getModule()), args_, "", ins);

        Builder::markSafepoint(i);
        return new GenericSetVar(i);
    }

    static GenericSetVar* insertBefore(Pattern* p, ir::Value value,
                                       ir::Value rho, ir::Value constantPool,
                                       ir::Value symbol) {
        return insertBefore(p->first(), value, rho, constantPool, symbol);
    }

    static char const* intrinsicName() { return "genericSetVar"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(
            t::Void, {t::SEXP, t::SEXP, t::SEXP, t::Int}, false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::GenericSetVar;
    }
};

class GenericSetVarParent : public PrimitiveCall {
  public:
    llvm::Value* value() { return getValue(0); }
    llvm::Value* rho() { return getValue(1); }
    llvm::Value* constantPool() { return getValue(2); }

    int symbol() { return getValueInt(3); }
    SEXP symbolValue() {
        llvm::Function* f = ins()->getParent()->getParent();
        JITModule* m = static_cast<JITModule*>(f->getParent());
        return VECTOR_ELT(m->constPool(f), symbol());
    }
    SEXP symbol(Builder const& b) { return b.constantPool(symbol()); }

    GenericSetVarParent(llvm::Instruction* ins)
        : PrimitiveCall(ins, Kind::GenericSetVarParent) {}

    static GenericSetVarParent* create(Builder& b, ir::Value value,
                                       llvm::Value* rho, SEXP symbol) {
        Sentinel s(b);
        return insertBefore(s, value, rho, b.consts(),
                            Builder::integer(b.constantPoolIndex(symbol)));
    }

    static GenericSetVarParent* insertBefore(llvm::Instruction* ins,
                                             ir::Value value, ir::Value rho,
                                             ir::Value constantPool,
                                             ir::Value symbol) {

        std::vector<llvm::Value*> args_;
        args_.push_back(value);
        args_.push_back(rho);
        args_.push_back(constantPool);
        args_.push_back(symbol);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<GenericSetVarParent>(ins->getModule()), args_, "",
            ins);

        Builder::markSafepoint(i);
        return new GenericSetVarParent(i);
    }

    static GenericSetVarParent* insertBefore(Pattern* p, ir::Value value,
                                             ir::Value rho,
                                             ir::Value constantPool,
                                             ir::Value symbol) {
        return insertBefore(p->first(), value, rho, constantPool, symbol);
    }

    static char const* intrinsicName() { return "genericSetVarParent"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(
            t::Void, {t::SEXP, t::SEXP, t::SEXP, t::Int}, false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::GenericSetVarParent;
    }
};

class GetFunction : public PrimitiveCall {
  public:
    llvm::Value* rho() { return getValue(0); }
    llvm::Value* constantPool() { return getValue(1); }

    int symbol() { return getValueInt(2); }
    SEXP symbolValue() {
        llvm::Function* f = ins()->getParent()->getParent();
        JITModule* m = static_cast<JITModule*>(f->getParent());
        return VECTOR_ELT(m->constPool(f), symbol());
    }
    SEXP symbol(Builder const& b) { return b.constantPool(symbol()); }

    GetFunction(llvm::Instruction* ins)
        : PrimitiveCall(ins, Kind::GetFunction) {}

    static GetFunction* create(Builder& b, ir::Value rho, SEXP symbol) {
        Sentinel s(b);
        return insertBefore(s, rho, b.consts(),
                            Builder::integer(b.constantPoolIndex(symbol)));
    }

    static GetFunction* insertBefore(llvm::Instruction* ins, ir::Value rho,
                                     ir::Value constantPool, ir::Value symbol) {

        std::vector<llvm::Value*> args_;
        args_.push_back(rho);
        args_.push_back(constantPool);
        args_.push_back(symbol);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<GetFunction>(ins->getModule()), args_, "", ins);

        Builder::markSafepoint(i);
        return new GetFunction(i);
    }

    static GetFunction* insertBefore(Pattern* p, ir::Value rho,
                                     ir::Value constantPool, ir::Value symbol) {
        return insertBefore(p->first(), rho, constantPool, symbol);
    }

    static char const* intrinsicName() { return "getFunction"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(t::SEXP, {t::SEXP, t::SEXP, t::Int},
                                       false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::GetFunction;
    }
};

class GetGlobalFunction : public PrimitiveCall {
  public:
    llvm::Value* constantPool() { return getValue(0); }

    int symbol() { return getValueInt(1); }
    SEXP symbolValue() {
        llvm::Function* f = ins()->getParent()->getParent();
        JITModule* m = static_cast<JITModule*>(f->getParent());
        return VECTOR_ELT(m->constPool(f), symbol());
    }
    SEXP symbol(Builder const& b) { return b.constantPool(symbol()); }

    GetGlobalFunction(llvm::Instruction* ins)
        : PrimitiveCall(ins, Kind::GetGlobalFunction) {}

    static GetGlobalFunction* create(Builder& b, SEXP symbol) {
        Sentinel s(b);
        return insertBefore(s, b.consts(),
                            Builder::integer(b.constantPoolIndex(symbol)));
    }

    static GetGlobalFunction* insertBefore(llvm::Instruction* ins,
                                           ir::Value constantPool,
                                           ir::Value symbol) {

        std::vector<llvm::Value*> args_;
        args_.push_back(constantPool);
        args_.push_back(symbol);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<GetGlobalFunction>(ins->getModule()), args_, "",
            ins);

        Builder::markSafepoint(i);
        return new GetGlobalFunction(i);
    }

    static GetGlobalFunction* insertBefore(Pattern* p, ir::Value constantPool,
                                           ir::Value symbol) {
        return insertBefore(p->first(), constantPool, symbol);
    }

    static char const* intrinsicName() { return "getGlobalFunction"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(t::SEXP, {t::SEXP, t::Int}, false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::GetGlobalFunction;
    }
};

class GetSymFunction : public PrimitiveCall {
  public:
    llvm::Value* constantPool() { return getValue(0); }

    int name() { return getValueInt(1); }
    SEXP nameValue() {
        llvm::Function* f = ins()->getParent()->getParent();
        JITModule* m = static_cast<JITModule*>(f->getParent());
        return VECTOR_ELT(m->constPool(f), name());
    }
    SEXP name(Builder const& b) { return b.constantPool(name()); }

    GetSymFunction(llvm::Instruction* ins)
        : PrimitiveCall(ins, Kind::GetSymFunction) {}

    static GetSymFunction* create(Builder& b, SEXP name) {
        Sentinel s(b);
        return insertBefore(s, b.consts(),
                            Builder::integer(b.constantPoolIndex(name)));
    }

    static GetSymFunction* insertBefore(llvm::Instruction* ins,
                                        ir::Value constantPool,
                                        ir::Value name) {

        std::vector<llvm::Value*> args_;
        args_.push_back(constantPool);
        args_.push_back(name);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<GetSymFunction>(ins->getModule()), args_, "",
            ins);

        Builder::markSafepoint(i);
        return new GetSymFunction(i);
    }

    static GetSymFunction* insertBefore(Pattern* p, ir::Value constantPool,
                                        ir::Value name) {
        return insertBefore(p->first(), constantPool, name);
    }

    static char const* intrinsicName() { return "getSymFunction"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(t::SEXP, {t::SEXP, t::Int}, false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::GetSymFunction;
    }
};

class GetBuiltinFunction : public PrimitiveCall {
  public:
    llvm::Value* constantPool() { return getValue(0); }

    int name() { return getValueInt(1); }
    SEXP nameValue() {
        llvm::Function* f = ins()->getParent()->getParent();
        JITModule* m = static_cast<JITModule*>(f->getParent());
        return VECTOR_ELT(m->constPool(f), name());
    }
    SEXP name(Builder const& b) { return b.constantPool(name()); }

    GetBuiltinFunction(llvm::Instruction* ins)
        : PrimitiveCall(ins, Kind::GetBuiltinFunction) {}

    static GetBuiltinFunction* create(Builder& b, SEXP name) {
        Sentinel s(b);
        return insertBefore(s, b.consts(),
                            Builder::integer(b.constantPoolIndex(name)));
    }

    static GetBuiltinFunction* insertBefore(llvm::Instruction* ins,
                                            ir::Value constantPool,
                                            ir::Value name) {

        std::vector<llvm::Value*> args_;
        args_.push_back(constantPool);
        args_.push_back(name);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<GetBuiltinFunction>(ins->getModule()), args_, "",
            ins);

        Builder::markSafepoint(i);
        return new GetBuiltinFunction(i);
    }

    static GetBuiltinFunction* insertBefore(Pattern* p, ir::Value constantPool,
                                            ir::Value name) {
        return insertBefore(p->first(), constantPool, name);
    }

    static char const* intrinsicName() { return "getBuiltinFunction"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(t::SEXP, {t::SEXP, t::Int}, false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::GetBuiltinFunction;
    }
};

class GetInternalBuiltinFunction : public PrimitiveCall {
  public:
    llvm::Value* constantPool() { return getValue(0); }

    int name() { return getValueInt(1); }
    SEXP nameValue() {
        llvm::Function* f = ins()->getParent()->getParent();
        JITModule* m = static_cast<JITModule*>(f->getParent());
        return VECTOR_ELT(m->constPool(f), name());
    }
    SEXP name(Builder const& b) { return b.constantPool(name()); }

    GetInternalBuiltinFunction(llvm::Instruction* ins)
        : PrimitiveCall(ins, Kind::GetInternalBuiltinFunction) {}

    static GetInternalBuiltinFunction* create(Builder& b, SEXP name) {
        Sentinel s(b);
        return insertBefore(s, b.consts(),
                            Builder::integer(b.constantPoolIndex(name)));
    }

    static GetInternalBuiltinFunction* insertBefore(llvm::Instruction* ins,
                                                    ir::Value constantPool,
                                                    ir::Value name) {

        std::vector<llvm::Value*> args_;
        args_.push_back(constantPool);
        args_.push_back(name);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<GetInternalBuiltinFunction>(ins->getModule()),
            args_, "", ins);

        Builder::markSafepoint(i);
        return new GetInternalBuiltinFunction(i);
    }

    static GetInternalBuiltinFunction*
    insertBefore(Pattern* p, ir::Value constantPool, ir::Value name) {
        return insertBefore(p->first(), constantPool, name);
    }

    static char const* intrinsicName() { return "getInternalBuiltinFunction"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(t::SEXP, {t::SEXP, t::Int}, false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::GetInternalBuiltinFunction;
    }
};

class CheckFunction : public PrimitiveCall {
  public:
    llvm::Value* f() { return getValue(0); }

    CheckFunction(llvm::Instruction* ins)
        : PrimitiveCall(ins, Kind::CheckFunction) {}

    static CheckFunction* create(Builder& b, ir::Value f) {
        Sentinel s(b);
        return insertBefore(s, f);
    }

    static CheckFunction* insertBefore(llvm::Instruction* ins, ir::Value f) {

        std::vector<llvm::Value*> args_;
        args_.push_back(f);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<CheckFunction>(ins->getModule()), args_, "", ins);

        Builder::markSafepoint(i);
        return new CheckFunction(i);
    }

    static CheckFunction* insertBefore(Pattern* p, ir::Value f) {
        return insertBefore(p->first(), f);
    }

    static char const* intrinsicName() { return "checkFunction"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(t::Void, {t::SEXP}, false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::CheckFunction;
    }
};

// Creates a promise out of the given code and environment and returns
// its value.
class CreatePromise : public PrimitiveCall {
  public:
    llvm::Value* fun() { return getValue(0); }
    llvm::Value* rho() { return getValue(1); }

    CreatePromise(llvm::Instruction* ins)
        : PrimitiveCall(ins, Kind::CreatePromise) {}

    static CreatePromise* create(Builder& b, ir::Value fun, llvm::Value* rho) {
        Sentinel s(b);
        return insertBefore(s, fun, rho);
    }

    static CreatePromise* insertBefore(llvm::Instruction* ins, ir::Value fun,
                                       ir::Value rho) {

        std::vector<llvm::Value*> args_;
        args_.push_back(fun);
        args_.push_back(rho);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<CreatePromise>(ins->getModule()), args_, "", ins);

        Builder::markSafepoint(i);
        return new CreatePromise(i);
    }

    static CreatePromise* insertBefore(Pattern* p, ir::Value fun,
                                       ir::Value rho) {
        return insertBefore(p->first(), fun, rho);
    }

    static char const* intrinsicName() { return "createPromise"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(t::SEXP, {t::SEXP, t::SEXP}, false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::CreatePromise;
    }
};

// Given a SEXP, returns its type. We can perfectly do this in LLVM, but
// having an function for it simplifies the analysis on our end.
class SexpType : public PrimitiveCall {
  public:
    llvm::Value* value() { return getValue(0); }

    SexpType(llvm::Instruction* ins) : PrimitiveCall(ins, Kind::SexpType) {}

    static SexpType* create(Builder& b, ir::Value value) {
        Sentinel s(b);
        return insertBefore(s, value);
    }

    static SexpType* insertBefore(llvm::Instruction* ins, ir::Value value) {

        std::vector<llvm::Value*> args_;
        args_.push_back(value);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<SexpType>(ins->getModule()), args_, "", ins);

        Builder::markSafepoint(i);
        return new SexpType(i);
    }

    static SexpType* insertBefore(Pattern* p, ir::Value value) {
        return insertBefore(p->first(), value);
    }

    static char const* intrinsicName() { return "sexpType"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(t::Int, {t::SEXP}, false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::SexpType;
    }
};

class AddArgument : public PrimitiveCall {
  public:
    llvm::Value* args() { return getValue(0); }
    llvm::Value* arg() { return getValue(1); }

    AddArgument(llvm::Instruction* ins)
        : PrimitiveCall(ins, Kind::AddArgument) {}

    static AddArgument* create(Builder& b, ir::Value args, llvm::Value* arg) {
        Sentinel s(b);
        return insertBefore(s, args, arg);
    }
    static AddArgument* insertBefore(llvm::Instruction* ins, ir::Value args,
                                     ir::Value arg) {

        std::vector<llvm::Value*> args_;
        args_.push_back(args);
        args_.push_back(arg);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<AddArgument>(ins->getModule()), args_, "", ins);

        Builder::markSafepoint(i);
        return new AddArgument(i);
    }

    static AddArgument* insertBefore(Pattern* p, ir::Value args,
                                     ir::Value arg) {
        return insertBefore(p->first(), args, arg);
    }

    static char const* intrinsicName() { return "addArgument"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(t::SEXP, {t::SEXP, t::SEXP}, false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::AddArgument;
    }
};

class AddKeywordArgument : public PrimitiveCall {
  public:
    llvm::Value* args() { return getValue(0); }
    llvm::Value* arg() { return getValue(1); }
    llvm::Value* name() { return getValue(2); }

    AddKeywordArgument(llvm::Instruction* ins)
        : PrimitiveCall(ins, Kind::AddKeywordArgument) {}

    static AddKeywordArgument* create(Builder& b, ir::Value args,
                                      llvm::Value* arg, ir::Value name) {
        Sentinel s(b);
        return insertBefore(s, args, arg, name);
    }

    static AddKeywordArgument* insertBefore(llvm::Instruction* ins,
                                            ir::Value args, ir::Value arg,
                                            ir::Value name) {

        std::vector<llvm::Value*> args_;
        args_.push_back(args);
        args_.push_back(arg);
        args_.push_back(name);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<AddKeywordArgument>(ins->getModule()), args_, "",
            ins);

        Builder::markSafepoint(i);
        return new AddKeywordArgument(i);
    }

    static AddKeywordArgument* insertBefore(Pattern* p, ir::Value args,
                                            ir::Value arg, ir::Value name) {
        return insertBefore(p->first(), args, arg, name);
    }

    static char const* intrinsicName() { return "addKeywordArgument"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(t::SEXP, {t::SEXP, t::SEXP, t::SEXP},
                                       false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::AddKeywordArgument;
    }
};

class AddEllipsisArgumentHead : public PrimitiveCall {
  public:
    llvm::Value* args() { return getValue(0); }
    llvm::Value* rho() { return getValue(1); }
    llvm::Value* eager() { return getValue(2); }

    AddEllipsisArgumentHead(llvm::Instruction* ins)
        : PrimitiveCall(ins, Kind::AddEllipsisArgumentHead) {}

    static AddEllipsisArgumentHead*
    create(Builder& b, ir::Value args, llvm::Value* rho, llvm::Value* eager) {
        Sentinel s(b);
        return insertBefore(s, args, rho, eager);
    }

    static AddEllipsisArgumentHead* insertBefore(llvm::Instruction* ins,
                                                 ir::Value args, ir::Value rho,
                                                 ir::Value eager) {

        std::vector<llvm::Value*> args_;
        args_.push_back(args);
        args_.push_back(rho);
        args_.push_back(eager);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<AddEllipsisArgumentHead>(ins->getModule()), args_,
            "", ins);

        Builder::markSafepoint(i);
        return new AddEllipsisArgumentHead(i);
    }

    static AddEllipsisArgumentHead*
    insertBefore(Pattern* p, ir::Value args, ir::Value rho, ir::Value eager) {
        return insertBefore(p->first(), args, rho, eager);
    }

    static char const* intrinsicName() { return "addEllipsisArgumentHead"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(t::SEXP, {t::SEXP, t::SEXP, t::Bool},
                                       false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::AddEllipsisArgumentHead;
    }
};

class AddEllipsisArgumentTail : public PrimitiveCall {
  public:
    llvm::Value* args() { return getValue(0); }
    llvm::Value* rho() { return getValue(1); }
    llvm::Value* eager() { return getValue(2); }

    AddEllipsisArgumentTail(llvm::Instruction* ins)
        : PrimitiveCall(ins, Kind::AddEllipsisArgumentTail) {}

    static AddEllipsisArgumentTail*
    create(Builder& b, ir::Value args, llvm::Value* rho, llvm::Value* eager) {
        Sentinel s(b);
        return insertBefore(s, args, rho, eager);
    }

    static AddEllipsisArgumentTail* insertBefore(llvm::Instruction* ins,
                                                 ir::Value args, ir::Value rho,
                                                 ir::Value eager) {

        std::vector<llvm::Value*> args_;
        args_.push_back(args);
        args_.push_back(rho);
        args_.push_back(eager);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<AddEllipsisArgumentTail>(ins->getModule()), args_,
            "", ins);

        Builder::markSafepoint(i);
        return new AddEllipsisArgumentTail(i);
    }

    static AddEllipsisArgumentTail*
    insertBefore(Pattern* p, ir::Value args, ir::Value rho, ir::Value eager) {
        return insertBefore(p->first(), args, rho, eager);
    }

    static char const* intrinsicName() { return "addEllipsisArgumentTail"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(t::SEXP, {t::SEXP, t::SEXP, t::Bool},
                                       false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::AddEllipsisArgumentTail;
    }
};

class CallBuiltin : public PrimitiveCall {
  public:
    llvm::Value* call() { return getValue(0); }
    llvm::Value* closure() { return getValue(1); }
    llvm::Value* arguments() { return getValue(2); }
    llvm::Value* rho() { return getValue(3); }

    CallBuiltin(llvm::Instruction* ins)
        : PrimitiveCall(ins, Kind::CallBuiltin) {}

    static CallBuiltin* create(Builder& b, ir::Value call, llvm::Value* closure,
                               ir::Value arguments, llvm::Value* rho) {
        Sentinel s(b);
        return insertBefore(s, call, closure, arguments, rho);
    }

    static CallBuiltin* insertBefore(llvm::Instruction* ins, ir::Value call,
                                     ir::Value closure, ir::Value arguments,
                                     ir::Value rho) {

        std::vector<llvm::Value*> args_;
        args_.push_back(call);
        args_.push_back(closure);
        args_.push_back(arguments);
        args_.push_back(rho);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<CallBuiltin>(ins->getModule()), args_, "", ins);

        Builder::markSafepoint(i);
        return new CallBuiltin(i);
    }

    static CallBuiltin* insertBefore(Pattern* p, ir::Value call,
                                     ir::Value closure, ir::Value arguments,
                                     ir::Value rho) {
        return insertBefore(p->first(), call, closure, arguments, rho);
    }

    static char const* intrinsicName() { return "callBuiltin"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(
            t::SEXP, {t::SEXP, t::SEXP, t::SEXP, t::SEXP}, false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::CallBuiltin;
    }
};

class CallSpecial : public PrimitiveCall {
  public:
    llvm::Value* call() { return getValue(0); }
    llvm::Value* closure() { return getValue(1); }
    llvm::Value* arguments() { return getValue(2); }
    llvm::Value* rho() { return getValue(3); }

    CallSpecial(llvm::Instruction* ins)
        : PrimitiveCall(ins, Kind::CallSpecial) {}

    static CallSpecial* create(Builder& b, ir::Value call, llvm::Value* closure,
                               ir::Value arguments, llvm::Value* rho) {
        Sentinel s(b);
        return insertBefore(s, call, closure, arguments, rho);
    }

    static CallSpecial* insertBefore(llvm::Instruction* ins, ir::Value call,
                                     ir::Value closure, ir::Value arguments,
                                     ir::Value rho) {

        std::vector<llvm::Value*> args_;
        args_.push_back(call);
        args_.push_back(closure);
        args_.push_back(arguments);
        args_.push_back(rho);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<CallSpecial>(ins->getModule()), args_, "", ins);

        Builder::markSafepoint(i);
        return new CallSpecial(i);
    }

    static CallSpecial* insertBefore(Pattern* p, ir::Value call,
                                     ir::Value closure, ir::Value arguments,
                                     ir::Value rho) {
        return insertBefore(p->first(), call, closure, arguments, rho);
    }

    static char const* intrinsicName() { return "callSpecial"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(
            t::SEXP, {t::SEXP, t::SEXP, t::SEXP, t::SEXP}, false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::CallSpecial;
    }
};

class CallClosure : public PrimitiveCall {
  public:
    llvm::Value* call() { return getValue(0); }
    llvm::Value* closure() { return getValue(1); }
    llvm::Value* arguments() { return getValue(2); }
    llvm::Value* rho() { return getValue(3); }

    CallClosure(llvm::Instruction* ins)
        : PrimitiveCall(ins, Kind::CallClosure) {}

    static CallClosure* create(Builder& b, ir::Value call, llvm::Value* closure,
                               ir::Value arguments, llvm::Value* rho) {
        Sentinel s(b);
        return insertBefore(s, call, closure, arguments, rho);
    }

    static CallClosure* insertBefore(llvm::Instruction* ins, ir::Value call,
                                     ir::Value closure, ir::Value arguments,
                                     ir::Value rho) {

        std::vector<llvm::Value*> args_;
        args_.push_back(call);
        args_.push_back(closure);
        args_.push_back(arguments);
        args_.push_back(rho);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<CallClosure>(ins->getModule()), args_, "", ins);

        Builder::markSafepoint(i);
        return new CallClosure(i);
    }

    static CallClosure* insertBefore(Pattern* p, ir::Value call,
                                     ir::Value closure, ir::Value arguments,
                                     ir::Value rho) {
        return insertBefore(p->first(), call, closure, arguments, rho);
    }

    static char const* intrinsicName() { return "callClosure"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(
            t::SEXP, {t::SEXP, t::SEXP, t::SEXP, t::SEXP}, false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::CallClosure;
    }
};

class CreateClosure : public PrimitiveCall {
  public:
    llvm::Value* rho() { return getValue(0); }
    llvm::Value* constantPool() { return getValue(1); }

    int forms() { return getValueInt(2); }
    SEXP formsValue() {
        llvm::Function* f = ins()->getParent()->getParent();
        JITModule* m = static_cast<JITModule*>(f->getParent());
        return VECTOR_ELT(m->constPool(f), forms());
    }
    SEXP forms(Builder const& b) { return b.constantPool(forms()); }

    int body() { return getValueInt(3); }
    SEXP bodyValue() {
        llvm::Function* f = ins()->getParent()->getParent();
        JITModule* m = static_cast<JITModule*>(f->getParent());
        return VECTOR_ELT(m->constPool(f), body());
    }
    SEXP body(Builder const& b) { return b.constantPool(body()); }

    CreateClosure(llvm::Instruction* ins)
        : PrimitiveCall(ins, Kind::CreateClosure) {}

    static CreateClosure* create(Builder& b, ir::Value rho, SEXP forms,
                                 SEXP body) {
        Sentinel s(b);
        return insertBefore(s, rho, b.consts(),
                            Builder::integer(b.constantPoolIndex(forms)),
                            Builder::integer(b.constantPoolIndex(body)));
    }

    static CreateClosure* insertBefore(llvm::Instruction* ins, ir::Value rho,
                                       ir::Value constantPool, ir::Value forms,
                                       ir::Value body) {

        std::vector<llvm::Value*> args_;
        args_.push_back(rho);
        args_.push_back(constantPool);
        args_.push_back(forms);
        args_.push_back(body);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<CreateClosure>(ins->getModule()), args_, "", ins);

        Builder::markSafepoint(i);
        return new CreateClosure(i);
    }

    static CreateClosure* insertBefore(Pattern* p, ir::Value rho,
                                       ir::Value constantPool, ir::Value forms,
                                       ir::Value body) {
        return insertBefore(p->first(), rho, constantPool, forms, body);
    }

    static char const* intrinsicName() { return "createClosure"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(
            t::SEXP, {t::SEXP, t::SEXP, t::Int, t::Int}, false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::CreateClosure;
    }
};

class GenericUnaryMinus : public PrimitiveCall {
  public:
    llvm::Value* op() { return getValue(0); }
    llvm::Value* rho() { return getValue(1); }
    llvm::Value* constantPool() { return getValue(2); }

    int call() { return getValueInt(3); }
    SEXP callValue() {
        llvm::Function* f = ins()->getParent()->getParent();
        JITModule* m = static_cast<JITModule*>(f->getParent());
        return VECTOR_ELT(m->constPool(f), call());
    }
    SEXP call(Builder const& b) { return b.constantPool(call()); }

    GenericUnaryMinus(llvm::Instruction* ins)
        : PrimitiveCall(ins, Kind::GenericUnaryMinus) {}

    static GenericUnaryMinus* create(Builder& b, ir::Value op, llvm::Value* rho,
                                     SEXP call) {
        Sentinel s(b);
        return insertBefore(s, op, rho, b.consts(),
                            Builder::integer(b.constantPoolIndex(call)));
    }

    static GenericUnaryMinus* insertBefore(llvm::Instruction* ins, ir::Value op,
                                           ir::Value rho,
                                           ir::Value constantPool,
                                           ir::Value call) {

        std::vector<llvm::Value*> args_;
        args_.push_back(op);
        args_.push_back(rho);
        args_.push_back(constantPool);
        args_.push_back(call);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<GenericUnaryMinus>(ins->getModule()), args_, "",
            ins);

        Builder::markSafepoint(i);
        return new GenericUnaryMinus(i);
    }

    static GenericUnaryMinus* insertBefore(Pattern* p, ir::Value op,
                                           ir::Value rho,
                                           ir::Value constantPool,
                                           ir::Value call) {
        return insertBefore(p->first(), op, rho, constantPool, call);
    }

    static char const* intrinsicName() { return "genericUnaryMinus"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(
            t::SEXP, {t::SEXP, t::SEXP, t::SEXP, t::Int}, false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::GenericUnaryMinus;
    }
};

class GenericUnaryPlus : public PrimitiveCall {
  public:
    llvm::Value* op() { return getValue(0); }
    llvm::Value* rho() { return getValue(1); }
    llvm::Value* constantPool() { return getValue(2); }

    int call() { return getValueInt(3); }
    SEXP callValue() {
        llvm::Function* f = ins()->getParent()->getParent();
        JITModule* m = static_cast<JITModule*>(f->getParent());
        return VECTOR_ELT(m->constPool(f), call());
    }
    SEXP call(Builder const& b) { return b.constantPool(call()); }

    GenericUnaryPlus(llvm::Instruction* ins)
        : PrimitiveCall(ins, Kind::GenericUnaryPlus) {}

    static GenericUnaryPlus* create(Builder& b, ir::Value op, llvm::Value* rho,
                                    SEXP call) {
        Sentinel s(b);
        return insertBefore(s, op, rho, b.consts(),
                            Builder::integer(b.constantPoolIndex(call)));
    }

    static GenericUnaryPlus* insertBefore(llvm::Instruction* ins, ir::Value op,
                                          ir::Value rho, ir::Value constantPool,
                                          ir::Value call) {

        std::vector<llvm::Value*> args_;
        args_.push_back(op);
        args_.push_back(rho);
        args_.push_back(constantPool);
        args_.push_back(call);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<GenericUnaryPlus>(ins->getModule()), args_, "",
            ins);

        Builder::markSafepoint(i);
        return new GenericUnaryPlus(i);
    }

    static GenericUnaryPlus* insertBefore(Pattern* p, ir::Value op,
                                          ir::Value rho, ir::Value constantPool,
                                          ir::Value call) {
        return insertBefore(p->first(), op, rho, constantPool, call);
    }

    static char const* intrinsicName() { return "genericUnaryPlus"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(
            t::SEXP, {t::SEXP, t::SEXP, t::SEXP, t::Int}, false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::GenericUnaryPlus;
    }
};

/** Binary operator represented by a primitive call.
 */
class PrimitiveBinaryOperator : public PrimitiveCall,
                                public ir::BinaryOperator {
  public:
    llvm::Instruction* first() const override { return Pattern::first(); }

    llvm::Instruction* last() const override { return Pattern::last(); }

    llvm::Value* lhs() override { return getValue(0); }
    llvm::Value* rhs() override { return getValue(1); }
    llvm::Value* rho() { return getValue(2); }
    llvm::Value* constantPool() { return getValue(3); }

    int call() { return getValueInt(4); }

    SEXP callValue() {
        llvm::Function* f = ins()->getParent()->getParent();
        JITModule* m = static_cast<JITModule*>(f->getParent());
        return VECTOR_ELT(m->constPool(f), call());
    }

    SEXP call(Builder const& b) { return b.constantPool(call()); }

  protected:
    PrimitiveBinaryOperator(llvm::Instruction* ins, Kind k)
        : PrimitiveCall(ins, k) {}
};

class GenericAdd : public ir::PrimitiveBinaryOperator {
  public:
    typedef ir::FAdd ScalarDouble;
    typedef ir::Add ScalarInt;

    static GenericAdd* create(Builder& b, ir::Value lhs, ir::Value rhs,
                              llvm::Value* rho, SEXP call) {
        Sentinel s(b);
        return insertBefore(s, lhs, rhs, rho, b.consts(),
                            Builder::integer(b.constantPoolIndex(call)));
    }

    static GenericAdd* insertBefore(llvm::Instruction* ins, ir::Value lhs,
                                    ir::Value rhs, ir::Value rho,
                                    ir::Value constantPool, ir::Value call) {

        std::vector<llvm::Value*> args_;
        args_.push_back(lhs);
        args_.push_back(rhs);
        args_.push_back(rho);
        args_.push_back(constantPool);
        args_.push_back(call);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<GenericAdd>(ins->getModule()), args_, "", ins);

        Builder::markSafepoint(i);
        return new GenericAdd(i);
    }

    static GenericAdd* insertBefore(Pattern* p, ir::Value lhs, ir::Value rhs,
                                    ir::Value rho, ir::Value constantPool,
                                    ir::Value call) {
        return insertBefore(p->first(), lhs, rhs, rho, constantPool, call);
    }

    static char const* intrinsicName() { return "genericAdd"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(
            t::SEXP, {t::SEXP, t::SEXP, t::SEXP, t::SEXP, t::Int}, false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::GenericAdd;
    }

  protected:
    GenericAdd(llvm::Instruction* ins)
        : PrimitiveBinaryOperator(ins, Kind::GenericAdd) {}
};

class GenericSub : public ir::PrimitiveBinaryOperator {
  public:
    typedef ir::FSub ScalarDouble;
    typedef ir::Sub ScalarInt;

    static GenericSub* create(Builder& b, ir::Value lhs, ir::Value rhs,
                              llvm::Value* rho, SEXP call) {
        Sentinel s(b);
        return insertBefore(s, lhs, rhs, rho, b.consts(),
                            Builder::integer(b.constantPoolIndex(call)));
    }

    static GenericSub* insertBefore(llvm::Instruction* ins, ir::Value lhs,
                                    ir::Value rhs, ir::Value rho,
                                    ir::Value constantPool, ir::Value call) {

        std::vector<llvm::Value*> args_;
        args_.push_back(lhs);
        args_.push_back(rhs);
        args_.push_back(rho);
        args_.push_back(constantPool);
        args_.push_back(call);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<GenericSub>(ins->getModule()), args_, "", ins);

        Builder::markSafepoint(i);
        return new GenericSub(i);
    }

    static GenericSub* insertBefore(Pattern* p, ir::Value lhs, ir::Value rhs,
                                    ir::Value rho, ir::Value constantPool,
                                    ir::Value call) {
        return insertBefore(p->first(), lhs, rhs, rho, constantPool, call);
    }

    static char const* intrinsicName() { return "genericSub"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(
            t::SEXP, {t::SEXP, t::SEXP, t::SEXP, t::SEXP, t::Int}, false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::GenericSub;
    }

  protected:
    GenericSub(llvm::Instruction* ins)
        : PrimitiveBinaryOperator(ins, Kind::GenericSub) {}
};

class GenericMul : public ir::PrimitiveBinaryOperator {
  public:
    typedef ir::FMul ScalarDouble;
    typedef ir::Mul ScalarInt;

    static GenericMul* create(Builder& b, ir::Value lhs, ir::Value rhs,
                              llvm::Value* rho, SEXP call) {
        Sentinel s(b);
        return insertBefore(s, lhs, rhs, rho, b.consts(),
                            Builder::integer(b.constantPoolIndex(call)));
    }

    static GenericMul* insertBefore(llvm::Instruction* ins, ir::Value lhs,
                                    ir::Value rhs, ir::Value rho,
                                    ir::Value constantPool, ir::Value call) {

        std::vector<llvm::Value*> args_;
        args_.push_back(lhs);
        args_.push_back(rhs);
        args_.push_back(rho);
        args_.push_back(constantPool);
        args_.push_back(call);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<GenericMul>(ins->getModule()), args_, "", ins);

        Builder::markSafepoint(i);
        return new GenericMul(i);
    }

    static GenericMul* insertBefore(Pattern* p, ir::Value lhs, ir::Value rhs,
                                    ir::Value rho, ir::Value constantPool,
                                    ir::Value call) {
        return insertBefore(p->first(), lhs, rhs, rho, constantPool, call);
    }

    static char const* intrinsicName() { return "genericMul"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(
            t::SEXP, {t::SEXP, t::SEXP, t::SEXP, t::SEXP, t::Int}, false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::GenericMul;
    }

  protected:
    GenericMul(llvm::Instruction* ins)
        : PrimitiveBinaryOperator(ins, Kind::GenericMul) {}
};

class GenericDiv : public ir::PrimitiveBinaryOperator {
  public:
    typedef ir::FDiv ScalarDouble;
    typedef ir::Div ScalarInt;

    static GenericDiv* create(Builder& b, ir::Value lhs, ir::Value rhs,
                              llvm::Value* rho, SEXP call) {
        Sentinel s(b);
        return insertBefore(s, lhs, rhs, rho, b.consts(),
                            Builder::integer(b.constantPoolIndex(call)));
    }

    static GenericDiv* insertBefore(llvm::Instruction* ins, ir::Value lhs,
                                    ir::Value rhs, ir::Value rho,
                                    ir::Value constantPool, ir::Value call) {

        std::vector<llvm::Value*> args_;
        args_.push_back(lhs);
        args_.push_back(rhs);
        args_.push_back(rho);
        args_.push_back(constantPool);
        args_.push_back(call);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<GenericDiv>(ins->getModule()), args_, "", ins);

        Builder::markSafepoint(i);
        return new GenericDiv(i);
    }

    static GenericDiv* insertBefore(Pattern* p, ir::Value lhs, ir::Value rhs,
                                    ir::Value rho, ir::Value constantPool,
                                    ir::Value call) {
        return insertBefore(p->first(), lhs, rhs, rho, constantPool, call);
    }

    static char const* intrinsicName() { return "genericDiv"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(
            t::SEXP, {t::SEXP, t::SEXP, t::SEXP, t::SEXP, t::Int}, false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::GenericDiv;
    }

  protected:
    GenericDiv(llvm::Instruction* ins)
        : PrimitiveBinaryOperator(ins, Kind::GenericDiv) {}
};

class GenericPow : public PrimitiveCall {
  public:
    llvm::Value* lhs() { return getValue(0); }
    llvm::Value* rhs() { return getValue(1); }
    llvm::Value* rho() { return getValue(2); }
    llvm::Value* constantPool() { return getValue(3); }

    int call() { return getValueInt(4); }
    SEXP callValue() {
        llvm::Function* f = ins()->getParent()->getParent();
        JITModule* m = static_cast<JITModule*>(f->getParent());
        return VECTOR_ELT(m->constPool(f), call());
    }
    SEXP call(Builder const& b) { return b.constantPool(call()); }

    GenericPow(llvm::Instruction* ins) : PrimitiveCall(ins, Kind::GenericPow) {}

    static GenericPow* create(Builder& b, ir::Value lhs, ir::Value rhs,
                              llvm::Value* rho, SEXP call) {
        Sentinel s(b);
        return insertBefore(s, lhs, rhs, rho, b.consts(),
                            Builder::integer(b.constantPoolIndex(call)));
    }

    static GenericPow* insertBefore(llvm::Instruction* ins, ir::Value lhs,
                                    ir::Value rhs, ir::Value rho,
                                    ir::Value constantPool, ir::Value call) {

        std::vector<llvm::Value*> args_;
        args_.push_back(lhs);
        args_.push_back(rhs);
        args_.push_back(rho);
        args_.push_back(constantPool);
        args_.push_back(call);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<GenericPow>(ins->getModule()), args_, "", ins);

        Builder::markSafepoint(i);
        return new GenericPow(i);
    }

    static GenericPow* insertBefore(Pattern* p, ir::Value lhs, ir::Value rhs,
                                    ir::Value rho, ir::Value constantPool,
                                    ir::Value call) {
        return insertBefore(p->first(), lhs, rhs, rho, constantPool, call);
    }

    static char const* intrinsicName() { return "genericPow"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(
            t::SEXP, {t::SEXP, t::SEXP, t::SEXP, t::SEXP, t::Int}, false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::GenericPow;
    }
};

class GenericSqrt : public PrimitiveCall {
  public:
    llvm::Value* op() { return getValue(0); }
    llvm::Value* rho() { return getValue(1); }
    llvm::Value* constantPool() { return getValue(2); }

    int call() { return getValueInt(3); }
    SEXP callValue() {
        llvm::Function* f = ins()->getParent()->getParent();
        JITModule* m = static_cast<JITModule*>(f->getParent());
        return VECTOR_ELT(m->constPool(f), call());
    }
    SEXP call(Builder const& b) { return b.constantPool(call()); }

    GenericSqrt(llvm::Instruction* ins)
        : PrimitiveCall(ins, Kind::GenericSqrt) {}

    static GenericSqrt* create(Builder& b, ir::Value op, ir::Value rho,
                               SEXP call) {
        Sentinel s(b);
        return insertBefore(s, op, rho, b.consts(),
                            Builder::integer(b.constantPoolIndex(call)));
    }

    static GenericSqrt* insertBefore(llvm::Instruction* ins, ir::Value op,
                                     ir::Value rho, ir::Value constantPool,
                                     ir::Value call) {

        std::vector<llvm::Value*> args_;
        args_.push_back(op);
        args_.push_back(rho);
        args_.push_back(constantPool);
        args_.push_back(call);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<GenericSqrt>(ins->getModule()), args_, "", ins);

        Builder::markSafepoint(i);
        return new GenericSqrt(i);
    }

    static GenericSqrt* insertBefore(Pattern* p, ir::Value op, ir::Value rho,
                                     ir::Value constantPool, ir::Value call) {
        return insertBefore(p->first(), op, rho, constantPool, call);
    }

    static char const* intrinsicName() { return "genericSqrt"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(
            t::SEXP, {t::SEXP, t::SEXP, t::SEXP, t::Int}, false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::GenericSqrt;
    }
};

class GenericExp : public PrimitiveCall {
  public:
    llvm::Value* op() { return getValue(0); }
    llvm::Value* rho() { return getValue(1); }
    llvm::Value* constantPool() { return getValue(2); }

    int call() { return getValueInt(3); }
    SEXP callValue() {
        llvm::Function* f = ins()->getParent()->getParent();
        JITModule* m = static_cast<JITModule*>(f->getParent());
        return VECTOR_ELT(m->constPool(f), call());
    }
    SEXP call(Builder const& b) { return b.constantPool(call()); }

    GenericExp(llvm::Instruction* ins) : PrimitiveCall(ins, Kind::GenericExp) {}

    static GenericExp* create(Builder& b, ir::Value op, ir::Value rho,
                              SEXP call) {
        Sentinel s(b);
        return insertBefore(s, op, rho, b.consts(),
                            Builder::integer(b.constantPoolIndex(call)));
    }

    static GenericExp* insertBefore(llvm::Instruction* ins, ir::Value op,
                                    ir::Value rho, ir::Value constantPool,
                                    ir::Value call) {

        std::vector<llvm::Value*> args_;
        args_.push_back(op);
        args_.push_back(rho);
        args_.push_back(constantPool);
        args_.push_back(call);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<GenericExp>(ins->getModule()), args_, "", ins);

        Builder::markSafepoint(i);
        return new GenericExp(i);
    }

    static GenericExp* insertBefore(Pattern* p, ir::Value op, ir::Value rho,
                                    ir::Value constantPool, ir::Value call) {
        return insertBefore(p->first(), op, rho, constantPool, call);
    }

    static char const* intrinsicName() { return "genericExp"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(
            t::SEXP, {t::SEXP, t::SEXP, t::SEXP, t::Int}, false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::GenericExp;
    }
};

class GenericEq : public PrimitiveCall {
  public:
    llvm::Value* lhs() { return getValue(0); }
    llvm::Value* rhs() { return getValue(1); }
    llvm::Value* rho() { return getValue(2); }
    llvm::Value* constantPool() { return getValue(3); }

    int call() { return getValueInt(4); }
    SEXP callValue() {
        llvm::Function* f = ins()->getParent()->getParent();
        JITModule* m = static_cast<JITModule*>(f->getParent());
        return VECTOR_ELT(m->constPool(f), call());
    }
    SEXP call(Builder const& b) { return b.constantPool(call()); }

    GenericEq(llvm::Instruction* ins) : PrimitiveCall(ins, Kind::GenericEq) {}

    static GenericEq* create(Builder& b, ir::Value lhs, ir::Value rhs,
                             llvm::Value* rho, SEXP call) {
        Sentinel s(b);
        return insertBefore(s, lhs, rhs, rho, b.consts(),
                            Builder::integer(b.constantPoolIndex(call)));
    }

    static char const* intrinsicName() { return "genericEq"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(
            t::SEXP, {t::SEXP, t::SEXP, t::SEXP, t::SEXP, t::Int}, false);
    }

    static GenericEq* insertBefore(llvm::Instruction* ins, ir::Value lhs,
                                   ir::Value rhs, ir::Value rho,
                                   ir::Value constantPool, ir::Value call) {

        std::vector<llvm::Value*> args_;
        args_.push_back(lhs);
        args_.push_back(rhs);
        args_.push_back(rho);
        args_.push_back(constantPool);
        args_.push_back(call);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<GenericEq>(ins->getModule()), args_, "", ins);

        Builder::markSafepoint(i);
        return new GenericEq(i);
    }

    static GenericEq* insertBefore(Pattern* p, ir::Value lhs, ir::Value rhs,
                                   ir::Value rho, ir::Value constantPool,
                                   ir::Value call) {
        return insertBefore(p->first(), lhs, rhs, rho, constantPool, call);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::GenericEq;
    }
};

class GenericNe : public PrimitiveCall {
  public:
    llvm::Value* lhs() { return getValue(0); }
    llvm::Value* rhs() { return getValue(1); }
    llvm::Value* rho() { return getValue(2); }
    llvm::Value* constantPool() { return getValue(3); }

    int call() { return getValueInt(4); }
    SEXP callValue() {
        llvm::Function* f = ins()->getParent()->getParent();
        JITModule* m = static_cast<JITModule*>(f->getParent());
        return VECTOR_ELT(m->constPool(f), call());
    }
    SEXP call(Builder const& b) { return b.constantPool(call()); }

    GenericNe(llvm::Instruction* ins) : PrimitiveCall(ins, Kind::GenericNe) {}

    static GenericNe* create(Builder& b, ir::Value lhs, ir::Value rhs,
                             llvm::Value* rho, SEXP call) {
        Sentinel s(b);
        return insertBefore(s, lhs, rhs, rho, b.consts(),
                            Builder::integer(b.constantPoolIndex(call)));
    }

    static GenericNe* insertBefore(llvm::Instruction* ins, ir::Value lhs,
                                   ir::Value rhs, ir::Value rho,
                                   ir::Value constantPool, ir::Value call) {

        std::vector<llvm::Value*> args_;
        args_.push_back(lhs);
        args_.push_back(rhs);
        args_.push_back(rho);
        args_.push_back(constantPool);
        args_.push_back(call);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<GenericNe>(ins->getModule()), args_, "", ins);

        Builder::markSafepoint(i);
        return new GenericNe(i);
    }

    static GenericNe* insertBefore(Pattern* p, ir::Value lhs, ir::Value rhs,
                                   ir::Value rho, ir::Value constantPool,
                                   ir::Value call) {
        return insertBefore(p->first(), lhs, rhs, rho, constantPool, call);
    }

    static char const* intrinsicName() { return "genericNe"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(
            t::SEXP, {t::SEXP, t::SEXP, t::SEXP, t::SEXP, t::Int}, false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::GenericNe;
    }
};

class GenericLt : public PrimitiveCall {
  public:
    llvm::Value* lhs() { return getValue(0); }
    llvm::Value* rhs() { return getValue(1); }
    llvm::Value* rho() { return getValue(2); }
    llvm::Value* constantPool() { return getValue(3); }

    int call() { return getValueInt(4); }
    SEXP callValue() {
        llvm::Function* f = ins()->getParent()->getParent();
        JITModule* m = static_cast<JITModule*>(f->getParent());
        return VECTOR_ELT(m->constPool(f), call());
    }
    SEXP call(Builder const& b) { return b.constantPool(call()); }

    GenericLt(llvm::Instruction* ins) : PrimitiveCall(ins, Kind::GenericLt) {}

    static GenericLt* create(Builder& b, ir::Value lhs, ir::Value rhs,
                             llvm::Value* rho, SEXP call) {
        Sentinel s(b);
        return insertBefore(s, lhs, rhs, rho, b.consts(),
                            Builder::integer(b.constantPoolIndex(call)));
    }

    static GenericLt* insertBefore(llvm::Instruction* ins, ir::Value lhs,
                                   ir::Value rhs, ir::Value rho,
                                   ir::Value constantPool, ir::Value call) {

        std::vector<llvm::Value*> args_;
        args_.push_back(lhs);
        args_.push_back(rhs);
        args_.push_back(rho);
        args_.push_back(constantPool);
        args_.push_back(call);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<GenericLt>(ins->getModule()), args_, "", ins);

        Builder::markSafepoint(i);
        return new GenericLt(i);
    }

    static GenericLt* insertBefore(Pattern* p, ir::Value lhs, ir::Value rhs,
                                   ir::Value rho, ir::Value constantPool,
                                   ir::Value call) {
        return insertBefore(p->first(), lhs, rhs, rho, constantPool, call);
    }

    static char const* intrinsicName() { return "genericLt"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(
            t::SEXP, {t::SEXP, t::SEXP, t::SEXP, t::SEXP, t::Int}, false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::GenericLt;
    }
};

class GenericLe : public PrimitiveCall {
  public:
    llvm::Value* lhs() { return getValue(0); }
    llvm::Value* rhs() { return getValue(1); }
    llvm::Value* rho() { return getValue(2); }
    llvm::Value* constantPool() { return getValue(3); }

    int call() { return getValueInt(4); }
    SEXP callValue() {
        llvm::Function* f = ins()->getParent()->getParent();
        JITModule* m = static_cast<JITModule*>(f->getParent());
        return VECTOR_ELT(m->constPool(f), call());
    }
    SEXP call(Builder const& b) { return b.constantPool(call()); }

    GenericLe(llvm::Instruction* ins) : PrimitiveCall(ins, Kind::GenericLe) {}

    static GenericLe* create(Builder& b, ir::Value lhs, ir::Value rhs,
                             llvm::Value* rho, SEXP call) {
        Sentinel s(b);
        return insertBefore(s, lhs, rhs, rho, b.consts(),
                            Builder::integer(b.constantPoolIndex(call)));
    }

    static GenericLe* insertBefore(llvm::Instruction* ins, ir::Value lhs,
                                   ir::Value rhs, ir::Value rho,
                                   ir::Value constantPool, ir::Value call) {

        std::vector<llvm::Value*> args_;
        args_.push_back(lhs);
        args_.push_back(rhs);
        args_.push_back(rho);
        args_.push_back(constantPool);
        args_.push_back(call);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<GenericLe>(ins->getModule()), args_, "", ins);

        Builder::markSafepoint(i);
        return new GenericLe(i);
    }

    static GenericLe* insertBefore(Pattern* p, ir::Value lhs, ir::Value rhs,
                                   ir::Value rho, ir::Value constantPool,
                                   ir::Value call) {
        return insertBefore(p->first(), lhs, rhs, rho, constantPool, call);
    }

    static char const* intrinsicName() { return "genericLe"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(
            t::SEXP, {t::SEXP, t::SEXP, t::SEXP, t::SEXP, t::Int}, false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::GenericLe;
    }
};

class GenericGe : public PrimitiveCall {
  public:
    llvm::Value* lhs() { return getValue(0); }
    llvm::Value* rhs() { return getValue(1); }
    llvm::Value* rho() { return getValue(2); }
    llvm::Value* constantPool() { return getValue(3); }

    int call() { return getValueInt(4); }
    SEXP callValue() {
        llvm::Function* f = ins()->getParent()->getParent();
        JITModule* m = static_cast<JITModule*>(f->getParent());
        return VECTOR_ELT(m->constPool(f), call());
    }
    SEXP call(Builder const& b) { return b.constantPool(call()); }

    GenericGe(llvm::Instruction* ins) : PrimitiveCall(ins, Kind::GenericGe) {}

    static GenericGe* create(Builder& b, ir::Value lhs, ir::Value rhs,
                             llvm::Value* rho, SEXP call) {
        Sentinel s(b);
        return insertBefore(s, lhs, rhs, rho, b.consts(),
                            Builder::integer(b.constantPoolIndex(call)));
    }

    static GenericGe* insertBefore(llvm::Instruction* ins, ir::Value lhs,
                                   ir::Value rhs, ir::Value rho,
                                   ir::Value constantPool, ir::Value call) {

        std::vector<llvm::Value*> args_;
        args_.push_back(lhs);
        args_.push_back(rhs);
        args_.push_back(rho);
        args_.push_back(constantPool);
        args_.push_back(call);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<GenericGe>(ins->getModule()), args_, "", ins);

        Builder::markSafepoint(i);
        return new GenericGe(i);
    }

    static GenericGe* insertBefore(Pattern* p, ir::Value lhs, ir::Value rhs,
                                   ir::Value rho, ir::Value constantPool,
                                   ir::Value call) {
        return insertBefore(p->first(), lhs, rhs, rho, constantPool, call);
    }

    static char const* intrinsicName() { return "genericGe"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(
            t::SEXP, {t::SEXP, t::SEXP, t::SEXP, t::SEXP, t::Int}, false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::GenericGe;
    }
};

class GenericGt : public PrimitiveCall {
  public:
    llvm::Value* lhs() { return getValue(0); }
    llvm::Value* rhs() { return getValue(1); }
    llvm::Value* rho() { return getValue(2); }
    llvm::Value* constantPool() { return getValue(3); }

    int call() { return getValueInt(4); }
    SEXP callValue() {
        llvm::Function* f = ins()->getParent()->getParent();
        JITModule* m = static_cast<JITModule*>(f->getParent());
        return VECTOR_ELT(m->constPool(f), call());
    }
    SEXP call(Builder const& b) { return b.constantPool(call()); }

    GenericGt(llvm::Instruction* ins) : PrimitiveCall(ins, Kind::GenericGt) {}

    static GenericGt* create(Builder& b, ir::Value lhs, ir::Value rhs,
                             llvm::Value* rho, SEXP call) {
        Sentinel s(b);
        return insertBefore(s, lhs, rhs, rho, b.consts(),
                            Builder::integer(b.constantPoolIndex(call)));
    }

    static GenericGt* insertBefore(llvm::Instruction* ins, ir::Value lhs,
                                   ir::Value rhs, ir::Value rho,
                                   ir::Value constantPool, ir::Value call) {

        std::vector<llvm::Value*> args_;
        args_.push_back(lhs);
        args_.push_back(rhs);
        args_.push_back(rho);
        args_.push_back(constantPool);
        args_.push_back(call);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<GenericGt>(ins->getModule()), args_, "", ins);

        Builder::markSafepoint(i);
        return new GenericGt(i);
    }

    static GenericGt* insertBefore(Pattern* p, ir::Value lhs, ir::Value rhs,
                                   ir::Value rho, ir::Value constantPool,
                                   ir::Value call) {
        return insertBefore(p->first(), lhs, rhs, rho, constantPool, call);
    }

    static char const* intrinsicName() { return "genericGt"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(
            t::SEXP, {t::SEXP, t::SEXP, t::SEXP, t::SEXP, t::Int}, false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::GenericGt;
    }
};

class GenericBitAnd : public PrimitiveCall {
  public:
    llvm::Value* lhs() { return getValue(0); }
    llvm::Value* rhs() { return getValue(1); }
    llvm::Value* rho() { return getValue(2); }
    llvm::Value* constantPool() { return getValue(3); }

    int call() { return getValueInt(4); }
    SEXP callValue() {
        llvm::Function* f = ins()->getParent()->getParent();
        JITModule* m = static_cast<JITModule*>(f->getParent());
        return VECTOR_ELT(m->constPool(f), call());
    }
    SEXP call(Builder const& b) { return b.constantPool(call()); }

    GenericBitAnd(llvm::Instruction* ins)
        : PrimitiveCall(ins, Kind::GenericBitAnd) {}

    static GenericBitAnd* create(Builder& b, ir::Value lhs, ir::Value rhs,
                                 llvm::Value* rho, SEXP call) {
        Sentinel s(b);
        return insertBefore(s, lhs, rhs, rho, b.consts(),
                            Builder::integer(b.constantPoolIndex(call)));
    }

    static GenericBitAnd* insertBefore(llvm::Instruction* ins, ir::Value lhs,
                                       ir::Value rhs, ir::Value rho,
                                       ir::Value constantPool, ir::Value call) {

        std::vector<llvm::Value*> args_;
        args_.push_back(lhs);
        args_.push_back(rhs);
        args_.push_back(rho);
        args_.push_back(constantPool);
        args_.push_back(call);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<GenericBitAnd>(ins->getModule()), args_, "", ins);

        Builder::markSafepoint(i);
        return new GenericBitAnd(i);
    }

    static GenericBitAnd* insertBefore(Pattern* p, ir::Value lhs, ir::Value rhs,
                                       ir::Value rho, ir::Value constantPool,
                                       ir::Value call) {
        return insertBefore(p->first(), lhs, rhs, rho, constantPool, call);
    }

    static char const* intrinsicName() { return "genericBitAnd"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(
            t::SEXP, {t::SEXP, t::SEXP, t::SEXP, t::SEXP, t::Int}, false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::GenericBitAnd;
    }
};

class GenericBitOr : public PrimitiveCall {
  public:
    llvm::Value* lhs() { return getValue(0); }
    llvm::Value* rhs() { return getValue(1); }
    llvm::Value* rho() { return getValue(2); }
    llvm::Value* constantPool() { return getValue(3); }

    int call() { return getValueInt(4); }
    SEXP callValue() {
        llvm::Function* f = ins()->getParent()->getParent();
        JITModule* m = static_cast<JITModule*>(f->getParent());
        return VECTOR_ELT(m->constPool(f), call());
    }
    SEXP call(Builder const& b) { return b.constantPool(call()); }

    GenericBitOr(llvm::Instruction* ins)
        : PrimitiveCall(ins, Kind::GenericBitOr) {}

    static GenericBitOr* create(Builder& b, ir::Value lhs, ir::Value rhs,
                                llvm::Value* rho, SEXP call) {
        Sentinel s(b);
        return insertBefore(s, lhs, rhs, rho, b.consts(),
                            Builder::integer(b.constantPoolIndex(call)));
    }

    static GenericBitOr* insertBefore(llvm::Instruction* ins, ir::Value lhs,
                                      ir::Value rhs, ir::Value rho,
                                      ir::Value constantPool, ir::Value call) {

        std::vector<llvm::Value*> args_;
        args_.push_back(lhs);
        args_.push_back(rhs);
        args_.push_back(rho);
        args_.push_back(constantPool);
        args_.push_back(call);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<GenericBitOr>(ins->getModule()), args_, "", ins);

        Builder::markSafepoint(i);
        return new GenericBitOr(i);
    }

    static GenericBitOr* insertBefore(Pattern* p, ir::Value lhs, ir::Value rhs,
                                      ir::Value rho, ir::Value constantPool,
                                      ir::Value call) {
        return insertBefore(p->first(), lhs, rhs, rho, constantPool, call);
    }

    static char const* intrinsicName() { return "genericBitOr"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(
            t::SEXP, {t::SEXP, t::SEXP, t::SEXP, t::SEXP, t::Int}, false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::GenericBitOr;
    }
};

class GenericNot : public PrimitiveCall {
  public:
    llvm::Value* op() { return getValue(0); }
    llvm::Value* rho() { return getValue(1); }
    llvm::Value* constantPool() { return getValue(2); }

    int call() { return getValueInt(3); }
    SEXP callValue() {
        llvm::Function* f = ins()->getParent()->getParent();
        JITModule* m = static_cast<JITModule*>(f->getParent());
        return VECTOR_ELT(m->constPool(f), call());
    }
    SEXP call(Builder const& b) { return b.constantPool(call()); }

    GenericNot(llvm::Instruction* ins) : PrimitiveCall(ins, Kind::GenericNot) {}

    static GenericNot* create(Builder& b, ir::Value op, ir::Value rho,
                              SEXP call) {
        Sentinel s(b);
        return insertBefore(s, op, rho, b.consts(),
                            Builder::integer(b.constantPoolIndex(call)));
    }

    static GenericNot* insertBefore(llvm::Instruction* ins, ir::Value op,
                                    ir::Value rho, ir::Value constantPool,
                                    ir::Value call) {

        std::vector<llvm::Value*> args_;
        args_.push_back(op);
        args_.push_back(rho);
        args_.push_back(constantPool);
        args_.push_back(call);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<GenericNot>(ins->getModule()), args_, "", ins);

        Builder::markSafepoint(i);
        return new GenericNot(i);
    }

    static GenericNot* insertBefore(Pattern* p, ir::Value op, ir::Value rho,
                                    ir::Value constantPool, ir::Value call) {
        return insertBefore(p->first(), op, rho, constantPool, call);
    }

    static char const* intrinsicName() { return "genericNot"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(
            t::SEXP, {t::SEXP, t::SEXP, t::SEXP, t::Int}, false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::GenericNot;
    }
};

class GenericGetVarMissOK : public PrimitiveCall {
  public:
    llvm::Value* symbol() { return getValue(0); }
    llvm::Value* rho() { return getValue(1); }

    GenericGetVarMissOK(llvm::Instruction* ins)
        : PrimitiveCall(ins, Kind::GenericGetVarMissOK) {}

    static GenericGetVarMissOK* create(Builder& b, ir::Value symbol,
                                       llvm::Value* rho) {
        Sentinel s(b);
        return insertBefore(s, symbol, rho);
    }

    static GenericGetVarMissOK* insertBefore(llvm::Instruction* ins,
                                             ir::Value symbol, ir::Value rho) {

        std::vector<llvm::Value*> args_;
        args_.push_back(symbol);
        args_.push_back(rho);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<GenericGetVarMissOK>(ins->getModule()), args_, "",
            ins);

        Builder::markSafepoint(i);
        return new GenericGetVarMissOK(i);
    }

    static GenericGetVarMissOK* insertBefore(Pattern* p, ir::Value symbol,
                                             ir::Value rho) {
        return insertBefore(p->first(), symbol, rho);
    }

    static char const* intrinsicName() { return "genericGetVarMissOK"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(t::SEXP, {t::SEXP, t::SEXP}, false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::GenericGetVarMissOK;
    }
};

class GenericGetEllipsisValueMissOK : public PrimitiveCall {
  public:
    llvm::Value* symbol() { return getValue(0); }
    llvm::Value* rho() { return getValue(1); }

    GenericGetEllipsisValueMissOK(llvm::Instruction* ins)
        : PrimitiveCall(ins, Kind::GenericGetEllipsisValueMissOK) {}

    static GenericGetEllipsisValueMissOK* create(Builder& b, ir::Value symbol,
                                                 ir::Value rho) {
        Sentinel s(b);
        return insertBefore(s, symbol, rho);
    }

    static GenericGetEllipsisValueMissOK*
    insertBefore(llvm::Instruction* ins, ir::Value symbol, ir::Value rho) {

        std::vector<llvm::Value*> args_;
        args_.push_back(symbol);
        args_.push_back(rho);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<GenericGetEllipsisValueMissOK>(ins->getModule()),
            args_, "", ins);

        Builder::markSafepoint(i);
        return new GenericGetEllipsisValueMissOK(i);
    }

    static GenericGetEllipsisValueMissOK*
    insertBefore(Pattern* p, ir::Value symbol, ir::Value rho) {
        return insertBefore(p->first(), symbol, rho);
    }

    static char const* intrinsicName() {
        return "genericGetEllipsisValueMissOK";
    }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(t::SEXP, {t::SEXP, t::SEXP}, false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::GenericGetEllipsisValueMissOK;
    }
};

class CheckSwitchControl : public PrimitiveCall {
  public:
    llvm::Value* ctrl() { return getValue(0); }
    llvm::Value* constantPool() { return getValue(1); }

    int call() { return getValueInt(2); }
    SEXP callValue() {
        llvm::Function* f = ins()->getParent()->getParent();
        JITModule* m = static_cast<JITModule*>(f->getParent());
        return VECTOR_ELT(m->constPool(f), call());
    }
    SEXP call(Builder const& b) { return b.constantPool(call()); }

    CheckSwitchControl(llvm::Instruction* ins)
        : PrimitiveCall(ins, Kind::CheckSwitchControl) {}

    static CheckSwitchControl* create(Builder& b, ir::Value ctrl, SEXP call) {
        Sentinel s(b);
        return insertBefore(s, ctrl, b.consts(),
                            Builder::integer(b.constantPoolIndex(call)));
    }

    static CheckSwitchControl* insertBefore(llvm::Instruction* ins,
                                            ir::Value ctrl,
                                            ir::Value constantPool,
                                            ir::Value call) {

        std::vector<llvm::Value*> args_;
        args_.push_back(ctrl);
        args_.push_back(constantPool);
        args_.push_back(call);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<CheckSwitchControl>(ins->getModule()), args_, "",
            ins);

        Builder::markSafepoint(i);
        return new CheckSwitchControl(i);
    }

    static CheckSwitchControl* insertBefore(Pattern* p, ir::Value ctrl,
                                            ir::Value constantPool,
                                            ir::Value call) {
        return insertBefore(p->first(), ctrl, constantPool, call);
    }

    static char const* intrinsicName() { return "checkSwitchControl"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(t::Void, {t::SEXP, t::SEXP, t::Int},
                                       false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::CheckSwitchControl;
    }
};

class SwitchControlCharacter : public PrimitiveCall {
  public:
    llvm::Value* ctrl() { return getValue(0); }
    llvm::Value* constantPool() { return getValue(1); }

    int call() { return getValueInt(2); }
    SEXP callValue() {
        llvm::Function* f = ins()->getParent()->getParent();
        JITModule* m = static_cast<JITModule*>(f->getParent());
        return VECTOR_ELT(m->constPool(f), call());
    }
    SEXP call(Builder const& b) { return b.constantPool(call()); }

    int cases() { return getValueInt(3); }
    SEXP casesValue() {
        llvm::Function* f = ins()->getParent()->getParent();
        JITModule* m = static_cast<JITModule*>(f->getParent());
        return VECTOR_ELT(m->constPool(f), cases());
    }
    SEXP cases(Builder const& b) { return b.constantPool(cases()); }

    SwitchControlCharacter(llvm::Instruction* ins)
        : PrimitiveCall(ins, Kind::SwitchControlCharacter) {}

    static SwitchControlCharacter* create(Builder& b, ir::Value ctrl, SEXP call,
                                          SEXP cases) {
        Sentinel s(b);
        return insertBefore(s, ctrl, b.consts(),
                            Builder::integer(b.constantPoolIndex(call)),
                            Builder::integer(b.constantPoolIndex(cases)));
    }

    static SwitchControlCharacter*
    insertBefore(llvm::Instruction* ins, ir::Value ctrl, ir::Value constantPool,
                 ir::Value call, ir::Value cases) {

        std::vector<llvm::Value*> args_;
        args_.push_back(ctrl);
        args_.push_back(constantPool);
        args_.push_back(call);
        args_.push_back(cases);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<SwitchControlCharacter>(ins->getModule()), args_,
            "", ins);

        Builder::markSafepoint(i);
        return new SwitchControlCharacter(i);
    }

    static SwitchControlCharacter* insertBefore(Pattern* p, ir::Value ctrl,
                                                ir::Value constantPool,
                                                ir::Value call,
                                                ir::Value cases) {
        return insertBefore(p->first(), ctrl, constantPool, call, cases);
    }

    static char const* intrinsicName() { return "switchControlCharacter"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(
            t::Int, {t::SEXP, t::SEXP, t::Int, t::Int}, false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::SwitchControlCharacter;
    }
};

class SwitchControlInteger : public PrimitiveCall {
  public:
    llvm::Value* ctrl() { return getValue(0); }
    int numCases() { return getValueInt(1); }

    SwitchControlInteger(llvm::Instruction* ins)
        : PrimitiveCall(ins, Kind::SwitchControlInteger) {}

    static SwitchControlInteger* create(Builder& b, ir::Value ctrl,
                                        int numCases) {
        Sentinel s(b);
        return insertBefore(s, ctrl, numCases);
    }

    static SwitchControlInteger* insertBefore(llvm::Instruction* ins,
                                              ir::Value ctrl, int numCases) {

        std::vector<llvm::Value*> args_;
        args_.push_back(ctrl);
        args_.push_back(Builder::integer(numCases));

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<SwitchControlInteger>(ins->getModule()), args_,
            "", ins);

        Builder::markSafepoint(i);
        return new SwitchControlInteger(i);
    }

    static SwitchControlInteger* insertBefore(Pattern* p, ir::Value ctrl,
                                              int numCases) {
        return insertBefore(p->first(), ctrl, numCases);
    }

    static char const* intrinsicName() { return "switchControlInteger"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(t::Int, {t::SEXP, t::Int}, false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::SwitchControlInteger;
    }
};

class ReturnJump : public PrimitiveCall {
  public:
    llvm::Value* value() { return getValue(0); }
    llvm::Value* rho() { return getValue(1); }

    ReturnJump(llvm::Instruction* ins) : PrimitiveCall(ins, Kind::ReturnJump) {}

    static ReturnJump* create(Builder& b, ir::Value value, llvm::Value* rho) {
        Sentinel s(b);
        return insertBefore(s, value, rho);
    }

    static ReturnJump* insertBefore(llvm::Instruction* ins, ir::Value value,
                                    ir::Value rho) {

        std::vector<llvm::Value*> args_;
        args_.push_back(value);
        args_.push_back(rho);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<ReturnJump>(ins->getModule()), args_, "", ins);

        Builder::markSafepoint(i);
        return new ReturnJump(i);
    }

    static ReturnJump* insertBefore(Pattern* p, ir::Value value,
                                    ir::Value rho) {
        return insertBefore(p->first(), value, rho);
    }

    static char const* intrinsicName() { return "returnJump"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(t::Void, {t::SEXP, t::SEXP}, false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::ReturnJump;
    }
};

class Recompile : public PrimitiveCall {
  public:
    Recompile(llvm::Instruction* ins) : PrimitiveCall(ins, Kind::Recompile) {}

    static Recompile* create(Builder& b, ir::Value closure, ir::Value caller,
                             ir::Value consts, ir::Value rho) {
        Sentinel s(b);
        return insertBefore(s, closure, caller, consts, rho);
    }

    static Recompile* insertBefore(llvm::Instruction* ins, ir::Value closure,
                                   ir::Value caller, ir::Value consts,
                                   ir::Value rho) {
        std::vector<llvm::Value*> args_;
        args_.push_back(closure);
        args_.push_back(caller);
        args_.push_back(consts);
        args_.push_back(rho);

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<Recompile>(ins->getModule()), args_, "", ins);

        Builder::markSafepoint(i);
        return new Recompile(i);
    }

    static char const* intrinsicName() { return "recompileFunction"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(
            t::SEXP, {t::SEXP, t::nativeFunctionPtr_t, t::SEXP, t::SEXP},
            false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::Recompile;
    }
};

class CheckType : public PrimitiveCall {
  public:
    CheckType(llvm::Instruction* ins) : PrimitiveCall(ins, Kind::CheckType) {}

    static CheckType* create(Builder& b, ir::Value value, TypeInfo expected) {
        Sentinel s(b);
        return insertBefore(s, value, expected);
    }

    static CheckType* insertBefore(llvm::Instruction* ins, ir::Value value,
                                   TypeInfo expected) {

        std::vector<llvm::Value*> args_;
        args_.push_back(value);
        args_.push_back(Builder::integer(static_cast<int>(expected)));

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<CheckType>(ins->getModule()), args_, "", ins);

        Builder::markSafepoint(i);
        return new CheckType(i);
    }

    static char const* intrinsicName() { return "checkType"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(t::Void, {t::SEXP, t::Int}, false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::CheckType;
    }
};

class RecordType : public PrimitiveCall {
  public:
    RecordType(llvm::Instruction* ins) : PrimitiveCall(ins, Kind::RecordType) {}

    static RecordType* create(Builder& b, SEXP sym, ir::Value value) {
        Sentinel s(b);
        ir::Value store =
            UserLiteral::insertBefore(s, b.consts(), Builder::integer(1));
        int idx = b.getInstrumentationIndex(sym);
        return insertBefore(s, value, store, idx);
    }

    static RecordType* insertBefore(llvm::Instruction* ins, ir::Value value,
                                    ir::Value store, int idx) {

        std::vector<llvm::Value*> args_;
        args_.push_back(value);
        args_.push_back(store);
        args_.push_back(Builder::integer(idx));

        llvm::CallInst* i = llvm::CallInst::Create(
            primitiveFunction<RecordType>(ins->getModule()), args_, "", ins);

        Builder::markSafepoint(i);
        return new RecordType(i);
    }

    static char const* intrinsicName() { return "recordType"; }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(t::Void, {t::SEXP, t::SEXP, t::Int},
                                       false);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::RecordType;
    }
};

} // namespace ir
} // namespace rjit
#endif // INTRINSICS_H_
