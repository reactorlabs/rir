#ifndef IR_TAGS_H
#define IR_TAGS_H

namespace llvm {
class Instruction;
}

namespace rjit {
namespace ir {

class TaggedInstruction {
  public:
    TaggedInstruction() = delete;
    TaggedInstruction(llvm::Instruction* ins) : ins_(ins) {}
    explicit operator llvm::Instruction*() { return ins_; }

  private:
    llvm::Instruction* ins_;
};

class NoParentEnvWrite : public TaggedInstruction {
  public:
    NoParentEnvWrite(llvm::Instruction* ins) : TaggedInstruction(ins) {}
    class Tag {};
};

class NoEnvWrite : public NoParentEnvWrite {
  public:
    NoEnvWrite(llvm::Instruction* ins) : NoParentEnvWrite(ins) {}
    class Tag {};
};

class NoEnvAccess : public NoEnvWrite {
  public:
    NoEnvAccess(llvm::Instruction* ins) : NoEnvWrite(ins) {}
    class Tag {};
};
}
}

#endif
