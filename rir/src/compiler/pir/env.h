#ifndef COMPILER_ENV_H
#define COMPILER_ENV_H

#include "instruction_list.h"
#include "tag.h"
#include "value.h"

#include <iostream>
#include <vector>

namespace rir {
namespace pir {

class Instruction;

class Env : public Value {
  public:
    static Env* theContext() {
        static Env e(nullptr);
        return &e;
    }

    static size_t envIdCount;

    size_t envId;
    Env* parent;

    Env(Env* parent)
        : Value(RType::env, Tag::Env), envId(envIdCount++), parent(parent) {}
    void printRef(std::ostream& out);

    static Env* Cast(Value* v) {
        return v->tag == Tag::Env ? static_cast<Env*>(v) : nullptr;
    }

    static bool isEnv(Value* v);
    static bool isParentEnv(Value* a, Value* b);
    static Value* parentEnv(Value* e);
};
}
}

#endif
