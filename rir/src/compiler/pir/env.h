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

/*
 * Statically known environments.
 *
 * Currently only "theContext"-singleton is used, to denote an unknown parent
 * envrionment.
 *
 */
class Env : public Value {
  public:
    Env* parent = nullptr;

    static Env* theContext() {
        static Env e;
        return &e;
    }

    Env() : Value(RType::env, Tag::Env) {}

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
