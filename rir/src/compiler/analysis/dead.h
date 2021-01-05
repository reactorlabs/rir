#ifndef PIR_DEAD_H
#define PIR_DEAD_H

#include "../pir/instruction.h"
#include "compiler/pir/pir.h"
#include "compiler/pir/tag.h"
#include <unordered_set>

namespace rir {
namespace pir {

constexpr static std::initializer_list<Tag> TypecheckInstrsList = {
    Tag::IsType, Tag::CastType, Tag::FrameState};
constexpr static std::initializer_list<Tag> BoxedUsesInstrsList = {
    Tag::MkEnv, Tag::StVar,    Tag::Call,      Tag::NamedCall,
    Tag::MkArg, Tag::DotsList, Tag::FrameState};
constexpr static std::initializer_list<Tag> IgnoreIntVsReal = {
    Tag::ColonCastLhs,
    Tag::ColonCastRhs,
    Tag::CastType,
    Tag::IsType,
    Tag::Lte,
    Tag::Lt,
    Tag::Gt,
    Tag::Gte,
    Tag::Eq,
    Tag::Neq,
    Tag::AsLogical,
    Tag::AsTest,
    Tag::CheckTrueFalse,
    Tag::LAnd,
    Tag::LOr,
    Tag::Not};

class DeadInstructions {
    std::unordered_set<Instruction*> unused_;

  public:
    enum DeadInstructionsMode {
        CountAll,
        IgnoreTypeTests,
        IgnoreBoxedUses,
        IgnoreUsesThatDontObserveIntVsReal,
    };

    DeadInstructions(Code*, uint8_t maxBurstSize, Effects ignoreEffects,
                     DeadInstructionsMode mode = CountAll);
    bool isAlive(Value* v);
    bool isAlive(Instruction* v);
    bool isDead(Value* v);
    bool isDead(Instruction* v);
};

} // namespace pir
} // namespace rir

#endif
