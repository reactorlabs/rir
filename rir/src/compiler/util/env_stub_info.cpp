#include "env_stub_info.h"
#include <algorithm>
#include <cassert>

namespace rir {
namespace pir {
// If we only see these (and call instructions) then we stub an environment,
// since it can only be accessed reflectively.
// These are only stubbed on the second try, since they seem to be better
// covered by type speculation pass.
static constexpr auto allowed = {
    Tag::Force,      Tag::PushContext, Tag::LdVar,         Tag::LdVarSuper,
    Tag::LdFun,      Tag::StVar,       Tag::StVarSuper,    Tag::Call,
    Tag::FrameState, Tag::CallBuiltin, Tag::StaticCall,    Tag::LdDots,
    Tag::Missing,    Tag::IsEnvStub,   Tag::MaterializeEnv};
static constexpr auto allowedExtra = {
    Tag::Add, Tag::Sub,   Tag::Mul, Tag::IDiv, Tag::Div,   Tag::Eq,
    Tag::Neq, Tag::Gt,    Tag::Lt,  Tag::Lte,  Tag::Gte,   Tag::LAnd,
    Tag::LOr, Tag::Colon, Tag::Mod, Tag::Pow,  Tag::Minus, Tag::Plus,
};
static constexpr auto allowedInProm = {Tag::LdVar,  Tag::LdVarSuper,
                                       Tag::StVar,  Tag::StVarSuper,
                                       Tag::LdDots, Tag::FrameState};
// Those do not materialize the stub in any case. PushContext doesn't
// materialize itself but it makes the environment accessible, so it's
// not on this list.
static constexpr auto dontMaterialize = {
    Tag::LdVar,      Tag::LdVarSuper, Tag::StVar,  Tag::StVarSuper,
    Tag::FrameState, Tag::IsEnvStub,  Tag::LdDots, Tag::Missing};

EnvStubInfo::Status EnvStubInfo::of(Tag t) {
    auto a1 = std::find(allowed.begin(), allowed.end(), t) != allowed.end();
    auto a2 = std::find(allowedExtra.begin(), allowedExtra.end(), t) !=
              allowedExtra.end();
    auto p = std::find(allowedInProm.begin(), allowedInProm.end(), t) !=
             allowedInProm.end();
    assert(!p || (a1 || a2));
    auto m = std::find(dontMaterialize.begin(), dontMaterialize.end(), t) !=
             dontMaterialize.end();
    assert(!m || (a1 || a2));
    return {a1 || a2, (unsigned)(a1 ? 0 : (a2 ? 1 : 2)), p, m};
}
} // namespace pir
} // namespace rir
