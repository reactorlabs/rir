#pragma once

#include "compiler/pir/tag.h"

#include <initializer_list>

namespace rir {
namespace pir {

struct EnvStubInfo {
  public:
    struct Status {
        bool allowed;
        unsigned priority;
        bool allowedNotMaterializing;
    };
    static Status of(Tag);
};

} // namespace pir
} // namespace rir
