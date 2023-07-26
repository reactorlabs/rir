//
// Created by Jakob Hain on 7/26/23.
//

#include "RirObjectPrintStyle.h"
#include <cassert>
#include <cstdlib>
#include <cstring>
#include <iostream>

namespace rir {

static RirObjectPrintStyle getDefaultDebugStyle() {
    const char* env = getenv("RIR_DEBUG_STYLE");
    if (env && strlen(env) > 0) {
#define V(name)                                                                \
        if (strcmp(env, #name) == 0) {                                         \
            return RirObjectPrintStyle::name;                                  \
        }
        LIST_OF_RIR_PRINT_STYLES(V)
#undef V
        std::cerr << "Unknown RIR_DEBUG_STYLE: " << env << "\n"
                  << "Supported options are: (unset)";
#define V(name) std::cerr << ", '" << #name << "'";
        LIST_OF_RIR_PRINT_STYLES(V)
#undef V
        assert(false);
    } else {
        return RirObjectPrintStyle::Default;
    }
}

RirObjectPrintStyle RIR_DEBUG_STYLE = getDefaultDebugStyle();

} // namespace rir