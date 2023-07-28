//
// Created by Jakob Hain on 7/26/23.
//

#pragma once

namespace rir {

#define LIST_OF_RIR_PRINT_STYLES(V) \
    V(Default) \
    V(Detailed) \
    V(PrettyGraph) \

/// Style to print RIR objects via `RirRuntimeObject::print`
/// (`compiler/log/DebugStyle` is for PIR objects).
enum class RirObjectPrintStyle {
#define V(name) name,
    LIST_OF_RIR_PRINT_STYLES(V)
#undef V
    // UNDOCUMENTED: Can't be selected by user
    /// Prints an object within another `PrettyGraph`.
    PrettyGraphInner
};

extern RirObjectPrintStyle RIR_DEBUG_STYLE;

} // namespace rir
