#pragma once

// Compile-time configuration for the recordless optimizations. This header is
// included broadly (e.g. via TypeFeedback.h), so it must contain only macros —
// no definitions. The runtime flag Compiler::recordLess_Leaf_Enabled (the
// ldvar-leaf optimization) is defined in Compiler.cpp.

// Expression-tree inner-node elision optimization toggle.
#define RECORDLESS_EXPTREE_ENABLED // comment out to disable
#ifdef RECORDLESS_EXPTREE_ENABLED
//    #define RECORDLESS_EXPTREE_DEBUG // uncomment to enable debug prints
#endif
