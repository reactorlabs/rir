#ifndef STATS_HOOK_H
#define STATS_HOOK_H

// Compile-time flag for stats collection.
// Define STATS_NO_USED_COMPILER to disable stats at compile time.
// This eliminates all runtime overhead: data structures, branches, and code.
#ifndef STATS_NO_USED_COMPILER
#define STATS_HOOK(code) code
#define STATS_COLLECT 1
#else
#define STATS_HOOK(code)
#define STATS_COLLECT 0
#endif

#endif // STATS_HOOK_H
