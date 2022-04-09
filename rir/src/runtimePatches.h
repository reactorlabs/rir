#ifndef RUNTIME_PATCHES_H
#define RUNTIME_PATCHES_H

#define SRC_HAST_MAP    1 // (1) For src to hast map
#define HAST_VTAB_MAP   2 // (2) Hast to vtable map
#define HAST_CLOS_MAP   3 // (3) Hast to closObj
#define BL_MAP          4 // (4) Hast blacklist, discard serialized code for these functions
#define HAST_DEPENDENCY_MAP   5 // (5) Hast to dependency map {map of contexts}
#define HAST_UNLOCK_MAP       6 // (6) Hast to unlock vector map [worklist 1]
#define OPT_UNLOCK_MAP        7 // (7) Hast -> { C1: [Worklist 2...],.. }

/*************************************/
// Serializer specific
//
#define PRINT_SERIALIZER_PROGRESS 0
#define PRINT_SERIALIZER_ERRORS 0
#define PRINT_UNHANDLED_ERRORS 0
#define PRINT_POOL_SERIALIZATION_ERRORS 0
#define BACKEND_PRINT_INITIAL_LLVM 0
#define PRINT_DONE_MAP 0
#define BACKEND_PRINT_NAME_UPDATES 0
#define BACKEND_PRINT_FINAL_LLVM 0
#define PRINT_PROM_MAP 0
#define PRINT_MODULE_BEFORE_POOL_PATCHES 0
#define PRINT_CP_ENTRIES 0
#define PRINT_SRC_ENTRIES 0
#define PRINT_PROM_ENTRIES 0
#define PRINT_MODULE_AFTER_POOL_PATCHES 0
//
/*************************************/

/*************************************/
// Runtime tables for the patches
//
#define RESERVE_SPACES_AT_STARTUP 1
//
/*************************************/

/*************************************/
// Debugging for runtime patches
//
#define DEBUG_TABLE_ENTRIES 0
#define DEBUG_BLACKLIST 0
#define PRINT_SRC_HAST_MAP_UPDATES 0
#define PRINT_PATCH_ERRORS 0
/*************************************/

#endif
