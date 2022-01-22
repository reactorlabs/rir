#ifndef RUNTIME_PATCHES_H
#define RUNTIME_PATCHES_H

#define SRC_HAST_MAP    1 // (1) For src to hast map
#define HAST_VTAB_MAP   2 // (2) Hast to vtable map
#define HAST_CLOS_MAP   3 // (3) Hast to closObj
#define BL_MAP          4 // (4) Hast blacklist, discard serialized code for these functions
#define PATCH_DEBUG_MAP 5 // (5) Patched Symbol to expected address, for debugging deopt and static call patches

/*************************************/
// Runtime tables for the patches
//
#define RESERVE_SPACES_AT_STARTUP 1
//
/*************************************/

/*************************************/
// Debugging for runtime patches
//
#define DEBUG_TABLE_ENTRIES 1
#define DEBUG_BLACKLIST 1
#define PRINT_SRC_HAST_MAP_UPDATES 1
#define PRINT_PATCH_ERRORS 1
/*************************************/

#endif
