#ifndef RUNTIME_PATCHES_H
#define RUNTIME_PATCHES_H

#define SRC_HAST_MAP    1 // (1) For src to hast map
#define HAST_VTAB_MAP   2 // (2) Hast to vtable map
#define HAST_CLOS_MAP   3 // (3) Hast to closObj
#define BL_MAP          4 // (4) Hast blacklist, discard serialized code for these functions
#define HAST_DEPENDENCY_MAP   5 // (5) Hast to dependency map {map of contexts}
#define HAST_UNLOCK_MAP       6 // (6) Worklist 1: For initial bytecode compilation
#define OPT_UNLOCK_MAP        7 // (7) Workiist 2: For disptach table insertions
#define LINKAGE_MAP           8 // (8) Linkage map: For linkage metadata

/*************************************/
// Runtime tables for the patches
//
#define RESERVE_SPACES_AT_STARTUP 1
//
/*************************************/

#endif
