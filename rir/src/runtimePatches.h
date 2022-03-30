#ifndef RUNTIME_PATCHES_H
#define RUNTIME_PATCHES_H

#define HAST_DEPENDENCY_MAP   1 // (1) Hast to dependency map {map of contexts}
#define HAST_UNLOCK_MAP       2 // (2) Hast to unlock vector map
#define HAST_VTAB_MAP         3 // (3) Hast to vtable map
#define HAST_CLOS_MAP         4 // (4) Hast to closure map
/*************************************/
// Deserializer specific
//
#define PRINT_DESERIALIZER_PROGRESS 1
#define PRINT_DEPENDENCY_MAP 0
#define PRINT_UNLOCK_MAP 0
#define PRINT_LINKING_STATUS 1
#define DESERIALIZED_PRINT_POOL_PATCHES 0
#define PRINT_PROMISE_MAP 0
#define PRINT_SRC_MAP 0
#define PRINT_ARG_MAP 0
#define PRINT_PROMISE_LINKAGE_MAP 0
#define PRINT_DESERIALIZED_MODULE_BEFORE_PATCH 0
#define PRINT_DESERIALIZED_MODULE_AFTER_PATCH 0
//
/*************************************/

/*************************************/
// Runtime tables for the patches (needed for deserializer to work)
//
#define RESERVE_SPACES_AT_STARTUP 1
//
/*************************************/

/*************************************/
// Debugging for runtime patches
//

/*************************************/

#endif
