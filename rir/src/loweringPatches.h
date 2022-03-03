#ifndef LOWERING_PATCHES_H
#define LOWERING_PATCHES_H

/*************************************/
// R special symbol patches
//
#define ENABLE_SPE_PATCHES 1
//
// Prefix: "spe_"
  // Symbol: "spe_BCNodeStackTop"
  #define PATCH_NODE_STACK_TOP 1
  // Symbol: "spe_Visible"
  #define PATCH_SET_VISIBLE 1
  // Symbol: "spe_returnedValue"
  #define PATCH_RETURNED_VALUE 1
  // Symbol: "spe_opaqueTrue"
  #define PATCH_OPAQUE_TRUE 1
/*************************************/

/*************************************/
// assert messages as llvm global strings, instead of ept pointers
#define PATCH_MSG 1
/*************************************/

/*************************************/
// Names constants
//
// Prefix: "named_"
// Symbol: "named_constantPool"
#define PATCH_CONSTANT_POOL_PTR 1
/*************************************/

/*************************************/
// Direct Constant Symbols
//
#define ENABLE_DCS_PATCHES 1
//
// Prefix: "dcs_"
  // 100 - R_GlobalEnv
  // 101 - R_BaseEnv
  // 102 - R_BaseNamespace
  // 103 - R_TrueValue
  // 104 - R_NilValue
  // 105 - R_FalseValue
  // 106 - R_UnboundValue
  // 107 - R_MissingArg
  // 108 - R_LogicalNAValue
  // 109 - R_EmptyEnv
  // 110 - R_RestartToken
  // 111 - R_DimSymbol
  //
  #define PATCH_100_102 1
  #define PATCH_103_109 1
  #define PATCH_110_111 1
/*************************************/

/*************************************/
// Patch SYMSXP
//
#define ENABLE_SYM_PATCHES 1
//
// Prefix: "sym_"
  #define PATCH_SYMSXP 1
/*************************************/

/*************************************/
// Patch BUILTINSXP
//
#define ENABLE_GCB_PATCHES 1
//
// Prefix: "gcb_"
  #define PATCH_BUILTINSXP 1
/*************************************/

/*************************************/
// Patch PATCH_SPECIALSXP (looks up the special by name from the global environment)
//
#define ENABLE_SPE1_PATCHES 1
//
// Prefix: "spe1_"
  #define PATCH_SPECIALSXP 1
/*************************************/

/*************************************/
// Pool patches, patches global const symbols and marks them for serialization
//
#define PATCH_GLOBAL_CONSTANT_NAMES 1
//
// Prefix: "copool_"
  #define PATCH_CP_ENTRIES 1
  #define PATCH_SRCIDX_ENTRY 1
/*************************************/

/*************************************/
// Patch callRBuiltin
//
#define ENABLE_BUILTINCALL_PATCHES 1
//
// Prefix: "cod_"
  #define PATCH_BUILTINCALL 1
/*************************************/

/*************************************/
// Deopt patches
//
#define ENABLE_DEOPT_PATCHES 1
//
  #define TRY_PATCH_DEOPTREASON 1
  #define TRY_PATCH_DEOPTMETADATA 1
/*************************************/

/*************************************/
// Static Call Patches
//
#define ENABLE_SCALL_PATCHES 1
//
  #define TRY_PATCH_STATIC_CALL3 1
  #define TRY_PATCH_OPT_DISPATCH 1
/*************************************/

/*************************************/
// Debugging
//
#define DEBUG_LOCATIONS 0
#define ADD_EXTRA_DEBUGGING_DATA 0
#define DEBUG_NATIVE_LOCATIONS 0
//
/*************************************/

#endif
