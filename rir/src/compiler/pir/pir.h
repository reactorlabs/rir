#ifndef PIR_PIR_H
#define PIR_PIR_H

//********** VERBOSE BIT MASK ***********
#define PRINT_ORIGINAL_MASK    0x1  //bit 1: RIR Original version
#define PRINT_RAW_PIR_MASK     0x2  //bit 2: PIR Raw
#define PRINT_OPT_PHASES_MASK  0x4  //bit 3: PIR Optimization phases
#define PRINT_INLINIG_MASK     0x8  //bit 4: PIR Inlining phases
#define PRINT_CSSA_MASK        0x10 //bit 5: PIR Convert to CSSA
#define PRINT_LIVENESS_MASK    0x20 //bit 6: PIR After liveness analysis phase
#define PRINT_STACK_MASK       0x40 //bit 7: RIR After stack allocation phase 
#define PRINT_FINAL_RIR_MASK   0x80 //bit 8: RIR After passing through PIR


#include "type.h"

// Forward declaration of PIR types. Use for headers.

namespace rir {
namespace pir {

class Closure;
class BB;
class Promise;
class Value;
class Code;
class Env;
class Instruction;
class LazyEnv;

}
}

#endif
