#ifndef PIR_PIR_H
#define PIR_PIR_H

#include <stdint.h>

// clang-format off
const uint8_t PRINT_ORIGINAL_MASK   = 0x1;  // bit 1: RIR Original version
const uint8_t PRINT_RAW_PIR_MASK    = 0x2;  // bit 2: PIR Raw
const uint8_t PRINT_OPT_PHASES_MASK = 0x4;  // bit 3: PIR Optimization phases
const uint8_t PRINT_INLINIG_MASK    = 0x8;  // bit 4: PIR Inlining phases
const uint8_t PRINT_CSSA_MASK       = 0x10; // bit 5: PIR Convert to CSSA
const uint8_t PRINT_LIVENESS_MASK   = 0x20; // bit 6: PIR After liveness analysis phase
const uint8_t PRINT_STACK_MASK      = 0x40; // bit 7: RIR After stack allocation phase
const uint8_t PRINT_FINAL_RIR_MASK  = 0x80; // bit 8: RIR After passing through PIR
// clang-format on

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
