#ifndef RIR_DEOPTIMIZER_H
#define RIR_DEOPTIMIZER_H

#include "interp_data.h"

#ifdef __cplusplus
extern "C" {
#else
#define bool int
#define true 1
#define false 0
#endif

typedef struct {
    OpcodeT* oldPc;
    size_t size;
    uint8_t payload[];
} DeoptInfo;

extern uint32_t Deoptimizer_register(OpcodeT* oldPc);
extern void Deoptimizer_print(uint32_t);
extern OpcodeT* Deoptimizer_pc(uint32_t);

#ifdef __cplusplus
}
#endif

#endif
