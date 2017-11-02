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
    rir::Opcode* oldPc;
    size_t size;
    uint8_t payload[];
} DeoptInfo;

extern uint32_t Deoptimizer_register(rir::Opcode* oldPc);
extern void Deoptimizer_print(uint32_t);
extern rir::Opcode* Deoptimizer_pc(uint32_t);

#ifdef __cplusplus
}
#endif

#endif
