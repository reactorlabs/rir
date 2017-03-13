#ifndef RIR_CONFIG_H
#define RIR_CONFIG_H

/** Everyone wants R */
#include "R/r.h"

/** C/C++ interoperability layer for declarations and common data types
 */
#ifdef __cplusplus

#define C_OR_CPP extern "C"

#else

#define C_OR_CPP
#define bool int
#define nullptr NULL
#define true 1
#define false 0

#endif


#endif // RIR_CONFIG_H
