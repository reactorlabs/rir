#ifndef RIR_CONFIG_H
#define RIR_CONFIG_H

/** Everyone wants R */
#include "R/r.h"


/** If RIR_AS_PACKAGE is equal to 1, rir is built in mode that is compatible with vanilla GNU-R sources.

  Disabling the option makes RIR link to a modified version of gnu-r where rir is able to see selected hidden functions from R runtime and bypass some of the wrappers.
 */

#ifndef RIR_AS_PACKAGE
#define RIR_AS_PACKAGE 0
#endif


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
