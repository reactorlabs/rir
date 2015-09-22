#ifndef STACK_SCAN_H
#define STACK_SCAN_H

#include "RDefs.h"

namespace rjit {

class StackScan {
  public:
    static void stackScanner(void (*forward_node)(SEXP));
};
}

#endif
