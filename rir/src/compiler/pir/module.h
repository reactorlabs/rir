#ifndef PIR_MODULE_H
#define PIR_MODULE_H

#include <iostream>
#include <vector>
#include "../../runtime/Function.h"

namespace rir {
namespace pir {

class Function;

struct IRTransformation {
  rir::Function* srcIR;
  pir::Function* dstIR;
};


class Module {
  public:
    std::vector<IRTransformation*> functions;
    void print(std::ostream& out = std::cout);
    ~Module();
};

}
}

#endif
