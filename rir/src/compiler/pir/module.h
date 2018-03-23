#ifndef PIR_MODULE_H
#define PIR_MODULE_H

#include <iostream>
#include <vector>
#include "../../runtime/Function.h"

namespace rir {
namespace pir {

class Function;

struct IRTransformation {
  rir::Function* srcFunction;
  rir::Code* srcCode;
  pir::Function* dstFunction;
  IRTransformation(rir::Function* src, pir::Function* dst)
      : srcFunction(src), srcCode(src->body()), dstFunction(dst) {}
  IRTransformation(rir::Function* src, rir::Code* srcCode, pir::Function* dst)
      : srcFunction(src), srcCode(srcCode), dstFunction(dst) {}
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
