#ifndef PIR_MODULE_H
#define PIR_MODULE_H

#include <iostream>
#include <vector>

namespace rir {
namespace pir {

class Function;
class Env;

class Module {
  public:
    std::vector<Function*> function;
    void print(std::ostream& out = std::cout);
    ~Module();
};
}
}

#endif
