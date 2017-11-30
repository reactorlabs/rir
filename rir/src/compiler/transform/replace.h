#ifndef PIR_REPLACE_H
#define PIR_REPLACE_H

#include "../pir/pir.h"

namespace rir {
namespace pir {

class Replace {
  public:
    static void usesOfValue(BB* start, Value*, Value*);
};
}
}

#endif
