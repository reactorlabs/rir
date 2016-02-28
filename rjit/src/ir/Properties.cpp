#include "ir/Properties.h"
#include "ir/Ir.h"

namespace rjit {
namespace ir {

ir::Pattern * ir::Property::pattern() {
    return dynamic_cast<ir::Pattern *>(this);

}

}
}
