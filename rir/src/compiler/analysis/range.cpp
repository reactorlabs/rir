#include "range.h"

namespace rir {
namespace pir {

Range Range::MAX = {INT_MIN, INT_MAX};

Range Range::NEG = {INT_MIN, -1};
Range Range::ABOVE0 = {1, INT_MAX};
Range Range::POS = {0, INT_MAX};

Range Range::ZERO = {0, 0};
Range Range::ONE = {1, 1};

} // namespace pir
} // namespace rir
