#include "visitor.h"

namespace rir {
namespace pir {

std::random_device Visitor::rd;
std::mt19937 Visitor::gen(rd());
std::bernoulli_distribution Visitor::coin(0.5);
}
}
