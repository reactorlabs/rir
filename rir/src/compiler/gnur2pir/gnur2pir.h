#ifndef GNUR_2_PIR_H
#define GNUR_2_PIR_H

#include "compiler/compiler.h"
#include "compiler/pir/builder.h"

#include <unordered_map>
#include <unordered_set>

namespace rir {
namespace pir {

class Gnur2Pir {
    Module& m;

  public:
    Gnur2Pir(Module& m) : m(m){};
    pir::ClosureVersion* compile(SEXP src, const std::string& name);
};

} // namespace pir
} // namespace rir

#endif
