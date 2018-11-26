#include "Sexp.h"
#include "r.h"

namespace rir {

void MatchStatement::fallThroughFail() {
    std::cerr << Rf_type2char(TYPEOF(subject)) << " not handled in Match "
              << file << ":" << line << "\n";
    assert(false);
}
}
