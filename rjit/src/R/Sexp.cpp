#include "Sexp.h"
#include "r.h"

namespace rir {

void MatchStatement::fallThroughFail() {
    std::stringstream s;
    s << line;
    std::cout << Rf_type2char(TYPEOF(subject)) << " not handled in Match "
              << file << ":" << s.str() << "\n";
    assert(false);
}
}
