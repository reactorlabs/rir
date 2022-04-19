#include "DispatchTable.h"

#include "utils/BitcodeLinkUtility.h"
namespace rir {
    void DispatchTable::tryLinking(SEXP currHastSym, const unsigned long & con, const int & nargs) {
        BitcodeLinkUtil::tryUnlockingOpt(hast, con, nargs);
    }
} // namespace rir
