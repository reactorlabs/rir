#ifndef RIR_BCLINK_H
#define RIR_BCLINK_H

#include "R/Preserve.h"
#include "R/Protect.h"
#include "R/r.h"
#include "runtime/DispatchTable.h"
#include "utils/FunctionWriter.h"
#include "utils/Pool.h"
#include "utils/UMap.h"

#include <unordered_map>
#include <iostream>
#include <functional>
#include <cassert>

#include "runtimePatches.h"
#include "R/Printing.h"
#include "api.h"

#include "compiler/pir/module.h"
#include "compiler/log/stream_logger.h"
#include "compiler/compiler.h"
#include "compiler/backend.h"


#define GROWTHRATE 5

namespace rir {

    class BitcodeLinkUtil {
    public:

        static SEXP getHast(SEXP body, SEXP env);

        static void populateHastSrcData(DispatchTable* vtable, SEXP hastSym);

        static void insertVTable(DispatchTable* vtable, SEXP hastSym);

        static void insertClosObj(SEXP clos, SEXP hastSym);

        static void insertToBlacklist(SEXP hastSym);

        static bool readyForSerialization(DispatchTable* vtable, SEXP hastSym);

        static SEXP getOptUnlockMap();

        static void addToWorklistTwo(SEXP hastOfReq, unsigned long & con, int & nargs, SEXP unlockMetaSym);

        // This worklist get called when a hast is initially compiled.
        // If the counter becomes zero, we also link the dependent bitcodes.
        static void addToWorklistOne(SEXP hastOfReq, SEXP unlockMeta);

        static void linkBitcode(SEXP cData, SEXP hSym, SEXP offsetSymbol, DispatchTable * vtab);

        static void tryUnlocking(SEXP currHastSym);
        static void tryUnlockingOpt(SEXP currHastSym, const unsigned long & con, const int & nargs);

        static void tryLinking(DispatchTable * vtab, SEXP hSym);

        static void markStale(SEXP currHastSym, const unsigned long & con);
    };

}

#endif
