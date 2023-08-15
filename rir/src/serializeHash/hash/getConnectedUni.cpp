//
// Created by Jakob Hain on 7/23/23.
//

#include "getConnectedUni.h"
#include "R/r.h"
#include "compiler/parameter.h"
#include "hashRoot_getConnected_common.h"
#include "runtime/Code.h"
#include "runtime/DispatchTable.h"
#include "runtime/Function.h"
#include "runtime/LazyArglist.h"
#include "runtime/LazyEnvironment.h"
#include "utils/Pool.h"
#include "utils/measuring.h"

namespace rir {

void ConnectedCollectorUni::write(SEXP s, const rir::SerialFlags& flags) {
    assert(flags.contains(SerialFlag::MaybeSexp) &&
               "Hashing non SEXP with SEXP flag");

    if (!willWrite(flags)) {
        return;
    }

    if (set.insert(s)) {
        worklist.push(s);
    }
}

void ConnectedCollectorUni::doGetConnected(SEXP root) {
    set.insert(root);
    writeInline(root);
    while (!worklist.empty()) {
        auto elem = worklist.front();
        worklist.pop();

        writeInline(elem);
    }
}

ConnectedSet getConnectedUni(SEXP root) {
    ConnectedSet set;
    disableInterpreter([&]{
        Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_INTERNING, "getConnected", root, [&] {
            ConnectedCollectorUni collector(set);
            collector.doGetConnected(root);
        });
    });
    return set;
}

} // namespace rir