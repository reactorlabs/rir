//
// Created by Jakob Hain on 7/23/23.
//

#include "getConnectedUni.h"
#include "R/r.h"
#include "compiler/parameter.h"
#include "runtime/LazyArglist.h"
#include "utils/measuring.h"

namespace rir {

bool ConnectedCollectorUni::willWrite(const rir::SerialFlags& flags) const {
    // We only care about writing SEXPs, all other writes are no-ops.
    // This also skips the assertion Code.cpp which requires the native code
    // object to be ready for serialization (which it's not, but we're not
    // actually serializing)
    return flags.contains(SerialFlag::MaybeSexp);
}

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