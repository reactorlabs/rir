//
// Created by Jakob Hain on 7/21/23.
//

#include "hashRootUni.h"
#include "R/disableGc.h"
#include "compiler/parameter.h"
#include "hashAst.h"
#include "hashRoot_getConnected_common.h"
#include "runtime/LazyArglist.h"
#include "utils/measuring.h"

namespace rir {

bool HasherUni::willWrite(const rir::SerialFlags& flags) const {
    return flags.contains(SerialFlag::Hashed);
}

void HasherUni::writeBytes(const void* data, size_t size,
                            const SerialFlags& flags) {
    if (!willWrite(flags)) {
        return;
    }

    hasher.hashBytes((uint8_t*)data, size);
}

void HasherUni::writeInt(int data, const SerialFlags& flags) {
    if (!willWrite(flags)) {
        return;
    }

    hasher.hashBytesOf<int>(data);
}

void HasherUni::write(SEXP s, const SerialFlags& flags) {
    assert(flags.contains(SerialFlag::MaybeSexp) &&
           "Hashing non SEXP with SEXP flag");

    if (!willWrite(flags)) {
        return;
    }

    if (flags.contains(SerialFlag::MaybeNotAst)) {
        worklist.push(s);
    } else {
        hasher.hashBytesOf<UUID>(hashAst(s));
    }
}

void HasherUni::doHashRoot(SEXP root) {
    writeInline(root);
    while (!worklist.empty()) {
        auto sexp = worklist.front();
        worklist.pop();
        writeInline(sexp);
    }
}

UUID hashRootUni(SEXP root) {
    UUID result;
    disableInterpreter([&]{
        disableGc([&]{
            Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "hashRootUni", root, [&]{
                UUID::Hasher uuidHasher;
                HasherUni hasher(uuidHasher);
                hasher.doHashRoot(root);
                result = uuidHasher.finalize();
            });
        });
    });
    return result;
}

} // namespace rir