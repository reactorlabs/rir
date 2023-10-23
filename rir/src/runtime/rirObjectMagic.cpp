//
// Created by Jakob Hain on 7/26/23.
//

#include "rirObjectMagic.h"
#include "Code.h"
#include "DispatchTable.h"
#include "GenericDispatchTable.h"
#include "LazyArglist.h"
#include "LazyEnvironment.h"
#include "PoolStub.h"
#include "RirRuntimeObject.h"

namespace rir {

const char* rirObjectClassName(unsigned magic) {
    switch (magic) {
        case CODE_MAGIC:
            return "Code";
        case DISPATCH_TABLE_MAGIC:
            return "DispatchTable";
        case FUNCTION_MAGIC:
            return "Function";
        case ARGLIST_ORDER_MAGIC:
            return "ArglistOrder";
        case LAZY_ARGS_MAGIC:
            return "LazyArglist";
        case LAZY_ENVIRONMENT_MAGIC:
            return "LazyEnvironment";
        case PIR_TYPE_FEEDBACK_MAGIC:
            return "PirTypeFeedback";
        case TYPEFEEDBACK_MAGIC:
            return "TypeFeedback";
        case POOL_STUB_MAGIC:
            return "PoolStub";
        case GENERIC_DISPATCH_TABLE_MAGIC:
            return "GenericDispatchTable";
        default:
            std::cerr << "unhandled RIR object magic: 0x" << std::hex << magic
                      << "\n";
            assert(false);
    }
}

unsigned rirObjectMagic(SEXP rirObject) {
    assert(TYPEOF(rirObject) == EXTERNALSXP && "Not a RIR object");
    return ((rir_header*)STDVEC_DATAPTR(rirObject))->magic;
}

const char* rirObjectClassName(SEXP rirObject) {
    return rirObjectClassName(rirObjectMagic(rirObject));
}

} // namespace rir