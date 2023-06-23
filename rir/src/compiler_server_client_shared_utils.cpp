//
// Created by Jakob Hain on 5/25/23.
//

#include "compiler_server_client_shared_utils.h"
#include "compiler/log/debug.h"
#include <zmq.h>

namespace rir {

bool PIR_CLIENT_DRY_RUN = getenv("PIR_CLIENT_DRY_RUN") != nullptr &&
                          strcmp(getenv("PIR_CLIENT_DRY_RUN"), "") != 0 &&
                          strcmp(getenv("PIR_CLIENT_DRY_RUN"), "0") != 0 &&
                          strcmp(getenv("PIR_CLIENT_DRY_RUN"), "false") != 0;

size_t PIR_CLIENT_COMPILE_SIZE_TO_HASH_ONLY =
    getenv("PIR_CLIENT_COMPILE_SIZE_TO_HASH_ONLY")
        ? strtol(getenv("PIR_CLIENT_COMPILE_SIZE_TO_HASH_ONLY"), nullptr, 10)
        : 1024 * 1024;

std::string printClosureVersionForCompilerServerComparison(pir::ClosureVersion* version) {
    std::stringstream pir;
    version->print(pir::DebugStyle::Standard, pir, false, false);
    return pir.str();
}

__attribute__((unused)) NORET void zeromq_error(const char* func) {
    printCBacktrace();
    std::cerr << "zeromq error in " << func << ": " << zmq_strerror(zmq_errno()) << std::endl;
    std::abort();
}

} // namespace rir
