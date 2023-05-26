//
// Created by Jakob Hain on 5/25/23.
//

#include "compiler_server_client_shared_utils.h"
#include "compiler/log/debug.h"
#include <zmq.h>

namespace rir {

std::string printClosureVersionForCompilerServerComparison(pir::ClosureVersion* version) {
    std::stringstream pir;
    version->print(pir::DebugStyle::Standard, pir, true, false);
    return pir.str();
}

__attribute__((unused)) NORET void zeromq_error(const char* func) {
    printCBacktrace();
    std::cerr << "zeromq error in " << func << ": " << zmq_strerror(zmq_errno()) << std::endl;
    std::abort();
}

} // namespace rir
