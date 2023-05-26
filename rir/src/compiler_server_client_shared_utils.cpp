//
// Created by Jakob Hain on 5/25/23.
//

#include "compiler_server_client_shared_utils.h"
#include <zmq.h>

namespace rir {

__attribute__((unused)) NORET void zeromq_error() {
    std::cerr << "zeromq error: " << zmq_strerror(zmq_errno()) << std::endl;
    std::abort();
}

} // namespace rir
