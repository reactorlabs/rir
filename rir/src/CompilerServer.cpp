//
// Created by Jakob Hain on 5/25/23.
//

#include "CompilerServer.h"
#include "api.h"
#include "compiler_server_client_shared_utils.h"
#include "hash/UUID.h"
#include "utils/ByteBuffer.h"
#include "utils/ctpl.h"
#include <array>
#include <zmq.hpp>

#define SOFT_ASSERT(x, msg) do {                                               \
    if (!(x)) {                                                                \
        std::cerr << "Assertion failed (client issue): " << msg << " (" << #x  \
                  << ")" << std::endl;                                         \
        break;                                                                 \
    } } while (false)

namespace rir {

using namespace ctpl;

bool CompilerServer::_isRunning = false;
static std::unordered_map<UUID, ByteBuffer> memoized;

void CompilerServer::tryRun() {
    // get the server address from the environment
    const char* serverAddr = getenv("PIR_SERVER_ADDR");
    if (serverAddr) {
        std::cerr << "PIR_SERVER_ADDR=" << serverAddr
                  << ", CompilerServer initializing..." << std::endl;
    } else {
#ifdef FORCE_LOG_COMPILER_SERVER
        std::cerr << "PIR_SERVER_ADDR not set, CompilerServer won't initialize" << std::endl;
#endif
        return;
    }

    // initialize the zmq context
    zmq::context_t context(
        // Only 1 thread and socket because PIR is currently single-threaded
        1,
        1
    );
    zmq::socket_t socket(context, zmq::socket_type::rep);
    socket.bind(serverAddr);

    _isRunning = true;
    // _isRunning is used because of nested calls in the for loop, but CLion
    // doesn't see
    (void)_isRunning;
    // Won't return
    for (;;) {
        std::cerr << "Waiting for next request..." << std::endl;
        // Receive the request
        zmq::message_t request;
        socket.recv(request, zmq::recv_flags::none);
        std::cerr << "Got request (" << request.size() << " bytes)" << std::endl;

        // Deserialize the request
        // Request data format =
        //   PIR_COMPILE_HASH_ONLY_MAGIC
        // + hash
        // | PIR_COMPILE_MAGIC
        // + serialize(what)
        // + sizeof(assumptions) (always 8)
        // + assumptions
        // + sizeof(name)
        // + name
        // + sizeof(debug.flags) (always 4)
        // + debug.flags
        // + sizeof(debug.passFilterString)
        // + debug.passFilterString
        // + sizeof(debug.functionFilterString)
        // + debug.functionFilterString
        // + sizeof(debug.style) (always 4)
        // + debug.style
        ByteBuffer requestBuffer((uint8_t*)request.data(), request.size());
        auto magic = requestBuffer.getLong();
        switch (magic) {
        case PIR_COMPILE_KILL_MAGIC: {
            std::cerr << "Received kill request" << std::endl;
            socket.send(zmq::message_t(
                            &PIR_COMPILE_KILL_ACKNOWLEDGEMENT_MAGIC,
                            sizeof(PIR_COMPILE_KILL_ACKNOWLEDGEMENT_MAGIC)),
                        zmq::send_flags::none);
            std::cerr << "Sent kill acknowledgement, will die" << std::endl;
            _isRunning = false;
            exit(0);
        }
        case PIR_COMPILE_HASH_ONLY_MAGIC: {
            UUID hash;
            requestBuffer.getBytes((uint8_t*)&hash, sizeof(UUID));
            if (memoized.count(hash)) {
                std::cerr << "Found memoized result for hash (hash-only) " << hash << std::endl;
                auto result = memoized[hash];
                socket.send(zmq::message_t(result.data(), result.size()),
                            zmq::send_flags::none);
                std::cerr << "Sent memoized result for hash (hash-only) " << hash << std::endl;
            } else {
                std::cerr << "No memoized result for hash (hash-only) " << hash << std::endl;
                socket.send(zmq::message_t(
                                &PIR_COMPILE_HASH_ONLY_RESPONSE_FAILURE_MAGIC,
                                sizeof(PIR_COMPILE_HASH_ONLY_RESPONSE_FAILURE_MAGIC)),
                            zmq::send_flags::none);
                std::cerr << "Sent request full for hash (hash-only) " << hash << std::endl;
            }
            break;
        }
        case PIR_COMPILE_MAGIC: {
            // Check if we memoized
            UUID requestHash = UUID::hash(request.data(), request.size());
            if (memoized.count(requestHash)) {
                std::cerr << "Found memoized result for hash " << requestHash << std::endl;
                auto result = memoized[requestHash];
                socket.send(zmq::message_t(
                                result.data(),
                                result.size()),
                            zmq::send_flags::none);
                std::cerr << "Sent memoized result for hash " << requestHash << std::endl;
                break;
            } else {
                std::cerr << "No memoized result for hash " << requestHash << std::endl;
            }

            SEXP what = deserialize(requestBuffer);
            auto assumptionsSize = requestBuffer.getLong();
            SOFT_ASSERT(assumptionsSize == sizeof(Context),
                        "Invalid assumptions size");
            Context assumptions;
            requestBuffer.getBytes((uint8_t*)&assumptions, assumptionsSize);
            auto nameSize = requestBuffer.getLong();
            std::string name;
            name.resize(nameSize);
            requestBuffer.getBytes((uint8_t*)name.data(), nameSize);
            auto debugFlagsSize = requestBuffer.getLong();
            SOFT_ASSERT(debugFlagsSize == sizeof(pir::DebugOptions::DebugFlags),
                        "Invalid debug flags size");
            pir::DebugOptions::DebugFlags debugFlags;
            requestBuffer.getBytes((uint8_t*)&debugFlags, debugFlagsSize);
            auto passFilterStringSize = requestBuffer.getLong();
            std::string passFilterString;
            passFilterString.resize(passFilterStringSize);
            requestBuffer.getBytes((uint8_t*)passFilterString.data(),
                                   passFilterStringSize);
            auto functionFilterStringSize = requestBuffer.getLong();
            std::string functionFilterString;
            functionFilterString.resize(functionFilterStringSize);
            requestBuffer.getBytes((uint8_t*)functionFilterString.data(),
                                   functionFilterStringSize);
            auto debugStyleSize = requestBuffer.getLong();
            SOFT_ASSERT(debugStyleSize == sizeof(pir::DebugStyle),
                        "Invalid debug style size");
            pir::DebugStyle debugStyle;
            requestBuffer.getBytes((uint8_t*)&debugStyle, debugStyleSize);
            pir::DebugOptions debug(debugFlags, passFilterString,
                                    functionFilterString, debugStyle);

            std::string pirPrint;
            what = pirCompile(what, assumptions, name, debug, &pirPrint);

            // Serialize the response
            // Response data format =
            //   PIR_COMPILE_RESPONSE_MAGIC
            // + serialize(what)
            // + sizeof(pirPrint)
            // + pirPrint
            ByteBuffer response;
            response.putLong(PIR_COMPILE_RESPONSE_MAGIC);
            serialize(what, response);
            auto pirPrintSize = pirPrint.size();
            response.putLong(pirPrintSize);
            response.putBytes((uint8_t*)pirPrint.data(), pirPrintSize);

            // Memoize the response
            memoized[requestHash] = response;

            // Send the response;
            auto responseSize =
                *socket.send(zmq::message_t(
                                 response.data(),
                                 response.size()),
                             zmq::send_flags::none);
            auto responseSize2 = response.size();
            SOFT_ASSERT(responseSize == responseSize2,
                        "Client didn't receive the full response");

            std::cerr << "Sent response (" << responseSize << " bytes)"
                      << std::endl;
            break;
        }
        default:
            std::cerr << "Invalid magic: " << magic << std::endl;
            break;
        }
    }
}

} // namespace rir
