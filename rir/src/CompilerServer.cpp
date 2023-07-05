//
// Created by Jakob Hain on 5/25/23.
//

#include "CompilerServer.h"
#include "api.h"
#include "compiler_server_client_shared_utils.h"
#include "compiler/parameter.h"
#include "hash/UUID.h"
#include "hash/UUIDPool.h"
#include "interpreter/serialize.h"
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
static std::unordered_map<UUID, ByteBuffer> memoizedRequests;

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
    pir::Parameter::DEBUG_SERIALIZE_LLVM = true;
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

        // Deserialize the request.
        // Request data format =
        // - Request
        // + ...
        ByteBuffer requestBuffer((uint8_t*)request.data(), request.size());
        auto magic = (Request)requestBuffer.getLong();

        // Handle Kill (not memoized) or Memoize
        switch (magic) {
        case Request::Kill: {
            // ... (end of request)
            std::cerr << "Received kill request" << std::endl;
            // Send Response::Killed
            auto response = Response::Killed;
            socket.send(zmq::message_t(&response, sizeof(response)),
                        zmq::send_flags::none);
            std::cerr << "Sent kill acknowledgement, will die" << std::endl;
            _isRunning = false;
            exit(0);
        }
        case Request::Memoize: {
            // ...
            // + UUID hash
            UUID hash;
            requestBuffer.getBytes((uint8_t*)&hash, sizeof(UUID));
            if (memoizedRequests.count(hash)) {
                std::cerr << "Found memoized result for hash (hash-only) "
                          << hash << std::endl;
                // Send the response (memoized)
                auto result = memoizedRequests[hash];
                socket.send(zmq::message_t(result.data(), result.size()),
                            zmq::send_flags::none);
                std::cerr << "Sent memoized result for hash (hash-only) "
                          << hash << std::endl;
            } else {
                std::cerr << "No memoized result for hash (hash-only) " << hash
                          << std::endl;
                // Send Response::NeedsFull
                auto response = Response::NeedsFull;
                socket.send(zmq::message_t(&response, sizeof(response)),
                            zmq::send_flags::none);
                std::cerr << "Sent request full for hash (hash-only) " << hash
                          << std::endl;
            }
            continue;
        }
        default:
            break;
        }

        // Handle if we memoized
        UUID requestHash = UUID::hash(request.data(), request.size());
        if (memoizedRequests.count(requestHash)) {
            std::cerr << "Found memoized result for hash " << requestHash << std::endl;
            // Send the response (memoized)
            auto result = memoizedRequests[requestHash];
            socket.send(zmq::message_t(
                            result.data(),
                            result.size()),
                        zmq::send_flags::none);
            std::cerr << "Sent memoized result for hash " << requestHash << std::endl;
            continue;
        } else {
            std::cerr << "No memoized result for hash " << requestHash << std::endl;
        }

        // Handle other request types
        ByteBuffer response;
        switch (magic) {
        case Request::Compile: {
            std::cerr << "Received compile request" << std::endl;
            // ...
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

            // Client won't sent hashed SEXPs because it doesn't necessarily
            // remember them, and because the server doesn't care about
            // connected SEXPs like the client; the only thing duplicate SEXPs
            // may cause is wasted memory, but since we're on the server and
            // preserving everything this is less of an issue.
            SEXP what = deserialize(requestBuffer, false);
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

            // Intern, not because we'll have reused it (highly unlikely since
            // we memoize requests, and it doesn't affect anything anyways), but
            // because we want to store it in the UUID pool for Retrieve requests
            // (since we memoize requests) so that compiler client can retrieve
            // it later
            UUIDPool::intern(what, true, true);

            // Serialize the response
            // Response data format =
            //   Response::Compiled
            // + sizeof(pirPrint)
            // + pirPrint
            // + hashSexp(what)
            // + serialize(what)
            response.putLong((uint64_t)Response::Compiled);
            auto pirPrintSize = pirPrint.size();
            response.putLong(pirPrintSize);
            response.putBytes((uint8_t*)pirPrint.data(), pirPrintSize);
            auto hash = UUIDPool::getHash(what);
            response.putBytes((uint8_t*)&hash, sizeof(UUID));
            serialize(what, response, true);
            break;
        }
        case Request::Retrieve: {
            std::cerr << "Received retrieve request" << std::endl;
            // ...
            // + UUID hash
            UUID hash;
            requestBuffer.getBytes((uint8_t*)&hash, sizeof(UUID));

            // Get SEXP
            SEXP what = UUIDPool::get(hash);

            // Serialize the response
            std::cerr << "Retrieve" << hash << " = ";
            if (what) {
                std::cerr << what << std::endl;
                Rf_PrintValue(what);
                // Response data format =
                //   Response::Retrieved
                // + serialize(what)
                response.putLong(Response::Retrieved);
                serialize(what, response, true);
            } else {
                std::cerr << " (not found)" << std::endl;
                // Response data format =
                //   Response::RetrieveFailed
                response.putLong(Response::RetrieveFailed);
            }
            break;
        }
        case Request::Kill:
        case Request::Memoize:
            assert(false);
        /*default:
            std::cerr << "Invalid magic: " << (uint64_t)magic << std::endl;
            break;*/
        }

        // Memoize the response
        memoizedRequests[requestHash] = response;

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
    }
}

} // namespace rir
