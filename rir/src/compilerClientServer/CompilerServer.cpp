//
// Created by Jakob Hain on 5/25/23.
//

#include "CompilerServer.h"
#include "R/Printing.h"
#include "api.h"
#include "bc/Compiler.h"
#include "compiler_server_client_shared_utils.h"
#include "serializeHash/hash/UUID.h"
#include "serializeHash/hash/UUIDPool.h"
#include "serializeHash/serialize/serialize.h"
#include "utils/ByteBuffer.h"
#include "utils/measuring.h"
#include <array>
#include <zmq.hpp>

#define SOFT_ASSERT(x, msg) do {                                               \
    if (!(x)) {                                                                \
        std::cerr << "Assertion failed (client issue): " << msg << " (" << #x  \
                  << ")" << std::endl;                                         \
        break;                                                                 \
    } } while (false)

namespace rir {

static const char* PROCESSING_REQUEST_TIMER_NAME = "CompilerServer.cpp: processing request (not sending, receiving, compiling, or interning)";
static const char* SENDING_RESPONSE_TIMER_NAME = "CompilerServer.cpp: sending response";

bool CompilerServer::_isRunning = false;
static zmq::socket_t* socket;
static std::unordered_map<UUID, ByteBuffer>* memoizedRequests;

void CompilerServer::tryRun() {
    // Get the server address from the environment
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

    // Initialize the zmq context
    zmq::context_t context(
        // Only 1 thread and socket because PIR is currently single-threaded
        1,
        1
    );
    socket = new zmq::socket_t(context, zmq::socket_type::rep);
    socket->bind(serverAddr);

    // Initialize memoized requests
    memoizedRequests = new std::unordered_map<UUID, ByteBuffer>();

    _isRunning = true;
    pir::Parameter::SERIALIZE_LLVM = true;
    // _isRunning is used because of nested calls in the for loop, but CLion
    // doesn't see
    (void)_isRunning;
    // Won't return
    for (;;) {
        std::cerr << "Waiting for next request..." << std::endl;
        // Receive the request
        zmq::message_t request;
        socket->recv(request, zmq::recv_flags::none);
        std::cerr << "Got request (" << request.size() << " bytes)" << std::endl;

        Measuring::startTimerIf(pir::Parameter::PIR_MEASURE_CLIENT_SERVER, PROCESSING_REQUEST_TIMER_NAME, true);
        // Deserialize the request.
        // Request data format =
        // - Request
        // + ...
        ByteBuffer requestBuffer((uint8_t*)request.data(), request.size());
        auto magic = (Request)requestBuffer.getLong();

        // Handle Kill, Retrieved, and RetrieveFailed (not memoized) or Memoize
        switch (magic) {
        case Request::Kill: {
            // ... (end of request)
            std::cerr << "Received kill request" << std::endl;
            // Send Response::Killed
            auto response = Response::Killed;
            Measuring::countTimerIf(pir::Parameter::PIR_MEASURE_CLIENT_SERVER, PROCESSING_REQUEST_TIMER_NAME, true);
            Measuring::startTimerIf(pir::Parameter::PIR_MEASURE_CLIENT_SERVER, SENDING_RESPONSE_TIMER_NAME, true);
            socket->send(zmq::message_t(&response, sizeof(response)),
                         zmq::send_flags::none);
            Measuring::countTimerIf(pir::Parameter::PIR_MEASURE_CLIENT_SERVER, SENDING_RESPONSE_TIMER_NAME, true);
            std::cerr << "Sent kill acknowledgement, will die" << std::endl;
            _isRunning = false;
            exit(0);
        }
        case Request::Retrieved:
        case Request::RetrieveFailed:
            std::cerr << "Unexpected client-side response (" << (uint64_t)magic
                      << ") server shouldn't have or didn't send a request. "
                      << "Ignoring" << std::endl;
            continue;
        case Request::Memoize: {
            // ...
            // + UUID hash
            UUID hash;
            requestBuffer.getBytes((uint8_t*)&hash, sizeof(UUID));
            if (memoizedRequests->count(hash)) {
                std::cerr << "Found memoized result for hash (hash-only) "
                          << hash << std::endl;
                // Send the response (memoized)
                auto result = (*memoizedRequests)[hash];
                Measuring::countTimerIf(pir::Parameter::PIR_MEASURE_CLIENT_SERVER, PROCESSING_REQUEST_TIMER_NAME, true);
                Measuring::startTimerIf(pir::Parameter::PIR_MEASURE_CLIENT_SERVER, SENDING_RESPONSE_TIMER_NAME, true);
                socket->send(zmq::message_t(result.data(), result.size()),
                             zmq::send_flags::none);
                Measuring::countTimerIf(pir::Parameter::PIR_MEASURE_CLIENT_SERVER, SENDING_RESPONSE_TIMER_NAME, true);
                std::cerr << "Sent memoized result for hash (hash-only) "
                          << hash << std::endl;
            } else {
                std::cerr << "No memoized result for hash (hash-only) " << hash
                          << std::endl;
                // Send Response::NeedsFull
                auto response = Response::NeedsFull;
                Measuring::countTimerIf(pir::Parameter::PIR_MEASURE_CLIENT_SERVER, PROCESSING_REQUEST_TIMER_NAME, true);
                Measuring::startTimerIf(pir::Parameter::PIR_MEASURE_CLIENT_SERVER, SENDING_RESPONSE_TIMER_NAME, true);
                socket->send(zmq::message_t(&response, sizeof(response)),
                             zmq::send_flags::none);
                Measuring::countTimerIf(pir::Parameter::PIR_MEASURE_CLIENT_SERVER, SENDING_RESPONSE_TIMER_NAME, true);
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
        if (memoizedRequests->count(requestHash)) {
            std::cerr << "Found memoized result for hash " << requestHash << std::endl;
            // Send the response (memoized)
            auto result = (*memoizedRequests)[requestHash];
            Measuring::countTimerIf(pir::Parameter::PIR_MEASURE_CLIENT_SERVER, PROCESSING_REQUEST_TIMER_NAME, true);
            Measuring::startTimerIf(pir::Parameter::PIR_MEASURE_CLIENT_SERVER, SENDING_RESPONSE_TIMER_NAME, true);
            socket->send(zmq::message_t(
                             result.data(),
                             result.size()),
                         zmq::send_flags::none);
            Measuring::countTimerIf(pir::Parameter::PIR_MEASURE_CLIENT_SERVER, SENDING_RESPONSE_TIMER_NAME, true);
            std::cerr << "Sent memoized result for hash " << requestHash << std::endl;
            continue;
        } else {
            std::cerr << "No memoized result for hash " << requestHash << std::endl;
        }

        // Handle other request types
        SEXP what = nullptr;
        ByteBuffer response;
        switch (magic) {
        case Request::Compile: {
            std::cerr << "Received compile request" << std::endl;
            // ...
            // + serialize(what, CompilerClientSourceAndFeedback)
            // + serialize(Compiler::decompileClosure(what), CompilerClientSource)
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

            // Client won't send hashed SEXPs because it doesn't necessarily
            // remember them, and because the server doesn't care about
            // connected SEXPs like the client. However, client will send hashed
            // record_call_ SEXPs, because those are very large and we can
            // handle the case where they are forgotten by just not speculating
            // on them.
            what = deserialize(requestBuffer, SerialOptions::CompilerClientSourceAndFeedback);
            PROTECT(what);
            auto what2 = deserialize(requestBuffer, SerialOptions::CompilerClientSource);
            PROTECT(what2);
            Compiler::compileClosure(what2);

            std::stringstream differencesStream;
            DispatchTable::debugCompare(
                DispatchTable::unpack(BODY(what)),
                DispatchTable::unpack(BODY(what2)),
                differencesStream,
                false
            );
            auto differences = differencesStream.str();
            if (!differences.empty()) {
                std::cerr << "Differences when we encode code via AST and bytecode without recorded calls:"
                          << std::endl << differences << std::endl;
            }

            // No longer need to protect what, and what2 is no longer used
            UNPROTECT(2);
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
            // It's a bit confusing that debug options are passed from the
            // client. We may want this to be the case, but we also want server
            // debug options; the current solution is to merge them and take
            // whatever's overridden from either.
            debug.flags = debug.flags | pir::DebugOptions::DefaultDebugOptions.flags;
            if (pir::DebugOptions::DefaultDebugOptions.passFilterString != ".*") {
                debug.passFilterString = pir::DebugOptions::DefaultDebugOptions.passFilterString;
                debug.passFilter = pir::DebugOptions::DefaultDebugOptions.passFilter;
            }
            if (pir::DebugOptions::DefaultDebugOptions.functionFilterString != ".*") {
                debug.functionFilterString = pir::DebugOptions::DefaultDebugOptions.functionFilterString;
                debug.functionFilter = pir::DebugOptions::DefaultDebugOptions.functionFilter;
            }
            if (pir::DebugOptions::DefaultDebugOptions.style != pir::DebugStyle::Standard) {
                debug.style = pir::DebugOptions::DefaultDebugOptions.style;
            }

            Measuring::countTimerIf(pir::Parameter::PIR_MEASURE_CLIENT_SERVER, PROCESSING_REQUEST_TIMER_NAME, true);
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
            // + hashRoot(what)
            // + serialize(what, CompilerServer)
            Measuring::startTimerIf(pir::Parameter::PIR_MEASURE_CLIENT_SERVER, PROCESSING_REQUEST_TIMER_NAME, true);
            response.putLong((uint64_t)Response::Compiled);
            auto pirPrintSize = pirPrint.size();
            response.putLong(pirPrintSize);
            response.putBytes((uint8_t*)pirPrint.data(), pirPrintSize);
            auto hash = UUIDPool::getHash(what);
            response.putBytes((uint8_t*)&hash, sizeof(hash));
            serialize(what, response, SerialOptions::CompilerServer);
            break;
        }
        case Request::Retrieve: {
            std::cerr << "Received retrieve request" << std::endl;
            // ...
            // + UUID hash
            UUID hash;
            requestBuffer.getBytes((uint8_t*)&hash, sizeof(UUID));

            // Get SEXP
            what = UUIDPool::get(hash);

            // Serialize the response
            std::cerr << "Retrieve " << hash << " = ";
            if (what) {
                std::cerr << what << " " << Print::dumpSexp(what) << std::endl;

                // Response data format =
                //   Response::Retrieved
                // + serialize(what, CompilerServer)
                response.putLong((uint64_t)Response::Retrieved);
                serialize(what, response, SerialOptions::CompilerServer);
            } else {
                std::cerr << "(not found)" << std::endl;
                // Response data format =
                //   Response::RetrieveFailed
                response.putLong((uint64_t)Response::RetrieveFailed);
            }
            break;
        }
        case Request::Kill:
        case Request::Memoize:
        case Request::Retrieved:
        case Request::RetrieveFailed:
            assert(false);
        /*default:
            std::cerr << "Invalid magic: " << (uint64_t)magic << std::endl;
            break;*/
        }

        // Memoize the response
        (*memoizedRequests)[requestHash] = response;

        // Send the response;
        Measuring::countTimerIf(pir::Parameter::PIR_MEASURE_CLIENT_SERVER, PROCESSING_REQUEST_TIMER_NAME, true);
        Measuring::startTimerIf(pir::Parameter::PIR_MEASURE_CLIENT_SERVER, SENDING_RESPONSE_TIMER_NAME, true);
        size_t responseSize;
        Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_CLIENT_SERVER && what, "CompilerServer.cpp: sending new response with SEXP", what, [&]{
            responseSize = *socket->send(zmq::message_t{
                                             response.data(),
                                             response.size()},
                                         zmq::send_flags::none);
        });
        auto responseSize2 = response.size();
        SOFT_ASSERT(responseSize == responseSize2,
                    "Client didn't receive the full response");
        Measuring::countTimerIf(pir::Parameter::PIR_MEASURE_CLIENT_SERVER, SENDING_RESPONSE_TIMER_NAME, true);

        std::cerr << "Sent response (" << responseSize << " bytes)"
                  << std::endl;
    }
}

SEXP CompilerServer::retrieve(const rir::UUID& hash) {
    std::cerr << "Retrieving from client " << hash << std::endl;
    // Send the server-side request
    // Data format =
    //   Response::NeedsRetrieve
    // + UUID hash
    ByteBuffer serverRequest;
    serverRequest.putLong((uint64_t)Response::NeedsRetrieve);
    serverRequest.putBytes((uint8_t*)&hash, sizeof(UUID));
    auto serverRequestSize = serverRequest.size();
    auto serverRequestSize2 = *socket->send(zmq::message_t(
                                                serverRequest.data(),
                                                serverRequest.size()),
                                            zmq::send_flags::none);
    SOFT_ASSERT(serverRequestSize == serverRequestSize2,
                "Client didn't receive the full request");

    // Receive the client-side response
    zmq::message_t clientResponse;
    socket->recv(clientResponse, zmq::recv_flags::none);
    std::cerr << "Got client-side response (" << clientResponse.size()
              << " bytes)" << std::endl;

    // Deserialize the client-side response
    // Data format =
    // - Response
    // + ...
    ByteBuffer clientResponseBuffer((uint8_t*)clientResponse.data(), clientResponse.size());
    auto magic = (Request)clientResponseBuffer.getLong();
    switch (magic) {
    case Request::Retrieved: {
        // ...
        // + serialize(what, CompilerClientRetrieve)
        SEXP what = deserialize(clientResponseBuffer,
                                SerialOptions::CompilerClientRetrieve, hash);
        // We've already recursively interned and preserved (deserialize with
        // useHashes causes children to be interned, and retrieveHash causes
        // `what` itself to be interned. Both have preserve=true because they
        // are explicitly coded to do that when the compiler server is running)
        return what;
    }
    case Request::RetrieveFailed:
        // ...
        // (no data)
        std::cerr << "Client doesn't have the SEXP" << std::endl;
        return nullptr;
    default:
        std::cerr << "Unexpected client request or client-side response ("
                  << (uint64_t)magic << "). Ignoring" << std::endl;
        return nullptr;
    }
}

} // namespace rir
