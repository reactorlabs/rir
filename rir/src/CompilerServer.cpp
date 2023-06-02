//
// Created by Jakob Hain on 5/25/23.
//

#include "CompilerServer.h"
#include "api.h"
#include "compiler_server_client_shared_utils.h"
#include "utils/ByteBuffer.h"
#include "utils/ctpl.h"
#include <array>
#include <zmq.hpp>

namespace rir {

using namespace ctpl;

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

    // Won't return
    for (;;) {
        std::cerr << "Waiting for next request..." << std::endl;
        // Receive the request
        zmq::message_t request;
        socket.recv(request, zmq::recv_flags::none);
        std::cerr << "Got request (" << request.size() << " bytes)" << std::endl;

        // Deserialize the request
        // Request data format =
        //   PIR_COMPILE_MAGIC
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
        assert(magic == PIR_COMPILE_MAGIC && "Invalid request magic");
        SEXP what = deserialize(requestBuffer);
        auto assumptionsSize = requestBuffer.getLong();
        assert(assumptionsSize == sizeof(Context) && "Invalid assumptions size");
        Context assumptions;
        requestBuffer.getBytes((uint8_t*)&assumptions, assumptionsSize);
        auto nameSize = requestBuffer.getLong();
        std::string name;
        name.resize(nameSize);
        requestBuffer.getBytes((uint8_t*)name.data(), nameSize);
        auto debugFlagsSize = requestBuffer.getLong();
        assert(debugFlagsSize == sizeof(pir::DebugOptions::DebugFlags) && "Invalid debug flags size");
        pir::DebugOptions::DebugFlags debugFlags;
        requestBuffer.getBytes((uint8_t*)&debugFlags, debugFlagsSize);
        auto passFilterStringSize = requestBuffer.getLong();
        std::string passFilterString;
        passFilterString.resize(passFilterStringSize);
        requestBuffer.getBytes((uint8_t*)passFilterString.data(), passFilterStringSize);
        auto functionFilterStringSize = requestBuffer.getLong();
        std::string functionFilterString;
        functionFilterString.resize(functionFilterStringSize);
        requestBuffer.getBytes((uint8_t*)functionFilterString.data(), functionFilterStringSize);
        auto debugStyleSize = requestBuffer.getLong();
        assert(debugStyleSize == sizeof(pir::DebugStyle) && "Invalid debug style size");
        pir::DebugStyle debugStyle;
        requestBuffer.getBytes((uint8_t*)&debugStyle, debugStyleSize);
        pir::DebugOptions debug(debugFlags, passFilterString, functionFilterString, debugStyle);

        std::string pirPrint;
        pirCompile(what, assumptions, name, debug, &pirPrint);

        // Send the response
        // Response data format =
        //   PIR_COMPILE_RESPONSE_MAGIC
        // + serialize(what)
        // + sizeof(pirPrint)
        // + pirPrint
        ByteBuffer responseBuffer;
        responseBuffer.putLong(PIR_COMPILE_RESPONSE_MAGIC);
        serialize(what, responseBuffer);
        auto pirPrintSize = pirPrint.size();
        responseBuffer.putLong(pirPrintSize);
        responseBuffer.putBytes((uint8_t*)pirPrint.data(), pirPrintSize);
        zmq::message_t response(responseBuffer.data(), requestBuffer.size());
        auto responseSize = *socket.send(std::move(response), zmq::send_flags::none);
        auto responseSize2 = responseBuffer.size();
        assert(responseSize == responseSize2);
        std::cerr << "Sent response (" << responseSize << " bytes)" << std::endl;
    }
}

} // namespace rir
