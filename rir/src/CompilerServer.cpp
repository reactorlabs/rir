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
        std::cerr << "PIR_SERVER_ADDR not set, CompilerServer won't initialize" << std::endl;
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
        zmq::message_t requestData;
        socket.recv(requestData, zmq::recv_flags::none);
        std::cerr << "Got request (" << requestData.size() << " bytes)" << std::endl;

        // Deserialize the request
        // Request data format =
        //   PIR_COMPILE_MAGIC
        // + sizeof(what)
        // + what
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
        ByteBuffer requestBuffer((uint8_t*)requestData.data(), requestData.size());
        assert(requestBuffer.getLong() == PIR_COMPILE_MAGIC && "Invalid request magic");
        auto whatSize = requestBuffer.getLong();
        assert(whatSize == sizeof(SEXP) && "Invalid what (compile closure) size");
        SEXP what;
        requestBuffer.getBytes((uint8_t*)&what, whatSize);
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

        // TODO: Actually deserialize what (won't be an SEXP) and call this or/
        //     something else to compile
        // pirCompile(what, assumptions, name, debug);
        (void)what; (void)assumptions; (void)name; (void)debug;

        // Send the response
        // TODO: Again, actually send something in a response data format
        //     (this just sends a dummy message to verify request and response
        //     are transmitted and are paired correctly)
        zmq::message_t response(&what, sizeof(what));
        auto responseSize = *socket.send(std::move(response), zmq::send_flags::none);
        std::cerr << "Sent response (" << responseSize << " bytes)" << std::endl;
    }
}

} // namespace rir