//
// Created by Jakob Hain on 5/25/23.
//

#include "CompilerClient.h"
#include "api.h"
#include "compiler_server_client_shared_utils.h"
#include "utils/ByteBuffer.h"
#include "utils/Terminal.h"
#include "utils/ctpl.h"
#include <array>
#include <zmq.hpp>

namespace rir {

using namespace ctpl;

// Thread pool to handle compiler-server requests (AKA will only wait for this
// many requests simultaneously). Right now it's #servers, because each server
// is single-threded, but if we have multi-threaded servers in the future we can
// increase.
static int NUM_THREADS;
static std::chrono::seconds PIR_CLIENT_TIMEOUT;
thread_pool* threads;

static bool didInit = false;
static zmq::context_t* context;
static std::vector<std::string> serverAddrs;
static std::vector<zmq::socket_t*> sockets;

void CompilerClient::tryInit() {
    // get the server address from the environment
    const char* serverAddrStr = getenv("PIR_CLIENT_ADDR");
    if (serverAddrStr) {
        std::cerr << "PIR_CLIENT_ADDR=" << serverAddrStr
                  << ", CompilerClient initializing..." << std::endl;
    } else {
#ifdef FORCE_LOG_COMPILER_SERVER
        std::cerr << "PIR_CLIENT_ADDR not set, CompilerClient won't initialize" << std::endl;
#endif
        return;
    }

    assert(!didInit);
    didInit = true;
    PIR_CLIENT_TIMEOUT = std::chrono::seconds(
        getenv("PIR_CLIENT_TIMEOUT") == nullptr
            ? 10
            : strtol(getenv("PIR_CLIENT_TIMEOUT"), nullptr, 10)
    );

    std::istringstream serverAddrReader(serverAddrStr);
    while (!serverAddrReader.fail()) {
        std::string serverAddr;
        std::getline(serverAddrReader, serverAddr, ',');
        if (serverAddr.empty())
            continue;
        serverAddrs.push_back(serverAddr);
    }
    NUM_THREADS = (int)serverAddrs.size();

    // initialize the thread pool
    threads = new thread_pool(NUM_THREADS);
    // initialize the zmq context
    context = new zmq::context_t(
        // We have our own thread pool, but zeromq also uses background threads.
        // Presumably the socket polls on the background while it blocks the
        // main thread for a response. Each socket runs on its own thread, and
        // ideally each socket will take one of these io_threads for any of its
        // background tasks, so that sockets won't have to wait for each other.
        NUM_THREADS,
        NUM_THREADS
    );
    // initialize the zmq sockets and connect to the servers
    for (const auto& serverAddr : serverAddrs) {
        auto socket = new zmq::socket_t(*context, zmq::socket_type::req);
        socket->connect(serverAddr);
        sockets.push_back(socket);
    }
}

CompilerClient::Handle* CompilerClient::pirCompile(SEXP what, const Context& assumptions, const std::string& name, const pir::DebugOptions& debug) {
    if (!didInit) {
      return nullptr;
    }
    return new CompilerClient::Handle(threads->push([=](int index) {
        auto socket = sockets[index];
        if (!socket->handle()) {
            const auto& serverAddr = serverAddrs[index];
            std::cerr << "CompilerClient: reconnecting to " << serverAddr << std::endl;
            socket->connect(serverAddr);
        }

        // Serialize the request
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
        ByteBuffer requestData;
        requestData.putLong(PIR_COMPILE_MAGIC);
        serialize(what, requestData);
        requestData.putLong(sizeof(Context));
        requestData.putBytes((uint8_t*)&assumptions, sizeof(Context));
        requestData.putLong(name.size());
        requestData.putBytes((uint8_t*)name.c_str(), name.size());
        requestData.putLong(sizeof(debug.flags));
        requestData.putBytes((uint8_t*)&debug.flags, sizeof(debug.flags));
        requestData.putLong(debug.passFilterString.size());
        requestData.putBytes((uint8_t*)debug.passFilterString.c_str(), debug.passFilterString.size());
        requestData.putLong(debug.functionFilterString.size());
        requestData.putBytes((uint8_t*)debug.functionFilterString.c_str(), debug.functionFilterString.size());
        requestData.putLong(sizeof(debug.style));
        requestData.putBytes((uint8_t*)&debug.style, sizeof(debug.style));
        zmq::message_t request(requestData.data(), requestData.size());

        // Send the request
        auto requestSize = *socket->send(std::move(request), zmq::send_flags::none);
        auto requestSize2 = requestData.size();
        assert(requestSize == requestSize2);
        // Wait for the response
        zmq::message_t response;
        socket->recv(response, zmq::recv_flags::none);
        // Receive the response
        // Response data format =
        //   PIR_COMPILE_RESPONSE_MAGIC
        // + serialize(what)
        // + sizeof(pirPrint)
        // + pirPrint
        ByteBuffer responseBuffer((uint8_t*)response.data(), response.size());
        auto responseMagic = responseBuffer.getLong();
        assert(responseMagic == PIR_COMPILE_RESPONSE_MAGIC);
        SEXP responseWhat = deserialize(responseBuffer);
        auto pirPrintSize = responseBuffer.getLong();
        std::string pirPrint((char*)responseBuffer.data(), pirPrintSize);
        return CompilerClient::ResponseData{responseWhat, pirPrint};
    }));
}

static void checkDiscrepancy(const std::string& localPir, const std::string& remotePir) {
    // Don't need to log if there is no discrepancy.
    if (localPir == remotePir) {
        return;
    }
    // TODO: Actually log diff
    std::cerr << console::with_red("Discrepancy between local and remote PIR")
              << std::endl;
    std::cerr << "Local PIR:\n" << localPir << "\n\n";
    std::cerr << "Remote PIR:\n" << remotePir << "\n\n";
}


void CompilerClient::Handle::compare(pir::ClosureVersion* version) {
    auto localPir = printClosureVersionForCompilerServerComparison(version);
    // Tried using a second thread-pool here but it causes "mutex lock failed:
    // Invalid argument" for `response` (and `shared_future` doesn't fix it)
    (void)std::async(std::launch::async, [=]() {
        // Wait for the response, with timeout if set
        if (PIR_CLIENT_TIMEOUT == std::chrono::seconds(0)) {
            response.wait();
        } else {
            switch (response.wait_for(PIR_CLIENT_TIMEOUT)) {
            case std::future_status::ready:
                break;
            case std::future_status::timeout:
                std::cerr << console::with_red("Timeout waiting for remote PIR")
                          << std::endl;
                return;
            case std::future_status::deferred:
                assert(false);
            }
        }
        // Get the response which is ready now, and check
        auto resp = response.get();
        checkDiscrepancy(localPir, resp.finalPir);
    });
}

} // namespace rir
