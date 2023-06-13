//
// Created by Jakob Hain on 5/25/23.
//

#include "CompilerClient.h"
#include "compiler_server_client_shared_utils.h"
#include "utils/ByteBuffer.h"
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
thread_pool* threads;

static bool didInit = false;
static zmq::context_t* context;
static std::vector<zmq::socket_t*> sockets;

void CompilerClient::tryInit() {
    // get the server address from the environment
    const char* serverAddrStr = getenv("PIR_CLIENT_ADDR");
    if (serverAddrStr) {
        std::cerr << "PIR_CLIENT_ADDR=" << serverAddrStr
                  << ", CompilerClient initializing..." << std::endl;
    } else {
        std::cerr << "PIR_CLIENT_ADDR not set, CompilerClient won't initialize" << std::endl;
        return;
    }

    assert(!didInit);
    didInit = true;

    std::istringstream serverAddrReader(serverAddrStr);
    std::vector<std::string> serverAddrs;
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
        const size_t requestSize =
            sizeof(PIR_COMPILE_MAGIC) +
            sizeof(size_t) +
            sizeof(uint64_t) +
            sizeof(size_t) +
            sizeof(Context) +
            sizeof(size_t) +
            name.size() +
            sizeof(size_t) +
            sizeof(debug.flags) +
            sizeof(size_t) +
            debug.passFilterString.size() +
            sizeof(size_t) +
            debug.functionFilterString.size() +
            sizeof(size_t) +
            sizeof(debug.style);
        ByteBuffer requestData(requestSize);
        requestData.putLong(PIR_COMPILE_MAGIC);
        requestData.putLong(sizeof(SEXP));
        requestData.putBytes((uint8_t*)&what, sizeof(SEXP));
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
        zmq::message_t request(requestData.data(), requestSize);

        // Send the request
        auto requestSize2 = *socket->send(std::move(request), zmq::send_flags::none);
        assert(requestSize2 == requestSize);
        // Wait for the response
        zmq::message_t response;
        auto responseSize = *socket->recv(response, zmq::recv_flags::none);
        assert(responseSize == response.size());
        // TODO: Actually deserialize final PIR and closure version from
        //     response (this just receives a dummy message to verify request
        //     and response are transmitted and paired correctly)
        assert(responseSize == sizeof(SEXP));
        SEXP responseWhat = *(SEXP*)response.data();
        assert(responseWhat == what && "PIR compiler server response doesn't match request");
        return CompilerClient::ResponseData{nullptr, std::string("hello ") + std::to_string((uint64_t)what)};
    }));
}

static void checkDiscrepancy(const std::string& localPir, const std::string& remotePir) {
    // Don't need to log if there is no discrepancy.
    if (localPir == remotePir) {
        return;
    }
    // TODO: Actually log diff
    std::cerr << "Discrepancy between local and remote PIR\n";
    std::cerr << "Local PIR:\n" << localPir << "\n\n";
    std::cerr << "Remote PIR:\n" << remotePir << "\n\n";
}


void CompilerClient::Handle::compare(pir::ClosureVersion* version) {
    auto localPir = printClosureVersionForCompilerServerComparison(version);
    // Tried using a second thread-pool here but it causes "mutex lock failed:
    // Invalid argument" for `response` (and `shared_future` doesn't fix it)
    std::async(std::launch::async, [=]() {
        response.wait();
        auto resp = response.get();
        checkDiscrepancy(localPir, resp.finalPir);
    });
}

} // namespace rir
