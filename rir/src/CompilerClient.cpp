//
// Created by Jakob Hain on 5/25/23.
//

#include "CompilerClient.h"
#include "compiler_server_client_shared_utils.h"
#include "utils/ByteBuffer.h"
#include "utils/ctpl.h"
#include <array>
#include <zmq.h>
#include <zmq.hpp>

namespace rir {

using namespace ctpl;

// Thread pool to handle compiler-server requests (AKA will only wait for this
// many requests simultaneously). Right now it's #servers, because each server
// is single-threded, but if we have multi-threaded servers in the future we can
// increase.
static int NUM_THREADS;
thread_pool* threads;
thread_pool* compareThreads;

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
    // initialize another thread pool for handles to wait for and compare their
    // results
    compareThreads = new thread_pool(NUM_THREADS);
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

CompilerClient::Handle CompilerClient::pirCompile(SEXP what, const Context& assumptions, const std::string& name, const pir::DebugOptions& debug) {
    return {threads->push([&](int index) {
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
        // + sizeof(debug)
        // + debug.flags (4 bytes)
        // + debug.passFilterString
        // + debug.functionFilterString
        // + debug.style (sizeof(DebugStyle) bytes)
        const size_t messageSize =
            sizeof(PIR_COMPILE_MAGIC) +
            sizeof(size_t) +
            sizeof(uint64_t) +
            sizeof(size_t) +
            sizeof(Context) +
            sizeof(size_t) +
            name.size() +
            sizeof(size_t) +
            sizeof(debug.flags) +
            debug.passFilterString.size() +
            debug.functionFilterString.size() +
            sizeof(debug.style);
        ByteBuffer messageData(messageSize);
        messageData.putLong(PIR_COMPILE_MAGIC);
        messageData.putLong(sizeof(SEXP));
        messageData.putBytes((uint8_t*)&what, sizeof(SEXP));
        messageData.putLong(sizeof(Context));
        messageData.putBytes((uint8_t*)&assumptions, sizeof(Context));
        messageData.putLong(name.size());
        messageData.putBytes((uint8_t*)name.c_str(), name.size());
        messageData.putLong(sizeof(debug.flags) + debug.passFilterString.size() + debug.functionFilterString.size() + sizeof(debug.style));
        messageData.putBytes((uint8_t*)&debug.flags, sizeof(debug.flags));
        messageData.putBytes((uint8_t*)debug.passFilterString.c_str(), debug.passFilterString.size());
        messageData.putBytes((uint8_t*)debug.functionFilterString.c_str(), debug.functionFilterString.size());
        messageData.putBytes((uint8_t*)&debug.style, sizeof(debug.style));
        assert(messageData.bytesRemaining() == 0);
        zmq::const_buffer message = zmq::buffer(messageData.data(), messageSize);

        // Send the request
        auto reqSize = socket->send(message, zmq::send_flags::none);
        // has_value() == false iff request didn't send correctly
        assert(reqSize.has_value() && *reqSize == messageSize);
        // Wait for the response
        zmq::message_t response;
        auto respSize = socket->recv(response, zmq::recv_flags::none);
        // has_value() == false iff response didn't receive correctly
        assert(respSize.has_value());
        // TODO: Deserialize final PIR and closure version from response
        return CompilerClient::ResponseData{nullptr, ""};
    })};
}

void CompilerClient::Handle::compare(pir::Log& log, pir::ClosureVersion* version) {
    auto versionLog = log.get(version);
    auto localPir = printClosureVersionForCompilerServerComparison(version);
    compareThreads->push([&](int index) {
        response.wait();
        auto resp = response.get();
        versionLog.checkDiscrepancy(localPir, resp.finalPir);
    });
}

} // namespace rir