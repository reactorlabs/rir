//
// Created by Jakob Hain on 5/25/23.
//

#include "CompilerClient.h"
#include "api.h"
#include "compiler_server_client_shared_utils.h"
#include "hash/UUID.h"
#include "utils/ByteBuffer.h"
#include "utils/Terminal.h"
#ifdef MULTI_THREADED_COMPILER_CLIENT
#include "utils/ctpl.h"
#endif
#include <array>
#include <zmq.hpp>

namespace rir {

#ifdef MULTI_THREADED_COMPILER_CLIENT
using namespace ctpl;

// Thread pool to handle compiler-server requests (AKA will only wait for this
// many requests simultaneously). Right now it's #servers, because each server
// is single-threded, but if we have multi-threaded servers in the future we can
// increase.
static int NUM_THREADS;
thread_pool* threads;
static std::chrono::seconds PIR_CLIENT_TIMEOUT;
#endif

static bool didInit = false;
static zmq::context_t* context;
static std::vector<std::string> serverAddrs;
static std::vector<zmq::socket_t*> sockets;
static std::vector<bool> socketsConnected;

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

    std::istringstream serverAddrReader(serverAddrStr);
    while (!serverAddrReader.fail()) {
        std::string serverAddr;
        std::getline(serverAddrReader, serverAddr, ',');
        if (serverAddr.empty())
            continue;
        serverAddrs.push_back(serverAddr);
    }
#ifdef MULTI_THREADED_COMPILER_CLIENT
    PIR_CLIENT_TIMEOUT = std::chrono::seconds(
        getenv("PIR_CLIENT_TIMEOUT") == nullptr
            ? 10
            : strtol(getenv("PIR_CLIENT_TIMEOUT"), nullptr, 10)
    );
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
#else
    assert(serverAddrs.size() == 1 &&
           "can't have multiple servers without multi-threaded client");
    context = new zmq::context_t(1, 1);
#endif

    // initialize the zmq sockets and connect to the servers
    for (const auto& serverAddr : serverAddrs) {
        auto socket = new zmq::socket_t(*context, zmq::socket_type::req);
        socket->connect(serverAddr);
        sockets.push_back(socket);
        socketsConnected.push_back(true);
    }
}

CompilerClient::Handle* CompilerClient::pirCompile(SEXP what, const Context& assumptions, const std::string& name, const pir::DebugOptions& debug) {
    if (!didInit) {
      return nullptr;
    }
    auto getResponse = [=](int index) {
        auto socket = sockets[index];
        auto socketConnected = socketsConnected[index];
        if (!socket->handle()) {
            std::cerr << "CompilerClient: socket closed" << std::endl;
            *socket = zmq::socket_t(*context, zmq::socket_type::req);
            socketConnected = false;
        }
        if (!socketConnected) {
            const auto& serverAddr = serverAddrs[index];
            std::cerr << "CompilerClient: reconnecting to " << serverAddr
                      << std::endl;
            socket->connect(serverAddr);
            socketsConnected[index] = true;
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
        requestData.putBytes((uint8_t*)debug.passFilterString.c_str(),
                             debug.passFilterString.size());
        requestData.putLong(debug.functionFilterString.size());
        requestData.putBytes((uint8_t*)debug.functionFilterString.c_str(),
                             debug.functionFilterString.size());
        requestData.putLong(sizeof(debug.style));
        requestData.putBytes((uint8_t*)&debug.style, sizeof(debug.style));

        if (requestData.size() >= PIR_COMPILE_SIZE_TO_HASH_ONLY) {
            UUID requestHash = UUID::hash(requestData.data(), requestData.size());
            // Serialize the hash-only request
            // Request data format =
            //   PIR_COMPILE_HASH_ONLY_MAGIC
            // + hash
            ByteBuffer hashOnlyRequestData;
            hashOnlyRequestData.putLong(PIR_COMPILE_HASH_ONLY_MAGIC);
            hashOnlyRequestData.putBytes((uint8_t*)&requestHash, sizeof(requestHash));

            // Send the hash-only request
            std::cerr << "Socket " << index << " sending hashOnly request"
                      << std::endl;
            zmq::message_t hashOnlyRequest(hashOnlyRequestData.data(), hashOnlyRequestData.size());
            auto hashOnlyRequestSize =
                *socket->send(std::move(hashOnlyRequest), zmq::send_flags::none);
            auto hashOnlyRequestSize2 = hashOnlyRequestData.size();
            assert(hashOnlyRequestSize == hashOnlyRequestSize2);
            // Wait for the response
            zmq::message_t hashOnlyResponse;
            socket->recv(hashOnlyResponse, zmq::recv_flags::none);
            // Receive the response
            // Response data format =
            //   PIR_COMPILE_RESPONSE_MAGIC
            // + serialize(what)
            // + sizeof(pirPrint)
            // + pirPrint
            // | PIR_COMPILE_HASH_ONLY_RESPONSE_FAILURE_MAGIC
            ByteBuffer hashOnlyResponseBuffer((uint8_t*)hashOnlyResponse.data(), hashOnlyResponse.size());
            auto hashOnlyResponseMagic = hashOnlyResponseBuffer.getLong();
            switch (hashOnlyResponseMagic) {
            case PIR_COMPILE_RESPONSE_MAGIC: {
                SEXP hashOnlyResponseWhat = deserialize(hashOnlyResponseBuffer);
                auto pirPrintSize = hashOnlyResponseBuffer.getLong();
                std::string pirPrint((char*)hashOnlyResponseBuffer.data(),
                                     pirPrintSize);
                return CompilerClient::ResponseData{hashOnlyResponseWhat,
                                                    pirPrint};
            }
            case PIR_COMPILE_HASH_ONLY_RESPONSE_FAILURE_MAGIC:
                break;
            default:
                assert(false && "invalid hash-only response magic");
            }
        }

        // Send the request
        zmq::message_t request(requestData.data(), requestData.size());
        std::cerr << "Socket " << index << " sending request" << std::endl;
        auto requestSize =
            *socket->send(std::move(request), zmq::send_flags::none);
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
    };
#ifdef MULTI_THREADED_COMPILER_CLIENT
    std::shared_ptr<int> socketIndexRef(new int(-1));
    return new CompilerClient::Handle{socketIndexRef, threads->push([=](index) {
        *socketIndexRef = index;
        return getResponse(index);
    })};
#else
    auto response = getResponse(0);
    return new CompilerClient::Handle{response};
#endif
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


void CompilerClient::Handle::compare(pir::ClosureVersion* version) const {
    auto localPir = printClosureVersionForCompilerServerComparison(version);
#ifdef MULTI_THREADED_COMPILER_CLIENT
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
            case std::future_status::timeout: {
                std::cerr << console::with_red("Timeout waiting for remote PIR")
                          << std::endl;
                // Disconnect because the server probably crashed, and we want
                // to be able to restart without restarting the client; it will
                // attempt to reconnect before sending the next request
                auto socketIndex = *socketIndexRef;
                if (socketIndex != -1) {
                    std::cerr << "Disconnecting " << socketIndex << ", will reconnect on next request" << std::endl;
                    auto socket = sockets[socketIndex];
                    auto socketAddr = serverAddrs[socketIndex];
                    socket->disconnect(socketAddr);
                    socketsConnected[socketIndex] = false;
                }
                return;
            }
            case std::future_status::deferred:
                assert(false);
            }
        }
        // Get the response which is ready now, and check
        auto resp = response.get();
        checkDiscrepancy(localPir, resp.finalPir);
    });
#else
    checkDiscrepancy(localPir, response.finalPir);
#endif
}

} // namespace rir
