//
// Created by Jakob Hain on 5/25/23.
//

#include "CompilerClient.h"
#include "api.h"
#include "compiler_server_client_shared_utils.h"
#include "hash/RirUID.h"
#include "hash/RirUIDPool.h"
#include "hash/UUID.h"
#include "interpreter/serialize.h"
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
static std::chrono::milliseconds PIR_CLIENT_TIMEOUT;
#endif

static bool PIR_CLIENT_SKIP_DISCREPANCY_CHECK =
    getenv("PIR_CLIENT_SKIP_DISCREPANCY_CHECK") != nullptr &&
    strcmp(getenv("PIR_CLIENT_SKIP_DISCREPANCY_CHECK"), "") != 0 &&
    strcmp(getenv("PIR_CLIENT_SKIP_DISCREPANCY_CHECK"), "0") != 0;

bool CompilerClient::_isRunning = false;
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

    assert(!isRunning());
    _isRunning = true;

    std::istringstream serverAddrReader(serverAddrStr);
    while (!serverAddrReader.fail()) {
        std::string serverAddr;
        std::getline(serverAddrReader, serverAddr, ',');
        if (serverAddr.empty())
            continue;
        serverAddrs.push_back(serverAddr);
    }
#ifdef MULTI_THREADED_COMPILER_CLIENT
    PIR_CLIENT_TIMEOUT = std::chrono::milliseconds(
        getenv("PIR_CLIENT_TIMEOUT") == nullptr
            ? 10000
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

template<typename T>
CompilerClient::Handle<T>* CompilerClient::request(
        const std::function<void(ByteBuffer&)>&& makeRequest,
        const std::function<T(ByteBuffer&)>&& makeResponse) {
    if (!isRunning()) {
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
        //   from makeRequest()
        ByteBuffer request;
        makeRequest(request);

        if (request.size() >= PIR_CLIENT_COMPILE_SIZE_TO_HASH_ONLY) {
            UUID requestHash = UUID::hash(request.data(), request.size());
            // Serialize the hash-only request
            // Request data format =
            //   Request::Memoize
            // + hash
            ByteBuffer hashOnlyRequest;
            hashOnlyRequest.putLong((uint64_t)Request::Memoize);
            hashOnlyRequest.putBytes((uint8_t*)&requestHash, sizeof(requestHash));

            // Send the hash-only request
            std::cerr << "Socket " << index << " sending hashOnly request"
                      << std::endl;
            auto hashOnlyRequestSize =
                *socket->send(zmq::message_t(
                                  hashOnlyRequest.data(),
                                  hashOnlyRequest.size()),
                              zmq::send_flags::none);
            auto hashOnlyRequestSize2 = hashOnlyRequest.size();
            assert(hashOnlyRequestSize == hashOnlyRequestSize2);
            // Wait for the response
            zmq::message_t hashOnlyResponse;
            socket->recv(hashOnlyResponse, zmq::recv_flags::none);
            // Receive the response
            // Response data format =
            //   Response::NeedsFull
            // | from makeResponse()
            ByteBuffer hashOnlyResponseBuffer((uint8_t*)hashOnlyResponse.data(), hashOnlyResponse.size());
            auto hashOnlyResponseMagic = hashOnlyResponseBuffer.getLong();
            if (hashOnlyResponseMagic != Response::NeedsFull) {
                hashOnlyResponseBuffer.setReadPos(0);
                return makeResponse(hashOnlyResponseBuffer);
            }
        }

        // Send the request
        std::cerr << "Socket " << index << " sending request" << std::endl;
        auto requestSize =
            *socket->send(zmq::message_t(
                              request.data(),
                              request.size()),
                          zmq::send_flags::none);
        auto requestSize2 = request.size();
        assert(requestSize == requestSize2);
        // Wait for the response
        zmq::message_t response;
        socket->recv(response, zmq::recv_flags::none);
        // Receive the response
        // Response data format =
        //   from makeResponse()
        ByteBuffer responseBuffer((uint8_t*)response.data(), response.size());
        return makeResponse(responseBuffer);
    };
#ifdef MULTI_THREADED_COMPILER_CLIENT
    std::shared_ptr<int> socketIndexRef(new int(-1));
    return new CompilerClient::Handle<T>{socketIndexRef, threads->push([=](index) {
                                             *socketIndexRef = index;
                                             return getResponse(index);
                                         })};
#else
    auto response = getResponse(0);
    return new CompilerClient::Handle<T>{response};
#endif
}

CompilerClient::CompiledHandle* CompilerClient::pirCompile(SEXP what, const Context& assumptions, const std::string& name, const pir::DebugOptions& debug) {
    auto handle = request<CompiledResponseData>(
        [=](ByteBuffer& request) {
            // Request data format =
            //   Request::Compile
            // + sizeof(what)
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
            request.putLong((uint64_t)Request::Compile);
            serialize(what, request, false);
            request.putLong(sizeof(Context));
            request.putBytes((uint8_t*)&assumptions, sizeof(Context));
            request.putLong(name.size());
            request.putBytes((uint8_t*)name.c_str(), name.size());
            request.putLong(sizeof(debug.flags));
            request.putBytes((uint8_t*)&debug.flags, sizeof(debug.flags));
            request.putLong(debug.passFilterString.size());
            request.putBytes((uint8_t*)debug.passFilterString.c_str(),
                             debug.passFilterString.size());
            request.putLong(debug.functionFilterString.size());
            request.putBytes((uint8_t*)debug.functionFilterString.c_str(),
                             debug.functionFilterString.size());
            request.putLong(sizeof(debug.style));
            request.putBytes((uint8_t*)&debug.style, sizeof(debug.style));
        },
        [](ByteBuffer& response) {
            // Response data format =
            //   Response::Compiled
            // + sizeof(pirPrint)
            // + pirPrint
            // + hashSexp(what)
            // + serialize(what)
            auto responseMagic = response.getLong();
            assert(responseMagic == Response::Compiled);
            auto pirPrintSize = response.getLong();
            std::string pirPrint;
            pirPrint.resize(pirPrintSize);
            response.getBytes((uint8_t*)pirPrint.data(), pirPrintSize);
            RirUID responseWhatHash;
            response.getBytes((uint8_t*)&responseWhatHash, sizeof(responseWhatHash));
            // Try to get hashed if we already have the compiled value
            // (unlikely but maybe possible)
            SEXP responseWhat = RirUIDPool::get(responseWhatHash);
            if (!responseWhat) {
                // Actually deserialize
                responseWhat = deserialize(response, true, responseWhatHash);
            }
            return CompilerClient::CompiledResponseData{responseWhat, pirPrint};
        }
    );
    return handle ? new CompilerClient::CompiledHandle{handle} : nullptr;
}

SEXP CompilerClient::retrieve(const rir::RirUID& hash) {
    auto handle = request<SEXP>(
        [=](ByteBuffer& request) {
            // Request data format =
            //   Request::Retrieve
            // + hash
            request.putLong((uint64_t)Request::Retrieve);
            request.putBytes((uint8_t*)&hash, sizeof(hash));
        },
        [=](ByteBuffer& response) -> SEXP {
            // Response data format =
            //   Response::Retrieved
            // + serialize(what)
            // | Response::RetrieveFailed
            auto responseMagic = response.getLong();
            switch (responseMagic) {
            case Response::Retrieved:
                return deserialize(response, true, hash);
            case Response::RetrieveFailed:
                return nullptr;
            default:
                assert(false && "Unexpected response magic");
            }
        }
    );
#ifdef MULTI_THREADED_COMPILER_CLIENT
#error "TODO create closure which blocks until the response is ready"
#else
    auto response = handle ? handle->response : nullptr;
    delete handle;
    return response;
#endif
}

void CompilerClient::killServers() {
    assert(isRunning() && "Can't kill servers, the client isn't running");
#ifdef MULTI_THREADED_COMPILER_CLIENT
    std::cerr << "Waiting for active server requests to end" << std::endl;
    threads->stop(true);
#endif
    std::cerr << "Killing connected servers" << std::endl;
    // Send the request PIR_COMPILE_KILL_MAGIC to all servers, and check the
    // acknowledgement (we do this synchronously)
    for (size_t i = 0; i < sockets.size(); i++) {
      auto& socket = sockets[i];
      // Send the request
      auto request = Request::Kill;
      socket->send(zmq::message_t(&request, sizeof(request)),
                   zmq::send_flags::none);
      // Check the acknowledgement
      zmq::message_t response;
      socket->recv(response, zmq::recv_flags::none);
      if (response.size() != sizeof(Response::Killed) ||
          *(Response*)response.data() != Response::Killed) {
        std::cerr << "Error: server " << i << " didn't acknowledge kill request"
                  << std::endl;
      }
    }
    // Close all sockets
    for (auto& socket : sockets) {
        socket->close();
    }
    std::fill(socketsConnected.begin(), socketsConnected.end(), false);
    // Mark that we've stopped running
    _isRunning = false;
    std::cerr << "Done killing connected servers, client is no longer running" << std::endl;
}

#ifdef MULTI_THREADED_COMPILER_CLIENT
const CompiledResponseData& CompilerClient::CompiledHandle::getResponse() {
    // Wait for the response, with timeout if set
    if (PIR_CLIENT_TIMEOUT == std::chrono::milliseconds(0)) {
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
    // Get the response which is ready now
    return response.get();
}
#endif

static void normalizePir(std::string& pir) {
    // Replace addresses with 0xXXXXXXXX, since they will be different
    static const std::regex ADDRESS_REGEX("0x[0-9a-fA-F]+");
    static const char* ADDRESS_REPLACE = "0xXXXXXXXX";
    pir = std::regex_replace(pir, ADDRESS_REGEX, ADDRESS_REPLACE);
}

static void checkDiscrepancy(std::string&& localPir, std::string&& remotePir) {
    if (PIR_CLIENT_SKIP_DISCREPANCY_CHECK) {
        return;
    }
    normalizePir(localPir);
    normalizePir(remotePir);
    // Don't need to log if there's no discrepancy.
    if (localPir == remotePir) {
        return;
    }
    std::cerr << console::with_red("Discrepancy between local and remote PIR")
              << std::endl;
    // Print a fancy line-by-line diff
    std::istringstream localPirStream(localPir);
    std::istringstream remotePirStream(remotePir);
    size_t lineNum = 0;
    std::string localLine;
    std::string remoteLine;
    while (std::getline(localPirStream, localLine) &&
               std::getline(remotePirStream, remoteLine)) {
        if (localLine == remoteLine) {
            std::cerr << std::setw(4) << lineNum << localLine << std::endl;
        } else {
            std::cerr << std::setw(4) << lineNum << console::with_red(localLine) << std::endl;
            std::cerr << std::setw(4) << lineNum << console::with_green(remoteLine) << std::endl;
        }
        lineNum++;
    }
    while (std::getline(localPirStream, localLine)) {
        std::cerr << std::setw(4) << lineNum << console::with_red(localLine) << std::endl;
        lineNum++;
    }
    while (std::getline(remotePirStream, remoteLine)) {
        std::cerr << std::setw(4) << lineNum << console::with_green(remoteLine) << std::endl;
        lineNum++;
    }
}

void CompilerClient::CompiledHandle::compare(pir::ClosureVersion* version) const {
    auto localPir = printClosureVersionForCompilerServerComparison(version);
#ifdef MULTI_THREADED_COMPILER_CLIENT
    // Tried using a second thread-pool here but it causes "mutex lock failed:
    // Invalid argument" for `response` (and `shared_future` doesn't fix it)
    (void)std::async(std::launch::async, [=]() {
        auto resp = inner->getResponse();
        auto remotePir = resp.finalPir;
        checkDiscrepancy(std::move(localPir), std::move(remotePir));
    });
#else
    auto remotePir = inner->response.finalPir;
    checkDiscrepancy(std::move(localPir), std::move(remotePir));
#endif
}

/// Block and get the SEXP
SEXP CompilerClient::CompiledHandle::getSexp() const {
#ifdef MULTI_THREADED_COMPILER_CLIENT
    auto& response = inner->getResponse();
#else
    const auto& response = inner->response;
#endif
    return response.sexp;
}

const std::string& CompilerClient::CompiledHandle::getFinalPir() const {
#ifdef MULTI_THREADED_COMPILER_CLIENT
    auto& response = inner->getResponse();
#else
    const auto& response = inner->response;
#endif
    return response.finalPir;
}

} // namespace rir
