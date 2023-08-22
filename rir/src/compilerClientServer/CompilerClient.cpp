//
// Created by Jakob Hain on 5/25/23.
//

#include "CompilerClient.h"
#include "api.h"
#include "compiler_server_client_shared_utils.h"
#include "serializeHash/hash/UUID.h"
#include "serializeHash/hash/UUIDPool.h"
#include "serializeHash/serialize/serialize.h"
#include "utils/ByteBuffer.h"
#include "utils/Terminal.h"
#include "utils/measuring.h"
#ifdef MULTI_THREADED_COMPILER_CLIENT
#include "utils/ctpl.h"
#endif
#include "R/Printing.h"
#include "bc/Compiler.h"
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

#define LOG_DETAILED(stmt) if (pir::Parameter::PIR_LOG_COMPILER_PEER_DETAILED) stmt
// Arrows are different directions than CompilerServer.cpp, since we send
// requests and receive responses, receive server requests and send client
// responses
#define LOG_REQUEST(message) LOG_DETAILED(std::cerr << "  >> " << message << std::endl)
#define LOG_RESPONSE(message) LOG_DETAILED(std::cerr << "  << " << message << std::endl)
#define LOG_SERVER_REQUEST(message) LOG_DETAILED(std::cerr << "  <<< " << message << std::endl)
#define LOG_CLIENT_RESPONSE(message) LOG_DETAILED(std::cerr << "  >>> " << message << std::endl)
#define LOG(stmt) if (pir::Parameter::PIR_LOG_COMPILER_PEER_DETAILED || pir::Parameter::PIR_LOG_COMPILER_CLIENT) stmt
#define LOG_WARN(stmt) if (pir::Parameter::PIR_LOG_COMPILER_PEER_DETAILED || pir::Parameter::PIR_LOG_COMPILER_CLIENT || pir::Parameter::PIR_WARN_COMPILER_CLIENT) stmt

static const char* SENDING_REQUEST_TIMER_NAME = "CompilerClient.cpp: sending request";
static const char* RECEIVING_RESPONSE_TIMER_NAME = "CompilerClient.cpp: receiving response";
static const char* RETRIEVE_TIMER_NAME = "CompilerClient.cpp: retriving SEXP";

static bool PIR_CLIENT_SKIP_DISCREPANCY_CHECK =
    getenv("PIR_CLIENT_SKIP_DISCREPANCY_CHECK") != nullptr &&
    strcmp(getenv("PIR_CLIENT_SKIP_DISCREPANCY_CHECK"), "") != 0 &&
    strcmp(getenv("PIR_CLIENT_SKIP_DISCREPANCY_CHECK"), "0") != 0;

bool CompilerClient::_isRunning = false;
static zmq::context_t* context;
static std::vector<std::string>* serverAddrs;
static std::vector<zmq::socket_t*>* sockets;
static std::vector<bool>* socketsConnected;

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

    serverAddrs = new std::vector<std::string>();
    std::istringstream serverAddrReader(serverAddrStr);
    while (!serverAddrReader.fail()) {
        std::string serverAddr;
        std::getline(serverAddrReader, serverAddr, ',');
        if (serverAddr.empty())
            continue;
        serverAddrs->push_back(serverAddr);
    }
#ifdef MULTI_THREADED_COMPILER_CLIENT
    PIR_CLIENT_TIMEOUT = std::chrono::milliseconds(
        getenv("PIR_CLIENT_TIMEOUT") == nullptr
            ? 10000
            : strtol(getenv("PIR_CLIENT_TIMEOUT"), nullptr, 10)
    );
    NUM_THREADS = (int)serverAddrs->size();
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
    assert(serverAddrs->size() == 1 &&
           "can't have multiple servers without multi-threaded client");
    context = new zmq::context_t(1, 1);
#endif

    // initialize the zmq sockets and connect to the servers
    sockets = new std::vector<zmq::socket_t*>();
    socketsConnected = new std::vector<bool>();
    for (const auto& serverAddr : *serverAddrs) {
        auto socket = new zmq::socket_t(*context, zmq::socket_type::req);
        socket->connect(serverAddr);
        sockets->push_back(socket);
        socketsConnected->push_back(true);
    }
}

static zmq::message_t
handleRetrieveServerRequest(int index, zmq::socket_t* socket,
                            const ByteBuffer& serverRequestBuffer) {
    LOG(std::cerr << "Socket " << index << " received retrieve request"
                  << std::endl);

    // Deserialize the retrieve server-side request
    // Data format =
    //   Response::NeedsRetrieve
    // + UUID hash
    auto requestMagic = (Response)serverRequestBuffer.getLong();
    assert(requestMagic == Response::NeedsRetrieve);
    LOG_SERVER_REQUEST("Response::NeedsRetrieve");
    UUID hash;
    serverRequestBuffer.getBytes((uint8_t*)&hash, sizeof(UUID));
    LOG_SERVER_REQUEST("hash = " << hash);
    LOG(std::cerr << "Retrieve " << hash << " -> ");

    // Get SEXP
    SEXP what = UUIDPool::get(hash);

    // Serialize the client-side response
    ByteBuffer clientResponse;
    if (what) {
        LOG(std::cerr << what << " " << Print::dumpSexp(what) << std::endl);
        // Data format =
        //   Request::Retrieved
        // + serialize(what, CompilerClientRetrieve)
        LOG_CLIENT_RESPONSE("Request::Retrieved");
        clientResponse.putLong((uint64_t)Request::Retrieved);
        LOG_CLIENT_RESPONSE("serialize(" << Print::dumpSexp(what) << ", CompilerClientRetrieve)");
        serialize(what, clientResponse, SerialOptions::CompilerClientRetrieve);
    } else {
        std::cerr << "(not found)" << std::endl;
        // Data format =
        //   Request::RetrieveFailed
        LOG_CLIENT_RESPONSE("Request::RetrieveFailed");
        clientResponse.putLong((uint64_t)Request::RetrieveFailed);
    }

    // Send the client response
    LOG(std::cerr << "Socket " << index << " sending retrieve response"
                  << std::endl);
    auto clientResponseSize = *socket->send(
        zmq::message_t(clientResponse.data(),
                       clientResponse.size()),
        zmq::send_flags::none);
    auto clientResponseSize2 = clientResponse.size();
    assert(clientResponseSize == clientResponseSize2);

    // Return the server's next response
    zmq::message_t serverResponse;
    socket->recv(serverResponse, zmq::recv_flags::none);
    return serverResponse;
}

template<typename T>
CompilerClient::Handle<T>* CompilerClient::request(
        const std::function<void(ByteBuffer&)>&& makeRequest,
        const std::function<T(const ByteBuffer&)>&& makeResponse) {
    if (!isRunning()) {
        return nullptr;
    }
    auto getResponse = [=](int index) {
        auto socket = (*sockets)[index];
        auto socketConnected = (*socketsConnected)[index];
        if (!socket->handle()) {
            LOG_WARN(std::cerr << "CompilerClient: socket closed" << std::endl);
            *socket = zmq::socket_t(*context, zmq::socket_type::req);
            socketConnected = false;
        }
        if (!socketConnected) {
            const auto& serverAddr = (*serverAddrs)[index];
            LOG_WARN(std::cerr << "CompilerClient: reconnecting to " << serverAddr
                               << std::endl);
            socket->connect(serverAddr);
            (*socketsConnected)[index] = true;
        }

        // Serialize the request
        // Request data format =
        //   from makeRequest()
        ByteBuffer request;
        LOG_DETAILED(std::cerr << "Socket " << index << " building request"
                               << std::endl);
        makeRequest(request);

        if (request.size() >= PIR_CLIENT_COMPILE_SIZE_TO_HASH_ONLY) {
            Measuring::startTimerIf(pir::Parameter::PIR_MEASURE_CLIENT_SERVER, SENDING_REQUEST_TIMER_NAME, true);
            UUID requestHash = UUID::hash(request.data(), request.size());
            LOG_DETAILED(std::cerr << "Socket " << index
                                   << " building hashOnly request" << std::endl);
            // Serialize the hash-only request
            // Request data format =
            //   Request::Memoize
            // + hash
            ByteBuffer hashOnlyRequest;
            LOG_REQUEST("Request::Memoize");
            hashOnlyRequest.putLong((uint64_t)Request::Memoize);
            LOG_REQUEST("hash = " << requestHash);
            hashOnlyRequest.putBytes((uint8_t*)&requestHash, sizeof(requestHash));

            // Send the hash-only request
            LOG(std::cerr << "Socket " << index << " sending hashOnly request"
                          << std::endl);
            auto hashOnlyRequestSize =
                *socket->send(zmq::message_t(
                                  hashOnlyRequest.data(),
                                  hashOnlyRequest.size()),
                              zmq::send_flags::none);
            auto hashOnlyRequestSize2 = hashOnlyRequest.size();
            assert(hashOnlyRequestSize == hashOnlyRequestSize2);
            Measuring::countTimerIf(pir::Parameter::PIR_MEASURE_CLIENT_SERVER, SENDING_REQUEST_TIMER_NAME, true);
            Measuring::startTimerIf(pir::Parameter::PIR_MEASURE_CLIENT_SERVER, RECEIVING_RESPONSE_TIMER_NAME, true);

            // Wait for and retrieve the response
            zmq::message_t hashOnlyResponse;
            socket->recv(hashOnlyResponse, zmq::recv_flags::none);

            // Process the response
            // Response data format =
            //   Response::NeedsFull
            // | from makeResponse()
            ByteBuffer hashOnlyResponseBuffer((uint8_t*)hashOnlyResponse.data(), hashOnlyResponse.size());
            Measuring::countTimerIf(pir::Parameter::PIR_MEASURE_CLIENT_SERVER, RECEIVING_RESPONSE_TIMER_NAME, true);
            auto hashOnlyResponseMagic = (Response)hashOnlyResponseBuffer.peekLong();
            if (hashOnlyResponseMagic != Response::NeedsFull) {
                LOG(std::cerr << "Socket " << index
                              << " received memoized hashOnly response"
                              << std::endl);
                return makeResponse(hashOnlyResponseBuffer);
            }
            LOG(std::cerr << "Socket " << index << " needs to send full request"
                          << std::endl);
            LOG_RESPONSE("Response::NeedsFull");
        }

        // Send the request
        LOG(std::cerr << "Socket " << index << " sending request" << std::endl);
        Measuring::startTimerIf(pir::Parameter::PIR_MEASURE_CLIENT_SERVER, SENDING_REQUEST_TIMER_NAME, true);
        auto requestSize =
            *socket->send(zmq::message_t(
                              request.data(),
                              request.size()),
                          zmq::send_flags::none);
        auto requestSize2 = request.size();
        assert(requestSize == requestSize2);
        Measuring::countTimerIf(pir::Parameter::PIR_MEASURE_CLIENT_SERVER, SENDING_REQUEST_TIMER_NAME, true);

        // Wait for and receive the response
        Measuring::startTimerIf(pir::Parameter::PIR_MEASURE_CLIENT_SERVER, RECEIVING_RESPONSE_TIMER_NAME, true);
        zmq::message_t response;
        socket->recv(response, zmq::recv_flags::none);
        ByteBuffer responseBuffer((uint8_t*)response.data(), response.size());
        Measuring::countTimerIf(pir::Parameter::PIR_MEASURE_CLIENT_SERVER, RECEIVING_RESPONSE_TIMER_NAME, true);

        // Handle retrieve requests
        auto responseMagic = (Response)responseBuffer.peekLong();
        while (responseMagic == Response::NeedsRetrieve) {
            response = handleRetrieveServerRequest(index, socket, responseBuffer);
            responseBuffer = ByteBuffer((uint8_t*)response.data(), response.size());
            responseMagic = (Response)responseBuffer.peekLong();
        }

        // Process the response
        // Response data format =
        //   from makeResponse()
        LOG(std::cerr << "Socket " << index << " received response" << std::endl);
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
    CompilerClient::CompiledHandle* handle = nullptr;
    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_CLIENT_SERVER, "CompilerClient.cpp: pirCompile", what, [&]{
        auto innerHandle = request<CompiledResponseData>(
            [=](ByteBuffer& request) {
                // Request data format =
                //   Request::Compile
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
                LOG_REQUEST("Request::Compile");
                request.putLong((uint64_t)Request::Compile);
                LOG_REQUEST("serialize(" << Print::dumpSexp(what) << ", CompilerClientSourceAndFeedback)");
                serialize(what, request, SerialOptions::CompilerClientSourceAndFeedback);
                LOG_REQUEST("* serialize(Compiler::decompileClosure(" << Print::dumpSexp(what) << "), CompilerClientSource)");
                serialize(Compiler::decompileClosure(what), request, SerialOptions::CompilerClientSource);
                LOG_REQUEST("assumptions = " << assumptions);
                request.putLong(sizeof(Context));
                request.putBytes((uint8_t*)&assumptions, sizeof(Context));
                LOG_REQUEST("name = " << name);
                request.putLong(name.size());
                request.putBytes((uint8_t*)name.c_str(), name.size());
                LOG_REQUEST("debug = pir::DebugOptions(...)");
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
            [](const ByteBuffer& response) {
                // Response data format =
                //   Response::Compiled
                // + sizeof(pirPrint)
                // + pirPrint
                // + hashRoot(what)
                // + serialize(what, CompilerServer)
                auto responseMagic = (Response)response.getLong();
                assert(responseMagic == Response::Compiled);
                LOG_RESPONSE("Response::Compiled");
                auto pirPrintSize = response.getLong();
                std::string pirPrint;
                pirPrint.resize(pirPrintSize);
                response.getBytes((uint8_t*)pirPrint.data(), pirPrintSize);
                LOG_RESPONSE("pirPrint = (size = " << pirPrint.size() << ")");
                UUID responseWhatHash;
                response.getBytes((uint8_t*)&responseWhatHash, sizeof(responseWhatHash));
                // Try to get hashed if we already have the compiled value
                // (unlikely but maybe possible)
                SEXP responseWhat = UUIDPool::get(responseWhatHash);
                bool isResponseReused = responseWhat != nullptr;
                if (!responseWhat) {
                    // Actually deserialize
                    responseWhat = deserialize(response, SerialOptions::CompilerServer, responseWhatHash);
                }
                LOG_RESPONSE(responseWhatHash << " + serialize("
                                              << Print::dumpSexp(responseWhat)
                                              << ", CompilerServer) "
                                              << (isResponseReused ? "(reused)" : "(new)"));
                return CompilerClient::CompiledResponseData{responseWhat, std::move(pirPrint)};
            }
        );
        if (innerHandle) {
            handle = new CompilerClient::CompiledHandle{innerHandle};
        }
    });
    return handle;
}

SEXP CompilerClient::retrieve(const rir::UUID& hash) {
    Measuring::startTimerIf(pir::Parameter::PIR_MEASURE_CLIENT_SERVER, RETRIEVE_TIMER_NAME, true);
    auto handle = request<SEXP>(
        [=](ByteBuffer& request) {
            // Request data format =
            //   Request::Retrieve
            // + hash
            LOG_REQUEST("Request::Retrieve");
            request.putLong((uint64_t)Request::Retrieve);
            LOG_REQUEST("hash = " << hash);
            request.putBytes((uint8_t*)&hash, sizeof(hash));
        },
        [=](const ByteBuffer& response) -> SEXP {
            // Response data format =
            //   Response::Retrieved
            // + serialize(what, CompilerServer)
            // | Response::RetrieveFailed
            auto responseMagic = (Response)response.getLong();
            switch (responseMagic) {
            case Response::Retrieved: {
                LOG_RESPONSE("Response::Retrieved");
                auto what = deserialize(response, SerialOptions::CompilerServer, hash);
                LOG_RESPONSE("serialize(" << Print::dumpSexp(what) << ", CompilerServer)");
                return what;
            }
            case Response::RetrieveFailed:
                LOG_RESPONSE("Response::RetrieveFailed");
                return nullptr;
            default:
                assert(false && "Unexpected response magic");
            }
        }
    );
    Measuring::countTimerIf(pir::Parameter::PIR_MEASURE_CLIENT_SERVER, RETRIEVE_TIMER_NAME, true);
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
    for (size_t i = 0; i < sockets->size(); i++) {
      auto& socket = (*sockets)[i];
      // Send the request
      LOG_REQUEST("Request::Kill");
      auto request = Request::Kill;
      socket->send(zmq::message_t(&request, sizeof(request)),
                   zmq::send_flags::none);
      // Check the acknowledgement
      zmq::message_t response;
      socket->recv(response, zmq::recv_flags::none);
      if (response.size() == sizeof(Response::Killed) &&
          *(Response*)response.data() == Response::Killed) {
        LOG_RESPONSE("Response::Killed");
      } else {
        std::cerr << "Error: server " << i << " didn't acknowledge kill request"
                  << std::endl;
      }
    }
    // Close all sockets
    for (auto& socket : *sockets) {
        socket->close();
    }
    std::fill(socketsConnected->begin(), socketsConnected->end(), false);
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
        LOG_WARN(std::cerr << console::with_red("Timeout waiting for remote PIR")
                           << std::endl);
        // Disconnect because the server probably crashed, and we want
        // to be able to restart without restarting the client; it will
        // attempt to reconnect before sending the next request
        auto socketIndex = *socketIndexRef;
        if (socketIndex != -1) {
            LOG_WARN(std::cerr << "Disconnecting " << socketIndex
                               << ", will reconnect on next request"
                               << std::endl);
            auto socket = (*sockets)[socketIndex];
            auto socketAddr = (*serverAddrs)[socketIndex];
            socket->disconnect(socketAddr);
            (*socketsConnected)[socketIndex] = false;
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
