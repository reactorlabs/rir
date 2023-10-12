# Compiler Server and Client

## How to use

### Locally

Start the compiler server

    PIR_SERVER_ADDR=tcp://*:5555 ./bin/R

**In a separate terminal window**, start the client

    PIR_CLIENT_ADDR=tcp://localhost:5555 ./bin/R

You can change the port if you'd like. You can also start multiple clients for one server. ~~And you can have one client connect to multiple servers separated by commas, e.g.:~~

    PIR_CLIENT_ADDR=tcp://localhost:1234,tcp://localhost:5678 ./bin/R

(multiple servers are currently only in statis and won't work, because retrieval for multiple servers isn't implemented)

We use [ZeroMQ](https://zeromq.org) for communication. See the ZeroMQ docs for all supported address types and how to connect to a remote server.

### Full configuration options

    PIR_CLIENT_ADDR=
        <address>        (on client) address of compiler server to connect to
        <addresses>      (on client) comma-separated addresses of compiler servers to connect to
    PIR_CLIENT_TIMEOUT=
        <milliseconds>   (on client) how long to wait for a reply from the server before timing out. Default is 10000 (10 seconds)
    PIR_CLIENT_COMPILE_SIZE_TO_HASH_ONLY=
        <bytes>          (on client) the server memoizes compile requests from all clients. If the client is going to send a request that is larger than this size, it will only hash the request and send the hash first. Then if the server has already compiled the request, it will reply with the compiled code, and if not, the server will send a response causing the client to send the full request 
    PIR_CLIENT_DRY_RUN=
        <0|1>            (on client) whether to actually use the server's code, or compile locally and just use it for comparison. Default is false (actually use the code)
        PIR_CLIENT_SKIP_DISCREPANCY_CHECK=
            <0|1>        (on client) whether to skip checking for discrepancies between local and remote compilation. Default is to not skip.
    PIR_SERVER_ADDR=
        <address>        (on server) address to listen on

#### Logging

    PIR_LOG_COMPILER_PEER_DETAILED=
        1           log the contents of every request sent to and received by the compiler client or server
    PIR_LOG_COMPILER_PEER=
        1           log every message sent from/to the compiler peer. Superseded by PIR_LOG_COMPILER_PEER_DETAILED
    PIR_WARN_COMPILER_PEER=
        1           warn when the compiler peer connection times out or closes. Superseded by PIR_LOG_COMPILER_PEER

These options are also in [./debugging.md](./debugging.md). They can be applied to client or server, and will log on whatever peer they're applied but not affect connected peers.

It's recommended to set `PIR_WARN_COMPILER_PEER` to see any issues. Try setting `PIR_LOG_COMPILER_PEER` on the server to see the requests and responses being made. 

## What is a compiler server?

A separate process which JIT-compiles code while the local process interprets your program. It can be on the same or different machine. This reduces the overhead of compiling.

## How it works

Both the compiler client and server are Ř processes. The server starts with `PIR_SERVER_ADDR=<addr>`, which will cause the server to wait for compile requests instead of running a REPL like normal. The client starts with `PIR_CLIENT_ADDR=<addr>`, which will cause it to connect to `<addr>` and send future compile requests there.

Whenever the compiler client attempts to compile a function (by default, this happens after running the function a few times), it sends a request to the compiler server containing the function's code along with context and speculation info such as runtime types. The compiler server processes the request and replies with the compiled (LLVM) code. The client inserts this into the function's **dispatch table**, and future calls trigger the compiled code. If there is a deoptimization or the function is called with a different context, the compiler client may request the server to compile the same function again, with new context and/or speculation info (there's no point in re-compiling the function with the exact same info).

The compiler server also memoizes requests by hashing the request data including R bytecode and feedback, so if it's asked to recompile the same closure again, it will return the already-compiled version. 

### SEXP intern pool

TODO: improve writing

Separate from requests, the compiler server interns SEXPs and will send SEXPs to the client with connected SEXPs as hashes. If the client doesn't have the SEXP locally, it can send a `Retrieve` request to the server to get it from the intern pool, but if it does, it can skip this request. This prevents transmitting redundant SEXPs and more importantly, creating separate SEXPs on the client; this is not just bad for performance, but can be a semantic issue.

The SEXPs are interned according to a hash computed from their immutable semantic data, using [xxHash](https://xxhash.com/). Data which is mutable but doesn't affect semantics, like feedback, isn't part of the hash. Environments are also not part of the hash since they are mutable and defined behavior shouldn't rely on their changes throughout the program's execution.

The client will also intern SEXPs it retrieves from the server. However, it explicitly *doesn't* send connected SEXPs as hashes, instead sending the full SEXP even if redundant, because the client's intern pool is temporary. When the server interns SEXPs it also preserves them for future clients, and the server will presumably have more memory so it can handle this. Because the server has an intern pool, even though it will receive and deserialize redundant SEXPs, it won't actually store the duplicates, they'll simply be discarded.