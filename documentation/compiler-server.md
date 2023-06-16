# Compiler Server and Client

## How to use

### Locally

Start the compiler server

    PIR_SERVER_ADDR=tcp://*:5555 ./bin/R

**In a separate terminal window**, start the client

    PIR_CLIENT_ADDR=tcp://localhost:5555 ./bin/R

You can change the port if you'd like. You can also start multiple clients for one server. And you can have one client connect to multiple servers separated by commas, e.g.:

    PIR_CLIENT_ADDR=tcp://localhost:1234,tcp://localhost:5678 ./bin/R

We use [ZeroMQ](https://zeromq.org) for communication. See the ZeroMQ docs for all supported address types and how to connect to a remote server.

### Full configuration options

    PIR_CLIENT_ADDR=
        <address>        (on client) address of compiler server to connect to
        <addresses>      (on client) comma-separated addresses of compiler servers to connect to
    PIR_SERVER_ADDR=
        <address>        (on server) address to listen on
    PIR_CLIENT_TIMEOUT=
        <milliseconds>   (on client) how long to wait for a reply from the server before timing out
                         default is 10000 (10 seconds)
    PIR_CLIENT_COMPILE_SIZE_TO_HASH_ONLY=
        <bytes>          (on client) the server memoizes compile requests from all clients. If the client is going to
                         send a request that is larger than this size, it will only hash the request and send the hash
                         first. Then if the server has already compiled the request, it will reply with the compiled
                         code, and if not, the server will send a response causing the client to send the full request 
    PIR_CLIENT_SKIP_DISCREPANCY_CHECK=
        <0|1>            (on client) whether to skip checking for discrepancies between local and remote compilation
                         default is to not skip, but this is enabled for tests because currently some compilation is
                         non-deterministic

## What is a compiler server?

A separate process which JIT-compiles code while the local process interprets your program. It can be on the same or different machine. This reduces the overhead of compiling.

## How it works

Both the compiler client and server are Ř processes. The server starts with `PIR_SERVER_ADDR=<addr>`, which will cause the server to wait for compile requests instead of running a REPL like normal. The client starts with `PIR_CLIENT_ADDR=<addr>`, which will cause it to connect to `<addr>` and send future compile requests there.

Whenever the compiler client attempts to compile a function (by default, this happens after running the function a few times), it sends a request to the compiler server containing the function's code along with context and speculation info such as runtime types. The compiler server processes the request and replies with the compiled (LLVM) code. The client inserts this into the function's **dispatch table**, and future calls trigger the compiled code. If there is a deoptimization or the function is called with a different context, the compiler client may request the server to compile the same function again, with new context and/or speculation info (there's no point in re-compiling the function with the exact same info).

The compiler server also memoizes requests by hashing the request data including R bytecode and feedback, so if it's asked to recompile the same closure again, it will return the already-compiled version. 

### Current status

Currently we don't quite do the above:

- We send and receieve the entire SEXP instead of just sending the function and receiving/inserting the new version
- We are still JIT compiling code locally, and we don't replace this with the server-side code. Instead, we compare the PIR for discrepancies

We can set up the compiler client and server, they will communicate with each other, and the server will compile closures requested by the client. The client will receive data (including LLVM bitcode) which it *would* use to replace the local JIT-compiled code, but it doesn't do that yet. 
