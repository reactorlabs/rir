# Compiler Server and Client

## How to use

### Locally

Start the compiler server

```
PIR_SERVER_ADDR=tcp://*:5555 ./bin/R
```

**In a separate terminal window**, start the client

```
PIR_CLIENT_ADDR=tcp://localhost:5555 ./bin/R
```

You can change the port if you'd like. You can also start multiple clients for one server. And you can have one client connect to multiple servers separated by commas, e.g.:

```
PIR_CLIENT_ADDR=tcp://localhost:1234,tcp://localhost:5678 ./bin/R
```

We use [ZeroMQ](https://zeromq.org) for communication. See the ZeroMQ docs for all supported address types and how to connect to a remote server.

## What is a compiler server?

A separate process which JIT-compiles code while the local process interprets your program. It can be on the same or different machine. This reduces the overhead of compiling.

## How it works

Both the compiler client and server are Ř processes. The server starts with `PIR_SERVER_ADDR=<addr>`, which will cause the server to wait for compile requests instead of running a REPL like normal. The client starts with `PIR_CLIENT_ADDR=<addr>`, which will cause it to connect to `<addr>` and send future compile requests there.

Whenever the compiler client attempts to compile a function (by default, this happens after running the function a few times), it sends a request to the compiler server containing the function's code along with context and speculation info such as runtime types. The compiler server processes the request and replies with the compiled (LLVM) code. The client inserts this into the function's **dispatch table**, and future calls trigger the compiled code. If there is a deoptimization or the function is called with a different context, the compiler client may request the server to compile the same function again, with new context and/or speculation info (there's no point in re-compiling the function with the exact same info).

### Current status

Currently we don't quite do the above, we are still JIT compiling code locally. We can set up the compiler client and server, and they will communicate with each other when the server *would* compile. However, right now we don't communicate the actual code (and therefore the server doesn't compile anything).

