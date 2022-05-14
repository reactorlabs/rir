#ifndef RIR_SERDES_H
#define RIR_SERDES_H

#include <unordered_map>
#include <iostream>
#include <functional>
#include <cassert>
#include <chrono>

#include "runtime/Code.h"

using namespace std::chrono;

namespace rir {

    class DebugMessages {
    private:
        // 0 -> print nothing
        // 1 -> print messages
        // 2 -> print messages and errors
        static int serializerDebug;
        static int deserializerDebug;

        static void printSpace(int space) {
            for (int i = 0; i < space; i++) std::cout << "\t";
        }

        static void printMsg(const std::string & msg, int s) {
            printSpace(s);
            std::cout << msg << "\n";
        }

        static void printErr(const std::string & msg, int s) {
            printSpace(s);
            std::cout << msg << "\n";
        }
    public:

        static int serializerDebugLevel() {
            return serializerDebug;
        }

        static int deserializerDebugLevel() {
            return deserializerDebug;
        }

        static void initializeDebugger() {
            serializerDebug = getenv("SER_DBG") ? std::stoi(getenv("SER_DBG")) : 0;
            deserializerDebug = getenv("DES_DBG") ? std::stoi(getenv("DES_DBG")) : 0;
        }

        static void printSerializerMessage(const std::string & msg, int s) {
            if (serializerDebug < 1) return;
            printMsg(msg, s);
        }

        static void printSerializerErrors(const std::string & msg, int s) {
            if (serializerDebug < 2) return;
            printMsg(msg, s);
        }

        static void printDeserializerMessage(const std::string & msg, int s) {
            if (deserializerDebug < 1) return;
            printMsg(msg, s);
        }

        static void printDeserializerErrors(const std::string & msg, int s) {
            if (deserializerDebug < 2) return;
            printMsg(msg, s);
        }


    };

    class DebugCheckpoints {
    private:
        static rir::Code* lastCheckpoint;
        static unsigned checkpoint;
        static unsigned BCidx;

        static std::chrono::time_point<std::chrono::high_resolution_clock> lastCheckpointTime;
        static std::chrono::time_point<std::chrono::high_resolution_clock> lastInstructionTime;


    public:
        static bool enabled;

        static void startDebugMessages() {
            enabled = true;
            lastCheckpointTime = lastInstructionTime = high_resolution_clock::now(); // setting epoch
        }

        static void stopDebugMessages() {
            enabled = false;
        }

        static void updateCheckpoint(const std::string & nativeCodeHandle) {
            if (!enabled) return;

            auto stop = high_resolution_clock::now();
            auto duration = duration_cast<microseconds>(stop - lastCheckpointTime);

            std::cout << "[" << checkpoint++ << "]: " << nativeCodeHandle << " { " << duration.count() << "us }" << std::endl;
            lastCheckpointTime = lastInstructionTime = high_resolution_clock::now();
        }

        static void updateCheckpoint(rir::Code* & code, bool cA) {
            if (!enabled) return;
            if (lastCheckpoint == code) return;
            lastCheckpoint = code;

            auto stop = high_resolution_clock::now();
            auto duration = duration_cast<microseconds>(stop - lastCheckpointTime);

            std::cout << "[" << checkpoint++ << "]: " << "interpreted_code" << " { " << duration.count() << "us }" << std::endl;

            lastCheckpointTime = lastInstructionTime = high_resolution_clock::now();
        }

        static void printInstruction(const std::string & id, const std::function< void() >& callback) {
            if (!enabled) return;

            auto stop = high_resolution_clock::now();
            auto duration = duration_cast<microseconds>(stop - lastInstructionTime);

            std::cout << "  " << "[" << BCidx++ << "]" << id << " { " << duration.count() << "us }" << std::endl;
            callback();

            lastInstructionTime = high_resolution_clock::now();
        }
    };
}

#endif
