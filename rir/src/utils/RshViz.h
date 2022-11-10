#pragma once
#include "sio_client.h"
#include <iostream>
#include <thread>
#include <mutex>
#include <condition_variable>
#include <string>
#include <functional>
#include <unistd.h>

class RshViz {
    public:

    static std::string APP_EVENT_SYN_REQ;
    static std::string APP_EVENT_SYN_RES;
    static std::string VIZ_TO_APP_REQUEST;
    static std::string APP_TO_VIZ_DATA;
    static std::string APP_VIZ_SYNCED;

    static void init(const std::string & address);

    static bool getConnectionStatus() {
        return connection;
    }

    // Requests
    static void doRequestSyn(const std::string &);

    // Callbacks
    static void onVizEventRequest(sio::event &);
    static void onVizEventCompleted(sio::event &);

    static void onSynDone(sio::event &);

    static std::function<void(sio::event &)> eventCallback;
    static void waitForSynDone();

    static std::mutex _eventLock;
    static std::condition_variable_any _eventWait;
    static sio::socket::ptr current_socket;

    private:

    static bool connection;
    static sio::client handler;
    static std::mutex _lock;
    static std::condition_variable_any _connWait;



    static void onConnected() {
        _lock.lock();
        _connWait.notify_all();
        std::cout << "[viz] socket connected" << std::endl;
        connection = true;
        _lock.unlock();
    }

    static void onClose(sio::client::close_reason const& reason) {
        std::cerr << "[viz] socket closed" << std::endl;
        connection = false;
    }

    static void onFail() {
        std::cout << "[viz] socket failed" << std::endl;
        connection = false;
    }


};
