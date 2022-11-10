#include "utils/RshViz.h"




void RshViz::init(const std::string & address) {
    // Connect to the server
    handler.set_open_listener(onConnected);
    handler.set_close_listener(onClose);
    handler.set_fail_listener(onFail);
    handler.connect(address);

    _lock.lock();
    _connWait.wait(_lock);
    _lock.unlock();

    // Connection is established
    current_socket = handler.socket();

    // Register event callbacks
    current_socket->on(APP_EVENT_SYN_RES, onSynDone);
    current_socket->on(VIZ_TO_APP_REQUEST, onVizEventRequest);
    current_socket->on(APP_VIZ_SYNCED, onVizEventCompleted);
}



void RshViz::onVizEventRequest(sio::event & event) {
    std::cout << "[viz #--> app DATA-REQ]  : " << event.get_message().get()->get_string() << std::endl;
    assert(eventCallback);
    // Callback
    eventCallback(event);
}

void RshViz::onVizEventCompleted(sio::event & event) {
    _eventLock.lock();
    std::cout << "[app #==# viz DATA-ACK]  : " << event.get_message().get()->get_int() << " items in service queue" << std::endl;
    _eventWait.notify_all();
    _eventLock.unlock();
}


void RshViz::doRequestSyn(const std::string & reqData) {
    _lock.lock();
    std::cout << "[app #==> viz SYN-START] : " << reqData << std::endl;
    current_socket->emit(APP_EVENT_SYN_REQ, reqData);
    _lock.unlock();
}

void RshViz::waitForSynDone() {
    _lock.lock();
    _connWait.wait(_lock);
    _lock.unlock();
}

void RshViz::onSynDone(sio::event & event) {
    _lock.lock();
    std::cout << "[app #==# viz SYN-DONE]  : " << event.get_message().get()->get_string() << std::endl;
    _connWait.notify_all();
    _lock.unlock();
}


sio::socket::ptr RshViz::current_socket;
sio::client RshViz::handler;
std::mutex RshViz::_lock, RshViz::_eventLock;
std::condition_variable_any RshViz::_connWait, RshViz::_eventWait;
bool RshViz::connection = false;
std::function<void(sio::event &)> RshViz::eventCallback = nullptr;

std::string RshViz::APP_EVENT_SYN_REQ = "app-req-syn";
std::string RshViz::APP_EVENT_SYN_RES = "app-res-syn";
std::string RshViz::VIZ_TO_APP_REQUEST = "viz-to-app-req";
std::string RshViz::APP_TO_VIZ_DATA = "app-to-viz-data";
std::string RshViz::APP_VIZ_SYNCED = "app-viz-ack";
