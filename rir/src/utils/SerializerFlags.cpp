#include "SerializerFlags.h"
#include <cstdlib>
namespace rir {
bool SerializerFlags::serializerEnabled = std::getenv("PIR_SERIALIZE_ALL") ? true : false;
bool SerializerFlags::bitcodeDebuggingData = false;
bool SerializerFlags::captureCompileStats = std::getenv("CAPTURE_ALL_COMPILE_STATS") ? true : false;
unsigned SerializerFlags::loadedFunctions = 0;
}
