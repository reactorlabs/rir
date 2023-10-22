//
// Created by Jakob Hain on 10/22/23.
//

#include "traceSerialize.h"
#include "R/Printing.h"
#include "compiler/parameter.h"
#include <algorithm>
#include <iostream>
#include <iomanip>
#include <sstream>

namespace rir {

static std::vector<unsigned> getPirTraceSerializationExcludeFlags() {
    std::vector<unsigned> flags;
    if (getenv("PIR_TRACE_SERIALIZATION_EXCLUDE_FLAGS") != nullptr) {
        std::string excludeFlags = getenv("PIR_TRACE_SERIALIZATION_EXCLUDE_FLAGS");
        std::stringstream ss(excludeFlags);
        std::string flag;
        while (std::getline(ss, flag, ',')) {
            flags.push_back(SerialFlags::parse(flag).id());
        }
    }
    return flags;
}

bool pir::Parameter::PIR_TRACE_SERIALIZATION =
    getenv("PIR_TRACE_SERIALIZATION") != nullptr &&
    strtol(getenv("PIR_TRACE_SERIALIZATION"), nullptr, 10);
unsigned pir::Parameter::PIR_TRACE_SERIALIZATION_MAX_RAW_PRINT_LENGTH =
    getenv("PIR_TRACE_SERIALIZATION_MAX_RAW_PRINT_LENGTH") != nullptr ?
    strtol(getenv("PIR_TRACE_SERIALIZATION_MAX_RAW_PRINT_LENGTH"), nullptr, 10) :
    48;
std::vector<unsigned> pir::Parameter::PIR_TRACE_SERIALIZATION_EXCLUDE_FLAGS = getPirTraceSerializationExcludeFlags();

TraceSerializer::TraceSerializer(rir::AbstractSerializer& inner,
                                 std::ostream& out)
    : TraceSerializer(inner, out,pir::Parameter::PIR_TRACE_SERIALIZATION_MAX_RAW_PRINT_LENGTH) {}

bool TraceSerializer::willWrite(const SerialFlags& flags) const {
    return inner.willWrite(flags);
}

bool Tracer::shouldTrace(const SerialFlags& flags) {
    return std::none_of(pir::Parameter::PIR_TRACE_SERIALIZATION_EXCLUDE_FLAGS.begin(),
                        pir::Parameter::PIR_TRACE_SERIALIZATION_EXCLUDE_FLAGS.end(),
                        [&flags](unsigned excludeFlagId) {
                            return flags.id() == excludeFlagId;
                        });
}

void Tracer::tracePrefix(char prefixChar, const rir::SerialFlags& flags) {
    assert(shouldTrace(flags));

    for (size_t i = 0; i < depth; i++) {
        out << "  ";
    }
    auto ioflags = out.flags();
    out << prefixChar << prefixChar << " (" << std::setfill(' ')
        << std::setw(16) << std::left << flags << ") ";
    out.flags(ioflags);
}

void Tracer::traceInt(char prefixChar, int data, const rir::SerialFlags& flags) {
    if (!shouldTrace(flags)) {
        return;
    }

    tracePrefix(prefixChar, flags);
    auto ioflags = out.flags();
    out << std::setfill('0') << std::setw(8) << std::right << std::hex;
    out << "int   0x" << data;
    out.flags(ioflags);
    out << " (" << data << ")" << std::endl;
}

void Tracer::traceBytes(char prefixChar, const void* data, size_t size,
                        const rir::SerialFlags& flags) {
    if (!shouldTrace(flags)) {
        return;
    }

    tracePrefix(prefixChar, flags);
    out << "bytes 0x";
    auto ioflags = out.flags();
    out << std::setfill('0') << std::setw(2) << std::right << std::hex;
    for (size_t i = 0; i < size; ++i) {
        out << (unsigned)((const uint8_t*)data)[i];
        if (i == maxRawPrintLength) {
            out.flags(ioflags);
            out << "... (" << size << ")";
            break;
        }
    }
    out.flags(ioflags);
    out << std::endl;
}

void Tracer::traceSexp(char prefixChar, SEXP s, const rir::SerialFlags& flags) {
    if (!shouldTrace(flags)) {
        return;
    }

    tracePrefix(prefixChar, flags);
    out << "SEXP  " << Print::dumpSexp(s, maxRawPrintLength) << std::endl;
}

void TraceSerializer::writeBytes(const void *data, size_t size, const SerialFlags& flags) {
    traceBytes('+', data, size, flags);
    inner.writeBytes(data, size, flags);
}

void TraceSerializer::writeInt(int data, const rir::SerialFlags& flags) {
    traceInt('+', data, flags);
    inner.writeInt(data, flags);
}

void TraceSerializer::write(SEXP s, const rir::SerialFlags& flags) {
    traceSexp('+', s, flags);

    depth++;
    inner.write(s, flags);
    depth--;
}

SerializedRefs* TraceSerializer::refs() { return inner.refs(); }

TraceDeserializer::TraceDeserializer(rir::AbstractDeserializer& inner,
                                     std::ostream& out)
    : TraceDeserializer(inner, out,pir::Parameter::PIR_TRACE_SERIALIZATION_MAX_RAW_PRINT_LENGTH) {}


bool TraceDeserializer::willRead(const SerialFlags& flags) const {
    return inner.willRead(flags);
}

void TraceDeserializer::readBytes(void *data, size_t size, const SerialFlags& flags) {
    inner.readBytes(data, size, flags);
    traceBytes('-', data, size, flags);
}

int TraceDeserializer::readInt(const rir::SerialFlags& flags) {
    int data = inner.readInt(flags);
    traceInt('-', data, flags);
    return data;
}

SEXP TraceDeserializer::read(const rir::SerialFlags& flags) {
    depth++;
    SEXP s = inner.read(flags);
    depth--;

    traceSexp('-', s, flags);
    return s;
}

DeserializedRefs* TraceDeserializer::refs() { return inner.refs(); }

void TraceDeserializer::addRef(SEXP sexp) { inner.addRef(sexp); }

} // namespace rir