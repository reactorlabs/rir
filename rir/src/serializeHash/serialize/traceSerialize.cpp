//
// Created by Jakob Hain on 10/22/23.
//

#include "traceSerialize.h"
#include "R/Printing.h"
#include "compiler/parameter.h"
#include "rPackFlags.h"
#include "runtime/rirObjectMagic.h"
#include <algorithm>
#include <iomanip>
#include <iostream>
#include <sstream>

namespace rir {

bool pir::Parameter::PIR_TRACE_SERIALIZATION =
    getenv("PIR_TRACE_SERIALIZATION") != nullptr &&
    strtol(getenv("PIR_TRACE_SERIALIZATION"), nullptr, 10);
unsigned pir::Parameter::PIR_TRACE_SERIALIZATION_MAX_RAW_PRINT_LENGTH =
    getenv("PIR_TRACE_SERIALIZATION_MAX_RAW_PRINT_LENGTH") != nullptr ?
    strtol(getenv("PIR_TRACE_SERIALIZATION_MAX_RAW_PRINT_LENGTH"), nullptr, 10) :
    48;
size_t pir::Parameter::PIR_TRACE_SERIALIZATION_MIN_SIZE =
    getenv("PIR_TRACE_SERIALIZATION_MIN_SIZE") != nullptr ?
    strtol(getenv("PIR_TRACE_SERIALIZATION_MIN_SIZE"), nullptr, 10) :
    0;
std::vector<unsigned>* pir::Parameter::PIR_TRACE_SERIALIZATION_EXCLUDE = nullptr;

static std::vector<unsigned>* getPirTraceSerializationExcludeFlags() {
    auto flags = new std::vector<unsigned>();
    if (getenv("PIR_TRACE_SERIALIZATION_EXCLUDE") != nullptr) {
        std::string flagsStr = getenv("PIR_TRACE_SERIALIZATION_EXCLUDE");
        std::stringstream ss(flagsStr);
        std::string flag;
        while (std::getline(ss, flag, ',')) {
            flags->push_back(SerialFlags::parse(flag).id());
        }
    }
    return flags;
}

void initPirTraceSerializationExcludeFlags() {
    pir::Parameter::PIR_TRACE_SERIALIZATION_EXCLUDE = getPirTraceSerializationExcludeFlags();
}

bool Tracer::shouldTrace(const SerialFlags& flags) {
    return std::none_of(pir::Parameter::PIR_TRACE_SERIALIZATION_EXCLUDE->begin(),
                        pir::Parameter::PIR_TRACE_SERIALIZATION_EXCLUDE->end(),
                        [&flags](unsigned excludeFlagId) {
                            return flags.id() == excludeFlagId;
                        });
}

bool Tracer::shouldTrace(const SerialFlags& flags, size_t size) {
    return shouldTrace(flags) &&
           size >= pir::Parameter::PIR_TRACE_SERIALIZATION_MIN_SIZE;
}

void Tracer::tracePrefix(char prefixChar, const SerialFlags& flags) {
    assert(shouldTrace(flags));

    for (size_t i = 0; i < depth; i++) {
        out << "  ";
    }
    auto ioflags = out.flags();
    out << prefixChar << prefixChar << " (" << std::setfill(' ')
        << std::setw(16) << std::left << flags << ") ";
    out.flags(ioflags);
}

bool Tracer::traceSpecial(const SerialFlags& flags, const void* data,
                          size_t size) {
    assert(shouldTrace(flags, size));

    if (flags.id() == SerialFlags::String.id() ||
        flags.id() == SerialFlags::SymbolName.id()) {
        out << "str   ";

        out << std::string((const char*)data, size);
    } else if (flags.id() == SerialFlags::RFlags.id()) {
        out << "type  ";

        unsigned rFlags = *(const unsigned*)data;
        SEXPTYPE type;
        int levs;
        bool isObj;
        bool hasAttr;
        bool hasTag;
        unpackFlags(rFlags, type, levs, isObj, hasAttr, hasTag);

        switch (type) {
        case (SEXPTYPE)SpecialType::Altrep:
            out << "altrep";
            break;
        case (SEXPTYPE)SpecialType::Global:
            out << "global";
            break;
        case (SEXPTYPE)SpecialType::Ref:
            out << "ref";
            break;
        default:
            out << Rf_type2char(type);
            break;
        }
        if (levs) {
            out << " +levs=" << levs;
        }
        if (isObj) {
            out << " +obj";
        }
        if (hasAttr) {
            out << " +attr";
        }
        if (hasTag) {
            out << " +tag";
        }
    } else if (flags.id() == SerialFlags::RirMagic.id()) {
        out << "rir   ";

        out << rirObjectClassName(*(const unsigned*)data);
    } else if (flags.id() == SerialFlags::BuiltinNr.id() ||
               flags.id() == SerialFlags::EnvType.id() ||
               flags.id() == SerialFlags::RefId.id() ||
               flags.id() == SerialFlags::GlobalId.id()) {
        out << "int   ";

        out << *(const unsigned*)data;
    } else {
        return false;
    }

    // A bit confusing: we handle all other cases in the else branch,
    // this saves LOC because we don't return true in any of the handled cases,
    // we just fall through to this
    return true;
}

void Tracer::traceInt(char prefixChar, int data, const SerialFlags& flags) {
    if (!shouldTrace(flags, sizeof(data))) {
        return;
    }

    tracePrefix(prefixChar, flags);
    if (!traceSpecial(flags, &data, sizeof(data))) {
        out << "int   0x";
        auto ioflags = out.flags();
        out << std::setfill('0') << std::setw(8) << std::right << std::hex;
        out << data;
        out.flags(ioflags);
        out << " (" << data << ")";
    }
    out << std::endl;
}

void Tracer::traceBytes(char prefixChar, const void* data, size_t size,
                        const SerialFlags& flags) {
    if (!shouldTrace(flags, size)) {
        return;
    }

    tracePrefix(prefixChar, flags);
    if (!traceSpecial(flags, data, size)) {
        out << "bytes ";
        out << "0x";
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
    }
    out << std::endl;
}

void Tracer::traceSexp(char prefixChar, SEXP s, unsigned size,
                       const SerialFlags& flags) {
    if (!shouldTrace(flags, size == UINT32_MAX ? 0 : size)) {
        return;
    }

    tracePrefix(prefixChar, flags);
    out << "SEXP  " << Print::dumpSexp(s, maxRawPrintLength);
    if (size != UINT32_MAX) {
        out << " (" << size << " bytes)";
    }
    out << std::endl;
}

void Tracer::traceSexp(char prefixChar, SEXP s, const SerialFlags& flags) {
    traceSexp(prefixChar, s, UINT32_MAX, flags);
}

void Tracer::traceSexpDone(char prefixChar, SEXP s, unsigned size,
                           const SerialFlags& flags) {
    if (!shouldTrace(flags, size)) {
        return;
    }

    tracePrefix(prefixChar, flags);
    out << "done  " << Print::dumpSexp(s, maxRawPrintLength);
    if (size != UINT32_MAX) {
        out << " (" << size << " bytes)";
    }
    out << std::endl;
}

TraceSerializer::TraceSerializer(ByteBuffer& buffer,
                                 const rir::SerialOptions& options,
                                 std::ostream& out)
    : TraceSerializer(buffer, options, out,
                      pir::Parameter::PIR_TRACE_SERIALIZATION_MAX_RAW_PRINT_LENGTH) {}

void TraceSerializer::writeBytes(const void *data, size_t size, const SerialFlags& flags) {
    if (willWrite(flags)) {
        traceBytes('+', data, size, flags);
    }
    Serializer::writeBytes(data, size, flags);
}

void TraceSerializer::writeInt(int data, const SerialFlags& flags) {
    if (willWrite(flags)) {
        traceInt('+', data, flags);
    }
    Serializer::writeInt(data, flags);
}

void TraceSerializer::write(SEXP s, const SerialFlags& flags) {
    if (willWrite(flags)) {
        traceSexp('+', s, flags);
    }

    depth++;
    auto startPos = getWritePos();
    Serializer::write(s, flags);
    auto size = getWritePos() - startPos;
    depth--;

    if (startPos != UINT32_MAX && willWrite(flags)) {
        traceSexpDone('+', s, size, flags);
    }
}

TraceDeserializer::TraceDeserializer(const ByteBuffer& buffer,
                                     const rir::SerialOptions& options,
                                     const rir::UUID& retrieveHash,
                                     std::ostream& out)
    : TraceDeserializer(buffer, options, retrieveHash, out,
                        pir::Parameter::PIR_TRACE_SERIALIZATION_MAX_RAW_PRINT_LENGTH) {}


void TraceDeserializer::readBytes(void *data, size_t size, const SerialFlags& flags) {
    Deserializer::readBytes(data, size, flags);
    if (willRead(flags)) {
        traceBytes('-', data, size, flags);
    }
}

int TraceDeserializer::readInt(const SerialFlags& flags) {
    int data = Deserializer::readInt(flags);
    if (willRead(flags)) {
        traceInt('-', data, flags);
    }
    return data;
}

SEXP TraceDeserializer::read(const SerialFlags& flags) {
    depth++;
    auto startPos = getReadPos();
    SEXP s = Deserializer::read(flags);
    auto size = getReadPos() - startPos;
    depth--;

    if (willRead(flags)) {
        traceSexp('-', s, startPos == UINT32_MAX ? UINT32_MAX : size, flags);
    }
    return s;
}

} // namespace rir