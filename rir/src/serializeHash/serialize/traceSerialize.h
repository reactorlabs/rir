//
// Created by Jakob Hain on 10/22/23.
//

#pragma once

#include "serialize.h"
#include <iostream>

namespace rir {

struct SerialOptions;
class UUID;

class Tracer {
    std::ostream& out;
    unsigned maxRawPrintLength;

    static bool shouldTrace(const SerialFlags& flags);
    void tracePrefix(char prefixChar, const SerialFlags& flags);
    bool traceSpecial(const SerialFlags& flags, const void* data, size_t size);

  protected:
    size_t depth;

    Tracer(std::ostream& out, unsigned maxRawPrintLength)
        : out(out), maxRawPrintLength(maxRawPrintLength), depth(0) {}

    void traceBytes(char prefixChar, const void* data, size_t size,
                    const SerialFlags& flags);
    void traceInt(char prefixChar, int data, const SerialFlags& flags);
    void traceSexp(char prefixChar, SEXP s, const SerialFlags& flags);
    void traceSexp(char prefixChar, SEXP s, unsigned size,
                   const SerialFlags& flags);
    void traceSexpDone(char prefixChar, SEXP s, unsigned size,
                       const SerialFlags& flags);
};

class TraceSerializer : public Serializer, private Tracer {
    TraceSerializer(ByteBuffer& buffer, const SerialOptions& options,
                    std::ostream& out = std::cerr);
    TraceSerializer(ByteBuffer& buffer, const SerialOptions& options,
                    std::ostream& out, unsigned maxRawPrintLength)
        : Serializer(buffer, options),
          Tracer(out, maxRawPrintLength) {}
    friend void serialize(SEXP sexp, ByteBuffer& buffer,
                          const SerialOptions& options);
  public:
    void writeBytes(const void *data, size_t size, const SerialFlags& flags) override;
    void writeInt(int data, const SerialFlags& flags) override;
    void write(SEXP s, const SerialFlags& flags) override;
};

class TraceDeserializer : public Deserializer, private Tracer {
    TraceDeserializer(const ByteBuffer& buffer, const SerialOptions& options,
                      const UUID& retrieveHash = UUID(),
                      std::ostream& out = std::cerr);
    TraceDeserializer(const ByteBuffer& buffer, const SerialOptions& options,
                      const UUID& retrieveHash, std::ostream& out,
                      unsigned maxRawPrintLength)
        : Deserializer(buffer, options, retrieveHash),
          Tracer(out, maxRawPrintLength) {}
    friend SEXP deserialize(const ByteBuffer& sexpBuffer,
                            const SerialOptions& options,
                            const UUID& retrieveHash);
  public:
    void readBytes(void *data, size_t size, const SerialFlags& flags) override;
    int readInt(const SerialFlags& flags) override;
    SEXP read(const SerialFlags& flags) override;
};

void initPirTraceSerializationExcludeFlags();

} // namespace rir
