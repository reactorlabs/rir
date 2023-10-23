//
// Created by Jakob Hain on 10/22/23.
//

#pragma once

#include "serializeHash/serializeUni.h"
#include <iostream>

namespace rir {

struct SerialOptions;
class UUID;

// TODO: This class is very tightly coupled with serialize.h and serializeUni.h,
//   to the point where serializeUni.h has a friend class so this can access a
//   protected member, and this has serialize and deserialize from serialize.h
//   as friend functions. Currently this doesn't matter because everything is in
//   the same module so extensibility isn't necessary, but may be something to
//   look at in the future. (Serializer and Deserializer are also tightly
//   coupled with serialize and deserialize, in that they can't be used
//   standalone and the correct way to serialize/deserialize an SEXP at the
//   surface is actually to call writeInline, which is a bit confusing)

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
};

class TraceSerializer : public AbstractSerializer, private Tracer {
    AbstractSerializer& inner;

    explicit TraceSerializer(AbstractSerializer& inner,
                             std::ostream& out = std::cerr);
    TraceSerializer(AbstractSerializer& inner, std::ostream& out,
                    unsigned maxRawPrintLength)
        : Tracer(out, maxRawPrintLength), inner(inner) {}
    friend void serialize(SEXP sexp, ByteBuffer& buffer,
                          const SerialOptions& options);
  public:
    bool willWrite(const SerialFlags& flags) const override;
    void writeBytes(const void *data, size_t size, const SerialFlags& flags) override;
    void writeInt(int data, const SerialFlags& flags) override;
    void write(SEXP s, const SerialFlags& flags) override;
    SerializedRefs* refs() override;
};

class TraceDeserializer : public AbstractDeserializer, private Tracer {
    AbstractDeserializer& inner;

    explicit TraceDeserializer(AbstractDeserializer& inner,
                               std::ostream& out = std::cerr);
    TraceDeserializer(AbstractDeserializer& inner, std::ostream& out,
                      unsigned maxRawPrintLength)
        : Tracer(out, maxRawPrintLength), inner(inner) {}
    friend SEXP deserialize(const ByteBuffer& sexpBuffer,
                            const SerialOptions& options,
                            const UUID& retrieveHash);
  public:
    bool willRead(const SerialFlags& flags) const override;
    void readBytes(void *data, size_t size, const SerialFlags& flags) override;
    int readInt(const SerialFlags& flags) override;
    SEXP read(const SerialFlags& flags) override;
    DeserializedRefs* refs() override;
    void addRef(SEXP sexp) override;
};

} // namespace rir
