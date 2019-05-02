#pragma once

#include <R/r.h>

#define REXPORT extern "C"

REXPORT void WriteItem(SEXP s, SEXP ref_table, R_outpstream_t stream);
REXPORT SEXP ReadItem(SEXP ref_table, R_inpstream_t stream);
REXPORT void OutStringVec(R_outpstream_t stream, SEXP s, SEXP ref_table);
REXPORT void WriteBC(SEXP s, SEXP ref_table, R_outpstream_t stream);
REXPORT SEXP ReadBC(SEXP ref_table, R_inpstream_t stream);
REXPORT void OutInteger(R_outpstream_t stream, int i);
REXPORT void OutReal(R_outpstream_t stream, double d);
REXPORT void OutComplex(R_outpstream_t stream, Rcomplex c);
REXPORT void OutByte(R_outpstream_t stream, Rbyte i);
REXPORT void OutString(R_outpstream_t stream, const char* s, int length);
REXPORT void InWord(R_inpstream_t stream, char* buf, int size);
REXPORT int InInteger(R_inpstream_t stream);
REXPORT double InReal(R_inpstream_t stream);
REXPORT Rcomplex InComplex(R_inpstream_t stream);
REXPORT void InString(R_inpstream_t stream, char* buf, int length);
REXPORT void OutRefIndex(R_outpstream_t stream, int i);
REXPORT int InRefIndex(R_inpstream_t stream, int flags);
REXPORT void OutStringVec(R_outpstream_t stream, SEXP s, SEXP ref_table);

static inline void OutChar(R_outpstream_t stream, int chr) {
    stream->OutChar(stream, chr);
}

static inline int InChar(R_inpstream_t stream) {
    return stream->InChar(stream);
}

static inline void OutBytes(R_outpstream_t stream, const void* buf,
                            int length) {
    stream->OutBytes(stream, (void*)buf, length);
}

static inline void InBytes(R_inpstream_t stream, void* buf, int length) {
    stream->InBytes(stream, buf, length);
}
