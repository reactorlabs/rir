//
// Created by Jakob Hain on 10/22/23.
//

#include "rPackFlags.h"
#include "R/r.h"

namespace rir {

/*
 * From serialize.c
 * Type/Flag Packing and Unpacking
 *
 * To reduce space consumption for serializing code (lots of list
 * structure) the type (at most 8 bits), several single bit flags,
 * and the sxpinfo gp field (LEVELS, 16 bits) are packed into a single
 * integer. The integer is signed, so this shouldn't be pushed too
 * far. It assumes at least 28 bits, but that should be no problem.
 */

#define IS_OBJECT_BIT_MASK (1 << 8)
#define HAS_ATTR_BIT_MASK (1 << 9)
#define HAS_TAG_BIT_MASK (1 << 10)
#define ENCODE_LEVELS(v) ((v) << 12)
#define DECODE_LEVELS(v) ((v) >> 12)
#define DECODE_TYPE(v) ((v) & 255)
#define CACHED_MASK (1<<5)
#define HASHASH_MASK 1

unsigned packFlags(SEXPTYPE type, int levs, bool isobj, bool hasattr,
                   bool hastag) {
    unsigned val;
    if (type == CHARSXP) levs &= (~(CACHED_MASK | HASHASH_MASK));
    val = type | ENCODE_LEVELS(levs);
    if (isobj) val |= IS_OBJECT_BIT_MASK;
    if (hasattr) val |= HAS_ATTR_BIT_MASK;
    if (hastag) val |= HAS_TAG_BIT_MASK;
    return val;
}


void unpackFlags(unsigned flags, SEXPTYPE& ptype, int& plevs, bool& pisobj,
                 bool& phasattr, bool& phastag) {
    ptype = DECODE_TYPE(flags);
    plevs = DECODE_LEVELS(flags);
    pisobj = !!(flags & IS_OBJECT_BIT_MASK);
    phasattr = !!(flags & HAS_ATTR_BIT_MASK);
    phastag = !!(flags & HAS_TAG_BIT_MASK);
}

} // namespace rir