#ifndef RINT_INC_H
#define RINT_INC_H

/*
 * this file contains our own wrapper around USE_RINTERNAL
 * to add a feature extend the class below and add the feature to RIntlns.cpp
 * this file should be safe to include in headers
 */

struct SEXPREC;
typedef SEXPREC* SEXP;

namespace rir {

namespace {

/* ============================================================================
 * === shameless c/p code from gnur Rinternals.h for performance reasons
 */

struct sxpinfo_struct_rjit {
    unsigned int type : 5; /* ==> (FUNSXP == 99) %% 2^5 == 3 == CLOSXP
                        * -> warning: `type' is narrower than values
                        *              of its type
                        * when SEXPTYPE was an enum */
    unsigned int obj : 1;
    unsigned int named : 2;
    unsigned int gp : 16;
    unsigned int mark : 1;
    unsigned int debug : 1;
    unsigned int trace : 1; /* functions and memory tracing */
    unsigned int spare : 1; /* currently unused */
    unsigned int gcgen : 1; /* old generation number */
    unsigned int gccls : 3; /* node class */
};                          /*		    Tot: 32 */

struct cons_rjit {
    SEXP car;
    SEXP cdr;
    SEXP tag;
};

struct sexprec_rjit {
    struct sxpinfo_struct_rjit sxpinfo;
    SEXP attrib;
    SEXP gengc_next_node, gengc_prev_node;
    union {
        struct cons_rjit cons;
        int i;
    } u;
};

typedef int R_len_t;
struct vecsxp_struct_rjit {
    R_len_t length;
    R_len_t truelength;
};

struct vector_sexprec_rjit {
    struct sxpinfo_struct_rjit sxpinfo;
    SEXP attrib;
    SEXP gengc_next_node, gengc_prev_node;
    struct vecsxp_struct_rjit vecsxp;
};

typedef union {
    vector_sexprec_rjit s;
    double align;
} vector_sexprec_rjit_align;

} // namespace

class Rinternals {
  public:
    static int primoffset(SEXP i) { return ((sexprec_rjit*)i)->u.i; }
    static int typeof(SEXP i) { return ((sexprec_rjit*)i)->sxpinfo.type; }
    static SEXP car(SEXP i) { return ((sexprec_rjit*)i)->u.cons.car; }
    static SEXP cdr(SEXP i) { return ((sexprec_rjit*)i)->u.cons.cdr; }
    static SEXP tag(SEXP i) { return ((sexprec_rjit*)i)->u.cons.tag; }
    static unsigned char* raw(SEXP i) {
        return (unsigned char*)(((vector_sexprec_rjit*)i) + 1);
    }
};
}

#endif
