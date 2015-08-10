#ifndef BCEVAL_H
#define BCEVAL_H

#define INSTRUCTION0(name) void instruction ## name(InterpreterContext * context)
#define INSTRUCTION1(name) void instruction ## name(InterpreterContext * context, int arg1)
#define INSTRUCTION2(name) void instruction ## name(InterpreterContext * context, int arg1, int arg2)
#define INSTRUCTION3(name) void instruction ## name(InterpreterContext * context, int arg1, int arg2, int arg3)
#define INSTRUCTION4(name) void instruction ## name(InterpreterContext * context, int arg1, int arg2, int arg3, int arg4)

// copied R internal datatypes so that we can get LLVM IR types out of them

#define SEXPREC_HEADER  \
    struct sxpinfo_struct sxpinfo; \
    struct SEXPREC *attrib; \
    struct SEXPREC *gengc_next_node, *gengc_prev_node

typedef unsigned int SEXPTYPE;

struct sxpinfo_struct {
    SEXPTYPE type      :  5;
    unsigned int obj   :  1;
    unsigned int named :  2;
    unsigned int gp    : 16;
    unsigned int mark  :  1;
    unsigned int debug :  1;
    unsigned int trace :  1;
    unsigned int fin   :  1;  /* has finalizer installed */
    unsigned int gcgen :  1;  /* old generation number */
    unsigned int gccls :  3;  /* node class */
}; /*           Tot: 32 */

struct primsxp_struct {
    int offset;
};

struct symsxp_struct {
    struct SEXPREC *pname;
    struct SEXPREC *value;
    struct SEXPREC *internal;
};

struct listsxp_struct {
    struct SEXPREC *carval;
    struct SEXPREC *cdrval;
    struct SEXPREC *tagval;
};

struct envsxp_struct {
    struct SEXPREC *frame;
    struct SEXPREC *enclos;
    struct SEXPREC *hashtab;
};

struct closxp_struct {
    struct SEXPREC *formals;
    struct SEXPREC *body;
    struct SEXPREC *env;
};

struct promsxp_struct {
    struct SEXPREC *value;
    struct SEXPREC *expr;
    struct SEXPREC *env;
};

typedef struct SEXPREC {
    SEXPREC_HEADER;
    union {
    struct primsxp_struct primsxp;
    struct symsxp_struct symsxp;
    struct listsxp_struct listsxp;
    struct envsxp_struct envsxp;
    struct closxp_struct closxp;
    struct promsxp_struct promsxp;
    } u;
} SEXPREC;

typedef struct SEXPREC * SEXP;


typedef enum { FALSE = 0, TRUE /*, MAYBE */ } Rboolean;

typedef union { void * p; int i; } IStackval;

#define TYPED_STACK
#ifdef TYPED_STACK
typedef struct {
    int tag;
    union {
        int ival;
        double dval;
        SEXP sval;
    } u;
} R_bcstack_t;
#else
typedef SEXP R_bcstack_t;
#endif

typedef R_bcstack_t * R_binding_cache_t;

typedef struct {
    SEXP body;
    SEXP rho;
    Rboolean useCache;
    SEXP value;
    SEXP constants;
    R_bcstack_t * oldntop;
    R_binding_cache_t vcache;
    Rboolean smallcache;
#ifdef BC_INT_STACK
    IStackval *olditop;
#endif
#ifdef BC_PROFILING
    int old_current_opcode;
#endif
} InterpreterContext;


INSTRUCTION0(BCMISMATCH_OP);
INSTRUCTION0(RETURN_OP);
INSTRUCTION1(GOTO_OP);
int instructionBRIFNOT_OP(InterpreterContext * context, int arg1, int arg2);
INSTRUCTION0(POP_OP);
INSTRUCTION0(DUP_OP);
INSTRUCTION0(PRINTVALUE_OP);
INSTRUCTION1(STARTLOOPCNTXT_OP);
INSTRUCTION0(ENDLOOPCNTXT_OP);
INSTRUCTION0(DOLOOPNEXT_OP);
INSTRUCTION0(DOLOOPBREAK_OP);
INSTRUCTION3(STARTFOR_OP);
int instructionSTEPFOR_OP(InterpreterContext * context, int arg1);
INSTRUCTION0(ENDFOR_OP);
INSTRUCTION0(SETLOOPVAL_OP);
INSTRUCTION0(INVISIBLE_OP);
INSTRUCTION1(LDCONST_OP);
INSTRUCTION0(LDNULL_OP);
INSTRUCTION0(LDTRUE_OP);
INSTRUCTION0(LDFALSE_OP);
INSTRUCTION1(GETVAR_OP);
INSTRUCTION1(DDVAL_OP);
INSTRUCTION1(SETVAR_OP);
INSTRUCTION1(GETFUN_OP);
INSTRUCTION1(GETGLOBFUN_OP);
INSTRUCTION1(GETSYMFUN_OP);
INSTRUCTION1(GETBUILTIN_OP);
INSTRUCTION1(GETINTLBUILTIN_OP);
INSTRUCTION0(CHECKFUN_OP);
INSTRUCTION1(MAKEPROM_OP);
INSTRUCTION0(DOMISSING_OP);
INSTRUCTION1(SETTAG_OP);
INSTRUCTION0(DODOTS_OP);
INSTRUCTION0(PUSHARG_OP);
INSTRUCTION1(PUSHCONSTARG_OP);
INSTRUCTION0(PUSHNULLARG_OP);
INSTRUCTION0(PUSHTRUEARG_OP);
INSTRUCTION0(PUSHFALSEARG_OP);
INSTRUCTION1(CALL_OP);
INSTRUCTION1(CALLBUILTIN_OP);
INSTRUCTION1(CALLSPECIAL_OP);
INSTRUCTION1(MAKECLOSURE_OP);
INSTRUCTION1(UMINUS_OP);
INSTRUCTION1(UPLUS_OP);
INSTRUCTION1(ADD_OP);
INSTRUCTION1(SUB_OP);
INSTRUCTION1(MUL_OP);
INSTRUCTION1(DIV_OP);
INSTRUCTION1(EXPT_OP);
INSTRUCTION1(SQRT_OP);
INSTRUCTION1(EXP_OP);
INSTRUCTION1(EQ_OP);
INSTRUCTION1(NE_OP);
INSTRUCTION1(LT_OP);
INSTRUCTION1(LE_OP);
INSTRUCTION1(GE_OP);
INSTRUCTION1(GT_OP);
INSTRUCTION1(AND_OP);
INSTRUCTION1(OR_OP);
INSTRUCTION1(NOT_OP);
INSTRUCTION0(DOTSERR_OP);
INSTRUCTION1(STARTASSIGN_OP);
INSTRUCTION1(ENDASSIGN_OP);
int instructionSTARTSUBSET_OP(InterpreterContext *context, int arg1, int arg2);
INSTRUCTION0(DFLTSUBSET_OP);
int instructionSTARTSUBASSIGN_OP(InterpreterContext *context, int arg1, int arg2);
INSTRUCTION0(DFLTSUBASSIGN_OP);
int instructionSTARTC_OP(InterpreterContext *context, int arg1, int arg2);
INSTRUCTION0(DFLTC_OP);
int instructionSTARTSUBSET2_OP(InterpreterContext *context, int arg1, int arg2);
INSTRUCTION0(DFLTSUBSET2_OP);
int instructionSTARTSUBASSIGN2_OP(InterpreterContext *context, int arg1, int arg2);
INSTRUCTION0(DFLTSUBASSIGN2_OP);
INSTRUCTION2(DOLLAR_OP);
INSTRUCTION2(DOLLARGETS_OP);
INSTRUCTION0(ISNULL_OP);
INSTRUCTION0(ISLOGICAL_OP);
INSTRUCTION0(ISINTEGER_OP);
INSTRUCTION0(ISDOUBLE_OP);
INSTRUCTION0(ISCOMPLEX_OP);
INSTRUCTION0(ISCHARACTER_OP);
INSTRUCTION0(ISSYMBOL_OP);
INSTRUCTION0(ISOBJECT_OP);
INSTRUCTION0(ISNUMERIC_OP);
INSTRUCTION1(VECSUBSET_OP);
INSTRUCTION1(MATSUBSET_OP);
INSTRUCTION1(VECSUBASSIGN_OP);
INSTRUCTION1(MATSUBASSIGN_OP);
int instructionAND1ST_OP(InterpreterContext * context, int arg1, int arg2);
INSTRUCTION1(AND2ND_OP);
int instructionOR1ST_OP(InterpreterContext * context, int arg1, int arg2);
INSTRUCTION1(OR2ND_OP);
INSTRUCTION1(GETVAR_MISSOK_OP);
INSTRUCTION1(DDVAL_MISSOK_OP);
INSTRUCTION0(VISIBLE_OP);
INSTRUCTION1(SETVAR2_OP);
INSTRUCTION1(STARTASSIGN2_OP);
INSTRUCTION1(ENDASSIGN2_OP);
INSTRUCTION2(SETTER_CALL_OP);
INSTRUCTION1(GETTER_CALL_OP);
INSTRUCTION0(SWAP_OP);
INSTRUCTION0(DUP2ND_OP);
int instructionSWITCH_OP_start(InterpreterContext * context, int arg1, int arg2, int arg3, int arg4);
int instructionSWITCH_OP_character(InterpreterContext * context, int arg1, int arg2, int arg3, int arg4);
int instructionSWITCH_OP_integral(InterpreterContext * context, int arg1, int arg2, int arg3, int arg4);
INSTRUCTION0(RETURNJMP_OP);
int instructionSTARTSUBSET_N_OP(InterpreterContext * context, int arg1, int arg2);
int instructionSTARTSUBASSIGN_N_OP(InterpreterContext * context, int arg1, int arg2);
INSTRUCTION1(VECSUBSET2_OP);
INSTRUCTION1(MATSUBSET2_OP);
INSTRUCTION1(VECSUBASSIGN2_OP);
INSTRUCTION1(MATSUBASSIGN2_OP);
int instructionSTARTSUBSET2_N_OP(InterpreterContext * context, int arg1, int arg2);
int instructionSTARTSUBASSIGN2_N_OP(InterpreterContext * context, int arg1, int arg2);
INSTRUCTION2(SUBSET_N_OP);
INSTRUCTION2(SUBSET2_N_OP);
INSTRUCTION2(SUBASSIGN_N_OP);
INSTRUCTION2(SUBASSIGN2_N_OP);
INSTRUCTION1(LOG_OP);
INSTRUCTION1(LOGBASE_OP);
INSTRUCTION2(MATH1_OP);
INSTRUCTION2(DOTCALL_OP);
INSTRUCTION1(COLON_OP);
INSTRUCTION1(SEQALONG_OP);
INSTRUCTION1(SEQLEN_OP);

void initializeInterpreter(InterpreterContext * context, SEXP body, SEXP rho, Rboolean useCache, int version);
SEXP finalizeInterpreter(InterpreterContext * context);

#endif // BCEVAL_H

