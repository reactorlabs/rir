#ifndef DEF_INSTR
#error "DEF_INSTR must be defined before including insns.h"
#endif

DEF_INSTR(invalid_,   0,    0,    0,   0,   0) // invalid operation
DEF_INSTR(push_,      1,    0,    1,   0,   0) // push a constant to the stack
DEF_INSTR(ldfun_,     1,    0,    1,   0,   0) // load function from env
DEF_INSTR(ldvar_,     1,    0,    1,   0,   0) // load variable from env
DEF_INSTR(call_,      2,    1,    1,   0,   0) // call fun
DEF_INSTR(promise_,   1,    0,    1,   0,   0) // create promise
DEF_INSTR(close_,     1,    2,    1,   0,   0) // create closure
DEF_INSTR(ret_,       0,    1,    0,   0,   0) // return
DEF_INSTR(force_,     0,    1,    1,   0,   0) // eval promise
DEF_INSTR(pop_,       0,    1,    0,   0,   0) // pop tos
DEF_INSTR(pusharg_,   1,    0,    1,   0,   0) // push argument to stack
DEF_INSTR(asast_,     0,    1,    1,   0,   0) // pop promise, push ast
DEF_INSTR(stvar_,     0,    2,    1,   0,   0) // was originally getvar
DEF_INSTR(asbool_,    0,    1,    1,   0,   0) // convert ?? on tos to bool
DEF_INSTR(condtrue_,  1,    1,    0,   0,   0) // jump … if
DEF_INSTR(condfals_,  1,    1,    0,   0,   0) // jump … if  FIXME -- push onthe i stack
DEF_INSTR(jmp_,       1,    0,    0,   0,   0) // unconditional jump
DEF_INSTR(lti_,       0,    0,    1,   2,   0) // less than on ints
DEF_INSTR(eqi_,       0,    0,    1,   2,   0) // equality on ints
DEF_INSTR(pushi_,     1,    0,    0,   0,   1) // push int
DEF_INSTR(dupi_,      0,    0,    0,   1,   2) // dup int
DEF_INSTR(dup_,       0,    1,    2,   0,   0) // dup tos
DEF_INSTR(add_,       0,    2,    1,   0,   0) // +
DEF_INSTR(sub_,       0,    2,    1,   0,   0) // -
DEF_INSTR(lt_,        0,    2,    1,   0,   0) // <
DEF_INSTR(isspecial_, 1,    0,    0,   0,   0) //
DEF_INSTR(isfun_,     0,    1,    1,   0,   0) // check tos is a function
DEF_INSTR(end_,       0,    0,    0,   0,   0)

#undef DEF_INSTR


//DEF_INSTR(numargi_,   0,    0,    0,   0,   1) //  DELETED
