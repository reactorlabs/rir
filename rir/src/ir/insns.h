#ifndef DEF_INSTR
#error "DEF_INSTR must be defined before including insns.h"
#endif

//        name       imm   pop   push popi pushi

DEF_INSTR(invalid_, 0, 0, 0, 0, 0)
/**
 * invalid_:: Opcode 0 acts as a sentinnel for unintialiazed Code.
 */
DEF_INSTR(push_, 1, 0, 1, 0, 0)
/**
 * push_:: take immediate CP index, and push CP value on object stack.
 */
DEF_INSTR(push_code_, 1, 0, 1, 0, 0)
/**
 * push_code_:: take immediate code object index, and push code object onto obj
 * stack
 */
DEF_INSTR(ldfun_, 1, 0, 1, 0, 0)
/**
 * ldfun_:: take immediate CP index of dd symbol (eg. ..1), find binding in env
 * and push value
 */
DEF_INSTR(ldddvar_, 1, 0, 1, 0, 0)
/**
 * ldfun_:: take immediate CP index of symbol, find binding in env and push
 * value
 */
DEF_INSTR(ldvar_, 1, 0, 1, 0, 0)
/**
 * ldvar_:: take immediate CP index of symbol, finding binding in env and push.
 */
DEF_INSTR(call_, 2, 1, 1, 0, 0)
/**
 * call_:: ...
 */
DEF_INSTR(promise_, 1, 0, 1, 0, 0)
/**
 * promise_:: take immediate CP index of Code, create promise & push on object
 * stack
 */
DEF_INSTR(close_, 1, 2, 1, 0, 0)
/**
 * close_:: pop body and argument list, create closure, and push on object stack
 */
DEF_INSTR(ret_, 0, 1, 0, 0, 0)
/**
 * ret_:: terminates execution and pops result off object stack
 */
DEF_INSTR(force_, 0, 1, 1, 0, 0)
/**
 * force_:: pop from objet stack, evaluate, push promise's value
 */
DEF_INSTR(pop_, 0, 1, 0, 0, 0)
/**
 * pop_:: pop from object stack
 */
DEF_INSTR(asast_, 0, 1, 1, 0, 0)
/**
 * asast_:: pop a promise off the object stack, push its AST on object stack
 */
DEF_INSTR(stvar_, 1, 1, 0, 0, 0)
/**
 * stvar_:: assign tos to the immediate symbol
 */
DEF_INSTR(asbool_, 0, 1, 1, 0, 0)
/**
 * asbool_:: pop object stack, convert to Logical and push on object stack
 */
DEF_INSTR(brtrue_, 1, 1, 0, 0, 0)
/**
 * brtrue_:: pop object stack, if TRUE branch to immediate offset
 */
DEF_INSTR(brfalse_, 1, 1, 0, 0, 0)
/**
 * brfalse_:: pop object stack, if FALSE branch to immediate offset
 */
DEF_INSTR(br_, 1, 0, 0, 0, 0)
/**
 * br_:: branch to immediate offset
 */
DEF_INSTR(invisible_, 0, 0, 0, 0, 0)
/**
 * invisible_:: set invisible flag
 */
DEF_INSTR(lti_, 0, 0, 1, 2, 0)
/**
 * lti_:: pop two integers from primitive stack, compare with with <, push
 * Logical on object stack
 */
DEF_INSTR(eqi_, 0, 0, 1, 2, 0)
/*
 * lti_:: pop two integers from primitive stack, compare with with ==. push
 * Logical on object stack
 */
DEF_INSTR(pushi_, 1, 0, 0, 0, 1)
/**
 * psuhi_:: push immediate integer to primitie stack
 */
DEF_INSTR(dupi_, 0, 0, 0, 1, 2)
/**
 * dupi_:: pop value from primitive stack, push it twice
 */
DEF_INSTR(dup_, 0, 1, 2, 0, 0)
/**
 * dup_:: pop value from object stack, push it twice
 */
DEF_INSTR(add_, 0, 2, 1, 0, 0)
/**
 * add_:: pop two values from object stack, add them, push result on object
 * stack
 */
DEF_INSTR(sub_, 0, 2, 1, 0, 0)
/**
 * add_:: pop two values from object stack, add them, push result on object
 * stack
 */
DEF_INSTR(lt_, 0, 2, 1, 0, 0)
/**
 * add_:: pop two values from object stack, add them, push result on object
 * stack
 */
DEF_INSTR(isspecial_, 1, 0, 0, 0, 0)
/**
 * isspecial_:: take imediate CP index of symbol, check if bound to a SPECIALSXP
*                or a BUILTINSXP.
 */
DEF_INSTR(isfun_, 0, 1, 1, 0, 0)
/**
 * isfun_:: pop object stack, convert to RIR code or assert error, push code to
 * object stack
 */
DEF_INSTR(is_, 1, 1, 1, 0, 0)
/**
 * is_:: immediate type tag, push T/F
 */
DEF_INSTR(extract1_, 0, 2, 1, 0, 0)
/**
 * extract1_:: do a[[b]], where a and b are on the stack
 */
DEF_INSTR(subset1_, 0, 2, 1, 0, 0)
/**
 * extract1_:: do a[b], where a and b are on the stack
 */
DEF_INSTR(brobj_, 1, 0, 0, 0, 0)
/**
 * brobj_:: branch if tos is object
 */
DEF_INSTR(dispatch_, 3, 1, 1, 0, 0)
/**
 * dispatch_:: similar to call, but receiver is tos and 3rd immediate
 *             is selector
 */
DEF_INSTR(swap_, 0, 0, 0, 0, 0)
/**
 * swap_:: swap two elements tos
 */
DEF_INSTR(pick_, 1, 0, 0, 0, 0)
/**
 * pick_:: remove n-th element on stack and push it tos
 */
DEF_INSTR(put_, 1, 0, 0, 0, 0)
/**
 * put_:: put tos at the n-th pos in the stack
 */
DEF_INSTR(call_stack_, 2, -1, 1, 0, 0)
/**
 * call_stack_:: like call, but arguments are taken from the stack
 *               immediate are number of args and names
 */
DEF_INSTR(uniq_, 0, 0, 0, 0, 0)
/**
 * uniq_:: duplicates tos if it is shared (ie. named > 1)
 */

// TODO these may be redundant but needed for the unification

// Increments top of integer stack
DEF_INSTR(inci_, 0, 0, 0, 1, 1)

// TODO pushes argument value (index of the argument is taken from integer
// stack)
DEF_INSTR(push_argi_, 0, 0, 1, 1, 0)

#undef DEF_INSTR

// DEF_INSTR(numargi_,   0,    0,    0,   0,   1) //  DELETED
