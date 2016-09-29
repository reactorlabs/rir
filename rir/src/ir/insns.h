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
 * ldddvar_:: loads the ellipsis values (such as ..1, ..2) and pushes them on stack.
 */
DEF_INSTR(ldvar_, 1, 0, 1, 0, 0)
/**
 * ldvar_:: take immediate CP index of symbol, finding binding in env and push.
 */
DEF_INSTR(ldarg_, 1, 0, 1, 0, 0)
/**
 * ldarg_:: like ldvar but guaranteed to be an argument
 */
DEF_INSTR(call_, 3, 1, 1, 0, 0)
/**
 * call_:: ... Takes a list of code objects, which represent the arguments, decides on eager/lazy evaluation and does the right thing with the code objs.
 */
DEF_INSTR(promise_, 1, 0, 1, 0, 0)
/**
 * promise_:: take immediate CP index of Code, create promise & push on object
 * stack
 */
DEF_INSTR(close_, 0, 3, 1, 0, 0)
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

 TODO: check whether this is used or not?
 */
DEF_INSTR(pop_, 0, 1, 0, 0, 0)
/**
 * pop_:: pop from object stack
 */
DEF_INSTR(asast_, 0, 1, 1, 0, 0)
/**
 * asast_:: pop a promise off the object stack, push its AST on object stack

 TODO: we do not use now, might not work... why?
 */
DEF_INSTR(stvar_, 1, 1, 0, 0, 0)
/**
 * stvar_:: assign tos to the immediate symbol
 */
DEF_INSTR(asbool_, 0, 1, 1, 0, 0)
/**
 * asbool_:: pop object stack, convert to Logical vector of size 1 and push on object stack. Throws an error if the result would be NA.
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
 * invisible_:: set invisible flag, so that value will not be printed. This is a global flag, many operations implicitly clear it.
 */
DEF_INSTR(visible_, 0, 0, 0, 0, 0)
/**
 * visible_:: reset invisible flag
 */
DEF_INSTR(dup_, 0, 1, 2, 0, 0)
/**
 * dup_:: pop value from object stack, push it twice
 */
DEF_INSTR(dup2_, 0, 2, 4, 0, 0)
/**
 * dup2_:: a b -> a b a b
 */
DEF_INSTR(mul_, 0, 2, 1, 0, 0)
/**
 * add_:: pop two values from object stack, add them, push result on object
 * stack
 */
DEF_INSTR(add_, 0, 2, 1, 0, 0)
/**
 * add_:: pop two values from object stack, add them, push result on object
 * stack

 Works on any SEXP.
 */
DEF_INSTR(sub_, 0, 2, 1, 0, 0)
/**
 * add_:: pop two values from object stack, add them, push result on object
 * stack

 Works on any SEXP.
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

This should go away and be replaced with a guard, now it checks certain function is special, and if not asserts.

Actually it also checks for special, or builtin.
 */
DEF_INSTR(isfun_, 0, 1, 1, 0, 0)
/**
 * isfun_:: pop object stack, convert to RIR code or assert error, push code to
 * object stack
 */
DEF_INSTR(is_, 1, 1, 1, 0, 0)
/**
 * is_:: immediate type tag (SEXPTYPE), push T/F
 */
DEF_INSTR(extract1_, 0, 2, 1, 0, 0)
/**
 * extract1_:: do a[[b]], where a and b are on the stack
 */
DEF_INSTR(subset1_, 0, 2, 1, 0, 0)
/**
 * extract1_:: do a[b], where a and b are on the stack
 */
DEF_INSTR(brobj_, 1, 1, 1, 0, 0)
/**
 * brobj_:: branch if tos is object
 */
DEF_INSTR(dispatch_stack_, 4, -1, 1, 0, 0)
/**
 * dispatch_stack_:: similar to dispatch, but all args on stack
 */
DEF_INSTR(dispatch_, 4, 1, 1, 0, 0)
/**
 * dispatch_:: similar to call, but receiver is tos and 3rd immediate
 *             is selector
 */
DEF_INSTR(swap_, 0, 2, 2, 0, 0)
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
DEF_INSTR(call_stack_, 3, -1, 1, 0, 0)
/**
 * call_stack_:: like call, but arguments are taken from the stack
 *               immediate are number of args and names
 */
DEF_INSTR(uniq_, 0, 1, 1, 0, 0)
/**
 * uniq_:: duplicates tos if it is shared (ie. named > 1)
 */
DEF_INSTR(lgl_or_, 0, 2, 1, 0, 0)
/**
 * lgl_or_:: computes the logical (ternary) or of the two tos vals
 */
DEF_INSTR(lgl_and_, 0, 2, 1, 0, 0)
/**
 * lgl_and_:: computes the logical (ternary) and of the two tos vals
 */
DEF_INSTR(aslogical_, 0, 1, 1, 0, 0)
/**
 * aslogical_:: converts tos to a (ternary) logical
 */
DEF_INSTR(beginloop_, 1, 0, 1, 0, 0)
/**
 * beginloop_:: begins loop context, break and continue target immediate (this is the target for break and next long jumps)
 */
DEF_INSTR(endcontext_, 0, 1, 0, 0, 0)
/**
 * endcontext_:: ends a context. Takes a context as input and removes it from the stack (the context to be removed does not have to be top one)

TODO black magic here
 */
DEF_INSTR(inc_, 0, 1, 1, 0, 0)
/**
 * inc_ :: increment tos integer
 */
DEF_INSTR(test_bounds_, 0, 2, 3, 0, 0)
/**
 * bounds_check_ :: check stack[0] is a valid inded into vector at stack[1]
 */
DEF_INSTR(return_, 0, 1, 0, 0, 0)
/**
 * return_ :: return instruction. Non-local return instruction as opposed to ret_.
 */
DEF_INSTR(subassign_, 0, 3, 1, 0, 0)
/**
 * subassign_ :: [<-(a,b,c)
 */
DEF_INSTR(subassign2_, 1, 3, 1, 0, 0)
/**
 * subassign2_ :: [[<-(a,b,c)
 */
DEF_INSTR(missing_, 1, 0, 1, 0, 0)
/**
 * missing_ :: check if symb is missing

 TODO we want to rename to isMissing
 */
DEF_INSTR(seq_, 0, 3, 1, 0, 0)
/**
 * seq_ :: seq(scalar, scalar, scalar)
 */
DEF_INSTR(names_, 0, 1, 1, 0, 0)
// read out names of a vector
DEF_INSTR(length_, 0, 1, 1, 0, 0)
// get length of a vector
DEF_INSTR(set_names_, 0, 2, 1, 0, 0)
// set names of a vector, takes vector and names, puts vector back
DEF_INSTR(alloc_, 1, 1, 1, 0, 0)
// allocate vector. type immediate, length as integer on stack
DEF_INSTR(pull_, 1, 0, 1, 0, 0)
// copy a value from the stack. examples: pull(0) == dup(), pull(1) takes 2nd
// element on stack and pushes it
DEF_INSTR(int3_, 0, 0, 0, 0, 0)
// low-level breakpoint

#undef DEF_INSTR
