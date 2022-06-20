#ifndef DEF_INSTR
#error "DEF_INSTR must be defined before including insns.h"
#endif

// DEF_INSTR(name, imm, pop, push)

/**
 * invalid_:: Opcode 0 acts as a sentinnel for unintialiazed Code.
 */
DEF_INSTR(invalid_, 0, 0, 0)

/**
 * nop_:: do nothing.
 */
DEF_INSTR(nop_, 0, 0, 0)

/*
 * clear_binding_cache_. Clear binding cache entries from start to start+size
 * (two
 * immediates).
 */
DEF_INSTR(clear_binding_cache_, 2, 0, 0)

/**
 * ldfun_:: take immediate CP index of symbol, find function bound to that name
 * and push it on stack.
 */
DEF_INSTR(ldfun_, 1, 0, 1)

/**
 * ldvar_:: take immediate CP index of symbol, finding binding in env and push.
 */
DEF_INSTR(ldvar_, 1, 0, 1)

/**
 * ldvar_noforce_:: like ldvar.
 * If a promise is found, it is not forced before it's returned
 */
DEF_INSTR(ldvar_noforce_, 1, 0, 1)

/**
 * ldvar_:: like ldvar.
 * Stores an additional immediate with a unique number for the cache bindings.
 */
DEF_INSTR(ldvar_cached_, 2, 0, 1)

/**
 * ldvar_:: like ldvar.
 * Additionally Increment named count if the variable is not local.
 */
DEF_INSTR(ldvar_for_update_cache_, 2, 0, 1)
DEF_INSTR(ldvar_for_update_, 1, 0, 1)

/**
 * ldvar_super_:: take immediate CP index of symbol, finding binding in
 * enclosing env and push.
 */
DEF_INSTR(ldvar_super_, 1, 0, 1)

/**
 * ldddvar_:: loads the ellipsis values (such as ..1, ..2) and pushes them on
 * stack.
 */
DEF_INSTR(ldddvar_, 1, 0, 1)

/**
 * stvar_:: assign tos to the immediate symbol. We know it was not previously
 * cached.
 */
DEF_INSTR(stvar_, 1, 1, 0)

/**
 * stvar_cache:: like stvar but the var may be in the cache.
 */
DEF_INSTR(stvar_cached_, 2, 1, 0)

/**
 * stvar_super_:: assign tos to the immediate symbol, lookup starts in the
 * enclosing environment
 */
DEF_INSTR(stvar_super_, 1, 1, 0)

/**
 * call_:: Call instruction. Takes n arguments on the stack
 *         on top of the callee; these arguments can be
 *         values, promises (even preseeded w/ a value), or R_MissingValue for
 *         explicitly missing arguments.
 */
DEF_INSTR(call_, 4, -1, 1)

/*
 * named_call_:: Same as above, but with names for the arguments as immediates
 *               THIS IS A VARIABLE LENGTH INSTRUCTION
 *               the actual number of immediates is 3 + nargs
 */
DEF_INSTR(named_call_, 4, -1, 1)

/*
 * call_dots_:: This instruction is like named_call_, but additionally it
 *              allows the caller to pass R_DotsSymbol as an argument. This
 *              argument will be expanded (on the stack) with the contents of
 *              `...` and passed to the callee.
 */
DEF_INSTR(call_dots_, 4, -1, 1)

/**
 * call_builtin_:: Like static call, but calls a builtin
 */
DEF_INSTR(call_builtin_, 3, -1, 1)

/**
 * close_:: pop body and argument list, create closure, and push on object stack
 */
DEF_INSTR(close_, 0, 3, 1)

/**
 * check_function_:: pop object stack, convert to RIR code or assert error, push
 * code to object stack
 */
DEF_INSTR(check_function_, 0, 1, 1)

/**
 * promise_:: take immediate CP index of Code, create promise & push on object
 * stack
 */
DEF_INSTR(mk_eager_promise_, 1, 1, 1)
DEF_INSTR(mk_promise_, 1, 0, 1)

/**
 * force_:: pop from objet stack, evaluate, push promise's value
 */
DEF_INSTR(force_, 0, 1, 1)

/**
 * push_:: take immediate CP index, and push CP value on object stack.
 */
DEF_INSTR(push_, 1, 0, 1)

/**
 * dup_:: pop value from object stack, push it twice
 */
DEF_INSTR(dup_, 0, 1, 2)

/**
 * dup2_:: a b -> a b a b
 */
DEF_INSTR(dup2_, 0, 2, 4)

/**
 * pop_:: pop from object stack
 */
DEF_INSTR(pop_, 0, 1, 0)

/**
 * popn_:: pop n elements from object stack
 */
DEF_INSTR(popn_, 1, -1, 0)

/**
 * swap_:: swap two elements tos
 */
DEF_INSTR(swap_, 0, 2, 2)

/**
 * put_:: put tos at the n-th pos in the stack
 */
DEF_INSTR(put_, 1, 0, 0)

/**
 * pick_:: remove n-th element on stack and push it tos
 */
DEF_INSTR(pick_, 1, 0, 0)

/**
 * pull_ :: copy a value from the stack. examples: pull(0) == dup(), pull(1)
 takes 2nd element on stack and pushes it
 */
DEF_INSTR(pull_, 1, 0, 1)

/**
 * inc_ :: increment tos integer
 */
DEF_INSTR(inc_, 0, 1, 1)

/**
 * uplus_:: unary plus
 */
DEF_INSTR(uplus_, 0, 1, 1)
DEF_INSTR(uminus_, 0, 1, 1)

/**
 * not_:: unary negation operator !
 */
DEF_INSTR(not_, 0, 1, 1)

/**
 * add_:: pop two values from object stack, add them, push result on object
 * stack. Works on any SEXP.
 */
DEF_INSTR(add_, 0, 2, 1)
DEF_INSTR(sub_, 0, 2, 1)
DEF_INSTR(mul_, 0, 2, 1)
DEF_INSTR(div_, 0, 2, 1)
DEF_INSTR(idiv_, 0, 2, 1)
DEF_INSTR(mod_, 0, 2, 1)
DEF_INSTR(pow_, 0, 2, 1)

/**
 * eq_:: relational operator <
 */
DEF_INSTR(eq_, 0, 2, 1)
DEF_INSTR(ne_, 0, 2, 1)
DEF_INSTR(lt_, 0, 2, 1)
DEF_INSTR(le_, 0, 2, 1)
DEF_INSTR(gt_, 0, 2, 1)
DEF_INSTR(ge_, 0, 2, 1)

/**
 * lgl_and_:: computes the logical (ternary) and of the two tos vals
 */
DEF_INSTR(lgl_and_, 0, 2, 1)

/**
 * lgl_or_:: computes the logical (ternary) or of the two tos vals
 */
DEF_INSTR(lgl_or_, 0, 2, 1)

/**
 * colon_:: takes two bounds a and b and pushes a:b
 */
DEF_INSTR(colon_, 0, 2, 1)

/**
 * as_switch_idx_ :: silently convert tos to integer, -1 if non-int
 */
DEF_INSTR(as_switch_idx_, 0, 1, 1)

DEF_INSTR(identical_noforce_, 0, 2, 1)

/**
 * aslogical_:: converts tos to a (ternary) logical
 */
DEF_INSTR(aslogical_, 0, 1, 1)

/**
 * asbool_:: pop object stack, convert to Logical vector of size 1 and push on
 * object stack. Throws an error if the result would be NA.
 */
DEF_INSTR(asbool_, 0, 1, 1)

/**
 * colon_input_effects_ :: pushes false if we can't compile a simple for loop.
 * Otherwise pushes true, and checks the lhs and rhs to throw errors which colon
 * would.
 */
DEF_INSTR(colon_input_effects_, 0, 2, 3)

/**
 * colon_cast_lhs_ :: Converts lhs of colon for simple for loop
 */
DEF_INSTR(colon_cast_lhs_, 0, 1, 1)

/**
 * colon_cast_rhs_ :: Converts rhs of colon for simple for loop
 */
DEF_INSTR(colon_cast_rhs_, 0, 2, 2)

/**
 * is_:: immediate type tag (SEXPTYPE), push T/F
 */
DEF_INSTR(is_, 1, 1, 1)

/**
 * missing_ :: check if symb is missing
 */
DEF_INSTR(missing_, 1, 0, 1)

/**
 * brtrue_:: pop object stack, if TRUE branch to immediate offset
 */
DEF_INSTR(brtrue_, 1, 1, 0)

/**
 * brfalse_:: pop object stack, if FALSE branch to immediate offset
 */
DEF_INSTR(brfalse_, 1, 1, 0)

/**
 * br_:: branch to immediate offset
 */
DEF_INSTR(br_, 1, 0, 0)

/**
 * extract1_1_:: do a[b], where a and b are on the stack and a is no obj
 */
DEF_INSTR(extract1_1_, 0, 2, 1)

/**
 * extract1_2_:: do a[b,c], where a, b and c are on the stack and a is no obj
 */
DEF_INSTR(extract1_2_, 0, 3, 1)

/**
 * extract1_3_:: do a[b,c,d], where a, b and c are on the stack and a is no obj
 */
DEF_INSTR(extract1_3_, 0, 4, 1)

/**
 * subassign1_1_ :: a[b] <- c
 *
 * this instruction creates the rhs part of a <- `[<-(a,b,c)` and still needs
 * to be assigned.
 *
 * Warning: on named == 1 it updates the array in-place! This is not the same
 * as GNUR's subassign, and it's not equivalent to `[<-(a,b,c)` itself
 */
DEF_INSTR(subassign1_1_, 0, 3, 1)

/**
 * subassign1_2_ :: a[b,c] <- d
 *
 * this instruction creates the rhs part of a <- `[<-(a,b,c,d)` and still needs
 * to be assigned.
 *
 * Warning: on named == 1 it updates the array in-place! This is not the same
 * as GNUR's subassign, and it's not equivalent to `[<-(a,b,c, d)` itself
 */
DEF_INSTR(subassign1_2_, 0, 4, 1)

/**
 * subassign1_3_ :: a[b,c,d] <- e
 */
DEF_INSTR(subassign1_3_, 0, 5, 1)

/**
 * extract2_1_:: do a[[b]], where a and b are on the stack and a is no obj
 */
DEF_INSTR(extract2_1_, 0, 2, 1)

/**
 * extract2_2_:: do a[[b,c]], where a, b and c are on the stack and a is no obj
 */
DEF_INSTR(extract2_2_, 0, 3, 1)

/**
 * set_vec_elt_:: do SET_VECTOR_ELT(x, i, v)
 */
DEF_INSTR(set_vec_elt_, 0, 3, 1)

/**
 * subassign2_1 :: a[[b]] <- c
 *
 * this instruction creates the rhs part of a <- `[[<-(a,b,c)` and still needs
 * to be assigned.
 *
 * Warning: on named == 1 it updates the array in-place! This is not the same
 * as GNUR's subassign, and it's not equivalent to `[[<-(a,b,c)` itself
 */
DEF_INSTR(subassign2_1_, 0, 3, 1)

/**
 * subassign2_2_ :: a[[b,c]] <- d
 *
 * this instruction creates the rhs part of a <- `[[<-(a,b,c,d)` and still needs
 * to be assigned.
 *
 * Warning: on named == 1 it updates the array in-place! This is not the same
 * as GNUR's subassign, and it's not equivalent to `[[<-(a,b,c,d)` itself
 */
DEF_INSTR(subassign2_2_, 0, 4, 1)

/**
 * guard_fun_:: takes symbol, target, id, checks findFun(symbol) == target
 */
DEF_INSTR(guard_fun_, 3, 0, 0)

/**
 * names_ :: read out names of a vector
 */
DEF_INSTR(names_, 0, 1, 1)

/**
 * set_names_ :: set names of a vector, takes vector and names, puts vector back
 */
DEF_INSTR(set_names_, 0, 2, 1)

/**
 * length_ :: get length of a small vector
 */
DEF_INSTR(length_, 0, 1, 1)

/**
 * for_seq_size_ :: get size of the for loop sequence
 */
DEF_INSTR(for_seq_size_, 0, 0, 1)

/**
 * visible_:: reset invisible flag
 */
DEF_INSTR(visible_, 0, 0, 0)

/**
 * invisible_:: set invisible flag, so that value will not be printed. This is a
 * global flag, many operations implicitly clear it.
 */
DEF_INSTR(invisible_, 0, 0, 0)

/**
 * set_shared:: ensures tos has named >= 2
 */
DEF_INSTR(set_shared_, 0, 1, 1)

/**
 * ensure_named_:: ensures tos has named >= 1
 */
DEF_INSTR(ensure_named_, 0, 1, 1)

/**
 * beginloop_:: begins loop context, break and continue target immediate (this
 * is the target for break and next long jumps)
 */
DEF_INSTR(beginloop_, 1, 0, 0)

/**
 * endloop_:: end marker for a loop with context

 */
DEF_INSTR(endloop_, 0, 0, 0)

/**
 * return_ :: return instruction. Non-local return instruction as opposed to
 * ret_.
 */
DEF_INSTR(return_, 0, 1, 0)

/**
 * ret_:: terminates execution and pops result off object stack
 */
DEF_INSTR(ret_, 0, 1, 0)

/*
 * recording bytecodes are used to collect information
 * They keep a struct from RuntimeFeedback.h inline, that's why they are quite
 * heavy in size.
 */
DEF_INSTR(record_call_, 4, 1, 1)
DEF_INSTR(record_type_, 1, 1, 1)
DEF_INSTR(record_test_, 1, 1, 1)

DEF_INSTR(int3_, 0, 0, 0)

#undef DEF_INSTR
