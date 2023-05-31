# FIXME: enable when finished
# A <- 1
#
# f <- function(x) {
#     x + A
# }
#
# # compile to RIR to record speculative contexts
# rir.compile(f)
#
# # initialize the speculative context
# f(1)
#
# rec <- recordings.eval(
#     # compile to PIR to trigger the compilation event
#     pir.compile(f)
# )
#
# stopifnot(length(rec) == 1)
# with(rec[[1]], {
#     stopifnot(name == "f")
#     stopifnot(env == ".GlobalEnv")
#     stopifnot(is.raw(closure))
#     # there should be three speculative contexts
#     # 1. for x
#     # 2. for A
#     # 3. for return of the add_ instruction
#     stopifnot(length(events[[1]]$speculative_contexts) == 3)
# })
#
# # PART 2:
#
# # change A to int so the speculation does not hold anymore
# A <- 1L
#
# rec <- recordings.eval({
#     # trigger deopt
#     f(2)
# })
#
# # one compilation event and one deopt
# stopifnot(length(rec) == 1)
# with(rec[[1]], {
#     stopifnot(name == "f")
#     stopifnot(length(events) == 2)
#     stopifnot(is(events[[2]], "event_deopt"))
#     # TODO: assert deopt
# })
#
# rm(f)
#
# recordings.replay(rec)
#
# stopifnot(is.function(f))
#
# rir.disassemble(f)
#
# recordings.reset()
#
# # there should no events after reset
# rec <- recordings.get()
# stopifnot(length(rec) == 0)
#
# rec <- recordings.eval({
#     f(2)
# })
#
# # FIXME: the following does not work
# ## there should be no events
# # stopifnot(length(rec) == 0)
# # FIXME: so instead we print this
# str(rec)
