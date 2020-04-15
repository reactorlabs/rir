if (Sys.getenv("R_ENABLE_JIT") == 0 || Sys.getenv("PIR_ENABLE") == "force" || Sys.getenv("PIR_ENABLE") == "off" || Sys.getenv("RIR_SERIALIZE_CHAOS") == "1")
  quit()

add_noinline1 <- rir.compile(function(a,b) a+b)
rir.markFunction(add_noinline1, DisableInline=TRUE)

add_noinline2 <- rir.compile(function(a,b) a+b)
rir.markFunction(add_noinline2, DisableInline=TRUE)

add_forceinline <- rir.compile(function(a,b) a+b)
rir.markFunction(add_forceinline, ForceInline=TRUE)

add_nospecial <- rir.compile(function(a,b) a+b)
rir.markFunction(add_nospecial, DisableInline=TRUE, DisableAllSpecialization=TRUE)

f1 <- function() add_noinline1(1,2)
f2 <- function() add_forceinline(1,2)
f3 <- function(b) {
    add_nospecial(1,2L)
    add_nospecial(1,2.2)
    a=2
    add_nospecial(1L,a)
    add_nospecial(1L,b)
}
f4 <- function(b) {
    add_noinline2(1,2L)
    add_noinline2(1,2.2)
    a=2
    add_noinline2(1L,a)
    add_noinline2(1,b)
}

x=1L
for (i in 1:10)
  f1()
for (i in 1:10)
  f2()
for (i in 1:100)
  f3(x)
for (i in 1:100)
  f4(x)

rir.disassemble(add_noinline1)
stopifnot(sum(rir.functionInvocations(add_noinline1)) == 10)
stopifnot(sum(rir.functionInvocations(add_nospecial)) > 10)
stopifnot(sum(rir.functionInvocations(add_forceinline)) <= 3)
stopifnot(length(rir.functionInvocations(add_nospecial)) == 2)
stopifnot(length(rir.functionInvocations(add_noinline2)) > 4)
