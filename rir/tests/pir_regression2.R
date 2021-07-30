print(callCC)
callCC <- rir.compile(callCC)
rir.disassemble(callCC)
callCC(function(k) 1)
callCC(function(k) k(1))
callCC(function(k) {k(1); 2})
callCC(function(k) repeat k(1))


# dealing with objects with no attributes
f12 <- function() {
    df <- data.frame(x=ts(c(41,42,43)), y=c(61,62,63))
  mf <- model.frame(df)
    # mf[["a"]] is an object without attributes
}

for (i in 1:10) {
    f12()
}
