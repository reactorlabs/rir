
f <- rir.compile(function(a=1) a)
g <- rir.compile(function() f(2))
h <- rir.compile(function() f())

l1 <- rir.compile(function() for (i in 1:10000) g())
l2 <- rir.compile(function() for (i in 1:10000) h())

l1()
l2()

rir.disassemble(h)
