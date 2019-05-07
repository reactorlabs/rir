count = 0

a <- 3

f <- rir.compile(function()
  for (i in 1:a) {
    count <<- count+1;
    i[[1]] <- 10L
  });

f()
f()
f()
f()

stopifnot(count == 12)
