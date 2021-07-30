s = 42

for(type in c("Wichmann-Hill", "Marsaglia-Multicarry", "Super-Duper",
              "Mersenne-Twister",
              "Knuth-TAOCP", "Knuth-TAOCP-2002"))
{
    set.seed(123, type)
    print(RNGkind())
    runif(100); print(runif(4))
    s = s * runif(1)
    set.seed(1000, type)
    runif(100); print(runif(4))
    s = s / runif(1)
    set.seed(77, type)
    runif(100); print(runif(4))
    s = s / runif(1)
}
print(s)
stopifnot(abs(s - 60127) < 0.1)
