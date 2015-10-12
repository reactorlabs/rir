require("rjit")

f <- jit.compile(
    function(x) {
        log <- "asdf"
        log <- strsplit(log, NULL)[[1L]]
        if("d" %in% log && any(ii <- x <= 0 & !is.na(x)))
            sum(ii)})

stopifnot(f(c(1,0,2,3,NA)) == 1)

s <- eval(jit.compile(quote(c(a="b"))))
stopifnot(s == c(a="b"))

f <- jit.compile(function() {
    c(a = "asdf")})
stopifnot(f() == c(a="asdf"))
stopifnot(f() == c(a="asdf"))


test <- function(id, code, o, e, w) {
    code <- jit.compile(substitute(code))
    jit.enable()

    stopifnot(o == eval(code, envir=new.env()))
}

expected <- structure(c(-0.0296690260968828, 0.200337918547016, -0.38901358729166, 
0.076054310915896, -0.5953576286578, 1.55058467328697, -0.189955959788191, 
-1.31965097077132, 0.596281133731208, 1.22982396127581, -0.0296690260968828, 
0.200337918547016, -0.38901358729166, 0.076054310915896, -0.5953576286578, 
1.55058467328697, -0.189955959788191, -1.31965097077132, 0.596281133731208, 
1.22982396127581), .Dim = c(10L, 2L), .Dimnames = list(NULL, 
    c("runif.10...pi.2..pi.2.", "runif.10...pi.2..pi.2.")))
test(id=164, code={
argv <- list(structure(c(-0.0296690260968828, 0.200337918547016, -0.38901358729166, 
0.076054310915896, -0.5953576286578, 1.55058467328697, -0.189955959788191, 
-1.31965097077132, 0.596281133731208, 1.22982396127581), .Dim = c(10L, 
1L), .Dimnames = list(NULL, "runif.10...pi.2..pi.2."), circularp = structure(list(
    type = "angles", units = "radians", template = "none", modulo = "asis", 
    zero = 0, rotation = "counter"), .Names = c("type", "units", 
"template", "modulo", "zero", "rotation")), class = c("circular", 
"matrix")), structure(c(-0.0296690260968828, 0.200337918547016, 
-0.38901358729166, 0.076054310915896, -0.5953576286578, 1.55058467328697, 
-0.189955959788191, -1.31965097077132, 0.596281133731208, 1.22982396127581
), .Dim = c(10L, 1L), .Dimnames = list(NULL, "runif.10...pi.2..pi.2."), circularp = structure(list(
    type = "angles", units = "radians", template = "none", modulo = "asis", 
    zero = 0, rotation = "counter"), .Names = c("type", "units", 
"template", "modulo", "zero", "rotation")), class = c("circular", 
"matrix")))
do.call('cbind', argv);
},  o = expected);

jit.enable()

require(graphics)

jit.compile(function() {
    xx <- 2:7
    nu <- seq(-10, 9, length.out = 2001)
    op <- par(lab = c(16, 5, 7))
    matplot(nu, t(outer(xx, nu, besselI)), type = "l", ylim = c(-50, 200),
            main = expression(paste("Bessel ", I[nu](x), " for fixed ", x,
                                    ",  as ", f(nu))),
            xlab = expression(nu))

    x0 <- 2^(-20:10)
    plot(x0, x0^-8, log = "xy", ylab = "", type = "n",
        main = "Bessel Functions  J_nu(x)  near 0\n log - log  scale")
})()
