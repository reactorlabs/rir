require("rjit")
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
