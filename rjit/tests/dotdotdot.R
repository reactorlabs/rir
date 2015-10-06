require("rjit")

f <- jit.compile(function(...) {
      names(list(...))
  }
)
stopifnot(f() == NULL)
stopifnot(f(1) == NULL)
stopifnot(f(a=1) == c("a"))
stopifnot(f(a=1,b=2) == c("a","b"))
stopifnot(f(1,a=1,2) == c("","a",""))


f <- jit.compile(function(){
  n <- function(...) {
      names(list(...))
  }
  n(bla=1, bla=2)
})
stopifnot(f() == c("bla","bla"))

checkFont <- function(font) UseMethod("checkFont")
checkFont <- jit.compile(checkFont)
checkFont.Type1Font <- function(font) {
        if (is.null(font$family) || !is.character(font$family))
                    stop("invalid family name in font specification")
    if (is.null(font$metrics) || !is.character(font$metrics) ||
                length(font$metrics) < 4L) 
                stop("invalid metric information in font specification")
            ## Add default symbol font metric if none provided
        if (length(font$metrics) == 4L) 
                    font$metrics <- c(font$metrics, "Symbol.afm")
            if (is.null(font$encoding) || !is.character(font$encoding))
                        stop("invalid encoding in font specification")
                font
}
checkFont.Type1Font <- jit.compile(checkFont.Type1Font)


Type1Font <- function(family, metrics, encoding="default")
{
    font <- list(family=family, metrics=metrics, encoding=encoding)
    class(font) <- "Type1Font"
    checkFont(font)
}

pdfFonts <- function(...)
{
    ndots <- length(fonts <- list(...))
    fontNames <- names(fonts)
    fontNames
}
pdfFonts <- jit.compile(pdfFonts)

res <- pdfFonts(Helvetica = Type1Font("Helvetica",
                                      c("Helvetica.afm", "Helvetica-Bold.afm",
                                        "Helvetica-Oblique.afm", "Helvetica-BoldOblique.afm",
                                        "Symbol.afm")),
                Courier = Type1Font("Courier",
                                    c("Courier.afm", "Courier-Bold.afm",
                                      "Courier-Oblique.afm", "Courier-BoldOblique.afm",
                                      "Symbol.afm")))

print(res)
stopifnot(length(res) > 1)


a <- jit.compile(function(...) {
  b(...)
})

b <- jit.compile(function(...) {
  list(...)
})

print(a(3,4))
stopifnot(a(3,4) == c(3,4))
stopifnot(a(3,4) == c(3,4))

a <- jit.compile(function(..., a) {
  b(..., a + 1)
})

b <- jit.compile(function(...) {
  paste(...)
})

stopifnot(a(a=1,1,2,3,4) == "1 2 3 4 2")
stopifnot(a(a=1,1,2,3,4) == "1 2 3 4 2")
stopifnot(a(a=1,1,2,3,4) == "1 2 3 4 2")

a <- jit.compile(function(b, ..., a, d) {
  c(..., a + b, b+d, d+a)
})

c <- jit.compile(function(x, ...) {
  b(..., x)
})

b <- jit.compile(function(...) {
  paste(...)
})

print(a(a=10,20,33,895,-1,d=99))
stopifnot(a(a=10,20,33,895,-1,d=99) == "895 -1 30 119 109 33")
stopifnot(a(a=10,20,33,895,-1,d=99) == "895 -1 30 119 109 33")
stopifnot(a(a=10,20,33,895,-1,d=99) == "895 -1 30 119 109 33")

a <- jit.compile(function(...) {
    cat(...)
})
b <- jit.compile(function(...) {
    a(1)
})
stopifnot(b() == 1)
stopifnot(b() == 1)
stopifnot(b() == 1)
stopifnot(b() == 1)
