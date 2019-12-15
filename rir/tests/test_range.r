a = c(1L, 2L)

t1 = function() {i=0; a[i]}
stopifnot(length(t1()) == 0)
stopifnot(length(t1()) == 0)
stopifnot(length(t1()) == 0)
stopifnot(length(t1()) == 0)

t1 = function (i) if (i > 0) a[i]

stopifnot(is.null(t1(0)))
stopifnot(t1(1) == 1L)
stopifnot(is.null(t1(0)))
stopifnot(t1(1) == 1L)
stopifnot(is.null(t1(0)))
stopifnot(t1(1) == 1L)

t1 = function (i) if (i >= 0) a[i]

stopifnot(length(t1(0)) == 0)
stopifnot(t1(1) == 1L)
stopifnot(length(t1(0)) == 0)
stopifnot(t1(1) == 1L)
stopifnot(length(t1(0)) == 0)
stopifnot(t1(1) == 1L)


t1 = function (i) if (i < 1) 1 else a[i]

stopifnot(t1(1) == 1L)
stopifnot(t1(0) == 1L)
stopifnot(t1(1) == 1L)
stopifnot(t1(0) == 1L)
stopifnot(t1(1) == 1L)
stopifnot(t1(0) == 1L)

t1 = function (i) if (i < 0) 1 else a[i]

stopifnot(t1(1) == 1L)
stopifnot(length(t1(0)) == 0)
stopifnot(t1(1) == 1L)
stopifnot(length(t1(0)) == 0)
stopifnot(t1(1) == 1L)
stopifnot(length(t1(0)) == 0)

t1 = function (i) if (i <= 0) 1 else a[i]

stopifnot(t1(1) == 1L)
stopifnot(t1(0) == 1L)
stopifnot(t1(1) == 1L)
stopifnot(t1(0) == 1L)
stopifnot(t1(1) == 1L)
stopifnot(t1(0) == 1L)
