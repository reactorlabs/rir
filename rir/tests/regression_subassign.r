f = function() {
  a = c(1,2,3,4)
  a[[1L]] = 4
  a
}

for (i in 1:5)
  stopifnot(f() == c(4,2,3,4))



f = function() {
  a = c(1L,2L,3L,4L)
  a[[1L]] = 4L
  a
}

for (i in 1:5)
  stopifnot(f() == c(4L,2L,3L,4L))


f = function() {
  a = c(1L,2L,3L,4L)
  a[[1L]] = 4
  a
}

for (i in 1:5)
  stopifnot(f() == c(4,2,3,4))


f = function() {
  a = c(1,2,3,4)
  a[[1.999]] = 4
  a
}

for (i in 1:5)
  stopifnot(f() == c(4,2,3,4))


f = function() {
  a = 1
  a[[1L]] = 4
  a
}

for (i in 1:5)
  stopifnot(f() == 4)



f = function() {
  a = 1L
  a[[1L]] = 4L
  a
}

for (i in 1:5)
  stopifnot(f() == 4L)



f = function() {
  a = 1
  a[[1L]] = 4L
  a
}

for (i in 1:5)
  stopifnot(f() == 4L)



f = function() {
  a = 1L
  a[[1L]] = 4
  a
}

for (i in 1:5)
  stopifnot(f() == 4)


f = function() {
  a = 1L
  a[[1L]] = TRUE
  a
}

for (i in 1:5)
  stopifnot(f() == TRUE)


f = function() {
  a = TRUE
  a[[1L]] = 1L
  a
}

for (i in 1:5)
  stopifnot(f() == 1L)



f = function() {
  a = TRUE
  a[[1L]] = 1
  a
}

for (i in 1:5)
  stopifnot(f() == 1)


f = function() {
  a = 1L
  a[[1L]] = NA_real_
  a
}

for (i in 1:5)
  stopifnot(is.na(f()))

f = function() {
  a = c(1,2,3,4)
  a[[1L]] = NA_integer_
  a
}

for (i in 1:5)
  stopifnot(is.na(f()[[1]]))


