f <- function() {
  gc(FALSE)
  1
}

for (i in 1:14)
  print(f())

# Above fails SOMETIMES
# Below code fails ALWAYS

f <- function(expr) {
  gc(FALSE)
  expr
}

for (i in 1:5)
  print(f(print(1)))