f <- function(x, y) x == y

regA <- c(1, 2, 3)
regB <- c(1, 4, 3)
nanA <- c(1, 2, NaN)
nanB <- c(1, 4, NaN)

# Get the type feedback to a non-NaN vector
f(regA, regB)
f(regA, regB)
f(regA, regB)
# Try with a NaN vector
f(nanA, nanB)
