f <- function(x, y) x == y

regA <- c(1, 2, 3)
regB <- c(1, 4, 3)
nanA <- c(1, 2, NaN)
nanB <- c(1, 4, NaN)

# Get the type feedback to a non-NaN vector
f(regA, regB)
f(regA, regB)
f(regA, regB)
f(regA, regB)
# Try with a NaN vector
f(nanA, nanB)

f <- function(x, y) x == y

regA <- 1
names(regA) <- "foo"
regB <- 4
names(regB) <- "bar"
nanA <- NaN
names(nanA) <- "foo"
nanB <- NaN
names(nanB) <- "bar"

# Get the type feedback to a non-NaN attrib scalar
f(regA, regB)
f(regA, regB)
f(regA, regB)
f(regA, regB)
# Try with a NaN attrib scalar
f(nanA, nanB)