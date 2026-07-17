




f <- function() {

    v  <- g()
    v[i] <- g()
    v
    v
}

# f <- function() {

#     v <- h()
#     for (i in 1:4) {
#         v[i] <- g()
#         v
#     }
# }

# f <- function() {

#     v <- h()
#     for (i in 1:4) {
#         v[i] <- g()
#         v
#         v
#     }
# }



# f <- function() {

#     v <- h()
#     v[i] <- g()
#     v[i] <- h()


# }



# f <- function() {

#     v <- h()
#     for (i in 1:4) {
#         v
#         v[i] <- g()
#     }
# }




# f <- function() {

#     #v <- h()
#     for (i in 1:4) {
#         v
#         v
#         v[i] <- g()
#     }
# }


# f <- function() {
#     v <- h()
#     for (i in 1:n) {
#         v       # RecordAlways → useDef slot0
#         v[i] <- g()  # ldvarForUpdate → NoRecord(slot0), then stvar → trackDef erases useDef, registers def slot1
#         v       # d != nullptr (slot1), postDominates → NoRecord(slot1) ✓
#     }
# }


# f <- function() {
#     v <- g()
#     v
#     v

# }



rir.compile(f)
