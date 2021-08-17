f <- function(a=1) {print(a); missing(a)}

for (i in 1:10)
    stopifnot(f() == TRUE)


lsNamespaceInfo <- function(ns, ...) {
    ns <- asNamespace(ns, base.OK = FALSE)
    ls(..., envir = get(".__NAMESPACE__.", envir = ns, inherits = FALSE))
}
allinfoNS <- function(ns) sapply(lsNamespaceInfo(ns), getNamespaceInfo, ns=ns)
utils::str(allinfoNS("stats"))
utils::str(allinfoNS("stats4"))
