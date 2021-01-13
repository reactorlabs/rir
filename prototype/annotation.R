rir.annotatedFunction <- function(fun_name, fun_obj, annotations) {
    parameter_names <- names(annotations$eager)[annotations$eager]
    all_parameter_names <- names(formals(fun_obj))
    if (length(setdiff(parameter_names, all_parameter_names)) != 0) {
        stop("incorrect parameter names in eager annotation list")
    }
    force_call_creator <- function(parameter_name) {
        if(parameter_name == "...") {
            quote(list(...))
        }
        else {
            substitute(NAME <- NAME, list(NAME = as.name(parameter_name)))
        }
    }
    force_calls <- unname(Map(force_call_creator, parameter_names))
    new_fun_name <- as.symbol(paste0(as.character(fun_name), "_old"))
    symbolic_parameter_names <- unname(Map(as.name, all_parameter_names))
    new_body <- bquote({ { ..(force_calls) }
                         .(new_fun_name) <- .(fun_obj)
                         .(new_fun_name)(..(symbolic_parameter_names))
                       },
                       splice = TRUE)
    body(fun_obj) <- new_body
    fun_obj
}

f <- function(a, b, c, ...) a + b + c
f <- rir.annotatedFunction("f", f, list(eager = c(a = T, b = F, c = T)))
print(f)

g <- function(a, b, c, ...) { a + b + c }
g <- rir.annotatedFunction("g", g, list(eager = c(a = T, b = F, c = T, ... = T)))
print(g)

g(1,2,3, print("hello"))
