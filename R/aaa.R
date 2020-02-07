# failwith replacment ------------------
default_try <- function(expr, default, quiet = FALSE){
  result <- default
  if (quiet) {
    tryCatch(result <- expr, error = function(e) NULL)
  } else {
    try(result <- expr)
  }
  result
}

fail_with <- function(default = NULL, f, quiet = FALSE){
  f <- match.fun(f)
  function(...) default_try(f(...), default, quiet = quiet)
}

# ldply replacement
# x <- list(a = 5, b = 6)
# nmdlst2df(x)
nmdlst2df <- function(x, stringsAsFactors = FALSE) {
  data.frame(.id = names(x), V1 = unname(unlist(x)),
    stringsAsFactors = stringsAsFactors)
}
