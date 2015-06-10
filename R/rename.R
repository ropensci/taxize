rename <- function (x, replace, warn_missing = TRUE) {
  names(x) <- revalue(names(x), replace, warn_missing = warn_missing)
  x
}

revalue <- function (x, replace = NULL, warn_missing = TRUE) {
  if (!is.null(x) && !is.factor(x) && !is.character(x)) {
    stop("x is not a factor or a character vector.")
  }
  mapvalues(x, from = names(replace), to = replace, warn_missing = warn_missing)
}

mapvalues <- function (x, from, to, warn_missing = TRUE) {
  if (length(from) != length(to)) {
    stop("`from` and `to` vectors are not the same length.")
  }
  if (!is.atomic(x)) {
    stop("`x` must be an atomic vector.")
  }
  if (is.factor(x)) {
    levels(x) <- mapvalues(levels(x), from, to, warn_missing)
    return(x)
  }
  mapidx <- match(x, from)
  mapidxNA <- is.na(mapidx)
  from_found <- sort(unique(mapidx))
  if (warn_missing && length(from_found) != length(from)) {
    message("The following `from` values were not present in `x`: ",
            paste(from[!(1:length(from) %in% from_found)], collapse = ", "))
  }
  x[!mapidxNA] <- to[mapidx[!mapidxNA]]
  x
}

