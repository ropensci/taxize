#' Print a subset of a character vector
#'
#' Prints the start and end values for a character vector. The number of values
#' printed depend on the width of the screen by default.
#'
#' @param chars (`character`) What to print.
#' @param prefix (`character` of length 1) What to print before
#'   `chars`, on the same line.
#' @param max_chars (`numeric` of length 1) The maximum number of
#'   characters to print.
#' @param type (`"error"`, `"warning"`, `"message"`, `"cat"`, `"print"`, `"silent"`)
#'
#' @return `NULL`
#' @keywords internal
limited_print <- function(chars, prefix = "",
                          max_chars = getOption("width") - nchar(prefix) - 5,
                          type = "message") {

  if (length(chars) == 0) {
    cat(prefix)
    return(invisible(NULL))
  }


  # https://stat.ethz.ch/pipermail/r-help/2006-March/101023.html
  interleave <- function(v1,v2) {
    ord1 <- 2*(1:length(v1))-1
    ord2 <- 2*(1:length(v2))
    c(v1,v2)[order(c(ord1,ord2))]
  }

  q = "'"
  interleaved <- interleave(chars[1:(length(chars) / 2)],
                            rev(chars[(length(chars) / 2 + 1):length(chars)]))
  is_greater_than_max <- cumsum(nchar(interleaved) + 2) + 10 > max_chars
  if (all(! is_greater_than_max)) {
    max_printed <- length(chars)
  } else {
    max_printed <- which.max(is_greater_than_max)
  }
  if (max_printed < length(chars)) {
    first_part <-  chars[1:as.integer(max_printed / 2 - 0.5)]
    second_part <-
      chars[as.integer(length(chars) - (max_printed / 2) + 1.5):length(chars)]
    output <- paste0(paste0(collapse = ", ", first_part),
                     " ... ",
                     paste0(collapse = ", ", second_part),
                     "\n")
  } else {
    output <- paste0(paste0(collapse = ", ", chars), "\n")
  }
  output <- paste(prefix, output, collapse = "")

  if (type == "error") {
    stop(output)
  } else if (type == "warning") {
    warning(output)
  } else if (type == "message") {
    message(output)
  } else if (type == "cat") {
    cat(output)
  } else if (type == "print") {
    message(paste0(utils::capture.output(output), collapse = "\n"))
  } else if (type != "silent") {
    stop("invalid type option")
  }
  return(invisible(output))
}
