#' Capitalize the first letter of a character string.
#'
#' @export
#' @param s A character string
#' @param strict Should the algorithm be strict about capitalizing.
#'    Defaults to FALSE.
#' @param onlyfirst Capitalize only first word, lowercase all others. Useful
#'    for taxonomic names.
#' @examples
#' taxize_capwords(c("using AIC for model selection"))
#' taxize_capwords(c("using AIC for model selection"), strict=TRUE)

taxize_capwords <- function(s, strict = FALSE, onlyfirst = FALSE) {
  if (onlyfirst) {
    if (strict) {
      gsub("^(.)(.*)$", "\\U\\1\\L\\2", s, perl = TRUE)
    } else {
      gsub("^(.)", "\\U\\1", s, perl = TRUE)
    }
  } else {
    if (strict) {
      gsub("(?<=^| )(.)(.*)(?=$| )", "\\U\\1\\L\\2", s, perl = TRUE)
    } else {
      gsub("(?<=^| )(.)", "\\U\\1", s, perl = TRUE)
    }

  }
}
