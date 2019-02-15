#' progressor
#'
#' @keywords internal
#' @examples
#' nms <- c("Quercus", "Sasdsfasdf")
#' x <- progressor$new(items = nms)
#' x
#' x$prog_start()
#'
#' x$completed(nms[1], "found")
#' x$prog_found()
#'
#' x$completed(nms[2], "not found")
#' x$prog_not_found()
#'
#' x$prog_summary()
progressor <- R6::R6Class(
  "taxon_state",
  public = list(
    total = 0,
    found = list(),
    not_found = list(),
    done = list(),

    initialize = function(items) {
      if (!missing(items)) self$total <- length(items)
    },
    completed = function(name, att) {
      switch(att,
        "found" = self$completed_found(name),
        "not found" = self$completed_not_found(name))
    },
    completed_found = function(name)
      self$found <- c(self$found, name),
    completed_not_found = function(name)
      self$not_found <- c(self$not_found, name),
    prog_start = function() {
      cli::cat_line(
        cli::rule(
          left = sprintf(" %s queries ", self$total),
          line = 2, line_col = "blue", width = 30)
      )
    },
    prog = function(att) {
      switch(att,
        "found" = self$prog_found(),
        "not found" = self$prog_not_found())
    },
    prog_found = function() {
      name <- private$last(self$found)
      cli::cat_line(
        paste(crayon::style(
          paste(private$sym$tick,  " Found: "), "green"), name)
      )
    },
    prog_not_found = function() {
      name <- private$last(self$not_found)
      cli::cat_line(
        paste(crayon::style(
          paste(private$sym$cross,  " Not Found: "), "red"), name)
      )
    },
    prog_summary = function() {
      cli::cat_line(
        cli::rule(left = " Results ", line = 2, line_col = "grey", width = 30),
        "\n"
      )
      cli::cat_line(
        paste(private$sym$bullet, "Total:",
          crayon::style(self$total, "green"), "\n"),
        paste(private$sym$bullet, "Found:",
          crayon::style(length(self$found), "green"), "\n"),
        paste(private$sym$bullet, "Not Found:",
          crayon::style(length(self$not_found), "green"))
      )
    }
  ),

  active = list(
    p = function() (length(self$done) / self$total) * 100,
    d = function() length(c(self$found, self$not_found))
  ),

  private = list(
    sym = list(
      bullet = cli::symbol$bullet,
      tick = cli::symbol$tick,
      cross = cli::symbol$cross
    ),
    last = function(x) x[length(x)]
  )
)
