taxon_state_env <- new.env()

#' Get the last taxon state object from a `get_*` function call
#' 
#' @export
#' @rdname taxon_state
#' @return an object of class `taxon_state`, the last one used, else `NULL`
#' if none found
taxon_last <- function() taxon_state_env$last

#' Keep track of queries in `get_*` functions
#'
#' This object lives inside each `get_*` function call, maintaining
#' results as they are accumulated.
#'
#' @export
#' @keywords internal
#' @format NULL
#' @usage NULL
#' @details
#' **Methods**
#'   \describe{
#'     \item{`add(query, result)`}{
#'       add a query with it's result
#'     }
#'     \item{`get(query)`}{
#'       get a result by query
#'     }
#'     \item{`remove(query)`}{
#'       remove a query
#'     }
#'     \item{`purge()`}{
#'       remove all records
#'     }
#'     \item{`count` (active binding)}{
#'       count number of records
#'     }
#'     \item{`exit` (active binding)}{
#'       record date/time function exited
#'     }
#'     \item{`remaining_taxa`}{
#'       get remaining taxa
#'     }
#'   }
#' @examples
#' ts <- taxon_state$new()
#' taxon_last()
#' ts
#' res <- list(
#'   id = 123456,
#'   att = "found",
#'   multiple = FALSE,
#'   direct = FALSE,
#'   class = "tsn"
#' )
#' ts$add(query = "Quercus robur", result = res)
#' ts
#' ts$get(query = "Quercus robur")
#' ts$count
#' ts$remove(query = "Quercus robur")
#' ts
#' ts$count
#'
#' res2 <- list(
#'   id = 3430834535,
#'   att = "found",
#'   multiple = FALSE,
#'   direct = FALSE,
#'   class = "gbifid"
#' )
#' ts$add(query = "Poa annua", result = res2)
#' res3 <- list(
#'   id = 1223424,
#'   att = "found",
#'   multiple = FALSE,
#'   direct = FALSE,
#'   class = "uid"
#' )
#' ts$add(query = "Puma concolor", result = res3)
#' ts
#' ts$count
#' ts$get("Puma concolor")
#' ts$get()
#'
#' # cleanup
#' ts$purge()
#' ts$count
taxon_state <- R6::R6Class(
  "taxon_state",
  public = list(
    initialized = NULL,
    finalized = NULL,
    class = NULL,
    names = NULL,

    initialize = function(class, names) {
      taxon_state_env$last <- self
      self$initialized <- Sys.time()
      if (!missing(class)) self$class <- class
      if (!missing(names)) self$names <- names
    },

    print = function(x, ...) {
      cat("<taxon state> ", sep = "\n")
      cat(paste0(" class: ", self$class %||% "none"), sep = "\n")
      if (!is.null(self$finalized)) {
        cat(paste0(" elapsed (sec): ",
          round(self$finalized - self$initialized, 2) %||% ""),
          sep = "\n")
      } else {
        cat(" elapsed (sec): 0", sep = "\n")
      }
      cat(paste0(" count: ", self$count %||% 0), sep = "\n")
      if (length(private$pool) > 0) {
        for (i in seq_along(private$pool)) {
          cat(sprintf("  %s: %s",
            names(private$pool)[i],
            private$pool[[i]]$id), sep = "\n")
        }
      } else {
        cat("  none ")
      }
      invisible(self)
    },
    
    add = function(query, result) private$pool[[query]] <- result,
    get = function(query = NULL) {
      if (is.null(query)) private$pool else private$pool[[query]]
    },
    remove = function(query) private$pool[[query]] <- NULL,
    purge = function() private$pool <- NULL,
    taxa_remaining = function() {
      done <- names(self$get())
      sort(self$names)[!sort(self$names) %in% sort(done)]
    },
    taxa_completed = function() {
      sort(names(self$get()))
    }
  ),
  active = list(
    count = function() length(private$pool),
    exit = function() self$finalized <- Sys.time()
  ),
  private = list(
    pool = list()
  )
)
