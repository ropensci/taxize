taxon_state_env <- new.env()

#' Last taxon state object from a `get_*` function call
#' 
#' @export
#' @name taxon-state
#' @details 
#' 
#' - `taxon_last()`: get the last `taxon_state` object in use
#' - `taxon_clear()`: clear any data from last `taxon_state` object
#' 
#' The `taxon_state` object is an R6 object that holds data and methods 
#' used for keeping track of results gathered within a `get_*` function.
#' You shouldn't create `taxon_state` R6 objects yourself.
#' 
#' Behaviors to be aware of:
#' 
#' - If a `taxon_state` object is not passed you don't need to worry about 
#' a previously run `get_*` function interfering with another `get_*` 
#' function call - you have to explicitly pass a `taxon_state` object
#' to use `taxon_state`
#' - The passed in `taxon_state` object must have a `$class` matching that of
#' the `get_*` function being called. For example, you can only pass a
#' `taxon_state` with `$class` of `gbifid` to `get_gbifid()`, and so on.
#' - If you run `taxon_clear()` while a `get*` function is running, you may 
#' lose track of any state known to this package before it was cleared
#' 
#' See the internal method [progressor] for information on how we control messages
#' in `get*` functions
#' 
#' @return `taxon_last()` returns an object of class `taxon_state`, the last
#' one used, else `NULL` if none found. `taxon_clear()` clears the saved state
#' 
#' @examples
#' spp <- names_list("species", 3)
#' res <- get_gbifid(spp)
#' z <- taxon_last()
#' z
#' z$taxa_remaining()
#' z$taxa_completed()
#' z$count # active binding; no parens needed
#' 
#' # cleanup
#' taxon_clear()
taxon_last <- function() taxon_state_env$last

#' @export
#' @rdname taxon-state
taxon_clear <- function() taxon_state_env$last <- NULL

#' Keep track of queries in `get_*` functions
#'
#' This object lives inside each `get_*` function call, maintaining
#' results as they are accumulated.
#'
#' @keywords internal
#' @format NULL
#' @usage NULL
#' @param class (character) a class name (e.g., "gbif")
#' @param names (character) one or more taxon names
#' @details
#' **Methods**
#'   \describe{
#'     \item{`add(query, result)`}{
#'       add a record with it's result; duplicates allowed
#'       - query (character), a taxon name
#'       - result (list) a named list
#'     }
#'     \item{`get(query)`}{
#'       get all records matching 'query'
#'       - query (character), a taxon name
#'     }
#'     \item{`remove(query)`}{
#'       remove's all records matching 'query'
#'       - query (character), a taxon name
#'     }
#'     \item{`purge()`}{
#'       removes all records
#'     }
#'     \item{`taxa_remaining()`}{
#'       get remaining taxa
#'     }
#'     \item{`taxa_completed()`}{
#'       get remaining taxa
#'     }
#'     \item{`count` (active binding)}{
#'       count number of records
#'     }
#'     \item{`exit` (active binding)}{
#'       record date/time function exited
#'     }
#'   }
#' @examples \dontrun{
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
#' }
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
    
    add = function(query, result) {
      assert(query, "character")
      assert(result, "list")
      private$pool <- c(private$pool, stats::setNames(list(result), query))
    },
    get = function(query = NULL) {
      if (is.null(query)) return(private$pool)
      private$pool[names(private$pool) %in% query]
    },
    remove = function(query) {
      private$pool[names(private$pool) %in% query] <- NULL
    },
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
