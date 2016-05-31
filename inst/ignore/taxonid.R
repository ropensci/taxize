library('R6')

MultiTaxonId <- R6Class(
  "MultiTaxonId",
  #inherit = TaxonId,
  public = list(
    ids = NA,
    print = function() {
      cat(paste0("<NCBI IDs> ", paste0(self$fetch_ids(), collapse = ", ")), sep = "")
      invisible(self)
    },
    get_ids = function(x) {
      tmp <- unclass(get_uid(x))
      taxa <- list()
      for (i in seq_along(tmp)) {
        taxa[[i]] <- TaxonId$new(tmp[i], x[i], attr(tmp, "uri")[i], attr(tmp, "match")[i])
      }
      self$ids <- taxa
    },
    fetch_uris = function() {
      res <- c()
      for (i in seq_along(self$ids)) res[i] <- self$ids[[i]]$uri
      return(res)
    },
    fetch_matches = function() {
      res <- c()
      for (i in seq_along(self$ids)) res[i] <- self$ids[[i]]$match
      return(res)
    },
    fetch_taxa = function() {
      res <- c()
      for (i in seq_along(self$ids)) res[i] <- self$ids[[i]]$taxa
      return(res)
    },
    fetch_ids = function() {
      res <- c()
      for (i in seq_along(self$ids)) res[i] <- self$ids[[i]]$id
      return(res)
    }
  )
)

TaxonId <- R6Class(
  "TaxonId",
  public = list(
    id = NA,
    taxa = NA,
    uri = NA,
    match = NA,
    initialize = function(id, taxa, uri, match) {
      if (!missing(id)) self$id <- id
      if (!missing(taxa)) self$taxa <- taxa
      if (!missing(uri)) self$uri <- uri
      if (!missing(match)) self$match <- match
    },
    print = function() {
      cat(paste0("<NCBI ID> ", self$id), "\n")
      cat(paste0("  taxon: ", self$taxa), "\n")
      cat(paste0("  match: ", self$match), "\n")
      cat(paste0("  uri: ", self$uri), "\n")
      invisible(self)
    }
  )
)

`[.MultiTaxonId` <- function(x, i) {
  x$ids[i]
}

`[[.MultiTaxonId` <- function(x, i) {
  x$ids[[i]]
}

`[.TaxonId` <- function(x, i) {
  x$id[i]
}

x <- MultiTaxonId$new()
x$get_ids(c("Chironomus riparius", "Quercus douglasii", "Poa annua"))
x
x$ids
x$fetch_ids()
x$fetch_uris()
x$fetch_matches()
x$fetch_taxa()

# subset
x$ids[[1]]
x$ids[3]
x$ids[[3]]
x$ids[2:3]
