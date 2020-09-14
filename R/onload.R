taxize_ds <- new.env() # nocov start
tx_itis_cache <- NULL
.onLoad <- function(libname, pkgname) {
  utils::data("rank_ref", package = "taxize", envir = taxize_ds)
  utils::data("rank_ref_zoo", package = "taxize", envir = taxize_ds)
  utils::data("theplantlist", package = "taxize", envir = taxize_ds)
  utils::data("apg_families", package = "taxize", envir = taxize_ds)
  utils::data("apg_orders", package = "taxize", envir = taxize_ds)
  utils::data("worrms_ranks", package = "taxize", envir = taxize_ds)
  
  # update taxa::db_ref
  taxa::db_ref$set("worms", url = "https://www.marinespecies.org/", id_regex = ".+")
  taxa::db_ref$set("tol", url = "https://tree.opentreeoflife.org/", id_regex = ".+")

  # cache object for itis lookup
  x <- hoardr::hoard()
  x$cache_path_set("taxize_itis")
  tx_itis_cache <<- x
} # nocov end
