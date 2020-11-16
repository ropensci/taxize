taxize_ds <- new.env() # nocov start
tx_itis_cache <- NULL
.onLoad <- function(libname, pkgname) {
  utils::data("rank_ref", package = "taxize", envir = taxize_ds)
  utils::data("rank_ref_zoo", package = "taxize", envir = taxize_ds)
  utils::data("theplantlist", package = "taxize", envir = taxize_ds)
  utils::data("apg_families", package = "taxize", envir = taxize_ds)
  utils::data("apg_orders", package = "taxize", envir = taxize_ds)
  utils::data("worrms_ranks", package = "taxize", envir = taxize_ds)

  # update taxa2::db_ref
  taxa2::db_ref$set("worms", url = "https://www.marinespecies.org/", 
    desc = "World Register of Marine Species ", id_regex = ".+")
  taxa2::db_ref$set("tol", url = "https://tree.opentreeoflife.org/",
    desc = "Open Tree of Life", id_regex = ".+")
  taxa2::db_ref$set("iucn", url = "https://www.iucnredlist.org/",
    desc = "The IUCN Red List of Threatened Species", id_regex = ".+")
  taxa2::db_ref$set("natserv", url = "https://www.natureserve.org/",
    desc = "Nature Serve", id_regex = ".+")
  taxa2::db_ref$set("pow", url = "http://plantsoftheworldonline.org/",
    desc = "Plants of the World", id_regex = "urn:lsid:ipni.org:names:[0-9]{5,8}-[12]")

  # cache object for itis lookup
  x <- hoardr::hoard()
  x$cache_path_set("taxize_itis")
  tx_itis_cache <<- x
} # nocov end
