taxize_ds <- new.env()
.onLoad <- function(libname, pkgname) {
  utils::data("rank_ref", package = "taxize", envir = taxize_ds)
  utils::data("rank_ref_zoo", package = "taxize", envir = taxize_ds)
  utils::data("theplantlist", package = "taxize", envir = taxize_ds)
  utils::data("apg_families", package = "taxize", envir = taxize_ds)
  utils::data("apg_orders", package = "taxize", envir = taxize_ds)
  utils::data("worrms_ranks", package = "taxize", envir = taxize_ds)
}
