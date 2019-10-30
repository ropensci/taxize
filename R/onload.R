.onLoad <- function(libname, pkgname) {
  utils::data("rank_ref", package = "taxize")
  utils::data("theplantlist", package = "taxize")
  utils::data("apg_families", package = "taxize")
  utils::data("apg_orders", package = "taxize")
}
