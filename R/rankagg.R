#' Aggregate data by given taxonomic rank
#'
#' @export
#' @param data A data.frame. Column headers must have capitalized ranks (e.g.,
#'    Genus, Tribe, etc.) (data.frame)
#' @param datacol The data column (character)
#' @param rank Taxonomic rank to aggregate by (character)
#' @param fxn Arithmetic function or vector or functions (character)
#' @examples
#' if (require(vegan)) {
#' data(dune.taxon, dune, package='vegan')
#' dat <- dune.taxon
#' dat$abundance <- colSums(dune)
#' rankagg(data=dat, datacol="abundance", rank="Genus")
#' rankagg(data=dat, "abundance", rank="Family")
#' rankagg(data=dat, "abundance", rank="Genus", fxn="mean")
#' rankagg(data=dat, "abundance", rank="Subclass")
#' rankagg(data=dat, "abundance", rank="Subclass", fxn="sd")
#' }
rankagg <- function(data=NULL, datacol=NULL, rank=NULL, fxn="sum") {
  if (is.null(data) | is.null(rank))
    stop("You must specify your data.frame and taxonomic rank")
  rank <- match.arg(rank, choices = c('Species', 'Genus', 'Tribe', 'Family',
                                      'Order', 'Subclass', 'Class', 'Phylum',
                                      'Kingdom'))
  fn <- match.fun(fxn)
  out <- aggregate(data[ , datacol], list(data[ , rank]), fxn)
  names(out) = c(rank, fxn)
  out <- tibble::as_tibble(out)
  return(out)
}
