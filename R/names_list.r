#' Get a random vector of species names.
#'
#' Family and order names come from the APG plant names list. Genus and
#' species names come from Theplantlist.org.
#'
#' @export
#' @param rank (character) Taxonomic rank, one of species, genus (default),
#' family, order
#' @param size (integer/numeric) Number of names to get. Maximum depends on
#' the rank
#' @return character vector of taxonomic names
#' @author Scott Chamberlain 
#' @examples
#' names_list()
#' names_list('species')
#' names_list('genus')
#' names_list('family')
#' names_list('order')
#' names_list('order', 2)
#' names_list('order', 15)
#'
#' # You can get a lot of genus or species names if you want
#' nrow(theplantlist)
#' names_list('genus', 500)
names_list <-  function(rank='genus', size=10) {
  assert(rank, "character")
  assert(size, c('integer', 'numeric'))
  getsp <- function(size){
    tmp <- apply(taxize_ds$theplantlist[
      sample(1:nrow(taxize_ds$theplantlist), size), c('genus','species')], 1,
      function(y) paste(y, collapse = " "))
    names(tmp) <- NULL
    tmp
  }
  switch(rank,
    family = as.character(sample(taxize_ds$apg_families$accepted_name, size)),
    order = as.character(sample(taxize_ds$apg_orders$accepted_name, size)),
    genus = sample(unique(taxize_ds$theplantlist$genus), size),
    species = getsp(size)
  )
}
