#' Get a random vector of species names.
#' 
#' Family and order names come from the APG plant names list. Genus and species names
#' come from Theplantlist.org.
#' 
#' @param rank Taxonomic rank, one of species, genus (default), family, order. 
#' @param size Number of names to get. Maximum depends on the rank.
#' @return Vector of taxonomic names.
#' @author Scott Chamberlain \email{myrmecocystus@@gmail.com}
#' @export
#' @examples
#' names_list()
#' names_list('species')
#' names_list('genus')
#' names_list('family')
#' names_list('order')
#' names_list('order', '2')
#' names_list('order', '15')
#' 
#' # You can get a lot of genus or species names if you want
#' nrow(theplantlist)
#' names_list('genus', 500)
names_list <-  function(rank='genus', size=10)
{
  getsp <- function(size){
    tmp <- apply(theplantlist[sample(1:nrow(theplantlist), size), c('genus','sp')], 1, 
                 function(y) paste(y, collapse = " "))
    names(tmp) <- NULL
    tmp
  }
  switch(rank,
         family = as.character(sample(apg_families$this, size)),
         order = as.character(sample(apg_orders$this, size)),
         genus = sample(unique(theplantlist$genus), size),
         species = getsp(size)
         )
}