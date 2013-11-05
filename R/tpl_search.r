#' A light wrapper around the taxonstand fxn to call Theplantlist.org database.
#' 
#' @import Taxonstand plyr
#' @param taxon A taxonomic name, or a vector of names.
#' @param paral Paralellize or not (logical). Which back-end package you use 
#' 		depends on your operating system and just general taste. Possibilities 
#' 		include: snow, multicore, parallel, doMC, etc.
#' @param ... Further arguments passed on to the TPL or TPLck function of Taxonstand.
#' 		See \code{TPL} and \code{TPLck} for arguments.
#' @examples \donttest{
#' # Regular non-parallel
#' splist <- c("Heliathus annuus","Abies procera","Poa annua",
#'    "Platanus occidentalis","Carex abrupta","Arctostaphylos canescens",
#'    "Ocimum basilicum","Vicia faba","Quercus kelloggii","Lactuca serriola")
#' tpl_search(taxon = splist)
#' 
#' # Use more arguments within TPLck
#' tpl_search(taxon = "Microbryum curvicollum", corr = TRUE)
#' tpl_search(taxon = "Microbryum curvicollum", corr = TRUE, max.distance=5)
#' }
#' @export
tpl_search <- function(taxon, paral = FALSE, ...)
{
  if(paral){
    out <- llply(taxon, function(x) TPLck(x, ...), .parallel=TRUE)
    ldply(out)
  } else
  { 
    out <- llply(taxon, function(x) try(TPLck(x, ...), silent=TRUE))
#     if(fun == "TPL")
#       TPL(taxon, )
#     out <- lapply(taxon, function(x) try(eval(fun)(x, ...), silent=TRUE))
    if(any(sapply(out, class)=="try-error"))
      message(geterrmessage())
    out <- out[!sapply(out, class)=="try-error"]
    df <- ldfast(out)
    df
  }
}