#' A light wrapper around the taxonstand fxn to call Theplantlist.org database.
#' 
#' This function is deprecated.
#'
#' This function was a very light wrapper around TPLck anyway - we don't see further
#' reason to keep this function. Use the Taxonstand functions \code{\link[Taxonstand]{TPL}} 
#' and \code{\link[Taxonstand]{TPLck}}, directly. However, we are keeping two functions
#' that give you access to download Theplantlist.org data \code{\link[taxize]{tpl_get}} 
#' and to get Theplantlist.org families \code{\link[taxize]{tpl_families}}.
#'
#' @import Taxonstand plyr
#' @param taxon A taxonomic name, or a vector of names.
#' @param paral Paralellize or not (logical). Which back-end package you use 
#' 		depends on your operating system and just general taste. Possibilities 
#' 		include: snow, multicore, parallel, doMC, etc.
#' @param ... Further arguments passed on to the TPL or TPLck function of Taxonstand.
#' 		See \code{TPL} and \code{TPLck} for arguments.
#' @seealso \code{\link[taxize]{tpl_get}}, \code{\link[taxize]{tpl_families}}
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
#' @rdname tpl_search-deprecated
tpl_search <- function(taxon, paral = FALSE, ...)
{
  .Deprecated(msg="This function is deprecated, and will be removed in a future version of this package. Use the Taxonstand functions TPL and TPLck, directly.")
  
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
    df <- taxize_ldfast(out)
    df
  }
}