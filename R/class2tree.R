#' Convert list of classifications to a tree.
#'
#' This function converts a list of hierarchies for individual species into
#' a single species by taxonomic level matrix, then calculates a distance matrix
#' based on taxonomy alone, and outputs either a phylo or dist object. See details
#' for more information.
#'
#' @import plyr ape
#' @importFrom vegan taxa2dist
#' @param input List of classification data.frame's from the function classification().
#' @param varstep Vary step lengths between successive levels relative to proportional
#' loss of the number of distinct classes.
#' @param check	If TRUE, remove all redundant levels which are different for all rows
#' or constant for all rows and regard each row as a different basal taxon (species).
#' If FALSE all levels are retained and basal taxa (species) also must be coded as
#' variables (columns). You will get a warning if species are not coded, but you can
#' ignore this if that was your intention.
#' @param ... Further arguments passed on to hclust.
#' @param x Input object to print or plot - output from class2tree function.
#' @return An object of class "classtree" with slots:
#' \itemize{
#'  \item phylo - The resulting object, a phylo object
#'  \item classification - The classification data.frame, with taxa as rows, and
#'  different classification levels as columns
#'  \item distmat - Distance matrix from a call to \code{\link[vegan]{taxa2dist}}
#'  \item names - The names of the tips of the phylogeny
#' }
#'
#' Note that when you execute the resulting object, you only get the phylo object.
#' You can get to the other 3 slots by calling them directly, like output$names,
#' etc.
#' @details See \code{\link[vegan]{taxa2dist}}. Thanks to Jari Oksanen for making
#' the taxa2dist function and pointing it out, and Clarke & Warwick (1998, 2001),
#' which taxa2dist was based on.
#' @export
#' @examples \dontrun{
#' spnames <- c('Klattia flava', 'Trollius sibiricus', 'Arachis paraguariensis',
#'  'Tanacetum boreale', 'Gentiana yakushimensis','Sesamum schinzianum',
#'  'Pilea verrucosa','Tibouchina striphnocalyx','Lycium dasystemum',
#'  'Schoenus centralis','Berkheya echinacea','Androcymbium villosum',
#'  'Helianthus annuus','Madia elegans','Lupinus albicaulis','Poa annua',
#'  'Pinus lambertiana')
#' out <- classification(spnames, db='ncbi')
#' tr <- class2tree(out)
#' plot(tr)
#'
#' # another example using random sets of names with names_list() fxn
#' spnames <- names_list('species', 50)
#' out <- classification(spnames, db='ncbi')
#' tr <- class2tree(out)
#' plot(tr)
#' plot(tr, no.margin=TRUE)
#' }

class2tree <- function(input, varstep=TRUE, check=TRUE, ...)
{
  if(any(is.na(input))){
    message('Removed species without classification.')
    input <- input[!is.na(input)]
  }
  # Check that there is more than 2 taxon
  if(length(input) < 3)
    stop("Your input list of classifications must be 3 or longer.")
  dat <- rbind.fill(lapply(input, class2tree_helper))
  df <- dat <- dat[ , !apply(dat, 2, function(x) any(is.na(x)))]
  row.names(df) <- df[,1]
  df <- df[,-1]
  taxdis <- tryCatch(taxa2dist(df, varstep=varstep, check=check), error = function(e) e)
  # check for incorrect dimensions error
  if(is(taxdis, 'simpleError'))
    stop("Try check=FALSE, but see docs for taxa2dist function in the vegan package for details.")
  out <- as.phylo.hclust(hclust(taxdis, ...))
  res <- list(phylo = out, classification = dat, distmat = taxdis, names = names(input))
  class(res) <- 'classtree'
  return( res )
}

class2tree_helper <- function(x){
  x <- x[!x$rank == "no rank",]
  df <- x[-nrow(x), 'name']
  names(df) <- x[-nrow(x), 'rank']
  df <- data.frame(t(data.frame(df)))
  data.frame(tip = x[nrow(x),"name"], df)
}

#' @method plot classtree
#' @export
#' @rdname class2tree
plot.classtree <- function(x, ...)
{
  if(!is(x$phylo, "phylo"))
    stop("Input object must have a slot in 'phylo' of class 'phylo'")
  plot(x$phylo, ...)
}

#' @method print classtree
#' @export
#' @rdname class2tree
print.classtree <- function(x, ...)
{
  if(!is(x$phylo, "phylo"))
    stop("Input object must have a slot in 'phylo' of class 'phylo'")
  print(x$phylo)
}
