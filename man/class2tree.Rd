\name{class2tree}
\alias{class2tree}
\alias{plot.classtree}
\alias{print.classtree}
\title{Convert list of classifications to a tree.}
\usage{
class2tree(input, varstep = TRUE, check = TRUE, ...)

\method{plot}{classtree}(x, ...)

\method{print}{classtree}(x, ...)
}
\arguments{
  \item{input}{List of classification data.frame's from the
  function classification()}

  \item{varstep}{Vary step lengths between successive
  levels relative to proportional loss of the number of
  distinct classes.}

  \item{check}{If TRUE, remove all redundant levels which
  are different for all rows or constant for all rows and
  regard each row as a different basal taxon (species). If
  FALSE all levels are retained and basal taxa (species)
  also must be coded as variables (columns). You will get a
  warning if species are not coded, but you can ignore this
  if that was your intention.}

  \item{...}{Further arguments passed on to hclust.}
}
\value{
An object of class "classtree" with slots: \itemize{ \item
phylo - The resulting object, a phylo object \item
classification - The classification data.frame, with taxa
as rows, and different classification levels as columns
\item distmat - Distance matrix from a call to
\code{\link[vegan]{taxa2dist}} \item names - The names of
the tips of the phylogeny }

Note that when you execute the resulting object, you only
get the phylo object. You can get to the other 3 slots by
calling them directly, like output$names, etc.
}
\description{
This function converts a list of hierarchies for individual
species into a single species by taxonomic level matrix,
then calculates a distance matrix based on taxonomy alone,
and outputs either a phylo or dist object. See details for
more information.
}
\details{
See \code{\link[vegan]{taxa2dist}}. Thanks to Jari Oksanen
for making the taxa2dist function and pointing it out, and
Clarke & Warwick (1998, 2001), which taxa2dist was based
on.
}
\examples{
\dontrun{
spnames <- c('Klattia flava', 'Trollius sibiricus', 'Arachis paraguariensis',
 'Tanacetum boreale', 'Gentiana yakushimensis','Sesamum schinzianum',
 'Pilea verrucosa','Tibouchina striphnocalyx','Lycium dasystemum',
 'Schoenus centralis','Berkheya echinacea','Androcymbium villosum',
 'Helianthus annuus','Madia elegans','Lupinus albicaulis','Poa annua',
 'Pinus lambertiana')
out <- classification(spnames, db='ncbi')
tr <- class2tree(out)
plot(tr)

# another example using random sets of names with names_list() fxn
spnames <- names_list('species', 50)
out <- classification(spnames, db='ncbi')
out <- out[!is.na(out)]
tr <- class2tree(out)
plot(tr)
plot(tr, no.margin=TRUE)
}
}

