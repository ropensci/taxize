\name{tpl_search}
\alias{tpl_search}
\title{A light wrapper around the taxonstand fxn to call Theplantlist.org database.}
\usage{
  tpl_search(taxon, paral = FALSE, ...)
}
\arguments{
  \item{taxon}{A taxonomic name, or a vector of names.}

  \item{paral}{Paralellize or not (logical). Which back-end
  package you use depends on your operating system and just
  general taste. Possibilities include: snow, multicore,
  parallel, doMC, etc.}

  \item{...}{Further arguments passed on to the TPLck
  function of Taxonstand.  See \code{TPLck} for arguments.}
}
\description{
  A light wrapper around the taxonstand fxn to call
  Theplantlist.org database.
}
\examples{
\donttest{
# Regular non-parallel
splist <- c("Heliathus annuus","Abies procera","Poa annua","Platanus occidentalis",
		"Carex abrupta","Arctostaphylos canescens","Ocimum basilicum","Vicia faba",
		"Quercus kelloggii","Lactuca serriola")
tpl_search(taxon = splist)

# In parallel, using package doMC
library(doMC)
registerDoMC(cores=4)
tpl_search(taxon = splist, TRUE)

## in parallel is much faster
system.time( out <- tpl_search(taxon = splist) )
registerDoMC(cores=4)
system.time( out2 <- tpl_search(taxon = splist, TRUE) )

# Use more arguments within TPLck
tpl_search(taxon = "Microbryum curvicollum", corr = TRUE)
tpl_search(taxon = "Microbryum curvicollum", corr = TRUE, max.distance=5)
}
}

