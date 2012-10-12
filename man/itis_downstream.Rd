\name{itis_downstream}
\alias{itis_downstream}
\title{Retrieve all taxa names or TSNs downstream in hierarchy from given TSN.}
\usage{
  itis_downstream(searchtsn = NA, attachrank = TRUE,
    outtaxlevel = c("Kingdom", "Subkingdom", "Phylum", "Subphylum", "Superclass", "Class", "Subclass", "Infraclass", "Superorder", "Order", "Suborder", "Infraorder", "Superfamily", "Family", "Subfamily", "Tribe", "Subtribe", "Genus", "Subgenus", "Species", "Subspecies", "Variety", "Infrakingdom", "Division", "Subdivision", "Infradivision", "Section", "Subsection", "Subvariety", "Form", "Subform", "Race", "Stirp", "Morph", "Aberration", "Unspecified"))
}
\arguments{
  \item{searchtsn}{Quoted TSN for a taxonomic group
  (character).}

  \item{outtaxlevel}{If not specified, function returns
  only direct children.  Other options = any quoted
  taxonomic level below that specified in searchtsn, e.g.,
  'genus' if searchtsn specifies a family}

  \item{attachrank}{Should rank be attached to the output?
  (default=TRUE).}
}
\value{
  Names or TSNs of all downstream taxa in a data.frame.
}
\description{
  Retrieve all taxa names or TSNs downstream in hierarchy
  from given TSN.
}
\examples{
\dontrun{
dat2 <- itis_downstream(searchtsn = 180415, outtaxlevel = 'Species') # the superclass Osteichthyes
}
}
\author{
  Scott Chamberlain {myrmecocystus@gmail.com}
}

