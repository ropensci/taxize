\name{fresh_traits}
\alias{fresh_traits}
\title{Query ecological parameters from \url{freshwaterbiology.info}}
\usage{
  fresh_traits(x)
}
\arguments{
  \item{x}{tvt-object; A object of class tvt as returned
  from \link{fresh_validate}.}
}
\value{
  A list of two data.frames: \item{citations}{Citations
  information for the retrieved data, see notes.}
  \item{traits}{Ecological parameters for each taxon. See
  \link{fresh_desc} for a description.}
}
\description{
  Query ecological parameters from
  \url{freshwaterbiology.info}. This function queries all
  parameters. If you want data only for specific traits use
  the \link{fresh_desc} table (see examples).
}
\note{
  Currently only the macro-invertebrate database is
  supported.

  The use of ecological information of individual taxonomic
  groups and individual ecological parameters must always
  be cited as returned by this function (see value). You
  can easily export the citation data to a .csv-file.
}
\examples{
\dontrun{
spec <- c("Acentrella sinaica",
"Acentria ephemerella",
"Acilius sp.",
"Acroloxus lacustris",
"Allotrichi pallicornis")

### Query freshwaterecology.info
a <- fresh_validate(spec)
a_traits <- fresh_traits(a)

a_traits$citations
a_traits$traits

### select only current preference using lookup table
take <- fresh_desc$Modality[fresh_desc$Trait == 'current preference']
a_traits$traits[ , take]
}
}
\author{
  Eduard Szoecs \email{szoe8822@uni-landau.de}
}
\references{
  Schmidt-Kloiber A. & Hering D. (eds.) (2012):
  www.freshwaterecology.info - the taxa and autecology
  database for freshwater organisms, version 5.0..
}

