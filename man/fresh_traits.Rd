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
  A data.frame with ecological parameters for each taxon
}
\description{
  Query ecological parameters from
  \url{freshwaterbiology.info}. This function queries all
  parameters. If you want data only for specific traits use
  the \link{fresh_desc} table (see examples).
}
\note{
  Currently only the Macro-invertebrate database is
  supported.
}
\examples{
\dontrun{
spec <- c("Acentrella sinaica",
"Acentria ephemerella",
"Acilius sp.",
"Acroloxus lacustris",
"Allotrichi pallicornis")

a <- fresh_validate(spec)
a_traits <- fresh_traits(a)
a_traits

### select only current preference using lookup table
take <- fresh_desc$Modality[fresh_desc$Trait == 'current preference']
a_traits[ , take]
}
}
\author{
  Eduard Szoecs \email{szoe8822@uni-landau.de}
}

