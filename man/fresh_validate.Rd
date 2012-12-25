\name{fresh_validate}
\alias{fresh_validate}
\title{Interface to the Taxa Validation Tool (TVT) of \url{http://www.freshwaterecology.info}}
\usage{
  fresh_validate(x)
}
\arguments{
  \item{x}{character; vector of (scientific) species
  names.}
}
\value{
  a data.frame with the status of submitted taxa.
}
\description{
  Check Species Names with the Taxa Validation Tool (TVT)
  from \url{http://www.freshwaterecology.info}.
}
\details{
  This also set up a temporary cookie, which is used in
  other funcions like \link{fresh_traits} or
  \link{fresh_codes}.
}
\note{
  Currently only the macro-invertebrate database is
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
a
}
}
\author{
  Eduard Szoecs \email{szoe8822@uni-landau.de}
}

