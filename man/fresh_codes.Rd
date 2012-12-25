\name{fresh_codes}
\alias{fresh_codes}
\title{Query species codes from \url{freshwaterbiology.info}}
\usage{
  fresh_codes(x)
}
\arguments{
  \item{x}{tvt-object; A object of class tvt as returned
  from \link{fresh_validate}.}
}
\value{
  A data.frame with species Codes (AQEM, DV, TCM, Furse,
  Perla, Ecoprof).
}
\description{
  Query species codes from \url{freshwaterbiology.info}
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
fresh_codes(a)
}
}
\author{
  Eduard Szoecs \email{szoe8822@uni-landau.de}
}

